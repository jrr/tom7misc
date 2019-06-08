
#include <vector>
#include <string>
#include <cstdint>
#include <unordered_set>
#include <time.h>
#include <stdio.h>

#include "../cc-lib/crypt/aes.h"
#include "../cc-lib/crypt/sha256.h"
#include "../cc-lib/arcfour.h"
#include "../cc-lib/crypt/cryptrand.h"

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "../cc-lib/base64.h"
#include "../cc-lib/util.h"

using namespace std;
using uint8 = uint8_t;
using uint64 = uint64_t;

#if defined(__MINGW32__) || defined(__MINGW64__)
# include <windows.h>
#else
# include <termios.h>
# include <unistd.h>
#endif

template<class F>
static void DisableEchoExcursion(F f) {
#if defined(__MINGW32__) || defined(__MINGW64__)

  // On windows running through cmd.exe, we have to manage this by
  // setting properties of the console. But if running inside bash,
  // then it's actually the wrapping shell that is echoing, so
  // we need to tell it to stop with stty. We do both, here.
  system("stty -echo");
  
  HANDLE hStdin = GetStdHandle(STD_INPUT_HANDLE); 
  DWORD old_mode;
  GetConsoleMode(hStdin, &old_mode);

  DWORD new_mode = old_mode & ~ENABLE_ECHO_INPUT;
  SetConsoleMode(hStdin, new_mode);

  f();

  SetConsoleMode(hStdin, old_mode);
  system("stty echo");
  
#else
  struct termios tty;
  tcgetattr(STDIN_FILENO, &tty);
  if (enable) tty.c_lflag |= ECHO;
  else tty.c_lflag &= ~ECHO;

  (void)tcsetattr(STDIN_FILENO, TCSANOW, &tty);
#endif
}


// Assumes vector is high-entropy data, and at least 64 bits long.
namespace {
struct HashHash {
  size_t operator ()(const std::vector<uint8> &v) const noexcept {
    return *(uint64 *)v.data();
  }
};
}

static std::vector<uint8> CryptRandom(int bytes) {
  CryptRand cr;
  std::vector<uint8> ret;
  ret.reserve(bytes);
  // PERF!
  for (int i = 0; i < bytes; i++) ret.push_back(cr.Byte());
  return ret;
}

static bool WipeFile(const string &filename) {
  // Careful on the mode here; some modes will 
  FILE *f = fopen(filename.c_str(), "rb+");
  if (!f) return false;
  struct OnReturn {
    OnReturn(FILE *f) : ff(f) {}
    ~OnReturn() { if (ff != nullptr) fclose(ff); }
    FILE *ff = nullptr;
  };
  OnReturn ae(f);

  CryptRand cr;
  ArcFour rc(StringPrintf("%llx.%llx", cr.Word64(), (uint64)time(nullptr)));
  rc.Discard(2049);

  // seek_end and ftell are not really portable, but neither is our
  // assumption that the data will be overwritten in place...
  if (-1 == fseek(f, 0, SEEK_END)) return false;
  int64 sz = ftell(f);
  // fprintf(stderr, "Wiping %lld...\n", sz);
  
  if (-1 == fseek(f, 0, SEEK_SET)) return false;
  for (int i = 0; i < sz; i++)
    if (EOF == fputc(0xAA, f)) return false;
  if (-1 == fseek(f, 0, SEEK_SET)) return false;
  for (int i = 0; i < sz; i++)
    if (EOF == fputc(0x55, f)) return false;
  if (-1 == fseek(f, 0, SEEK_SET)) return false;
  for (int i = 0; i < sz; i++)
    if (EOF == fputc(rc.Byte(), f)) return false;

  // fprintf(stderr, "OK\n");
  return true;
}

// Keyed hash.
// This is basically SHA256(key :: SHA256(key :: passphrase)), with
// the two keys being modified by flipping some of their bits.
static std::vector<uint8> HMAC_SHA256(const string &passphrase,
				      const std::vector<uint8> &key) {
  // It's good for this code to be efficient, since we run it hundreds
  // of thousands of times. We should assume an attacker has the
  // fastest possible implementation of this.

  CHECK(key.size() == 32);
  std::vector<uint8> lkey, rkey;
  lkey.resize(64);
  rkey.resize(64);

  for (int i = 0; i < 32; i++) {
    lkey[i] = key[i] ^ 0x5c;
    rkey[i] = key[i] ^ 0x36;
  }
  for (int i = 0; i < 32; i++) {
    lkey[32 + i] = 0x5c;
    rkey[32 + i] = 0x36;
  }
  
  SHA256::Ctx ctx;
  // Inner hash SHA256(rkey :: passphrase)
  SHA256::Init(&ctx);
  SHA256::Update(&ctx, rkey.data(), rkey.size());
  SHA256::UpdateString(&ctx, passphrase);
  std::vector<uint8> inner = SHA256::FinalVector(&ctx);

  // Outer hash SHA256(lkey :: inner)
  SHA256::Init(&ctx);
  SHA256::Update(&ctx, lkey.data(), lkey.size());
  SHA256::Update(&ctx, inner.data(), inner.size());
  return SHA256::FinalVector(&ctx);
}


template<int LEN>
static void XorInto(vector<uint8> &dest, const vector<uint8> &src) {
  static_assert(LEN % 8 == 0, "using uint64 for this...");
  constexpr int words = LEN / 8;
  uint64 *dest64 = (uint64 *)dest.data();
  const uint64 *src64 = (uint64 *)src.data();
  // PERF unroll?
  for (int i = 0; i < words; i++) {
    dest64[i] ^= src64[i];
  }
}

// Get 32-byte AES-256 key from passphrase and file-specific salt.
// This is like PBKDF2 (NIST 800-132).
static std::vector<uint8> GetKey(const string &passphrase,
				 const std::vector<uint8> &salt) {
  // PBKDF2 would create a series of chunks to fill the
  // key's width, but here SHA-256 produces a 32-byte key, which
  // is exactly the width we need. So we just have one chunk.

  // Takes about 500ms on high-end desktop in 2019.
  static constexpr int C = 300000;
  
  // T starts as 0, and we keep XOR-ing into it.
  std::vector<uint8> key(32, 0);
  
  CHECK(salt.size() == 32);
  std::vector<uint8> u = salt;
  for (int i = 0; i < C; i++) {
    std::vector<uint8> hash = HMAC_SHA256(passphrase, u);
    XorInto<32>(key, hash);
    u = std::move(hash);
  }
  return key;
}

// 128 bits in base64. This takes 22 characters formally, but always
// has two bytes of padding with =. So we actually leave that off.
static int LINE_IV_BASE64_LENGTH = 22;
// Same for the encrypted payload, which is formally 88 characters but
// has two = padding characters.
static int LINE_PAYLOAD_BASE64_LENGTH = 86;

static bool IsBase64String(const string &s) {
  for (int i = 0; i < s.size(); i++)
    if (!Base64::IsBase64Char(s[i]))
      return false;
  return true;
}

static string Encrypt(const string &passphrase,
		      const string &contents) {
  std::vector<string> lines = Util::SplitToLines(contents);
  CHECK(lines.size() > 0);
  string saltspec = lines[0];
  CHECK(Util::chop(saltspec) == "salt") << "First line of the file "
    "needs to specify the salt.";
  string salt_str = Util::chop(saltspec);
  CHECK(salt_str.size() == 44) << "Should have 44 bytes of base64-encoded "
    "salt now.";

  std::vector<uint8> salt = Base64::DecodeV(salt_str);
  CHECK(salt.size() == 32) << "Invalid base64 salt? " << salt.size();
  string result = StringPrintf("salt %s\n", salt_str.c_str());
  fflush(stdout);
  
  std::unordered_set<std::vector<uint8>, HashHash> seen_ivs;

  std::vector<uint8> key = GetKey(passphrase, salt);
  
  // Now, each line may start with some base64-encoded salt. We recognize this
  // by finding a | separator in the appropriate column.
  for (int lineno = 1; lineno < lines.size(); lineno++) {
    string &line = lines[lineno];

    std::vector<uint8> iv;
    if (line.size() >= LINE_IV_BASE64_LENGTH + 1 &&
	line[LINE_IV_BASE64_LENGTH] == '|') {
      string iv_base64 = line.substr(0, LINE_IV_BASE64_LENGTH);
      iv_base64 += "==";
      CHECK(IsBase64String(iv_base64)) << "Found apparent IV but it is not "
	"base64: " << iv_base64;
      line = line.substr(LINE_IV_BASE64_LENGTH + 1, string::npos);
      iv = Base64::DecodeV(iv_base64);
    } else {
      iv = CryptRandom(16);
    }
    CHECK(iv.size() == 16);

    CHECK(seen_ivs.find(iv) == seen_ivs.end()) << "Found a duplicate IV (" <<
      Base64::EncodeV(iv) << ")!\nThis weakens security. Just delete the IV "
      "from the line and fresh one will be generated.";
    
    // To encrypt, we need a multiple of the block in bytes.
    // To avoid leaking information, we actually use a fixed size for each line.
    // We only really need to support lines (including iv) up to 80 characters,
    // so this is 80 - (22 + 1) = 57 characters, which rounds up to 64 bytes.

    // To simplify matters, we strip any trailing whitespace.
    line = Util::LoseWhiteR(line);
    
    CHECK(line.size() <= 64) << "An input line must be at most 64 characters, "
      "not including the iv| prefix. But found one of length " << line.size();

    AES256::Ctx ctx;
    AES256::InitCtxIV(&ctx, key.data(), iv.data());
    vector<uint8> data(64, (uint8)' ');
    for (int i = 0; i < line.size(); i++) {
      data[i] = line[i];
    }
    AES256::EncryptCBC(&ctx, data.data(), data.size());
    string iv64 = Base64::EncodeV(iv);
    CHECK(iv64.length() == LINE_IV_BASE64_LENGTH + 2) << iv64.size()
						      << "\n" << iv64;
    CHECK(iv64[iv64.length() - 1] == '=');
    CHECK(iv64[iv64.length() - 2] == '=');
    iv64.resize(LINE_IV_BASE64_LENGTH);
    string enc = Base64::EncodeV(data);
    CHECK(enc.size() == LINE_PAYLOAD_BASE64_LENGTH + 2) << enc.size();
    CHECK(enc[enc.length() - 1] == '=');
    CHECK(enc[enc.length() - 2] == '=');
    enc.resize(LINE_PAYLOAD_BASE64_LENGTH);
    
    string outline = iv64 + "|" + enc;
    StringAppendF(&result, "%s\n", outline.c_str());
  }
  return result;
}

static string FlattenDecrypted(
    const string &header,
    const std::vector<std::pair<string, string>> &lines) {
  string result = header;
  for (const auto &p : lines) {
    StringAppendF(&result, "%s|%s\n", p.first.c_str(), p.second.c_str());
  }
  return result;
}

static string HumanOnly(const std::vector<std::pair<string, string>> &lines) {
  string result;
  for (const auto &p : lines) {
    result += p.second;
    result.push_back('\n');
  }
  return result;
}
  
// Decrypts, returning false (or aborting) if something is wrong with
// the file.
static bool Decrypt(const string &passphrase, const string &contents,
		    string *header,
		    std::vector<std::pair<string, string>> *lines_out) {
  std::vector<string> lines = Util::SplitToLines(contents);
  CHECK(lines.size() > 0);
  string saltspec = lines[0];
  CHECK(Util::chop(saltspec) == "salt") << "First line of the file "
    "needs to specify the salt.";
  string salt_str = Util::chop(saltspec);
  CHECK(salt_str.size() == 44) << "Should have 44 bytes of base64-encoded "
    "salt now.";
  std::vector<uint8> salt = Base64::DecodeV(salt_str);
  CHECK(salt.size() == 32) << "Invalid base64 salt? " << salt.size();

  std::vector<uint8> key = GetKey(passphrase, salt);
  *header = StringPrintf("salt %s\n", salt_str.c_str());
  
  // Decrypting is simpler because every line must have exactly the
  // same format, which is LINE_IV_BASE64_LENGTH characters of
  // base64-encoded IV, then |, then LINE_PAYLOAD_BASE64_LENGTH
  // characters of base64-encoded payload.
  for (int lineno = 1; lineno < lines.size(); lineno++) {
    string &line = lines[lineno];
    // Allow and ignore totally blank lines.
    if (line.empty()) continue;
    
    CHECK(line.size() ==
	  LINE_IV_BASE64_LENGTH + 1 + LINE_PAYLOAD_BASE64_LENGTH &&
	  line[LINE_IV_BASE64_LENGTH] == '|') << "Invalid line: " << line;

    string iv_base64 = line.substr(0, LINE_IV_BASE64_LENGTH) + "==";

    CHECK(IsBase64String(iv_base64)) << "Found apparent IV but it is not "
      "base64: " << iv_base64;

    string payload_base64 =
      line.substr(LINE_IV_BASE64_LENGTH + 1, string::npos) + "==";

    std::vector<uint8> iv = Base64::DecodeV(iv_base64);
    CHECK(iv.size() == 16);

    std::vector<uint8> payload = Base64::DecodeV(payload_base64);
    CHECK(payload.size() == 64);
    
    // Decrypt the line with the IV.
    AES256::Ctx ctx;
    AES256::InitCtxIV(&ctx, key.data(), iv.data());
    AES256::DecryptCBC(&ctx, payload.data(), payload.size());
    
    // Strip trailing whitespace.
    int sz = payload.size();
    while (sz > 0 && payload[sz - 1] == ' ') sz--;

    // We assume this produces ascii (e.g. no premature nul characters).
    string decoded;
    decoded.reserve(sz);
    for (int i = 0; i < sz; i++) decoded.push_back(payload[i]);

    // Drop == padding.
    iv_base64.resize(LINE_IV_BASE64_LENGTH);
    lines_out->emplace_back(iv_base64, decoded);

    // For the first line, verify that it contains a correct preimage
    // of the salt. We use this instead of some specific known string
    // so that we are able to verify successful decryption (an
    // attacker too) but admits no "known plaintext."
    if (lineno == 1) {
      if (!IsBase64String(decoded)) {
	fprintf(stderr, "Preimage not base64.\n");
	return false;
      }
      
      std::vector<uint8> preimage = Base64::DecodeV(decoded);
      std::vector<uint8> image = SHA256::HashVector(preimage);
      if (image != salt) {
	fprintf(stderr, "Preimage does not hash to salt.\n");
	return false;
      }
    }
  }
  return true;
}

// Read passphrase from console. 
static string ReadPass() {
  fprintf(stderr, "Password: ");
  fflush(stderr);
  string pass;
  DisableEchoExcursion([&]() {
      // HANDLE hStdin = GetStdHandle(STD_INPUT_HANDLE); 
      getline(cin, pass);
    });
  return pass;
}


int main(int argc, char **argv) {
  CHECK(argc >= 3);

  string cmd = argv[1];
  string file = argv[2];
  if (cmd == "enc") {
    const string pass = ReadPass();

    string plaintext = Util::ReadFile(file);
    string enctext = Encrypt(pass, plaintext);
    printf("%s", enctext.c_str());
    
  } else if (cmd == "dec") {
    const string pass = ReadPass();
	
    string enctext = Util::ReadFile(file);
    string header;
    std::vector<std::pair<string, string>> lines;
    CHECK(Decrypt(pass, enctext, &header, &lines)) <<
      "Failed to decrypt: " << file;
    string plaintext = FlattenDecrypted(header, lines);

    printf("%s", plaintext.c_str());

  } else if (cmd == "cat") {

    const string pass = ReadPass();
	
    string enctext = Util::ReadFile(file);
    string header;
    std::vector<std::pair<string, string>> lines;
    CHECK(Decrypt(pass, enctext, &header, &lines)) <<
      "Failed to decrypt: " << file;
    string plaintext = HumanOnly(lines);
    
    printf("%s", plaintext.c_str());
    
  } else if (cmd == "emacs") {
    const string pass = ReadPass();
    
    CryptRand cr;
    const string tmpname = StringPrintf("deleteme-%llx.txt", cr.Word64());
    const string enctext = Util::ReadFile(file);
    // If we had the wrong password (typo), it's important that we
    // detect it here, or else we'll reencrypt the gibberish with the
    // wrong password.

    string header;
    std::vector<std::pair<string, string>> lines;
    CHECK(Decrypt(pass, enctext, &header, &lines)) <<
      "Failed to decrypt: " << file;
    string plaintext = FlattenDecrypted(header, lines);
    Util::WriteFile(tmpname, plaintext);

    // "emacs -nw"
    std::system(StringPrintf("notepad %s", tmpname.c_str()).c_str());

    string newplain = Util::ReadFile(tmpname);
    // Note we can use a version that merges IVs from old, and checks
    // for IV reuse too. XXX if this fails, we might want to be
    // clearer about where the data is currently stored?
    string newenc = Encrypt(pass, newplain);
    Util::WriteFile(file, newenc);
    if (!WipeFile(tmpname))
      fprintf(stderr, "Warning: Couldn't wipe %s\n", tmpname.c_str());
    
    CHECK(Util::remove(tmpname));

  } else if (cmd == "new") {
    const string pass = ReadPass();
    
    // Generate a random salt preimage, and then a random salt from it.
    std::vector<uint8> preimage = CryptRandom(32);
    std::vector<uint8> salt = SHA256::HashVector(preimage);

    // We generate a decrypted file.
    string salt64 = Base64::EncodeV(salt);
    string preimage64 = Base64::EncodeV(preimage);
    string contents = StringPrintf("salt %s\n"
				   "%s\n",
				   salt64.c_str(), preimage64.c_str());
    string enc = Encrypt(pass, contents);
    Util::WriteFile(file, enc);

  } else if (cmd == "wipe") {

    CHECK(WipeFile(file)) << file;
    
  } else {
    LOG(FATAL) << "Unknown command " << cmd;
  }
  
  return 0;
}
