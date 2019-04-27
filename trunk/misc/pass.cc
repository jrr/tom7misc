
#include <vector>
#include <string>
#include <cstdint>
#include <unordered_set>

#include "../cc-lib/crypt/aes.h"
#include "../cc-lib/crypt/sha256.h"
#include "../cc-lib/crypt/cryptrand.h"

#include "../cc-lib/base/logging.h"
#include "../cc-lib/base/stringprintf.h"

#include "../cc-lib/base64.h"
#include "../cc-lib/util.h"

using namespace std;
using uint8 = uint8_t;
using uint64 = uint64_t;

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

// Keyed hash.
// This is basically SHA256(key :: SHA256(key :: passphrase)), with
// the two keys being modified by flipping some of their bits.
static std::vector<uint8> HMAC_SHA256(const string &passphrase,
				      const std::vector<uint8> &key) {
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

  // printf("L: %s\n", SHA256::Ascii(lkey).c_str());
  // printf("R: %s\n", SHA256::Ascii(rkey).c_str());  
  
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
void XorInto(vector<uint8> &dest, const vector<uint8> &src) {
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
    // printf("U: %s\n", SHA256::Ascii(u).c_str());
    std::vector<uint8> hash = HMAC_SHA256(passphrase, u);
    // printf("H: %s\n", SHA256::Ascii(hash).c_str());
    // for (int x = 0; x < 32; x++)
    // key[x] ^= hash[x];
    XorInto<32>(key, hash);
    u = std::move(hash);
  }
  return key;
}

// 128 bits in base64. This takes 22 characters formally, but always has two bytes
// of padding with =. So we actually leave that off.
static int LINE_IV_BASE64_LENGTH = 22;
// Same for the encrypted payload, which is formally 88 characters but has two =
// padding characters.
static int LINE_PAYLOAD_BASE64_LENGTH = 86;

void Encrypt(const string &passphrase,
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
  printf("salt %s\n", salt_str.c_str());
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
      for (int i = 0; i < iv_base64.size(); i++) {
	CHECK(Base64::IsBase64Char(iv_base64[i])) << "Found apparent IV but it is not "
	  "base64: " << iv_base64;
      }
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
    
    CHECK(line.size() <= 64) << "An input line must be at most 64 characters, not "
      "including the iv| prefix. But found one of length " << line.size();

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
    printf("%s\n", outline.c_str());
  }
}

void Decrypt(const string &passphrase, const string &contents) {
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
  printf("salt %s\n", salt_str.c_str());
  
  // Decrypting is simpler because every line must have exactly the same
  // format, which is LINE_IV_BASE64_LENGTH characters of base64-encoded IV,
  // then |, then LINE_PAYLOAD_BASE64_LENGTH characters of base64-encoded payload.
  for (int lineno = 1; lineno < lines.size(); lineno++) {
    string &line = lines[lineno];
    // Allow and ignore totally blank lines.
    if (line.empty()) continue;
    
    CHECK(line.size() == LINE_IV_BASE64_LENGTH + 1 + LINE_PAYLOAD_BASE64_LENGTH &&
	  line[LINE_IV_BASE64_LENGTH] == '|') << "Invalid line: " << line;

    string iv_base64 = line.substr(0, LINE_IV_BASE64_LENGTH) + "==";

    for (int i = 0; i < iv_base64.size(); i++) {
      CHECK(Base64::IsBase64Char(iv_base64[i])) << "Found apparent IV but it is not "
	"base64: " << iv_base64;
    }

    string payload_base64 = line.substr(LINE_IV_BASE64_LENGTH + 1, string::npos) + "==";

    std::vector<uint8> iv = Base64::DecodeV(iv_base64);
    CHECK(iv.size() == 16);

    std::vector<uint8> payload = Base64::DecodeV(payload_base64);
    CHECK(payload.size() == 64);
    
    // Decrypt the line with the IV.
    AES256::Ctx ctx;
    AES256::InitCtxIV(&ctx, key.data(), iv.data());
    AES256::DecryptCBC(&ctx, payload.data(), payload.size());

    /*
    for (int i = 0; i < 64; i++) {
      printf("%02x ", payload[i]);
    }
    */
    
    // Strip trailing whitespace.
    int sz = payload.size();
    while (sz > 0 && payload[sz - 1] == ' ') sz--;

    // We assume this produces ascii (e.g. no premature nul characters).
    string decoded;
    decoded.reserve(sz);
    for (int i = 0; i < sz; i++) decoded.push_back(payload[i]);

    // Drop == padding.
    iv_base64.resize(LINE_IV_BASE64_LENGTH);
    printf("%s|%s\n", iv_base64.c_str(), decoded.c_str());
  }
}


int main(int argc, char **argv) {
  CHECK(argc >= 3);
  /*
  std::vector<uint8> salt;
  CHECK(SHA256::UnAscii(
	    "0123456789ABCDEF0123456789ABCDEF"
	    "0123456789ABCDEF0123456789ABCDEF", &salt));
  printf("Salt: %s\n", SHA256::Ascii(salt).c_str());
  std::vector<uint8> key = GetKey(argv[1], salt);
  printf("Key: %s\n", SHA256::Ascii(key).c_str());
  string b64k = Base64::EncodeV(key);
  printf("B64: %s\n", b64k.c_str());
  printf("(256 bits in base64 has size %d)\n", (int)b64k.size());
  */
  string pass = argv[1];
  string cmd = argv[2];
  string file = argv[3];
  if (cmd == "enc") {
    string plaintext = Util::ReadFile(file);
    Encrypt(pass, plaintext);

  } else if (cmd == "dec") {
    string enctext = Util::ReadFile(file);
    Decrypt(argv[1], enctext);

  } else {
    // LOG(FATAL) << "Unknown command " << cmd;
    printf("Unknown command %s\n", cmd.c_str());
  }
  
  return 0;
}
