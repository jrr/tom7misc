
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <vector>
#include <string>

using namespace std;

// Dead simple program that extracts the PRG ROM from .NES files
// written here. Assumes a lot about the size/format of the .NES
// files, so it's not going to work in the general case...

int main(int argc, char **argv) {
  std::vector<string> args;

  bool got_type = false;
  bool mirror = false;
  bool dump_prg = false;
  for (int i = 1; i < argc; i++) {
    string arg = argv[i];
    if (arg == "-prg") {
      if (got_type) {
	fprintf(stderr, "-prg or -chr only once.\n");
	return -1;
      }
      dump_prg = true;
      got_type = true;
    } else if (arg == "-chr") {
      if (got_type) {
	fprintf(stderr, "-prg or -chr only once.\n");
	return -1;
      }

      dump_prg = false;
      got_type = true;
    } else if (arg == "-mirror") {
      mirror = true;
    } else if (arg[0] == '-') {
      fprintf(stderr, "Unknown flag %s\n", arg.c_str());
    } else {
      args.push_back(arg);
    }
  }

  if (!got_type || args.size() != 2) {
    fprintf(stderr, "usage: makeimage.exe [-mirror] -prg|-chr cart.nes cart.rom\n");
    return -1;
  }

  const string infile = args[0];
  const string outfile = args[1];
  
  FILE *inf = fopen(infile.c_str(), "rb");
  if (inf == 0) {
    fprintf(stderr, "Can't read %s\n", infile.c_str());
    return -1;
  }

  FILE *outf = fopen(outfile.c_str(), "wb");
  if (outf == 0) {
    fclose(inf);
    fprintf(stderr, "Can't open %s for writing\n", outfile.c_str());
    return -1;
  }

  // Read the header.
  unsigned char header[16];
  if (16 != fread(&header, 1, 16, inf)) {
    fprintf(stderr, "Can't read header\n");
    return -1;
  }

  if (0 != memcmp("NES\x1a", header, 4)) {
    fprintf(stderr, "Not a NES file\n");
    return -1;
  }

  int prg_bytes = header[4] * 16384;
  int chr_bytes = header[5] * 8192;
  unsigned char *prg = (unsigned char *)malloc(prg_bytes);
  if (prg_bytes != fread(prg, 1, prg_bytes, inf)) {
    fprintf(stderr, "Couldn't read %d PRG bytes?\n", prg_bytes);
    return -1;
  }

  unsigned char *chr = (unsigned char *)malloc(chr_bytes);
  if (chr_bytes != fread(chr, 1, chr_bytes, inf)) {
    fprintf(stderr, "Couldn't read %d CHR bytes?\n", chr_bytes);
    return -1;
  }

  int mult = mirror ? 2 : 1;
  
  if (dump_prg) {
    for (int i = 0; i < mult; i++) {
      if (prg_bytes != fwrite(prg, 1, prg_bytes, outf)) {
	fprintf(stderr, "Couldn't write %d rom bytes?\n", prg_bytes);
	return -1;
      }
    }
    fprintf(stderr, "Successfully wrote %d PRG Bytes to %s.\n",
            mult * prg_bytes, outfile.c_str());
  } else {
    for (int i = 0; i < mult; i++) {
      if (chr_bytes != fwrite(chr, 1, chr_bytes, outf)) {
	fprintf(stderr, "Couldn't write %d rom bytes?\n", chr_bytes);
	return -1;
      }
    }
    fprintf(stderr, "Successfully wrote %d CHR Bytes to %s.\n",
            mult * chr_bytes, outfile.c_str());
  }

  free(chr);
  free(prg);
  fclose(inf);
  fclose(outf);
  return 0;
}
