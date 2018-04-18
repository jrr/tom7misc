
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Dead simple program that extracts the PRG ROM from .NES files
// written here. Assumes a lot about the size/format of the .NES
// files, so it's not going to work in the general case...

int main(int argc, char **argv) {
  if (argc != 4) {
    fprintf(stderr, "usage: makeimage.exe -prg|-chr cart.nes cart.rom\n");
    return -1;
  }

  bool dump_prg = false;
  if (0 == strcmp(argv[1], "-prg")) {
    dump_prg = true;
  } else if (0 == strcmp(argv[1], "-chr")) {
    dump_prg = false;
  } else {
    fprintf(stderr, "First argument must be -chr or -prg.\n");
    return -1;
  }

  FILE *inf = fopen(argv[2], "rb");
  if (inf == 0) {
    fprintf(stderr, "Can't read %s\n", argv[1]);
    return -1;
  }

  FILE *outf = fopen(argv[3], "wb");
  if (outf == 0) {
    fclose(inf);
    fprintf(stderr, "Can't open %s for writing\n", argv[3]);
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

  if (dump_prg) {
    if (prg_bytes != fwrite(prg, 1, prg_bytes, outf)) {
      fprintf(stderr, "Couldn't write %d rom bytes?\n", prg_bytes);
      return -1;
    }
    fprintf(stderr, "Successfully wrote %d PRG Bytes to %s.\n",
            prg_bytes, argv[3]);
  } else {
    if (chr_bytes != fwrite(chr, 1, chr_bytes, outf)) {
      fprintf(stderr, "Couldn't write %d rom bytes?\n", chr_bytes);
      return -1;
    }
    fprintf(stderr, "Successfully wrote %d CHR Bytes to %s.\n",
            chr_bytes, argv[3]);
  }

  free(chr);
  free(prg);
  fclose(inf);
  fclose(outf);
  return 0;
}
