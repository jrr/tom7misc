
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Dead simple program that extracts the PRG ROM from .NES files
// written here. Assumes a lot about the size/format of the .NES
// files, so it's not going to work in the general case...

int main(int argc, char **argv) {
  if (argc != 3) {
    fprintf(stderr, "usage: makeimage.exe cart.nes cart.rom\n");
    return -1;
  }

  FILE *inf = fopen(argv[1], "rb");
  if (inf == 0) {
    fprintf(stderr, "Can't read %s\n", argv[1]);
    return -1;
  }

  FILE *outf = fopen(argv[2], "wb");
  if (outf == 0) {
    fclose(inf);
    fprintf(stderr, "Can't open %s for writing\n", argv[2]);
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

  int rombytes = header[4] * 16384;
  unsigned char *rom = (unsigned char *)malloc(rombytes);
  if (rombytes != fread(rom, 1, rombytes, inf)) {
    fprintf(stderr, "Couldn't read %d rom bytes?\n", rombytes);
    return -1;
  }

  if (rombytes != fwrite(rom, 1, rombytes, outf)) {
    fprintf(stderr, "Couldn't write %d rom bytes?\n", rombytes);
    return -1;
  }

  fprintf(stderr, "Successfully wrote %d ROM Bytes to %s.\n",
          rombytes, argv[2]);

  free(rom);
  fclose(inf);
  fclose(outf);
  return 0;
}
