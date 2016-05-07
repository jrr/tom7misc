#ifndef __FCEU_ENDIAN
#define __FCEU_ENDIAN

#include <stdlib.h>
#include <iosfwd>
#include <stdio.h>
#include "../types.h"

class EMUFILE;

int write16le(uint16 b, FILE *fp);
int write32le(uint32 b, FILE *fp);
int write32le(uint32 b, std::ostream* os);
int write64le(uint64 b, std::ostream* os);
int read64le(uint64 *buf, std::istream *is);
int read32le(uint32 *buf, std::istream *is);
int read32le(uint32 *buf, FILE *fp);
int read16le(uint16 *buf, std::istream *is);

void FlipByteOrder(uint8 *src, uint32 count);

//well. just for the sake of consistency
int write8le(uint8 b, EMUFILE *fp);
inline int write8le(uint8* b, EMUFILE *fp) {
  return write8le(*b,fp);
}
int write16le(uint16 b, EMUFILE* os);
int write32le(uint32 b, EMUFILE* os);
int write64le(uint64 b, EMUFILE* os);

int read8le(uint8 *buf, EMUFILE*is);
int read16le(uint16 *buf, EMUFILE*is);
inline int read16le(int16 *buf, EMUFILE*is) {
  return read16le((uint16*)buf,is);
}
int read32le(uint32 *buf, EMUFILE*is);
inline int read32le(int32 *buf, EMUFILE*is) {
  return read32le((uint32*)buf,is);
}
int read64le(uint64 *buf, EMUFILE*is);

#endif


