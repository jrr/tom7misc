#ifndef __CART_H
#define __CART_H

#include "types.h"
#include "fceu.h"

#include "fc.h"

struct CartInterface {
  explicit CartInterface(FC *fc) : fc(fc) {}
  virtual ~CartInterface() {}
  /* Set by mapper/board code: */
  virtual void Power() {}
  virtual void Reset() {}
  virtual void Close() {}

 protected:
  FC *fc = nullptr;
 private:
  CartInterface() = delete;
};

// Same idea, but for old _init style mappers that
// do their work by modifying global variables.
struct MapInterface {
  explicit MapInterface(FC *fc) : fc(fc) {}
  virtual ~MapInterface() {}
  virtual void StateRestore(int version) {}
  virtual void MapperReset() {}
  virtual void MapperClose() {}
 protected:
  FC *fc = nullptr;
 private:
  MapInterface() = delete;
};

struct CartInfo {
  // Maybe some of this should go into CartInterface.

  /* Pointers to memory to save/load. */
  uint8 *SaveGame[4];
  /* How much memory to save/load. */
  uint32 SaveGameLen[4];

  /* Set by iNES/UNIF loading code. */
  /* As set in the header or chunk.
     iNES/UNIF specific.  Intended
     to help support games like "Karnov"
     that are not really MMC3 but are
     set to mapper 4. */
  int mirror;
  /* Presence of an actual battery. */
  int battery;
  uint8 MD5[16];
  /* Should be set by the iNES/UNIF loading
     code, used by mapper/board code, maybe
     other code in the future. */
  uint32 CRC32;
};

struct Cart {
  void FCEU_SaveGameSave(CartInfo *LocalHWInfo);
  void FCEU_LoadGameSave(CartInfo *LocalHWInfo);
  void FCEU_ClearGameSave(CartInfo *LocalHWInfo);

  // Should use these accessors instead of modifying the pages
  // directly.
  void WritePage(uint32 A, uint8 V) { Page[A >> 11][A] = V; }
  uint8 ReadPage(uint32 A) const { return Page[A >> 11][A]; }

  void WriteVPage(uint32 A, uint8 V) { VPage[A >> 10][A] = V; }
  uint8 ReadVPage(uint32 A) const { return VPage[A >> 10][A]; }
  const uint8 *VPagePointer(uint32 A) const { return &VPage[A >> 10][A]; }
  // TODO: Gross, but better than just modifying VPage from afar.
  // Maybe can update callers to use setvramb*.
  void SetVPage(uint32 A, uint8 *p) { VPage[A >> 10] = p - A; }
  // Ugh, even worse!
  void SetSpecificVPage(int num, uint32 A, uint8 *p) { VPage[num] = p - A; }
  
  // Each page is a 2k chunk of memory, corresponding to the address
  // (A >> 11), but the pointer is offset such that it is still
  // indexed by A, not A & 2047. (TODO: verify, and maybe "fix" -tom7)
  // TODO: Make private and use accessors so that we can either keep
  // the address offsetting trick internal, or even stamp it out
  // TODO: In the process of making these private. -tom7
private:
  uint8 *Page[32] = {};
  uint8 *VPage[8] = {};
public:

  // A cartridge consists of a set of PRG and CHR (video) ROMs (or RAMs),
  // each usually a chip on the board. These can be set up by the mapper
  // or by the cart format itself (e.g., iNES always sets chip 0 to "the
  // ROM" and the unif format describes the chips with metadata).
  void ResetCartMapping();
  void SetupCartPRGMapping(int chip, uint8 *p, uint32 size, bool is_ram);
  void SetupCartCHRMapping(int chip, uint8 *p, uint32 size, bool is_ram);
  void SetupCartMirroring(int m, int hard, uint8 *extra);

  // Maybe should always be true? -tom7
  static constexpr bool disableBatteryLoading = false;

  uint8 *PRGptr[32] = {};
  uint8 *CHRptr[32] = {};

  uint32 PRGsize[32] = {};
  uint32 CHRsize[32] = {};

  uint32 PRGmask2[32] = {};
  uint32 PRGmask4[32] = {};
  uint32 PRGmask8[32] = {};
  uint32 PRGmask16[32] = {};
  uint32 PRGmask32[32] = {};

  uint32 CHRmask1[32] = {};
  uint32 CHRmask2[32] = {};
  uint32 CHRmask4[32] = {};
  uint32 CHRmask8[32] = {};


  // These functions perform bank switching. The versions without r
  // just assume r=0. A is the base address that gets switched (this
  // is basically always a constant at the call site). V is the value,
  // which I think is like the bank number to select. (In UNROM, we
  // use latch & 7, but then also ~0! It gets anded with one of the
  // PRGmasks, though.)
  //  
  // 2, 4, 8, 16, 32 seem to refer to 2k, 4k, 8k, 16k and 32k banks.
  //
  // I haven't figured it out beyond that.
  // -tom7
  void setprg2(uint32 A, uint32 V);
  void setprg4(uint32 A, uint32 V);
  void setprg8(uint32 A, uint32 V);
  void setprg16(uint32 A, uint32 V);
  void setprg32(uint32 A, uint32 V);

  void setprg2r(int r, unsigned int A, unsigned int V);
  void setprg4r(int r, unsigned int A, unsigned int V);
  void setprg8r(int r, unsigned int A, unsigned int V);
  void setprg16r(int r, unsigned int A, unsigned int V);
  void setprg32r(int r, unsigned int A, unsigned int V);

  void setchr1r(int r, unsigned int A, unsigned int V);
  void setchr2r(int r, unsigned int A, unsigned int V);
  void setchr4r(int r, unsigned int A, unsigned int V);
  void setchr8r(int r, unsigned int V);

  void setchr1(unsigned int A, unsigned int V);
  void setchr2(unsigned int A, unsigned int V);
  void setchr4(unsigned int A, unsigned int V);
  void setchr8(unsigned int V);

  void setvram4(uint32 A, uint8 *p);
  void setvram8(uint8 *p);

  void setvramb1(uint8 *p, uint32 A, uint32 b);
  void setvramb2(uint8 *p, uint32 A, uint32 b);
  void setvramb4(uint8 *p, uint32 A, uint32 b);
  void setvramb8(uint8 *p, uint32 b);

  void setmirror(int t);
  void setmirrorw(int a, int b, int c, int d);
  void setntamem(uint8 *p, int ram, uint32 b);

  Cart(FC *fc);

  // Write to or read from the mapped address. The BROB version (OB is
  // presumably "out of bounds") returns the current value of the data
  // bus when reading from an unmapped page.
  DECLFR_RET CartBR_Direct(DECLFR_ARGS);
  DECLFR_RET CartBROB_Direct(DECLFR_ARGS);
  DECLFW_RET CartBW_Direct(DECLFW_ARGS);

  // TODO: Kill these static versions.
  static DECLFW_RET CartBW(DECLFW_ARGS);
  static DECLFR_RET CartBR(DECLFR_ARGS);
  static DECLFR_RET CartBROB(DECLFR_ARGS);

private:
  bool PRGIsRAM[32] = { };  /* This page is/is not PRG RAM. */

  // See comment on ResetCartMapping where negative offsets of nothing
  // are used..? TODO: Sort this out.
  uint8 nothing_safetynet[65536] = { };
  uint8 nothing[8192] = { };

  /* 16 are (sort of) reserved for UNIF/iNES and 16 to map other stuff. */
  bool CHRram[32] = { };
  bool PRGram[32] = { };

  int mirrorhard = 0;

  void SetPagePtr(int s, uint32 A, uint8 *p, bool is_ram);

  FC *fc;
};

#define MI_H 0
#define MI_V 1
#define MI_0 2
#define MI_1 3

#endif
