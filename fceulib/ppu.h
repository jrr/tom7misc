#ifndef __PPU_H
#define __PPU_H

#include "state.h"
#include <utility>
#include <vector>

#include "fceu.h"
#include "fc.h"

#define TRACK_INTERFRAME_SCROLL 1

struct PPU {
 public:
  PPU(FC *fc);

  void FCEUPPU_Reset();
  void FCEUPPU_Power();
  // Runs one frame. The CPU is driven by the PPU timing.
  void FrameLoop();

  void LineUpdate();
  void SetVideoSystem(int w);

  void FCEUPPU_SaveState();
  void FCEUPPU_LoadState(int version);

  // 0 to keep 8-sprites limitation, 1 to remove it.
  // Note that this does appear to affect the CPU behavior
  // (calling hooks, especially MMC5), not just remove
  // graphical glitches. -tom7
  void DisableSpriteLimitation(int a);


  void (*PPU_hook)(FC *, uint32 A) = nullptr;
  void (*GameHBIRQHook)(FC *) = nullptr;
  void (*GameHBIRQHook2)(FC *) = nullptr;

  // This is nametable ram, palette ram,
  // and sprite ram.
  uint8 NTARAM[0x800] = {}, PALRAM[0x20] = {};
  uint8 SPRAM[0x100] = {};

  // X scroll offset within the first tile (0-7)
  uint8 GetXOffset() const { return XOffset; }

  uint32 GetTempAddr() const { return TempAddr; }

  // Get the x scroll value within the selected nametable.
  uint8 GetXScroll8() const {
    // Combine coarse and fine positions.
    return ((TempAddr & 31) << 3) | XOffset;
  }

  uint8 GetYScroll8() const {
    // These bits are stored in weird places. Shifted all the way
    // down here for clarity.
    const uint8 fine_y = (TempAddr >> 12) & 7;
    // 0x3E0 = 1111100000
    const uint8 coarse_y = (TempAddr & 0x03E0) >> 5;

    // XXXX
    //    const uint8 ppu_ctrl = ppu->PPU_values[0];
    // const uint8 ytable_select = (ppu_ctrl & 2) ? 240 : 0;

    return (coarse_y << 3) | fine_y;
  }
  
  // PPU values are:
  //  [0] 0x2000  PPU Control Register #1  (PPUCTRL)
  //  [1] 0x2001  PPU Control Register #2  (PPUMASK)
  //  [2] 0x2002  PPU Status Register      (PPUSTATUS)
  //  [3] 0x2003  SPR-RAM Address Register (OAMADDR)
  uint8 PPU_values[4] = {};

  int MMC5Hack = 0;
  uint8 mmc5ABMode = 0; /* A=0, B=1 */
  uint32 MMC5HackVROMMask = 0;
  uint8 *MMC5HackExNTARAMPtr = nullptr;
  uint8 *MMC5HackVROMPTR = nullptr;
  uint8 MMC5HackCHRMode = 0;
  uint8 MMC5HackSPMode = 0;
  uint8 MMC50x5130 = 0;
  uint8 MMC5HackSPScroll = 0;
  uint8 MMC5HackSPPage = 0;
  
  /* For cart.c and banksw.h, mostly */
  uint8 *vnapage[4] = { nullptr, nullptr, nullptr, nullptr };
  uint8 PPUNTARAM = 0;
  uint8 PPUCHRRAM = 0;

  // scanline is equal to the current visible scanline we're on.
  int scanline = 0;
  int g_rasterpos = 0;

  #ifdef TRACK_INTERFRAME_SCROLL
  // Contains the x and y scroll positions (not taking into account
  // table select) the last time each scanline was rendered. Since
  // this doesn't affect the behavior of the emulator, it is NOT SAVED
  // in savestates.
  uint8 interframe_x[256] = {};
  uint8 interframe_y[256] = {};
  #endif

  // TODO: Kill these.
  // XXX do they need to be exposed, btw? that might have been
  // my mistake.
  static DECLFR_RET A2002(DECLFR_ARGS);
  static DECLFR_RET A2004(DECLFR_ARGS);
  static DECLFR_RET A200x(DECLFR_ARGS);
  static DECLFR_RET A2007(DECLFR_ARGS);

  // TODO: Indirect static hooks (which go through the global object)
  // should instead get a local ppu object and call these.
  DECLFR_RET A2002_Direct(DECLFR_ARGS);
  DECLFR_RET A2004_Direct(DECLFR_ARGS);
  DECLFR_RET A200x_Direct(DECLFR_ARGS);
  DECLFR_RET A2007_Direct(DECLFR_ARGS);


  // Some static methods herein call these, but they should be
  // getting a local ppu object rather than using the global one.
  void B2000_Direct(DECLFW_ARGS);
  void B2001_Direct(DECLFW_ARGS);
  void B2002_Direct(DECLFW_ARGS);
  void B2003_Direct(DECLFW_ARGS);
  void B2004_Direct(DECLFW_ARGS);
  void B2005_Direct(DECLFW_ARGS);
  void B2006_Direct(DECLFW_ARGS);
  void B2007_Direct(DECLFW_ARGS);
  void B4014_Direct(DECLFW_ARGS);

  const std::vector<SFORMAT> &FCEUPPU_STATEINFO() {
    return stateinfo;
  }

 private:
  const std::vector<SFORMAT> stateinfo;

  void FetchSpriteData();
  void RefreshLine(int lastpixel);
  void RefreshSprites();
  void CopySprites(uint8 *target);

  void Fixit1();
  void Fixit2();
  void ResetRL(uint8 *target);
  void CheckSpriteHit(int p);
  void EndRL();
  void DoLine();

  const uint8 *MMC5BGVRAMADR(uint32 V);

  template<bool PPUT_MMC5, bool PPUT_MMC5SP, bool PPUT_HOOK, bool PPUT_MMC5CHR1>
  std::pair<uint32, uint8 *> PPUTile(const int X1, uint8 *P,
                                     const uint32 vofs,
                                     uint32 refreshaddr_local);

  // Sprite buffer, used internally (it's the sprites on the current
  // scanline). This contains numsprites SPRB structs. XXX Should be
  // structs, not uint8.
  // Most of this stuff is transient data for communicating during an
  // internal scanline render, not really state. It can probably be
  // cleaned up, or at least consolidated to make its lifetime clear.
  uint8 SPRBUF[0x100] = {};
  // Number of sprites on this scanline.
  uint8 numsprites = 0;
  // True if sprite 0 is on this scanline. It will be sprite index 0
  // in SPRBUF in that case.
  bool sprite_0_in_sprbuf = 0;
  // Information for the sprite 0 hit test.
  int32 sprite_hit_x = 0;
  uint8 sprite_hit_mask = 0;
  
  int ppudead = 1;
  int cycle_parity = 0;

  uint8 VRAMBuffer = 0;
  // The bus used to communicate with the CPU has significant bus
  // capacitance, so it retains the last value written to it for
  // a frame or two. This stores that value.
  uint8 PPUGenLatch = 0;

  // Color deemphasis emulation.
  uint8 deemp = 0;
  int deempcnt[8] = {};

  // A few addresses actually take 16-bit values as two consecutive
  // writes. This has value 0 or 1 to indicate which byte it is.
  uint8 vtoggle = 0;
  // Low 3 bits of the scroll x offset. Coarse x scroll position is
  // stored in TempAddr.
  uint8 XOffset = 0;

  // Current scroll position and PPU memory location for read/write.
  // See: http://wiki.nesdev.com/w/index.php/PPU_scrolling
  //
  // Note that games may modify this during the scan. Also note that
  // the 0th bit of PPUCTRL can be seen as being the 9th bit (MSB) of
  // this value, since it selects between two horizontally adjacent
  // (and cylindrical) nametables.
  uint32 TempAddr = 0;
  uint32 RefreshAddr = 0;
  uint16 TempAddrT = 0, RefreshAddrT = 0;

  // Perhaps should be compile-time constant.
  int maxsprites = 8;

  uint32 scanlines_per_frame = 0;

  uint8 PPUSPL = 0;

  // These alias XBuf. Note that there is at least one meaningful read
  // from these, to check sprite collision.
  uint8 *Pline = nullptr, *Plinef = nullptr;
  int firsttile = 0;
  int linestartts = 0;
  int tofix = 0;
  // Temporary sprite data 
  uint8 sprlinebuf[256 + 8] = {};

  // Any sprites on this line? Then this will be set to 1.
  // Needed for zapper emulation and sprite emulation.
  int any_sprites_on_line = 0;

  // These used to be static inside RefreshLine, but interleavings of
  // save/restore in "Ultimate Basketball" can cause execution to diverge.
  // Now saved in stateinfo.
  uint32 pshift[2] = {};
  // This was also static; why not save it too? -tom7
  uint32 atlatch = 0;

  // Only used within RefreshLine. Prevents reentrant calls (through
  // mappers making PPU calls).
  int norecurse = 0;

  FC *fc = nullptr;
};

#endif
