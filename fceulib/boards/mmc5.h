#include "../types.h"
#include "../cart.h"

// Unlike most mappers, this one has a "public" interface so that
// hacks in the PPU can commune with it. It's gross. I've tried to
// move as much MMC5-specific stuff in here as possible, but it's
// rather a mess. -tom7

struct MMC5 final : public CartInterface {
 public:
  // PPU.
  void MMC5HackHB(int scanline);

  const uint8 *SPRVPagePtr(uint32 v) { return &MMC5SPRVPage[v >> 10][v]; }
  const uint8 *BGVPagePtr(uint32 v) { return &MMC5BGVPage[v >> 10][v]; }
  
  void Power() final override;

  // Should just be used by internal mapper creators.
  MMC5(FC *fc, CartInfo *info, int wsize, int battery);

 private:

  // This used to be in Cart and referenced from PPU, but I moved it
  // here in an attempt to keep MMC5 hacks in MMC5 as much as
  // possible. These are video pages (presumably sprite and bg)
  // indexed like [A >> 10][A] (the addresses are pre-offset by
  // subtracting A when initializing them). -tom7
  uint8 *MMC5SPRVPage[8] = {};
  uint8 *MMC5BGVPage[8] = {};
  
  struct MMC5APU {
    uint16 wl[2] = {};
    uint8 env[2] = {};
    uint8 enable = 0;
    uint8 running = 0;
    uint8 raw = 0;
    uint8 rawcontrol = 0;
    int32 dcount[2] = {};
    int32 BC[3] = {};
    int32 vcount[2] = {};
  };

  MMC5APU MMC5Sound;

  uint8 PRGBanks[4] = {};
  uint8 WRAMPage = 0;
  uint16 CHRBanksA[8] = {}, CHRBanksB[4] = {};
  uint8 WRAMMaskEnable[2] = {};
  // Used in ppu -tom7
  // uint8 mmc5ABMode = 0; /* A=0, B=1 */

  uint8 IRQScanline = 0, IRQEnable = 0;
  uint8 CHRMode = 0, NTAMirroring = 0, NTFill = 0, ATFill = 0;

  uint8 MMC5IRQR = 0;
  uint8 MMC5LineCounter = 0;
  uint8 mmc5psize = 0, mmc5vsize = 0;
  uint8 mul[2] = {};

  // PERF Initial backing ram for SPRVpage and BGVPage. I copied this
  // from cart. Perhaps it's not necessary if this mapper always
  // initializes them? -tom7
  uint8 nothing[8192] = {};

  uint8 *WRAM = nullptr;
  uint8 *MMC5fill = nullptr;
  uint8 *ExRAM = nullptr;

  uint8 MMC5WRAMsize = 0;
  uint8 MMC5WRAMIndex[8] = {};

  uint8 MMC5ROMWrProtect[4] = {};
  uint8 MMC5MemIn[5] = {};

  void (MMC5::*sfun)(int P) = nullptr;
  void (MMC5::*psfun)() = nullptr;

  inline void MMC5SPRVROM_BANK1(uint32 A, uint32 V) {
    if (fc->cart->CHRptr[0]) {
      V &= fc->cart->CHRmask1[0];
      MMC5SPRVPage[(A) >> 10] =
        &fc->cart->CHRptr[0][(V) << 10] - (A);
    }
  }

  inline void MMC5BGVROM_BANK1(uint32 A, uint32 V) {
    if (fc->cart->CHRptr[0]) {
      V &= fc->cart->CHRmask1[0];
      MMC5BGVPage[(A) >> 10] =
        &fc->cart->CHRptr[0][(V) << 10] - (A);
    }
  }

  inline void MMC5SPRVROM_BANK2(uint32 A, uint32 V) {
    if (fc->cart->CHRptr[0]) {
      V &= fc->cart->CHRmask2[0];
      MMC5SPRVPage[(A) >> 10] =
        MMC5SPRVPage[((A) >> 10) + 1] =
        &fc->cart->CHRptr[0][(V) << 11] - (A);
    }
  }

  inline void MMC5BGVROM_BANK2(uint32 A, uint32 V) {
    if (fc->cart->CHRptr[0]) {
      V &= fc->cart->CHRmask2[0];
      MMC5BGVPage[(A) >> 10] =
        MMC5BGVPage[((A) >> 10) + 1] =
        &fc->cart->CHRptr[0][(V) << 11] - (A);
    }
  }

  inline void MMC5SPRVROM_BANK4(uint32 A, uint32 V) {
    if (fc->cart->CHRptr[0]) {
      V &= fc->cart->CHRmask4[0];
      MMC5SPRVPage[(A) >> 10] =
        MMC5SPRVPage[((A) >> 10) + 1] =
        MMC5SPRVPage[((A) >> 10) + 2] =
        MMC5SPRVPage[((A) >> 10) + 3] =
        &fc->cart->CHRptr[0][(V) << 12] - (A);
    }
  }

  inline void MMC5BGVROM_BANK4(uint32 A, uint32 V) {
    if (fc->cart->CHRptr[0]) {
      V &= fc->cart->CHRmask4[0];
      MMC5BGVPage[(A) >> 10] =
        MMC5BGVPage[((A) >> 10) + 1] =
        MMC5BGVPage[((A) >> 10) + 2] =
        MMC5BGVPage[((A) >> 10) + 3] =
        &fc->cart->CHRptr[0][(V) << 12] - (A);
    }
  }

  inline void MMC5SPRVROM_BANK8(uint32 V) {
    if (fc->cart->CHRptr[0]) {
      V &= fc->cart->CHRmask8[0];
      MMC5SPRVPage[0] = MMC5SPRVPage[1] =
        MMC5SPRVPage[2] = MMC5SPRVPage[3] =
        MMC5SPRVPage[4] = MMC5SPRVPage[5] =
        MMC5SPRVPage[6] = MMC5SPRVPage[7] =
        &fc->cart->CHRptr[0][(V) << 13];
    }
  }

  inline void MMC5BGVROM_BANK8(uint32 V) {
    if (fc->cart->CHRptr[0]) {
      V &= fc->cart->CHRmask8[0];
      MMC5BGVPage[0] = MMC5BGVPage[1] =
        MMC5BGVPage[2] = MMC5BGVPage[3] =
        MMC5BGVPage[4] = MMC5BGVPage[5] =
        MMC5BGVPage[6] = MMC5BGVPage[7] =
        &fc->cart->CHRptr[0][(V) << 13];
    }
  }

  void BuildWRAMSizeTable();
  void MMC5CHRA();
  void MMC5CHRB();
  void MMC5WRAM(uint32 A, uint32 V);
  void MMC5PRG();
  void Mapper5_write(DECLFW_ARGS);

  DECLFR_RET MMC5_ReadROMRAM(DECLFR_ARGS);
  void MMC5_WriteROMRAM(DECLFW_ARGS);
  void MMC5_ExRAMWr(DECLFW_ARGS);

  DECLFR_RET MMC5_ExRAMRd(DECLFR_ARGS);
  DECLFR_RET MMC5_read(DECLFR_ARGS);

  void MMC5Synco();

  static void MMC5_StateRestore(FC *fc, int version);

  void Do5PCM();
  void Do5PCMHQ();
  void Mapper5_SW(DECLFW_ARGS);
  void Do5SQ(int P);
  void Do5SQHQ(int P);
  void MMC5RunSoundHQ();
  void MMC5HiSync(int32 ts);
  void MMC5RunSound(int count);
  void Mapper5_ESI();
};
