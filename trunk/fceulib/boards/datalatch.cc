/* FCE Ultra - NES/Famicom Emulator
 *
 * Copyright notice for this file:
 *  Copyright (C) 2002 Xodnizel
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */

#include "mapinc.h"
#include "../ines.h"

static constexpr uint32 WRAMSIZE = 8192;

namespace {

// Many mappers simply save a byte written to certain regions (e.g. ROM),
// which is then used for some cart-specific purpose (e.g. to bank switch).
// This byte is saved as the "latch" here, and the WSync virtual function
// actually does the business.
struct DataLatch : public CartInterface {
  using CartInterface::CartInterface;

  // Sometimes a write to a ROM is also treated as a read
  // simultaneously, which means the value written on the data bus by
  // the CPU conflicts with the value that the ROM writes. This causes
  // a "bus conflict." This flag indicates whether the mapper has this
  // issue.
  const bool has_bus_conflicts = false;
  
  uint8 latch = 0, latchinit = 0;
  uint16 addrreg0 = 0, addrreg1 = 0;
  uint8 *WRAM = nullptr;

  // mapper-specific sync
  virtual void WSync() {}

  static DECLFW(LatchWrite) {
    return ((DataLatch*)fc->fceu->cartiface)->LatchWrite_Direct(DECLFW_FORWARD);
  }
      
  DECLFW_RET LatchWrite_Direct(DECLFW_ARGS) {
    // Since the bus seems to be implemented "pull-down" style, if
    // either one writes a zero for a bit, it will be read as a zero.
    if (has_bus_conflicts)
      latch = V & Cart::CartBR(fc, A);
    else
      latch = V;
    WSync();
  }

  void Power() override {
    latch = latchinit;
    WSync();
    if (WRAM) {
      fc->fceu->SetReadHandler(0x6000, 0xFFFF, Cart::CartBR);
      fc->fceu->SetWriteHandler(0x6000, 0x7FFF, Cart::CartBW);
    } else {
      fc->fceu->SetReadHandler(0x8000, 0xFFFF, Cart::CartBR);
    }
    fc->fceu->SetWriteHandler(addrreg0, addrreg1, LatchWrite);
  }

  void Close() override {
    free(WRAM);
    WRAM = nullptr;
  }

  static void StateRestore(FC *fc, int version) {
    DataLatch *me = ((DataLatch*)fc->fceu->cartiface);
    me->WSync();
  }

  DataLatch(FC *fc, CartInfo *info, uint8 init,
	    uint16 adr0, uint16 adr1, uint8 wram_flag,
	    bool bus_conflict)
    : CartInterface(fc), has_bus_conflicts(bus_conflict) {
    latchinit = init;
    addrreg0 = adr0;
    addrreg1 = adr1;
    fc->fceu->GameStateRestore = StateRestore;
    if (wram_flag) {
      WRAM = (uint8 *)FCEU_gmalloc(WRAMSIZE);
      fc->cart->SetupCartPRGMapping(0x10, WRAM, WRAMSIZE, true);
      if (info->battery) {
	info->SaveGame[0] = WRAM;
	info->SaveGameLen[0] = WRAMSIZE;
      }
      fc->state->AddExState(WRAM, WRAMSIZE, 0, "WRAM");
    }
    fc->state->AddExState(&latch, 1, 0, "LATC");
    // XXX should be no reason to save this -- it's a property of
    // the mapper itself, right?
    // fc->state->AddExState(&bus_conflict, 1, 0, "BUSC");
  }
};
}

//------------------ Map 0 ---------------------------

// This mapper used to derive from DataLatch, but in a way that
// was simply confusing, not useful (it does not map any latched
// write regions). -tom7
namespace {
struct NROM final : public CartInterface {
  uint8 *WRAM = nullptr;

  void Power() final override {
    // Famili BASIC (v3.0) need it (uses only 4KB), FP-BASIC uses 8KB
    fc->cart->setprg8r(0x10, 0x6000, 0);
    fc->cart->setprg16(0x8000, 0);
    fc->cart->setprg16(0xC000, ~0);
    fc->cart->setchr8(0);

    fc->fceu->SetReadHandler(0x6000, 0x7FFF, Cart::CartBR);
    fc->fceu->SetWriteHandler(0x6000, 0x7FFF, Cart::CartBW);
    fc->fceu->SetReadHandler(0x8000, 0xFFFF, Cart::CartBR);
  }

  void Close() final override {
    free(WRAM);
    WRAM = nullptr;
  }
  
  NROM(FC *fc, CartInfo *info) : CartInterface(fc) {
    WRAM = (uint8 *)FCEU_gmalloc(WRAMSIZE);
    fc->cart->SetupCartPRGMapping(0x10, WRAM, WRAMSIZE, true);
    if (info->battery) {
      info->SaveGame[0] = WRAM;
      info->SaveGameLen[0] = WRAMSIZE;
    }
    fc->state->AddExState(WRAM, WRAMSIZE, 0, "WRAM");
  }
};
}

CartInterface *NROM_Init(FC *fc, CartInfo *info) {
  return new NROM(fc, info);
}

//------------------ Map 2 ---------------------------

namespace {
struct UNROM final : public DataLatch {
  using DataLatch::DataLatch;
  uint32 mirror_in_use = 0;
  void WSync() final override {
    if (fc->cart->PRGsize[0] <= 128 * 1024) {
      fc->cart->setprg16(0x8000, latch & 0x7);
      if (latch & 8) mirror_in_use = 1;
      if (mirror_in_use) {
	// Higway Star Hacked mapper
	fc->cart->setmirror(((latch >> 3) & 1) ^ 1);
      }
    } else {
      fc->cart->setprg16(0x8000, latch & 0xf);
    }
    fc->cart->setprg16(0xc000, ~0);
    fc->cart->setchr8(0);
  }
};
}

CartInterface *UNROM_Init(FC *fc, CartInfo *info) {
  return new UNROM(fc, info, 0, 0x8000, 0xFFFF, 0, true);
}

//------------------ Map 3 ---------------------------
// Popular, simple mapper with bank switching and that's it.
// http://wiki.nesdev.com/w/index.php/CNROM
namespace {
struct CNROM final : public DataLatch {
  using DataLatch::DataLatch;
  // For CNROM, writing to rom (0x8000-0xFFFF) does a bank
  // switch.
  void WSync() final override {
    fc->cart->setchr8(latch);
    fc->cart->setprg32(0x8000, 0);
    // Hayauchy IGO uses 2Kb or RAM
    fc->cart->setprg8r(0x10, 0x6000, 0);
  }
};
}

CartInterface *CNROM_Init(FC *fc, CartInfo *info) {
  return new CNROM(fc, info, 0, 0x8000, 0xFFFF, 1, true);
}

//------------------ Map 7 ---------------------------

namespace {
struct ANROM final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg32(0x8000, latch & 0xf);
    fc->cart->setmirror(MI_0 + ((latch >> 4) & 1));
    fc->cart->setchr8(0);
  }
};
}

CartInterface *ANROM_Init(FC *fc, CartInfo *info) {
  return new ANROM(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 8 ---------------------------
namespace {
struct Mapper8 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg16(0x8000, latch >> 3);
    fc->cart->setprg16(0xc000, 1);
    fc->cart->setchr8(latch & 3);
  }
};
}

CartInterface *Mapper8_Init(FC *fc, CartInfo *info) {
  return new Mapper8(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 11 ---------------------------
namespace {
struct Mapper11 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg32(0x8000, latch & 0xf);
    fc->cart->setchr8(latch >> 4);
  }
};
}

CartInterface *Mapper11_Init(FC *fc, CartInfo *info) {
  return new Mapper11(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

CartInterface *Mapper144_Init(FC *fc, CartInfo *info) {
  return new Mapper11(fc, info, 0, 0x8001, 0xFFFF, 0, false);
}

//------------------ Map 13 ---------------------------
namespace {
struct CPROM final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setchr4(0x0000, 0);
    fc->cart->setchr4(0x1000, latch & 3);
    fc->cart->setprg32(0x8000, 0);
  }
};
}

CartInterface *CPROM_Init(FC *fc, CartInfo *info) {
  return new CPROM(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 36 ---------------------------
namespace {
struct Mapper36 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg32(0x8000, latch >> 4);
    fc->cart->setchr8(latch & 0xF);
  }
};
}

CartInterface *Mapper36_Init(FC *fc, CartInfo *info) {
  return new Mapper36(fc, info, 0, 0x8400, 0xfffe, 0, false);
}

//------------------ Map 38 ---------------------------
namespace {
struct Mapper38 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg32(0x8000, latch & 3);
    fc->cart->setchr8(latch >> 2);
  }
};
}

CartInterface *Mapper38_Init(FC *fc, CartInfo *info) {
  return new Mapper38(fc, info, 0, 0x7000, 0x7FFF, 0, false);
}

//------------------ Map 66, 140 ---------------------------
namespace {
struct MHROM final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg32(0x8000, latch >> 4);
    fc->cart->setchr8(latch & 0xf);
  }
};
}

CartInterface *MHROM_Init(FC *fc, CartInfo *info) {
  return new MHROM(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

CartInterface *Mapper140_Init(FC *fc, CartInfo *info) {
  return new MHROM(fc, info, 0, 0x6000, 0x7FFF, 0, false);
}

//------------------ Map 70 ---------------------------
namespace {
struct Mapper70 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg16(0x8000, latch >> 4);
    fc->cart->setprg16(0xc000, ~0);
    fc->cart->setchr8(latch & 0xf);
  }
};
}

CartInterface *Mapper70_Init(FC *fc, CartInfo *info) {
  return new Mapper70(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 78 ---------------------------
/* Should be two separate emulation functions for this "mapper".  Sigh.
   URGE TO KILL RISING. */
namespace {
struct Mapper78 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg16(0x8000, (latch & 7));
    fc->cart->setprg16(0xc000, ~0);
    fc->cart->setchr8(latch >> 4);
    fc->cart->setmirror(MI_0 + ((latch >> 3) & 1));
  }
};
}

CartInterface *Mapper78_Init(FC *fc, CartInfo *info) {
  return new Mapper78(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 86 ---------------------------
namespace {
struct Mapper86 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg32(0x8000, (latch >> 4) & 3);
    fc->cart->setchr8((latch & 3) | ((latch >> 4) & 4));
  }
};
}

CartInterface *Mapper86_Init(FC *fc, CartInfo *info) {
  return new Mapper86(fc, info, ~0, 0x6000, 0x6FFF, 0, false);
}

//------------------ Map 87 ---------------------------
namespace {
struct Mapper87 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg32(0x8000, 0);
    fc->cart->setchr8(((latch >> 1) & 1) | ((latch << 1) & 2));
  }
};
}

CartInterface *Mapper87_Init(FC *fc, CartInfo *info) {
  return new Mapper87(fc, info, ~0, 0x6000, 0xFFFF, 0, false);
}

//------------------ Map 89 ---------------------------
namespace {
struct Mapper89 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg16(0x8000, (latch >> 4) & 7);
    fc->cart->setprg16(0xc000, ~0);
    fc->cart->setchr8((latch & 7) | ((latch >> 4) & 8));
    fc->cart->setmirror(MI_0 + ((latch >> 3) & 1));
  }
};
}

CartInterface *Mapper89_Init(FC *fc, CartInfo *info) {
  return new Mapper89(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 93 ---------------------------
namespace {
struct SUNSOFTUNROM final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg16(0x8000, latch >> 4);
    fc->cart->setprg16(0xc000, ~0);
    fc->cart->setchr8(0);
  }
};
}

CartInterface *SUNSOFT_UNROM_Init(FC *fc, CartInfo *info) {
  return new SUNSOFTUNROM(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 94 ---------------------------
namespace {
struct Mapper94 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg16(0x8000, latch >> 2);
    fc->cart->setprg16(0xc000, ~0);
    fc->cart->setchr8(0);
  }
};
}

CartInterface *Mapper94_Init(FC *fc, CartInfo *info) {
  return new Mapper94(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 97 ---------------------------
namespace {
struct Mapper97 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setchr8(0);
    fc->cart->setprg16(0x8000, ~0);
    fc->cart->setprg16(0xc000, latch & 15);
    switch (latch >> 6) {
      case 0: break;
      case 1: fc->cart->setmirror(MI_H); break;
      case 2: fc->cart->setmirror(MI_V); break;
      case 3: break;
    }
    fc->cart->setchr8(((latch >> 1) & 1) | ((latch << 1) & 2));
  }
};
}

CartInterface *Mapper97_Init(FC *fc, CartInfo *info) {
  return new Mapper97(fc, info, ~0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 101 ---------------------------
namespace {
struct Mapper101 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg32(0x8000, 0);
    fc->cart->setchr8(latch);
  }
};
}

CartInterface *Mapper101_Init(FC *fc, CartInfo *info) {
  return new Mapper101(fc, info, ~0, 0x6000, 0x7FFF, 0, false);
}

//------------------ Map 107 ---------------------------
namespace {
struct Mapper107 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg32(0x8000, (latch >> 1) & 3);
    fc->cart->setchr8(latch & 7);
  }
};
}

CartInterface *Mapper107_Init(FC *fc, CartInfo *info) {
  return new Mapper107(fc, info, ~0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 113 ---------------------------
namespace {
struct Mapper113 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg32(0x8000, (latch >> 3) & 7);
    fc->cart->setchr8(((latch >> 3) & 8) | (latch & 7));
    //	fc->cart->setmirror(latch>>7); // only for HES 6in1
  }
};
}

CartInterface *Mapper113_Init(FC *fc, CartInfo *info) {
  return new Mapper113(fc, info, 0, 0x4100, 0x7FFF, 0, false);
}


//------------------ Map 152 ---------------------------
namespace {
struct Mapper152 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg16(0x8000, (latch >> 4) & 7);
    fc->cart->setprg16(0xc000, ~0);
    fc->cart->setchr8(latch & 0xf);
    fc->cart->setmirror(MI_0 +
			((latch >> 7) & 1)); /* Saint Seiya...hmm. */
  }
};
}

CartInterface *Mapper152_Init(FC *fc, CartInfo *info) {
  return new Mapper152(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 180 ---------------------------
namespace {
struct Mapper180 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg16(0x8000, 0);
    fc->cart->setprg16(0xc000, latch);
    fc->cart->setchr8(0);
  }
};
}

CartInterface *Mapper180_Init(FC *fc, CartInfo *info) {
  return new Mapper180(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 184 ---------------------------
namespace {
struct Mapper184 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setchr4(0x0000, latch);
    fc->cart->setchr4(0x1000, latch >> 4);
    fc->cart->setprg32(0x8000, 0);
  }
};
}

CartInterface *Mapper184_Init(FC *fc, CartInfo *info) {
  return new Mapper184(fc, info, 0, 0x6000, 0x7FFF, 0, false);
}

//------------------ Map 203 ---------------------------
namespace {
struct Mapper203 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg16(0x8000, (latch >> 2) & 3);
    fc->cart->setprg16(0xC000, (latch >> 2) & 3);
    fc->cart->setchr8(latch & 3);
  }
};
}

CartInterface *Mapper203_Init(FC *fc, CartInfo *info) {
  return new Mapper203(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ Map 240 ---------------------------
namespace {
struct Mapper240 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setprg8r(0x10, 0x6000, 0);
    fc->cart->setprg32(0x8000, latch >> 4);
    fc->cart->setchr8(latch & 0xf);
  }
};
}

CartInterface *Mapper240_Init(FC *fc, CartInfo *info) {
  return new Mapper240(fc, info, 0, 0x4020, 0x5FFF, 1, false);
}

//------------------ Map 241 ---------------------------
// Mapper 7 mostly, but with SRAM or maybe prot circuit
// figure out, which games do need 5xxx area reading
namespace {
struct Mapper241 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    fc->cart->setchr8(0);
    fc->cart->setprg8r(0x10, 0x6000, 0);
    fc->cart->setprg32(0x8000, latch);
  }
};
}

CartInterface *Mapper241_Init(FC *fc, CartInfo *info) {
  return new Mapper241(fc, info, 0, 0x8000, 0xFFFF, 1, false);
}

//------------------ A65AS ---------------------------

// actually, there is two cart in one... First have extra mirroring
// mode (one screen) and 32K bankswitching, second one have only
// 16 bankswitching mode and normal mirroring... But there is no any
// correlations between modes and they can be used in one mapper code.
namespace {
struct BMCA65AS final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    if (latch & 0x40) {
      fc->cart->setprg32(0x8000, (latch >> 1) & 0x0F);
    } else {
      fc->cart->setprg16(0x8000, ((latch & 0x30) >> 1) | (latch & 7));
      fc->cart->setprg16(0xC000, ((latch & 0x30) >> 1) | 7);
    }
    fc->cart->setchr8(0);
    if (latch & 0x80)
      fc->cart->setmirror(MI_0 + (((latch >> 5) & 1)));
    else
      fc->cart->setmirror(((latch >> 3) & 1) ^ 1);
  }
};
}

CartInterface *BMCA65AS_Init(FC *fc, CartInfo *info) {
  return new BMCA65AS(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

//------------------ BMC-11160 ---------------------------
// Simple BMC discrete mapper by TXC
namespace {
struct BMC11160 final : public DataLatch {
  using DataLatch::DataLatch;
  void WSync() final override {
    uint32 bank = (latch >> 4) & 7;
    fc->cart->setprg32(0x8000, bank);
    fc->cart->setchr8((bank << 2) | (latch & 3));
    fc->cart->setmirror((latch >> 7) & 1);
  }
};
}

CartInterface *BMC11160_Init(FC *fc, CartInfo *info) {
  return new BMC11160(fc, info, 0, 0x8000, 0xFFFF, 0, false);
}

