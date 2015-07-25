/* FCE Ultra - NES/Famicom Emulator
 *
 * Copyright notice for this file:
 *  Copyright (C) 2006 CaH4e3
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

static uint16 latche, latcheinit;
static uint16 addrreg0, addrreg1;
static uint8 dipswitch;
static void (*WSync)();
static readfunc defread;
static uint8 *WRAM = NULL;
static uint32 WRAMSIZE;

static DECLFW(LatchWrite) {
  latche = A;
  WSync();
}

static void LatchReset(FC *fc) {
  latche = latcheinit;
  WSync();
}

static void LatchPower(FC *fc) {
  latche = latcheinit;
  WSync();
  if (WRAM) {
    fceulib__.fceu->SetReadHandler(0x6000, 0x7FFF, Cart::CartBR);
    fceulib__.fceu->SetWriteHandler(0x6000, 0x7FFF, Cart::CartBW);
  } else {
    fceulib__.fceu->SetReadHandler(0x6000, 0xFFFF, defread);
  }
  fceulib__.fceu->SetWriteHandler(addrreg0, addrreg1, LatchWrite);
}

static void LatchClose(FC *fc) {
  free(WRAM);
  WRAM = nullptr;
}

static void StateRestore(FC *fc, int version) {
  WSync();
}

static void Latch_Init(CartInfo *info, void (*proc)(), readfunc func,
                       uint16 linit, uint16 adr0, uint16 adr1, uint8 wram) {
  latcheinit = linit;
  addrreg0 = adr0;
  addrreg1 = adr1;
  WSync = proc;
  if (func != NULL)
    defread = func;
  else
    defread = Cart::CartBROB;
  info->Power = LatchPower;
  info->Reset = LatchReset;
  info->Close = LatchClose;
  if (wram) {
    WRAMSIZE = 8192;
    WRAM = (uint8 *)FCEU_gmalloc(WRAMSIZE);
    fceulib__.cart->SetupCartPRGMapping(0x10, WRAM, WRAMSIZE, 1);
    if (info->battery) {
      info->SaveGame[0] = WRAM;
      info->SaveGameLen[0] = WRAMSIZE;
    }
    fceulib__.state->AddExState(WRAM, WRAMSIZE, 0, "WRAM");
  }
  fceulib__.fceu->GameStateRestore = StateRestore;
  fceulib__.state->AddExState(&latche, 2, 0, "LATC");
}

//------------------ UNLCC21 ---------------------------

static void UNLCC21Sync() {
  fceulib__.cart->setprg32(0x8000, 0);
  fceulib__.cart->setchr8(latche & 1);
  fceulib__.cart->setmirror(MI_0 + ((latche & 2) >> 1));
}

void UNLCC21_Init(CartInfo *info) {
  Latch_Init(info, UNLCC21Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}

//------------------ BMCD1038 ---------------------------

static void BMCD1038Sync() {
  if (latche & 0x80) {
    fceulib__.cart->setprg16(0x8000, (latche & 0x70) >> 4);
    fceulib__.cart->setprg16(0xC000, (latche & 0x70) >> 4);
  } else {
    fceulib__.cart->setprg32(0x8000, (latche & 0x60) >> 5);
  }
  fceulib__.cart->setchr8(latche & 7);
  fceulib__.cart->setmirror(((latche & 8) >> 3) ^ 1);
}

static DECLFR(BMCD1038Read) {
  if (latche & 0x100)
    return dipswitch;
  else
    return Cart::CartBR(DECLFR_FORWARD);
}

static void BMCD1038Reset(FC *fc) {
  dipswitch++;
  dipswitch &= 3;
}

void BMCD1038_Init(CartInfo *info) {
  Latch_Init(info, BMCD1038Sync, BMCD1038Read, 0x0000, 0x8000, 0xFFFF, 0);
  info->Reset = BMCD1038Reset;
  fceulib__.state->AddExState(&dipswitch, 1, 0, "DIPSW");
}

//------------------ UNL43272 ---------------------------
// mapper much complex, including 16K bankswitching
static void UNL43272Sync() {
  if ((latche & 0x81) == 0x81) {
    fceulib__.cart->setprg32(0x8000, (latche & 0x38) >> 3);
  } else
    FCEU_printf("unrecognized command %04!\n", latche);
  fceulib__.cart->setchr8(0);
  fceulib__.cart->setmirror(0);
}

static DECLFR(UNL43272Read) {
  if (latche & 0x400)
    return Cart::CartBR(fc, A & 0xFE);
  else
    return Cart::CartBR(DECLFR_FORWARD);
}

static void UNL43272Reset(FC *fc) {
  latche = 0;
  UNL43272Sync();
}

void UNL43272_Init(CartInfo *info) {
  Latch_Init(info, UNL43272Sync, UNL43272Read, 0x0081, 0x8000, 0xFFFF, 0);
  info->Reset = UNL43272Reset;
  fceulib__.state->AddExState(&dipswitch, 1, 0, "DIPSW");
}

//------------------ Map 058 ---------------------------

static void BMCGK192Sync() {
  if (latche & 0x40) {
    fceulib__.cart->setprg16(0x8000, latche & 7);
    fceulib__.cart->setprg16(0xC000, latche & 7);
  } else {
    fceulib__.cart->setprg32(0x8000, (latche >> 1) & 3);
  }
  fceulib__.cart->setchr8((latche >> 3) & 7);
  fceulib__.cart->setmirror(((latche & 0x80) >> 7) ^ 1);
}

void BMCGK192_Init(CartInfo *info) {
  Latch_Init(info, BMCGK192Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}

//------------------ Map 059 ---------------------------
// One more forbidden mapper
static void M59Sync() {
  fceulib__.cart->setprg32(0x8000, (latche >> 4) & 7);
  fceulib__.cart->setchr8(latche & 0x7);
  fceulib__.cart->setmirror((latche >> 3) & 1);
}

static DECLFR(M59Read) {
  if (latche & 0x100)
    return 0;
  else
    return Cart::CartBR(DECLFR_FORWARD);
}

void Mapper59_Init(CartInfo *info) {
  Latch_Init(info, M59Sync, M59Read, 0x0000, 0x8000, 0xFFFF, 0);
}

//------------------ Map 092 ---------------------------
// Another two-in-one mapper, two Jaleco carts uses similar
// hardware, but with different wiring.
// Original code provided by LULU
// Additionally, PCB contains DSP extra sound chip, used for voice samples
// (unemulated)

static void M92Sync() {
  uint8 reg = latche & 0xF0;
  fceulib__.cart->setprg16(0x8000, 0);
  if (latche >= 0x9000) {
    switch (reg) {
      case 0xD0: fceulib__.cart->setprg16(0xc000, latche & 15); break;
      case 0xE0: fceulib__.cart->setchr8(latche & 15); break;
    }
  } else {
    switch (reg) {
      case 0xB0: fceulib__.cart->setprg16(0xc000, latche & 15); break;
      case 0x70: fceulib__.cart->setchr8(latche & 15); break;
    }
  }
}

void Mapper92_Init(CartInfo *info) {
  Latch_Init(info, M92Sync, NULL, 0x80B0, 0x8000, 0xFFFF, 0);
}

//------------------ Map 200 ---------------------------

static void M200Sync() {
  fceulib__.cart->setprg16(0x8000, latche & 7);
  fceulib__.cart->setprg16(0xC000, latche & 7);
  fceulib__.cart->setchr8(latche & 7);
  fceulib__.cart->setmirror((latche & 8) >> 3);
}

void Mapper200_Init(CartInfo *info) {
  Latch_Init(info, M200Sync, NULL, 0xFFFF, 0x8000, 0xFFFF, 0);
}

//------------------ Map 201 ---------------------------

static void M201Sync() {
  if (latche & 8) {
    fceulib__.cart->setprg32(0x8000, latche & 3);
    fceulib__.cart->setchr8(latche & 3);
  } else {
    fceulib__.cart->setprg32(0x8000, 0);
    fceulib__.cart->setchr8(0);
  }
}

void Mapper201_Init(CartInfo *info) {
  Latch_Init(info, M201Sync, NULL, 0xFFFF, 0x8000, 0xFFFF, 0);
}

//------------------ Map 202 ---------------------------

static void M202Sync() {
  // According to more carefull hardware tests and PCB study
  int32 mirror = latche & 1;
  int32 bank = (latche >> 1) & 0x7;
  int32 select = (mirror & (bank >> 2));
  fceulib__.cart->setprg16(0x8000, select ? (bank & 6) | 0 : bank);
  fceulib__.cart->setprg16(0xc000, select ? (bank & 6) | 1 : bank);
  fceulib__.cart->setmirror(mirror ^ 1);
  fceulib__.cart->setchr8(bank);
}

void Mapper202_Init(CartInfo *info) {
  Latch_Init(info, M202Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}

//------------------ Map 204 ---------------------------

static void M204Sync() {
  int32 tmp2 = latche & 0x6;
  int32 tmp1 = tmp2 + ((tmp2 == 0x6) ? 0 : (latche & 1));
  fceulib__.cart->setprg16(0x8000, tmp1);
  fceulib__.cart->setprg16(0xc000, tmp2 + ((tmp2 == 0x6) ? 1 : (latche & 1)));
  fceulib__.cart->setchr8(tmp1);
  fceulib__.cart->setmirror(((latche >> 4) & 1) ^ 1);
}

void Mapper204_Init(CartInfo *info) {
  Latch_Init(info, M204Sync, NULL, 0xFFFF, 0x8000, 0xFFFF, 0);
}

//------------------ Map 212 ---------------------------

static DECLFR(M212Read) {
  uint8 ret = Cart::CartBROB(DECLFR_FORWARD);
  if ((A & 0xE010) == 0x6000) ret |= 0x80;
  return ret;
}

static void M212Sync() {
  if (latche & 0x4000) {
    fceulib__.cart->setprg32(0x8000, (latche >> 1) & 3);
  } else {
    fceulib__.cart->setprg16(0x8000, latche & 7);
    fceulib__.cart->setprg16(0xC000, latche & 7);
  }
  fceulib__.cart->setchr8(latche & 7);
  fceulib__.cart->setmirror(((latche >> 3) & 1) ^ 1);
}

void Mapper212_Init(CartInfo *info) {
  Latch_Init(info, M212Sync, M212Read, 0xFFFF, 0x8000, 0xFFFF, 0);
}

//------------------ Map 213 ---------------------------

static void M213Sync() {
  fceulib__.cart->setprg32(0x8000, (latche >> 1) & 3);
  fceulib__.cart->setchr8((latche >> 3) & 7);
}

void Mapper213_Init(CartInfo *info) {
  Latch_Init(info, M213Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}

//------------------ Map 214 ---------------------------

static void M214Sync() {
  fceulib__.cart->setprg16(0x8000, (latche >> 2) & 3);
  fceulib__.cart->setprg16(0xC000, (latche >> 2) & 3);
  fceulib__.cart->setchr8(latche & 3);
}

void Mapper214_Init(CartInfo *info) {
  Latch_Init(info, M214Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}

//------------------ Map 217 ---------------------------

static void M217Sync() {
  fceulib__.cart->setprg32(0x8000, (latche >> 2) & 3);
  fceulib__.cart->setchr8(latche & 7);
}

void Mapper217_Init(CartInfo *info) {
  Latch_Init(info, M217Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}

//------------------ Map 227 ---------------------------

static void M227Sync() {
  uint32 S = latche & 1;
  uint32 p = ((latche >> 2) & 0x1F) + ((latche & 0x100) >> 3);
  uint32 L = (latche >> 9) & 1;

  if ((latche >> 7) & 1) {
    if (S) {
      fceulib__.cart->setprg32(0x8000, p >> 1);
    } else {
      fceulib__.cart->setprg16(0x8000, p);
      fceulib__.cart->setprg16(0xC000, p);
    }
  } else {
    if (S) {
      if (L) {
        fceulib__.cart->setprg16(0x8000, p & 0x3E);
        fceulib__.cart->setprg16(0xC000, p | 7);
      } else {
        fceulib__.cart->setprg16(0x8000, p & 0x3E);
        fceulib__.cart->setprg16(0xC000, p & 0x38);
      }
    } else {
      if (L) {
        fceulib__.cart->setprg16(0x8000, p);
        fceulib__.cart->setprg16(0xC000, p | 7);
      } else {
        fceulib__.cart->setprg16(0x8000, p);
        fceulib__.cart->setprg16(0xC000, p & 0x38);
      }
    }
  }

  fceulib__.cart->setmirror(((latche >> 1) & 1) ^ 1);
  fceulib__.cart->setchr8(0);
  fceulib__.cart->setprg8r(0x10, 0x6000, 0);
}

void Mapper227_Init(CartInfo *info) {
  Latch_Init(info, M227Sync, NULL, 0x0000, 0x8000, 0xFFFF, 1);
}

//------------------ Map 229 ---------------------------

static void M229Sync() {
  fceulib__.cart->setchr8(latche);
  if (!(latche & 0x1e)) {
    fceulib__.cart->setprg32(0x8000, 0);
  } else {
    fceulib__.cart->setprg16(0x8000, latche & 0x1F);
    fceulib__.cart->setprg16(0xC000, latche & 0x1F);
  }
  fceulib__.cart->setmirror(((latche >> 5) & 1) ^ 1);
}

void Mapper229_Init(CartInfo *info) {
  Latch_Init(info, M229Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}

//------------------ Map 231 ---------------------------

static void M231Sync() {
  fceulib__.cart->setchr8(0);
  if (latche & 0x20) {
    fceulib__.cart->setprg32(0x8000, (latche >> 1) & 0x0F);
  } else {
    fceulib__.cart->setprg16(0x8000, latche & 0x1E);
    fceulib__.cart->setprg16(0xC000, latche & 0x1E);
  }
  fceulib__.cart->setmirror(((latche >> 7) & 1) ^ 1);
}

void Mapper231_Init(CartInfo *info) {
  Latch_Init(info, M231Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}

//------------------ Map 242 ---------------------------

static void M242Sync() {
  fceulib__.cart->setchr8(0);
  fceulib__.cart->setprg8r(0x10, 0x6000, 0);
  fceulib__.cart->setprg32(0x8000, (latche >> 3) & 0xf);
  fceulib__.cart->setmirror(((latche >> 1) & 1) ^ 1);
}

void Mapper242_Init(CartInfo *info) {
  Latch_Init(info, M242Sync, NULL, 0x0000, 0x8000, 0xFFFF, 1);
}

//------------------ 190in1 ---------------------------

static void BMC190in1Sync() {
  fceulib__.cart->setprg16(0x8000, (latche >> 2) & 7);
  fceulib__.cart->setprg16(0xC000, (latche >> 2) & 7);
  fceulib__.cart->setchr8((latche >> 2) & 7);
  fceulib__.cart->setmirror((latche & 1) ^ 1);
}

void BMC190in1_Init(CartInfo *info) {
  Latch_Init(info, BMC190in1Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}

//-------------- BMC810544-C-A1 ------------------------

static void BMC810544CA1Sync() {
  uint32 bank = latche >> 7;
  if (latche & 0x40) {
    fceulib__.cart->setprg32(0x8000, bank);
  } else {
    fceulib__.cart->setprg16(0x8000, (bank << 1) | ((latche >> 5) & 1));
    fceulib__.cart->setprg16(0xC000, (bank << 1) | ((latche >> 5) & 1));
  }
  fceulib__.cart->setchr8(latche & 0x0f);
  fceulib__.cart->setmirror(((latche >> 4) & 1) ^ 1);
}

void BMC810544CA1_Init(CartInfo *info) {
  Latch_Init(info, BMC810544CA1Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}

//-------------- BMCNTD-03 ------------------------

static void BMCNTD03Sync() {
  // 1PPP Pmcc spxx xccc
  // 1000 0000 0000 0000 v
  // 1001 1100 0000 0100 h
  // 1011 1010 1100 0100
  uint32 prg = ((latche >> 10) & 0x1e);
  uint32 chr = ((latche & 0x0300) >> 5) | (latche & 7);
  if (latche & 0x80) {
    fceulib__.cart->setprg16(0x8000, prg | ((latche >> 6) & 1));
    fceulib__.cart->setprg16(0xC000, prg | ((latche >> 6) & 1));
  } else {
    fceulib__.cart->setprg32(0x8000, prg >> 1);
  }
  fceulib__.cart->setchr8(chr);
  fceulib__.cart->setmirror(((latche >> 10) & 1) ^ 1);
}

void BMCNTD03_Init(CartInfo *info) {
  Latch_Init(info, BMCNTD03Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}

//-------------- BMCG-146 ------------------------

static void BMCG146Sync() {
  fceulib__.cart->setchr8(0);
  if (latche & 0x800) {  // UNROM mode
    fceulib__.cart->setprg16(
        0x8000, (latche & 0x1F) | (latche & ((latche & 0x40) >> 6)));
    fceulib__.cart->setprg16(0xC000, (latche & 0x18) | 7);
  } else {
    if (latche & 0x40) {  // 16K mode
      fceulib__.cart->setprg16(0x8000, latche & 0x1F);
      fceulib__.cart->setprg16(0xC000, latche & 0x1F);
    } else {
      fceulib__.cart->setprg32(0x8000, (latche >> 1) & 0x0F);
    }
  }
  fceulib__.cart->setmirror(((latche & 0x80) >> 7) ^ 1);
}

void BMCG146_Init(CartInfo *info) {
  Latch_Init(info, BMCG146Sync, NULL, 0x0000, 0x8000, 0xFFFF, 0);
}
