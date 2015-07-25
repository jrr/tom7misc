/* FCE Ultra - NES/Famicom Emulator
 *
 * Copyright notice for this file:
 *  Copyright (C) 2007 CaH4e3
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
 *
 * FDS Conversion
 *
 */

#include "mapinc.h"

static uint8 reg, IRQa;
static int32 IRQCount;
static uint8 *WRAM = NULL;
static uint32 WRAMSIZE;

static SFORMAT StateRegs[] = {
    {&reg, 1, "REG"}, {&IRQa, 1, "IRQA"}, {&IRQCount, 4, "IRQC"}, {0}};

static void Sync() {
  fceulib__.cart->setchr8(0);
  fceulib__.cart->setprg8(0x6000, reg);
  fceulib__.cart->setprg8(0x8000, 0xc);
  fceulib__.cart->setprg4(0xa000, (0xd << 1));
  fceulib__.cart->setprg2(0xb000, (0xd << 2) + 2);
  fceulib__.cart->setprg2r(0x10, 0xb800, 4);
  fceulib__.cart->setprg2r(0x10, 0xc000, 5);
  fceulib__.cart->setprg2r(0x10, 0xc800, 6);
  fceulib__.cart->setprg2r(0x10, 0xd000, 7);
  fceulib__.cart->setprg2(0xd800, (0xe << 2) + 3);
  fceulib__.cart->setprg8(0xe000, 0xf);
}

static DECLFW(LH53RamWrite) {
  WRAM[(A - 0xB800) & 0x1FFF] = V;
}

static DECLFW(LH53Write) {
  reg = V;
  Sync();
}

static DECLFW(LH53IRQaWrite) {
  IRQa = V & 2;
  IRQCount = 0;
  if (!IRQa) fceulib__.X->IRQEnd(FCEU_IQEXT);
}

static void LH53IRQ(int a) {
  if (IRQa) {
    IRQCount += a;
    if (IRQCount > 7560) fceulib__.X->IRQBegin(FCEU_IQEXT);
  }
}

static void LH53Power(FC *fc) {
  Sync();
  fceulib__.fceu->SetReadHandler(0x6000, 0xFFFF, Cart::CartBR);
  fceulib__.fceu->SetWriteHandler(0xB800, 0xD7FF, LH53RamWrite);
  fceulib__.fceu->SetWriteHandler(0xE000, 0xEFFF, LH53IRQaWrite);
  fceulib__.fceu->SetWriteHandler(0xF000, 0xFFFF, LH53Write);
}

static void LH53Close(FC *fc) {
  free(WRAM);
  WRAM = nullptr;
}

static void StateRestore(FC *fc, int version) {
  Sync();
}

void LH53_Init(CartInfo *info) {
  info->Power = LH53Power;
  info->Close = LH53Close;
  fceulib__.X->MapIRQHook = LH53IRQ;
  fceulib__.fceu->GameStateRestore = StateRestore;

  WRAMSIZE = 8192;
  WRAM = (uint8 *)FCEU_gmalloc(WRAMSIZE);
  fceulib__.cart->SetupCartPRGMapping(0x10, WRAM, WRAMSIZE, 1);
  fceulib__.state->AddExState(WRAM, WRAMSIZE, 0, "WRAM");

  fceulib__.state->AddExState(&StateRegs, ~0, 0, 0);
}
