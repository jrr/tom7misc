/* FCE Ultra - NES/Famicom Emulator
 *
 * Copyright notice for this file:
 *  Copyright (C) 2009 CaH4e3
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

static uint8 prg[3], chr[8], mirr;
static uint8 IRQLatch, IRQa, IRQd;
static uint32 IRQCount, CycleCount;

static vector<SFORMAT> StateRegs = {
    {prg, 3, "PRG0"},        {chr, 8, "CHR0"},          {&mirr, 1, "MIRR"},
    {&IRQa, 1, "IRQA"},     {&IRQd, 1, "IRQD"},       {&IRQLatch, 1, "IRQC"},
    {&IRQCount, 4, "IRQC"}, {&CycleCount, 4, "CYCC"}};

static void Sync() {
  fceulib__.cart->setprg8(0x8000, prg[0]);
  fceulib__.cart->setprg8(0xa000, prg[1]);
  fceulib__.cart->setprg8(0xc000, prg[2]);
  fceulib__.cart->setprg8(0xe000, ~0);
  for (uint8 i = 0; i < 8; i++) fceulib__.cart->setchr1(i << 10, chr[i]);
  switch (mirr & 3) {
    case 0: fceulib__.cart->setmirror(MI_V); break;
    case 1: fceulib__.cart->setmirror(MI_H); break;
    case 2: fceulib__.cart->setmirror(MI_0); break;
    case 3: fceulib__.cart->setmirror(MI_1); break;
  }
}

static DECLFW(UNLVRC7Write) {
  switch (A & 0xF008) {
    case 0x8000:
      prg[0] = V;
      Sync();
      break;
    case 0x8008:
      prg[1] = V;
      Sync();
      break;
    case 0x9000:
      prg[2] = V;
      Sync();
      break;
    case 0xa000:
      chr[0] = V;
      Sync();
      break;
    case 0xa008:
      chr[1] = V;
      Sync();
      break;
    case 0xb000:
      chr[2] = V;
      Sync();
      break;
    case 0xb008:
      chr[3] = V;
      Sync();
      break;
    case 0xc000:
      chr[4] = V;
      Sync();
      break;
    case 0xc008:
      chr[5] = V;
      Sync();
      break;
    case 0xd000:
      chr[6] = V;
      Sync();
      break;
    case 0xd008:
      chr[7] = V;
      Sync();
      break;
    case 0xe000:
      mirr = V;
      Sync();
      break;
    case 0xe008:
      IRQLatch = V;
      fceulib__.X->IRQEnd(FCEU_IQEXT);
      break;
    case 0xf000:
      IRQa = V & 2;
      IRQd = V & 1;
      if (V & 2) IRQCount = IRQLatch;
      CycleCount = 0;
      fceulib__.X->IRQEnd(FCEU_IQEXT);
      break;
    case 0xf008:
      if (IRQd)
        IRQa = 1;
      else
        IRQa = 0;
      fceulib__.X->IRQEnd(FCEU_IQEXT);
      break;
  }
}

static void UNLVRC7Power(FC *fc) {
  Sync();
  fceulib__.fceu->SetReadHandler(0x8000, 0xFFFF, Cart::CartBR);
  fceulib__.fceu->SetWriteHandler(0x8000, 0xFFFF, UNLVRC7Write);
}

static void UNLVRC7IRQHook(FC *fc, int a) {
  if (IRQa) {
    CycleCount += a * 3;
    while (CycleCount >= 341) {
      CycleCount -= 341;
      IRQCount++;
      if (IRQCount == 248) {
        fceulib__.X->IRQBegin(FCEU_IQEXT);
        IRQCount = IRQLatch;
      }
    }
  }
}

static void StateRestore(FC *fc, int version) {
  Sync();
}

void UNLVRC7_Init(CartInfo *info) {
  info->Power = UNLVRC7Power;
  fceulib__.X->MapIRQHook = UNLVRC7IRQHook;
  fceulib__.fceu->GameStateRestore = StateRestore;
  fceulib__.state->AddExVec(StateRegs);
}