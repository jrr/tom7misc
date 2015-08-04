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
 * Mortal Kombat 2 YOKO */

#include "mapinc.h"
#include "mmc3.h"

static uint8 reg[8];

static vector<SFORMAT> StateRegs = {{reg, 8, "REGS"}};

static void Sync() {
  //  FCEU_printf("(%02x, %02x)\n",reg[3],reg[4]);
  fceulib__.cart->setprg8(0x8000, reg[0]);
  fceulib__.cart->setprg8(0xA000, reg[1]);
  fceulib__.cart->setprg8(0xC000, reg[2]);
  fceulib__.cart->setprg8(0xE000, ~0);
  //  setchr2(0x0000,reg[3]);
  //  setchr2(0x0800,reg[4]);
  //  setchr2(0x1000,reg[5]);
  //  setchr2(0x1800,reg[6]);
  fceulib__.cart->setchr2(0x0000, reg[3]);
  fceulib__.cart->setchr2(0x0800, reg[4]);
  fceulib__.cart->setchr2(0x1000, reg[5]);
  fceulib__.cart->setchr2(0x1800, reg[6]);
}

static DECLFW(MCN22MWrite) {
  // FCEU_printf("bs %04x %02x\n",A,V);
  switch (A) {
    case 0x8c00:
    case 0x8c01:
    case 0x8c02: reg[A & 3] = V; break;
    case 0x8d10: reg[3] = V; break;
    case 0x8d11: reg[4] = V; break;
    case 0x8d16: reg[5] = V; break;
    case 0x8d17: reg[6] = V; break;
  }
  Sync();
}

static void MCN22MPower(FC *fc) {
  reg[0] = reg[1] = reg[2] = 0;
  Sync();
  fceulib__.fceu->SetReadHandler(0x8000, 0xFFFF, Cart::CartBR);
  fceulib__.fceu->SetWriteHandler(0x8000, 0xFFFF, MCN22MWrite);
}
/*
static void MCN22MIRQHook()
{
  int count = IRQCount;
  if(!count || IRQReload)
  {
     IRQCount = IRQLatch;
     IRQReload = 0;
  }
  else
     IRQCount--;
  if(!IRQCount)
  {
     if(IRQa)
     {
        fceulib__.X->IRQBegin(FCEU_IQEXT);
     }
  }
}
*/
static void StateRestore(FC *fc, int version) {
  Sync();
}

void UNLCN22M_Init(CartInfo *info) {
  info->Power = MCN22MPower;
  //  GameHBIRQHook=MCN22MIRQHook;
  fceulib__.fceu->GameStateRestore = StateRestore;
  fceulib__.state->AddExVec(StateRegs);
}