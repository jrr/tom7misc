/* FCE Ultra - NES/Famicom Emulator
 *
 * Copyright notice for this file:
 *  Copyright (C) 2012 CaH4e3
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

static uint8 mram[4], vreg;
static uint16 areg;

static SFORMAT StateRegs[] = {
    {mram, 4, "MRAM"}, {&areg, 2, "AREG"}, {&vreg, 1, "VREG"}, {0}};

static void Sync() {
  uint32 prgl, prgh, page = (areg >> 7) & 0x3F;
  if ((page & 0x30) == 0x30) page -= 0x10;
  prgl = prgh = (page << 1) + (((areg >> 6) & 1) & ((areg >> 5) & 1));
  prgh += ((areg >> 5) & 1) ^ 1;

  fceulib__.cart->setmirror(((areg >> 13) & 1) ^ 1);
  fceulib__.cart->setprg16(0x8000, prgl);
  fceulib__.cart->setprg16(0xc000, prgh);
  fceulib__.cart->setchr8(((vreg & 0x3) | ((areg & 0xF) << 2)));
}

static DECLFW(M228RamWrite) {
  mram[A & 3] = V & 0x0F;
}

static DECLFR(M228RamRead) {
  return mram[A & 3];
}

static DECLFW(M228Write) {
  areg = A;
  vreg = V;
  Sync();
}

static void M228Reset(FC *fc) {
  areg = 0x8000;
  vreg = 0;
  Sync();
}

static void M228Power(FC *fc) {
  M228Reset(fc);
  fceulib__.fceu->SetReadHandler(0x5000, 0x5FFF, M228RamRead);
  fceulib__.fceu->SetWriteHandler(0x5000, 0x5FFF, M228RamWrite);
  fceulib__.fceu->SetReadHandler(0x8000, 0xFFFF, Cart::CartBR);
  fceulib__.fceu->SetWriteHandler(0x8000, 0xFFFF, M228Write);
}

static void StateRestore(FC *fc, int version) {
  Sync();
}

void Mapper228_Init(CartInfo *info) {
  info->Reset = M228Reset;
  info->Power = M228Power;
  fceulib__.fceu->GameStateRestore = StateRestore;
  fceulib__.state->AddExState(&StateRegs, ~0, 0, 0);
}
