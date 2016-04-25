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
 * City Fighter IV sith Sound VRC4 hacked
 */

#include "mapinc.h"

namespace {
struct UNLCITYFIGHT final : public CartInterface {
  int32 IRQCount = 0;
  uint8 IRQa = 0;
  uint8 prg_reg = 0, prg_mode = 0, mirr = 0;
  uint8 chr_reg[8] = {};
  writefunc pcmwrite;

  void Sync() {
    fc->cart->setprg32(0x8000, prg_reg >> 2);
    if (!prg_mode) fc->cart->setprg8(0xC000, prg_reg);
    for (int i = 0; i < 8; i++) fc->cart->setchr1(i << 10, chr_reg[i]);
    switch (mirr) {
      case 0: fc->cart->setmirror(MI_V); break;
      case 1: fc->cart->setmirror(MI_H); break;
      case 2: fc->cart->setmirror(MI_0); break;
      case 3: fc->cart->setmirror(MI_1); break;
    }
  }

  void UNLCITYFIGHTWrite(DECLFW_ARGS) {
    // FCEU_printf("%04x %02x",A,V);
    switch (A & 0xF00C) {
      case 0x9000:
	prg_reg = V & 0xC;
	mirr = V & 3;
	break;
      case 0x9004:
      case 0x9008:
      case 0x900C:
	if (A & 0x800)
	  pcmwrite(fc, 0x4011, (V & 0xf) << 3);
	else
	  prg_reg = V & 0xC;
	break;
      case 0xC000:
      case 0xC004:
      case 0xC008:
      case 0xC00C: prg_mode = V & 1; break;
      case 0xD000: chr_reg[0] = (chr_reg[0] & 0xF0) | (V & 0x0F); break;
      case 0xD004: chr_reg[0] = (chr_reg[0] & 0x0F) | (V << 4); break;
      case 0xD008: chr_reg[1] = (chr_reg[1] & 0xF0) | (V & 0x0F); break;
      case 0xD00C: chr_reg[1] = (chr_reg[1] & 0x0F) | (V << 4); break;
      case 0xA000: chr_reg[2] = (chr_reg[2] & 0xF0) | (V & 0x0F); break;
      case 0xA004: chr_reg[2] = (chr_reg[2] & 0x0F) | (V << 4); break;
      case 0xA008: chr_reg[3] = (chr_reg[3] & 0xF0) | (V & 0x0F); break;
      case 0xA00C: chr_reg[3] = (chr_reg[3] & 0x0F) | (V << 4); break;
      case 0xB000: chr_reg[4] = (chr_reg[4] & 0xF0) | (V & 0x0F); break;
      case 0xB004: chr_reg[4] = (chr_reg[4] & 0x0F) | (V << 4); break;
      case 0xB008: chr_reg[5] = (chr_reg[5] & 0xF0) | (V & 0x0F); break;
      case 0xB00C: chr_reg[5] = (chr_reg[5] & 0x0F) | (V << 4); break;
      case 0xE000: chr_reg[6] = (chr_reg[6] & 0xF0) | (V & 0x0F); break;
      case 0xE004: chr_reg[6] = (chr_reg[6] & 0x0F) | (V << 4); break;
      case 0xE008: chr_reg[7] = (chr_reg[7] & 0xF0) | (V & 0x0F); break;
      case 0xE00C: chr_reg[7] = (chr_reg[7] & 0x0F) | (V << 4); break;
      case 0xF000: IRQCount = ((IRQCount & 0x1E0) | ((V & 0xF) << 1)); break;
      case 0xF004: IRQCount = ((IRQCount & 0x1E) | ((V & 0xF) << 5)); break;
      case 0xF008:
	IRQa = V & 2;
	fc->X->IRQEnd(FCEU_IQEXT);
	break;
      default: break;
    }
    Sync();
  }

  void UNLCITYFIGHTIRQ(int a) {
    if (IRQa) {
      IRQCount -= a;
      if (IRQCount <= 0) {
	fc->X->IRQBegin(FCEU_IQEXT);
      }
    }
  }

  void Power() final override {
    prg_reg = 0;
    Sync();
    pcmwrite = fc->fceu->GetWriteHandler(0x4011);
    fc->fceu->SetReadHandler(0x8000, 0xFFFF, Cart::CartBR);
    fc->fceu->SetWriteHandler(0x8000, 0xFFFF, [](DECLFW_ARGS) {
      ((UNLCITYFIGHT*)fc->fceu->cartiface)->UNLCITYFIGHTWrite(DECLFW_FORWARD);
    });
  }

  static void StateRestore(FC *fc, int version) {
    ((UNLCITYFIGHT *)fc->fceu->cartiface)->Sync();
  }

  UNLCITYFIGHT(FC *fc, CartInfo *info) : CartInterface(fc) {
    fc->X->MapIRQHook = [](FC *fc, int a) {
      return ((UNLCITYFIGHT *)fc->fceu->cartiface)->UNLCITYFIGHTIRQ(a);
    };
    fc->fceu->GameStateRestore = StateRestore;
    fc->state->AddExVec({{&IRQCount, 4, "IRQC"},
			 {&IRQa, 1, "IRQA"},
			 {&prg_reg, 1, "PREG"},
			 {&prg_mode, 1, "PMOD"},
			 {&mirr, 1, "MIRR"},
			 {chr_reg, 8, "CREG"}});
  }
};
}

CartInterface *UNLCITYFIGHT_Init(FC *fc, CartInfo *info) {
  return new UNLCITYFIGHT(fc, info);
}
