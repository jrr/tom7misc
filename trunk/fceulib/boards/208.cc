/* FCE Ultra - NES/Famicom Emulator
 *
 * Copyright notice for this file:
 *  Copyright (C) 2005 CaH4e3
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
#include "mmc3.h"

static constexpr uint8 lut[256] = {
    0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x49, 0x19, 0x09,
    0x59, 0x49, 0x19, 0x09, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59,
    0x51, 0x41, 0x11, 0x01, 0x51, 0x41, 0x11, 0x01, 0x59, 0x59, 0x59, 0x59,
    0x59, 0x59, 0x59, 0x59, 0x59, 0x49, 0x19, 0x09, 0x59, 0x49, 0x19, 0x09,
    0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x51, 0x41, 0x11, 0x01,
    0x51, 0x41, 0x11, 0x01, 0x00, 0x10, 0x40, 0x50, 0x00, 0x10, 0x40, 0x50,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x18, 0x48, 0x58,
    0x08, 0x18, 0x48, 0x58, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x10, 0x40, 0x50, 0x00, 0x10, 0x40, 0x50, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x08, 0x18, 0x48, 0x58, 0x08, 0x18, 0x48, 0x58,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x59, 0x59, 0x59, 0x59,
    0x59, 0x59, 0x59, 0x59, 0x58, 0x48, 0x18, 0x08, 0x58, 0x48, 0x18, 0x08,
    0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x50, 0x40, 0x10, 0x00,
    0x50, 0x40, 0x10, 0x00, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59, 0x59,
    0x58, 0x48, 0x18, 0x08, 0x58, 0x48, 0x18, 0x08, 0x59, 0x59, 0x59, 0x59,
    0x59, 0x59, 0x59, 0x59, 0x50, 0x40, 0x10, 0x00, 0x50, 0x40, 0x10, 0x00,
    0x01, 0x11, 0x41, 0x51, 0x01, 0x11, 0x41, 0x51, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x09, 0x19, 0x49, 0x59, 0x09, 0x19, 0x49, 0x59,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x11, 0x41, 0x51,
    0x01, 0x11, 0x41, 0x51, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x09, 0x19, 0x49, 0x59, 0x09, 0x19, 0x49, 0x59, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00,
};

namespace {
struct Mapper208 final : public MMC3 {
  uint8 EXPREGS[8] = {};

  void PWrap(uint32 A, uint8 V) final override {
    fc->cart->setprg32(0x8000, EXPREGS[5]);
  }

  void M208Write(DECLFW_ARGS) {
    EXPREGS[5] = (V & 0x1) | ((V >> 3) & 0x2);
    FixMMC3PRG(MMC3_cmd);
  }

  void M208ProtWrite(DECLFW_ARGS) {
    if (A <= 0x57FF)
      EXPREGS[4] = V;
    else
      EXPREGS[(A & 0x03)] = V ^ lut[EXPREGS[4]];
  }

  DECLFR_RET M208ProtRead(DECLFR_ARGS) {
    return EXPREGS[(A & 0x3)];
  }

  void Power() final override {
    EXPREGS[5] = 3;
    MMC3::Power();
    fc->fceu->SetWriteHandler(0x4800, 0x4FFF, [](DECLFW_ARGS) {
        ((Mapper208*)fc->fceu->cartiface)->M208Write(DECLFW_FORWARD);
      });
    fc->fceu->SetWriteHandler(0x5000, 0x5fff, [](DECLFW_ARGS) {
        ((Mapper208*)fc->fceu->cartiface)->M208ProtWrite(DECLFW_FORWARD);
      });
    fc->fceu->SetReadHandler(0x5800, 0x5FFF, [](DECLFR_ARGS) {
        return ((Mapper208*)fc->fceu->cartiface)->
          M208ProtRead(DECLFR_FORWARD);
      });
    fc->fceu->SetReadHandler(0x8000, 0xffff, Cart::CartBR);
  }

  Mapper208(FC *fc, CartInfo *info) : MMC3(fc, info, 128, 256, 0, 0) {
    fc->state->AddExState(EXPREGS, 6, 0, "EXPR");
  }
};
}

CartInterface *Mapper208_Init(FC *fc, CartInfo *info) {
  return new Mapper208(fc, info);
}
