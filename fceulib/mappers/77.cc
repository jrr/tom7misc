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

/* Original code provided by LULU */

namespace {
struct Mapper77 final : public MapInterface {
  using MapInterface::MapInterface;

  void Mapper77_write(DECLFW_ARGS) {
    GMB_mapbyte1(fc)[0] = V;
    fc->ines->ROM_BANK32(V & 0x7);
    fc->ines->VROM_BANK2(0x0000, (V & 0xf0) >> 4);
  }

  void StateRestore(int version) final override {
    if (version >= 7200) {
      fc->ines->ROM_BANK32(GMB_mapbyte1(fc)[0] & 0x7);
      fc->ines->VROM_BANK2(0x0000, (GMB_mapbyte1(fc)[0] & 0xf0) >> 4);
    }
    for (int x = 2; x < 8; x++) fc->ines->VRAM_BANK1(x * 0x400, x);
  }
};
}

MapInterface *Mapper77_init(FC *fc) {
  Mapper77 *m = new Mapper77(fc);

  fc->ines->ROM_BANK32(0);
  for (int x = 2; x < 8; x++) fc->ines->VRAM_BANK1(x * 0x400, x);
  fc->fceu->SetWriteHandler(0x6000, 0xffff, [](DECLFW_ARGS) {
    ((Mapper77*)fc->fceu->mapiface)->Mapper77_write(DECLFW_FORWARD);
  });
  return m;
}

