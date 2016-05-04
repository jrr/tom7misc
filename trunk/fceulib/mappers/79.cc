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
 * TXC mapper variation, F-15 City War
 */

#include "mapinc.h"

static DECLFW(Mapper79_write) {
  if (A < 0x8000 && ((A ^ 0x4100) == 0)) {
    fc->ines->ROM_BANK32((V >> 3) & 1);
  }
  fc->ines->VROM_BANK8(V);
}

MapInterface *Mapper79_init(FC *fc) {
  fc->ines->ROM_BANK32(~0);
  fc->fceu->SetWriteHandler(0x8000, 0xffff, Mapper79_write);
  fc->fceu->SetWriteHandler(0x4020, 0x5fff, Mapper79_write);
  return new MapInterface(fc);
}
