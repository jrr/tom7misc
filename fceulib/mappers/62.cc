/* FCE Ultra - NES/Famicom Emulator
 *
 * Copyright notice for this file:
 *  Copyright (C) 2003 CaH4e3
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

static DECLFW(Mapper62_write) {
  fc->ines->VROM_BANK8(((A & 0x1F) << 2) | (V & 0x03));
  if (A & 0x20) {
    fc->ines->ROM_BANK16(0x8000, (A & 0x40) | ((A >> 8) & 0x3F));
    fc->ines->ROM_BANK16(0xc000, (A & 0x40) | ((A >> 8) & 0x3F));
  } else
    fc->ines->ROM_BANK32(((A & 0x40) | ((A >> 8) & 0x3F)) >> 1);
  fc->ines->MIRROR_SET((A & 0x80) >> 7);
}

MapInterface *Mapper62_init(FC *fc) {
  fc->fceu->SetWriteHandler(0x8000, 0xffff, Mapper62_write);
  fc->ines->ROM_BANK32(0);
  return new MapInterface(fc);
}
