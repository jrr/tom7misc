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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include "mapinc.h"

static uint16 cmdreg;
static SFORMAT StateRegs[]=
{
  {&cmdreg, 2, "CREG"},
  {0}
};

static void Sync(void)
{
  if(cmdreg&0x400)
    fceulib__cart.setmirror(MI_0);
  else
    fceulib__cart.setmirror(((cmdreg>>13)&1)^1);
  if(cmdreg&0x800) {
    fceulib__cart.setprg16(0x8000,((cmdreg&0x300)>>3)|((cmdreg&0x1F)<<1)|((cmdreg>>12)&1));
    fceulib__cart.setprg16(0xC000,((cmdreg&0x300)>>3)|((cmdreg&0x1F)<<1)|((cmdreg>>12)&1));
  }
  else {
    fceulib__cart.setprg32(0x8000,((cmdreg&0x300)>>4)|(cmdreg&0x1F));
  }
}

static DECLFW(M235Write)
{
  cmdreg=A;
  Sync();
}

static void M235Power(void)
{
  fceulib__cart.setchr8(0);
  SetWriteHandler(0x8000,0xFFFF,M235Write);
  SetReadHandler(0x8000,0xFFFF,Cart::CartBR);
  cmdreg=0;
  Sync();
}

static void M235Restore(int version)
{
  Sync();
}

void Mapper235_Init(CartInfo *info)
{
  info->Power=M235Power;
  GameStateRestore=M235Restore;
  AddExState(&StateRegs, ~0, 0, 0);
}
