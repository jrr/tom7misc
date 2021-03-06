/* FCE Ultra - NES/Famicom Emulator
*
* Copyright notice for this file:
*  Copyright (C) 2003 Xodnizel
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

#include <string>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include "types.h"
#include "x6502.h"
#include "fceu.h"
#include "ppu.h"
#include "sound.h"
#include "file.h"
#include "utils/endian.h"
#include "utils/memory.h"
#include "utils/crc32.h"

#include "cart.h"
#include "fds.h"
#include "ines.h"
#include "unif.h"
#include "palette.h"
#include "state.h"
#include "input.h"
#include "file.h"
#include "vsuni.h"
#include "ines.h"

#include "driver.h"

#include "tracing.h"

#include <fstream>
#include <sstream>

// XXX
#include "base/logging.h"

using namespace std;

FCEU::FCEU(FC *fc) : fc(fc) {
  GameMemBlock = (uint8*)FCEU_gmalloc(GAME_MEM_BLOCK_SIZE);
  RAM = (uint8*)FCEU_gmalloc(0x800);
  XBuf = (uint8*)FCEU_gmalloc(256 * 256);
  XBackBuf = (uint8*)FCEU_gmalloc(256 * 256);
}

FCEUGI::FCEUGI() { }

FCEUGI::~FCEUGI() { }

void FCEU::FCEU_CloseGame() {
  if (GameInfo != nullptr) {
    CHECK(GameInterface != nullptr);
    GameInterface(fc, GI_CLOSE);

    fc->state->ResetExState(nullptr, nullptr);

    delete GameInfo;
    GameInfo = nullptr;
  }

  // Zero out all the buffers I own. FCEU didn't do this (just xbuf),
  // but it seems like the sane thing to do. -tom7
  memset(GameMemBlock, 0, GAME_MEM_BLOCK_SIZE);
  memset(RAM, 0, 0x800);
  memset(XBuf, 0, 256 * 256);
  memset(XBackBuf, 0, 256 * 256);
}

static DECLFW(BNull) {
}

static DECLFR(ANull) {
  TRACEF("Read unmapped: %02x", fc->X->DB);
  return fc->X->DB;
}

readfunc FCEU::GetReadHandler(int32 a) {
  return ARead[a];
}

writefunc FCEU::GetWriteHandler(int32 a) {
  return BWrite[a];
}

void FCEU::SetWriteHandler(int32 start, int32 end, writefunc func) {
  if (!func)
    func = BNull;

  for (int32 x = start; x <= end; x++) {
    BWrite[x] = func;
  }
}

FCEU::~FCEU() {
  free(GameMemBlock);
  free(RAM);
  free(XBuf);
  free(XBackBuf);
}


static DECLFW(WriteRamNoMask) {
  fc->fceu->RAM[A] = V;
}

static DECLFW(WriteRamMask) {
  fc->fceu->RAM[A & 0x7FF] = V;
}

static DECLFR(ReadRamNoMask) {
  return fc->fceu->RAM[A];
}

static DECLFR(ReadRamMask) {
  return fc->fceu->RAM[A & 0x7FF];
}

void FCEU::ResetGameLoaded() {
  if (GameInfo) FCEU_CloseGame();
  GameStateRestore = nullptr;
  fc->ppu->PPU_hook = nullptr;
  fc->ppu->GameHBIRQHook = nullptr;
  // Probably this should happen within sound itself.
  if (fc->sound->GameExpSound.Kill)
    fc->sound->GameExpSound.Kill(fc);
  fc->sound->GameExpSound = EXPSOUND();
  fc->X->MapIRQHook = nullptr;
  fc->ppu->MMC5Hack = 0;
  PAL &= 1;
  fc->palette->pale = 0;
}

FCEUGI *FCEU::FCEUI_LoadGame(const char *name, int OverwriteVidMode) {
  //----------
  //attempt to open the files
  // FCEU_printf("Loading %s...\n\n",name);

  FceuFile *fp = FCEU_fopen(name,"rb",0);

  if (!fp) {
    FCEU_PrintError("Error opening \"%s\"!",name);
    return 0;
  }

  // file opened ok. start loading.

  ResetGameLoaded();

  // reset parameters so they're cleared just in case a format's loader
  // doesnt know to do the clearing
  fc->ines->ClearMasterRomInfoParams();

  FCEU_CloseGame();
  GameInfo = new FCEUGI();

  GameInfo->type = GIT_CART;
  GameInfo->vidsys = GIV_USER;
  GameInfo->input[0] = GameInfo->input[1] = SI_UNSET;
  GameInfo->inputfc = SIFC_UNSET;
  GameInfo->cspecial = SIS_NONE;

  // Try to load each different format
  if (fc->ines->iNESLoad(name, fp, OverwriteVidMode) ||
      fc->unif->UNIFLoad(name, fp) ||
      fc->fds->FDSLoad(name, fp)) {

    FCEU_fclose(fp);
    FCEU_ResetVidSys();

    PowerNES();
    TRACEF("PowerNES done.");

    fc->palette->ResetPalette();

    return GameInfo;
  }
    
  FCEU_PrintError("An error occurred while loading the file.");
  FCEU_fclose(fp);

  delete GameInfo;
  GameInfo = nullptr;

  return nullptr;
}


// Return: Flag that indicates whether the function was succesful or not.
bool FCEU::FCEUI_Initialize() {
  GameInterface = (void (*)(FC *, GI))0xDEADBEEF;

  fc->X->Init();

  return true;
}

// Emulates a single frame.
// "skip" no longer does anything for PPU; I'm trying to accomplish this
// with compile-time flags instead. Probably should remove it entirely.
//  -tom7
void FCEU::FCEUI_Emulate(int skip) {
  fc->input->UpdateInput();

  // fprintf(stderr, "ppu loop..\n");

  fc->ppu->FrameLoop();

  // fprintf(stderr, "sound thing loop skip=%d..\n", skip);

  // If skip = 2 we are skipping sound processing
  if (skip != 2)
    (void)fc->sound->FlushEmulateSound();

  // This is where cheat list stuff happened.
  timestampbase += fc->X->timestamp;
  fc->X->timestamp = 0;
}

void FCEU::ResetNES() {
  if (GameInfo == nullptr) return;
  GameInterface(fc, GI_RESETM2);
  fc->sound->FCEUSND_Reset();
  fc->ppu->FCEUPPU_Reset();
  fc->X->Reset();

  // clear back baffer
  memset(XBackBuf, 0, 256 * 256);
}

void FCEU::PowerNES() {
  if (GameInfo == nullptr) return;

  FCEU_InitMemory(RAM, 0x800);

  SetReadHandler(0x0000, 0xFFFF, ANull);
  SetWriteHandler(0x0000, 0xFFFF, BNull);

  // These reads wrap (mask into the 0x800 bytes of RAM), but we
  // can avoid the mask when we know the read is already in range.
  SetReadHandler(0, 0x7FF, ReadRamNoMask);
  SetWriteHandler(0, 0x7FF, WriteRamNoMask);
  SetReadHandler(0x800, 0x1FFF, ReadRamMask);
  SetWriteHandler(0x800, 0x1FFF, WriteRamMask);

  fc->input->InitializeInput();
  fc->sound->FCEUSND_Power();
  fc->ppu->FCEUPPU_Power();

  // Have the external game hardware "powered" after the internal NES
  // stuff. Needed for the NSF code and VS System code.
  GameInterface(fc, GI_POWER);
  if (GameInfo->type == GIT_VSUNI)
    fc->vsuni->FCEU_VSUniPower();

  // if we are in a movie, then reset the saveram
  if (fc->cart->disableBatteryLoading)
    GameInterface(fc, GI_RESETSAVE);

  timestampbase = 0ULL;
  fc->X->Power();

  // clear back baffer
  memset(XBackBuf, 0, 256 * 256);

  // fprintf(stderr, "Power on\n");
}

void FCEU::FCEU_ResetVidSys() {
  int w;

  if (GameInfo->vidsys == GIV_NTSC)
    w = 0;
  else if (GameInfo->vidsys == GIV_PAL)
    w = 1;
  else
    w = fsettings_pal;

  PAL = !!w;
  fc->ppu->SetVideoSystem(w);
  fc->sound->SetSoundVariables();
}

void FCEU::SetVidSystem(int a) {
  fsettings_pal = a ? 1 : 0;
  if (GameInfo) {
    FCEU_ResetVidSys();
    fc->palette->ResetPalette();
  }
}

bool FCEU::FCEU_IsValidUI(EFCEUI ui) {
  switch(ui) {
  case FCEUI_RESET:
  case FCEUI_POWER:
  case FCEUI_EJECT_DISK:
  case FCEUI_SWITCH_DISK:
    if (!GameInfo) return false;
    break;
  }
  return true;
}

void FCEU::SetReadHandler(int32 start, int32 end, readfunc func) {
  if (!func)
    func = ANull;

  for (int x = start; x <= end; x++) {
    ARead[x] = func;
  }
}

// This is kind of silly since it just eta-expands printf with
// a limit of 2k. Probably nothing should be printing in fceulib
// unless there's an error, though.
void FCEU_printf(const char *format, ...) {
  char temp[2048];

  va_list ap;

  va_start(ap,format);
  vsnprintf(temp,sizeof(temp),format,ap);

  fputs(temp, stdout);

  va_end(ap);
}

void FCEU_PrintError(const char *format, ...) {
  char temp[2048];

  va_list ap;

  va_start(ap,format);
  vsnprintf(temp,sizeof(temp),format,ap);
  FCEUD_PrintError(temp);

  va_end(ap);
}

void FCEU_InitMemory(uint8 *ptr, uint32 size) {
  int x = 0;
  while (size) {
    *ptr = (x & 4) ? 0xFF : 0x00;
    x++;
    size--;
    ptr++;
  }
}
