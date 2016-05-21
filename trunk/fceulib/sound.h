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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifndef _SOUND_H_
#define _SOUND_H_

#include <vector>
#include "state.h"
#include "fceu.h"

#include "x6502.h"
#include "fc.h"

struct EXPSOUND {
  void (*Fill)(FC *fc, int Count) = nullptr; /* Low quality ext sound. */

  /* NeoFill is for sound devices that are emulated in a more
     high-level manner(VRC7) in HQ mode.  Interestingly,
     this device has slightly better sound quality(updated more
     often) in lq mode than in high-quality mode.  Maybe that
     should be fixed. :)
  */
  void (*NeoFill)(FC *fc, int32 *Wave, int Count) = nullptr;
  void (*HiFill)(FC *fc) = nullptr;
  void (*HiSync)(FC *fc, int32 ts) = nullptr;

  void (*RChange)(FC *fc) = nullptr;
  void (*Kill)(FC *fc) = nullptr;
};

struct Sound {
  explicit Sound(FC *fc);

  int32 Wave[2048+512] = {};
  int32 WaveHi[40000] = {};
  int32 WaveFinal[2048+512] = {};

  EXPSOUND GameExpSound;

  int32 nesincsize = 0;

  void SetSoundVariables();
  int GetSoundBuffer(int32 **bufptr);
  int FlushEmulateSound();

  uint32 soundtsinc = 0;
  uint32 soundtsoffs = 0;

  // For LQ sound
  uint32 soundtsi = 0;
  int32 sqacc[2] = {};

  void FCEUSND_Power();
  void FCEUSND_Reset();
  void FCEUSND_SaveState();
  void FCEUSND_LoadState(int version);

  // Initialize sound constants. Sampling rate comes from fsettings.h.
  void FCEUI_InitSound();

  // Called every CPU frame to update sound.
  void FCEU_SoundCPUHook(int);

  // This block for experimental AOT support. If it doesn't stay, can
  // be made private / deleted.
  void FCEU_SoundCPUHookNoDMA(int);
  void DMCDMA();
  
  const std::vector<SFORMAT> &FCEUSND_STATEINFO() {
    return stateinfo;
  }

  uint32 SoundTS() const {
    return fc->X->timestamp - soundtsoffs;
  }

  // TODO: Indirect static hooks (which go through the global object)
  // should instead get a local sound object and call these.
  void Write_PSG_Direct(DECLFW_ARGS);
  void Write_DMCRegs_Direct(DECLFW_ARGS);
  void StatusWrite_Direct(DECLFW_ARGS);
  DECLFR_RET StatusRead_Direct(DECLFR_ARGS);
  void Write_IRQFM_Direct(DECLFW_ARGS);

 private:
  const std::vector<SFORMAT> stateinfo;

  struct ENVUNIT {
    uint8 Speed = 0;
    /* Fixed volume(1), and loop(2) */
    uint8 Mode = 0;
    uint8 DecCountTo1 = 0;
    uint8 decvolume = 0;
    int reloaddec = 0;
  };

  uint32 wlookup1[32] = {};
  uint32 wlookup2[203] = {};

  // Used when exporting WaveFinal.
  int32 sound_buffer_length = 0;

  uint8 TriCount = 0;
  uint8 TriMode = 0;

  int32 tristep = 0;

  /* Wave length counters. */
  int32 wlcount[4] = {0,0,0,0};

  /* $4017 / xx000000 */
  uint8 IRQFrameMode = 0;
  uint8 PSG[0x10];
  /* $4011 0xxxxxxx */
  uint8 RawDALatch = 0;

  /* Byte written to $4015 */
  // Influences lengthcount, which influences CPU (WritePSG)
  uint8 EnabledChannels = 0;

  ENVUNIT EnvUnits[3];

  int32 RectDutyCount[2] = {};
  uint8 sweepon[2] = {};
  int32 curfreq[2] = {};
  uint8 SweepCount[2] = {};

  // For noise generation. Seems to only affect wave output, not
  // cpu state.
  uint16 nreg = 0;

  uint8 fcnt = 0;
  int32 fhcnt = 0;
  int32 fhinc = 0;

  // Influences CPU (StatusRead)
  int32 lengthcount[4] = {};

  // Representation invariant: positive.
  int32 DMCacc = 1;

  // tom7 made uint32 from int32. The value is always
  // positive.
  uint32 DMCPeriod = 0;
  uint8 DMCBitCount = 0;

  /* writes to 4012 and 4013 */
  uint8 DMCAddressLatch = 0, DMCSizeLatch = 0;
  /* Write to $4010 */
  uint8 DMCFormat = 0;

  // Source address for DMC. Must be in 0-7fff.
  uint32 DMCAddress = 0;
  int32 DMCSize = 0;
  uint8 DMCShift = 0;
  uint8 SIRQStat = 0;

  uint8 DMCHaveDMA = 0;
  uint8 DMCDMABuf = 0;
  uint8 DMCHaveSample = 0;

  // State for RDoTriangleNoisePCMLQ
  uint32 triangle_noise_tcout = 0;
  int32 triangle_noise_triacc = 0;
  int32 triangle_noise_noiseacc = 0;

  void Dummyfunc() {}
  void (Sound::*DoNoise)() = &Sound::Dummyfunc;
  void (Sound::*DoTriangle)() = &Sound::Dummyfunc;
  void (Sound::*DoPCM)() = &Sound::Dummyfunc;
  void (Sound::*DoSQ1)() = &Sound::Dummyfunc;
  void (Sound::*DoSQ2)() = &Sound::Dummyfunc;

  uint32 ChannelBC[5] = {};


  void LoadDMCPeriod(uint8 V);
  void PrepDPCM();
  void SQReload(int x, uint8 V);
  int CheckFreq(uint32 cf, uint8 sr);

  void FrameSoundStuff(int V);
  void FrameSoundUpdate();
  void RDoPCM();

  void RDoSQ(int x);
  void RDoSQ1();
  void RDoSQ2();
  void RDoSQLQ();
  void RDoTriangle();
  void RDoTriangleNoisePCMLQ();
  void RDoNoise();

  void SetNESSoundMap();

  FC *fc = nullptr;
};

#endif
