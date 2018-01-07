#ifndef _INPUT_H_
#define _INPUT_H_

#include <ostream>

#include "git.h"
#include "state.h"
#include "fceu.h"

#include "fc.h"

// This used to have Log/Load code for different exotic inputs, but now
// it's fairly committed to a pair of gamepads, since that's what the
// external interface of emulator.h loads. 
// old state saving approach; we should get rid of that and expose it some
// other way (right now the emulator client calls Step with the joypad inputs
// for that step, which precludes zapper and other exotic inputs). -tom7

// MBG TODO - COMBINE THESE INPUTC AND INPUTCFC
// (tom7: I think this TODO still makes sense...)

// Interface for standard joystic port device drivers.
struct InputC {
  explicit InputC(FC *fc);
  virtual uint8 Read(int w) {
    return 0;
  }
  virtual void Write(uint8 w) {}
  virtual void Strobe(int w) {}
  // update will be called if input is coming from the user. refresh
  // your logical state from user input devices
  virtual void Update(int w, void *data, int arg) {}
  virtual void SLHook(int w, uint8 *bg, uint8 *spr,
                      uint32 linets, int final) {}
  virtual void Draw(int w, uint8 *buf, int arg) {}

  virtual ~InputC() {}

 protected:
  FC *fc = nullptr;
};

struct InputCFC {
  explicit InputCFC(FC *fc);
  virtual uint8 Read(int w, uint8 ret) {
    return ret;
  }
  virtual void Write(uint8 v) {}
  virtual void Strobe() {}
  virtual void Update(void *data, int arg) {}
  virtual void SLHook(uint8 *bg, uint8 *spr, uint32 linets, int last) {}
  virtual void Draw(uint8 *buf, int arg) {}

 protected:
  FC *fc = nullptr;
};

struct Input {
  Input(FC *fc);
  ~Input();

  void FCEUI_ResetNES();
  void FCEUI_PowerNES();

  void FCEUI_FDSInsert();
  void FCEUI_FDSSelect();

  void FCEUI_VSUniCoin();
  void FCEUI_VSUniToggleDIP(int w);

  // tells the emulator whether a fourscore is attached
  void FCEUI_SetInputFourscore(bool attachFourscore);
  // tells whether a fourscore is attached
  bool FCEUI_GetInputFourscore();
  // tells whether the microphone is used
  bool FCEUI_GetInputMicrophone();

  void FCEUI_SetInput(int port, ESI type, void *ptr, int attrib);
  void FCEUI_SetInputFC(ESIFC type, void *ptr, int attrib);

  void FCEU_DrawInput(uint8 *buf);
  void UpdateInput();
  void InitializeInput();

  // called from PPU on scanline events.
  void InputScanlineHook(uint8 *bg, uint8 *spr, uint32 linets, int last);

  const std::vector<SFORMAT> &FCEUINPUT_STATEINFO() { return stateinfo; }

 private:
  friend struct InputC;

  struct FCPORT {
    int attrib = 0;
    ESIFC type = SIFC_NONE;
    void *ptr = nullptr;
    InputCFC *driver = nullptr;
  };
  
  struct JOYPORT {
    explicit JOYPORT(int w) : w(w) {}

    int w;
    int attrib = 0;
    ESI type = SI_NONE;
    void *ptr = nullptr;
    InputC *driver = nullptr;
  };
  
  struct GPC;
  struct GPCVS;
  struct Fami4C;
  
  // Joyports are where the emulator looks to find input during simulation.
  // They're set by FCEUI_SetInput. Each joyport knows its index (w), type,
  // and pointer to data. I think the data pointer for two gamepads is usually
  // the same. -tom7
  //
  // Ultimately these get copied into joy[4]. I don't know which is which
  // (see the confusing UpdateGP below) but it seems joy[0] is the least
  // significant byte of the pointer. -tom7
  JOYPORT joyports[2] = {JOYPORT(0), JOYPORT(1)};

  FCPORT portFC;
  
  const std::vector<SFORMAT> stateinfo;

  // This is used to strobe the microphone data on Famicom. Currently
  // dead because microphone is disabled at compile time, but maybe
  // we should support this. -tom7
  bool microphone = false;

  uint8 joy_readbit[2] = {0, 0};
  // HACK - should be static but movie needs it (still? -tom7)
  uint8 joy[4] = {0, 0, 0, 0};
  uint8 LastStrobe = 0;

  // set to true if the fourscore is attached
  bool FSAttached = false;

  uint8 F4ReadBit[2] = {};

  DECLFR_RET VSUNIRead1_Direct(DECLFR_ARGS);
  DECLFR_RET VSUNIRead0_Direct(DECLFR_ARGS);
  DECLFR_RET JPRead_Direct(DECLFR_ARGS);
  void B4016_Direct(DECLFW_ARGS);

  void SetInputStuff(int port);
  void SetInputStuffFC();

  void FCEU_DoSimpleCommand(int cmd);

  void UpdateGP(int w, void *data, int arg);

  uint8 ReadGP(int w);
  uint8 ReadGPVS(int w);
  void StrobeGP(int w);

  uint8 ReadFami4(int w, uint8 ret);
  void StrobeFami4();

  FC *fc = nullptr;
};

#endif  //_INPUT_H_
