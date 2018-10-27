/*
  Library interface to FCEUX. This is an attempt to proper encapsulation
  of the emulator so that there can be multiple instances at once and
  they can run in separate threads. Should work, although I can't promise
  comprehensive thread safety!

  TODO PERF: At some point I changed emulator so that it is rendering
  the frame regardless of whether Step or StepFull. Although the PPU
  code is pretty complicated and can't just be skipped, it seems like
  we could save a lot of work with a mode (e.g. template parameter)
  that didn't update the screen buffer. There could also be an
  approximate version of GetImage that renders the screen purely from
  the PPU state; I have written this several times. (It would not
  support e.g. intraframe scrolling, but in something like pftwo we
  only care about it for diagnostic output.)
*/

#ifndef __EMULATOR_H
#define __EMULATOR_H

#include <vector>
#include <string>

#include "types.h"

#include "fc.h"

using namespace std;

struct FCEUGI;
struct Emulator {
  static constexpr int AUDIO_SAMPLE_RATE = 44100;

  // Returns nullptr (or aborts) on error. Upon success, returns
  // a new-ly allocated instance.
  static Emulator *Create(const string &romfile);

  ~Emulator();

  // Serialize the state to the vector, allowing it to be restored
  // with Load. This version may use compression to minimize the
  // state size; for a much faster version, use SaveUncompressed.
  void Save(vector<uint8> *out);
  void Load(const vector<uint8> &in);

  // Make one emulator step with the given input.
  // Bits from MSB to LSB are
  //    RLDUTSBA (Right, Left, Down, Up, sTart, Select, B, A)
  //
  // Consider StepFull if you want video or sound.
  void Step(uint8 controller1, uint8 controller2);
  // High 8 bits are controller1, low are controller2.
  void Step16(uint16 controllers);
  
  // Copy the 0x800 bytes of RAM.
  void GetMemory(vector<uint8> *mem);
  vector<uint8> GetMemory();

  // Fancy stuff.

  // Same, but run the video and sound code as well. This is slower,
  // but allows calling GetImage and GetSound.
  void StepFull(uint8 controller1, uint8 controller2);
  void StepFull16(uint16 controllers);
  
  // Get image. StepFull must have been called to produce this frame,
  // or else who knows what's in there? (Note that restoring a state
  // will not necessarily restore the image.) Size is 256 x 256 pixels,
  // 4 color channels (bytes) per pixel in RGBA order, though only
  // 240 pixels high contain anything interesting.
  void GetImage(vector<uint8> *rgba) const;
  vector<uint8> GetImage() const;
  // Same, but Alpha, Red, Green, Blue.
  // (XXX: This actually returns BGRA. SDL requires some platform-
  // specific color channel reordering.)
  void GetImageARGB(vector<uint8> *abgr) const;
  vector<uint8> GetImageARGB() const;

  // Get sound. StepFull must have been called to produce this wave.
  // The result is a vector of signed 16-bit samples, mono.
  void GetSound(vector<int16> *wav);

  // Returns 64-bit checksum (based on MD5, endianness-dependent)
  // of RAM (only). Note there are other important bits of state.
  uint64 RamChecksum();
  // Same, of the RGBA image. We only look at 240 scanlines here.
  // Note that the image checksum can only be computed if compiling
  // without DISABLE_VIDEO and after calling StepFull.
  uint64 ImageChecksum();
  // Checksum for CPU (and PPU) state. This includes real state like
  // the registers, but not internal emulation state like timing
  // counters (to facilitate testing changes to the way emulation
  // works) or video state that cannot be inspected (to facilitate
  // excising code paths that don't affect CPU execution). Should
  // usually be paired with RamChecksum.
  uint64 CPUStateChecksum();

  // States often only differ by a small amount, so a way to reduce
  // their entropy is to diff them against a representative savestate.
  // This gets an uncompressed basis for the current state, which can
  // be used in the SaveEx and LoadEx routines.
  void GetBasis(vector<uint8> *out);

  // Save and load uncompressed. The memory will always be the same
  // size (Save and SaveEx may compress, which makes their output
  // significantly smaller), but this is the fastest in terms of CPU.
  void SaveUncompressed(vector<uint8> *out);
  vector<uint8> SaveUncompressed();
  void LoadUncompressed(const vector<uint8> &in);

  // Save and load with a basis vector. The vector can contain anything, and
  // doesn't even have to be the same length as an uncompressed save state,
  // but a state needs to be loaded with the same basis as it was saved.
  // basis can be NULL, and then these behave the same as Save/Load.
  void SaveEx(const vector<uint8> *basis, vector<uint8> *out);
  void LoadEx(const vector<uint8> *basis, const vector<uint8> &in);

  // Get the X Scroll offset from the PPU.
  // This may be confused by games that are not actually horizontally
  // scrolling or that do something funny like change the scroll position
  // during scanlines.
  uint32 GetXScroll() const;

  // Same for y scroll. This one was only lightly tested. -tom7 2018
  uint32 GetYScroll() const;

  // Access a 256*256 (row-major) array containing the current color
  // indices. For performance sake, this does not copy, so the caller
  // should make a copy before calling any other Emulator functions.
  // Additionally, each entry has control bits with unspecified
  // meaning, and should be bitwise-anded with INDEX_MASK in order to
  // get a NES palette index in [0, 63].
  static constexpr uint8 INDEX_MASK = 63;
  const uint8 *RawIndexedImage() const;
  // Creates a new 256x240 (row-major) vector containing pre-masked
  // NES palette indices in [0, 63].
  vector<uint8> IndexedImage() const;

  // Returns the X6502 register file packed into uint64, as
  // 0 PC (16 bit) A X Y S P
  // P is flags. The high 8 bits are always 0.
  uint64 Registers() const;

  // No bounds checking; idx must be in [0, 2047].
  uint8 ReadRAM(int idx) const;
  void SetRAM(int idx, uint8 value);
  
  // XXXXX debugging only.
  FC *GetFC() { return fc; }
  const FC *GetFC() const { return fc; }

 protected:
  // Use factory method.
  Emulator(FC *fc);

 private:
  bool DriverInitialize(FCEUGI *gi);
  bool LoadGame(const string &path);

  FC *fc = nullptr;

  // Joystick data. I think used for both controller 0 and 1. Part of
  // the "API". TODO: Move into FCEU or input object?
  uint32 joydata = 0;

  // Maybe we should consider supporting cloning, actually.
  Emulator(const Emulator &) = delete;
  Emulator &operator =(const Emulator &) = delete;
};


#endif
