#ifndef __PALETTE_H
#define __PALETTE_H

#include "fc.h"

struct PaletteEntry {
  uint8 r, g, b;
};

struct Palette {
 public:
  explicit Palette(FC *fc);

  // Aliases the current palette. Read by zapper, which kind of makes
  // sense, but maybe should be abstracted. -tom7
  const PaletteEntry *palo = nullptr;
  uint8 pale = 0;

  void ResetPalette();
  void LoadGamePalette();
  void SetNESDeemph(uint8 d, int force);

  // Gets the color for a particular index in the palette.
  // Note that the index appears to take into account flags stuffed
  // into the high bits by the PPU emulator; it is not the native
  // NES palette. (TODO: examine what these are and document!) -tom7
  void FCEUD_GetPalette(uint8 index, uint8 *r, uint8 *g, uint8 *b) const;

 private:
  uint8 lastd = 0;

  // It used to be possible to have a custom palette for a game;
  // I removed this but there may be further simplifications. -tom7
  PaletteEntry paletten[64]; // Mathematically generated palette.

  void CalculatePalette();
  void ChoosePalette();
  void WritePalette();

  void FCEUI_GetNTSCTH(int *tint, int *hue);

  void FCEUD_SetPalette(uint8 index, uint8 r, uint8 g, uint8 b);

  struct Color {
    Color() : r(0), g(0), b(0) {}
    uint8 r, g, b;
  };
  Color s_psdl[256] = {};

  FC *fc = nullptr;
};

#endif
