// Self-contained bitmap font.
// Only use C++ builtins/std.
// XXX Still need to figure out the right interface for loading these guys,
// and probably a version where W/H are not fixed (... rename this to FixedBitmapFont?)

#include <vector>

template<int W, int H>
struct BitmapFont {
  static constexpr int CHAR_WIDTH = W;
  static constexpr int CHAR_HEIGHT = H;

  bool GetBit(int c, int x, int y) const {
    return bits[c * (CHAR_WIDTH * CHAR_HEIGHT) + y * CHAR_WIDTH + x];
  }

  // Draw the bits using custom pixel-drawing functions. SetBit and
  // ClearBit are called (if non-null) for each pixel. They should be
  // callable like SetPixel(int x, int y).
  template<class FS, class FC>
  void Blit(int c, int x, int y, 
	    FS SetPixel, FC ClearPixel = [](int, int){}) const {
    for (int sy = 0; sy < CHAR_HEIGHT; sy++) {
      for (int sx = 0; sx < CHAR_WIDTH; sx++) {
	if (GetBit(c, sx, sy)) {
	  SetPixel(x + sx, y + sy);
	} else {
	  ClearPixel(x + sx, y + sy);
	}
      }
    }
  }

  explicit BitmapFont(std::vector<bool> bits) : bits(std::move(bits)) {}

private:
  // Bits are arranged as if each character in one tall column.
  // This improves cache locality somewhat.
  const std::vector<bool> bits;
};

