
#include "autolives.h"

#include <string>
#include <vector>
#include "../cc-lib/arcfour.h"
#include "../fceulib/simplefm2.h"
#include "../fceulib/fc.h"
#include "../fceulib/fceu.h"

// How to set?
static constexpr int TEST_CONTROL_FRAMES = 180;

// ? Probably better with game-specific markov model?
namespace {
struct InputGenerator {
  uint8 Next(ArcFour *rc) {
    uint8 b = rc->Byte();

    auto Randomize =
      [this, &b](int L, int R) {
	// Switch 1/4 of the time.
	const bool do_switch = (b & 3) == 0;
	b >>= 2;
	if (do_switch) {
	  if (last & (L | R)) {
	    // Currently going. Either swap or stop.
	    if (b & 1) {
	      if (last & L) {
		last &= ~L;
		last |= R;
	      } else {
		last &= ~R;
		last |= L;
	      }
	    } else {
	      last &= ~(L | R);
	    }
	    b >>= 1;
	  } else {
	    // Not going. Go one way or the other.
	    last |= (b & 1) ? L : R;
	    b >>= 1;
	  }
	}
      };

    Randomize(INPUT_L, INPUT_R);
    Randomize(INPUT_U, INPUT_D);

    if ((b & 3) == 0) last ^= INPUT_A;
    b >>= 2;
    if ((b & 3) == 0) last ^= INPUT_B;
    b >>= 2;

    // Never generate select or start, since these often have
    // special meaning.
    return last;
  }
 private:
  uint8 last = INPUT_A;
};
}

AutoLives::AutoLives(const string &game) : rc(game) {
  emu.reset(Emulator::Create(game));
  lemu.reset(Emulator::Create(game));
  remu.reset(Emulator::Create(game));
  memu.reset(Emulator::Create(game));
  CHECK(emu.get() != nullptr) << game;
  CHECK(lemu.get() != nullptr) << game;
  CHECK(remu.get() != nullptr) << game;
  CHECK(memu.get() != nullptr) << game;
}

AutoLives::~AutoLives() {}

// PERF it would be possible to do both players at once
// without emulating twice. (As one simple example, emu
// does the same thing either way.)
float AutoLives::IsInControl(const vector<uint8> &save,
			     int xloc, int yloc,
			     bool player_two) {

  // Initialize them all to the same state.
  emu->LoadUncompressed(save);
  lemu->LoadUncompressed(save);
  remu->LoadUncompressed(save);
  memu->LoadUncompressed(save);

  InputGenerator gen;
  int success = 0;
  for (int i = 0; i < TEST_CONTROL_FRAMES; i++) {
    // emu gets no inputs
    emu->StepFull(0, 0);
    // lemu holds left,
    if (player_two)
      lemu->StepFull(0, INPUT_L);
    else
      lemu->StepFull(INPUT_L, 0);
    // remu holds right,
    if (player_two)
      remu->StepFull(0, INPUT_R);
    else
      remu->StepFull(INPUT_R, 0);
    // mash emu gets random inputs
    const uint8 input = gen.Next(&rc);
    if (player_two)
      memu->StepFull(0, input);
    else
      memu->StepFull(input, 0);

    // Now, has xloc changed? We're not looking for change relative to
    // the start state (the player may be moving while a death
    // animation plays, for example), but rather whether our inputs
    // are having any effect on the progression of the state. So,
    // increment the score whenever any pair of the above emulators
    // disagree on theplayer's location.

    // TODO: Sometimes "moving" the player just means moving the
    // scroll, so we could consider adding the x and y scroll to the
    // player location before comparing.
    
    // TODO: Instead of just using inequality, we could consider the
    // distance traveled.
    const uint8 *ram = emu->GetFC()->fceu->RAM;
    const uint8 *lram = lemu->GetFC()->fceu->RAM;
    const uint8 *rram = remu->GetFC()->fceu->RAM;
    const uint8 *mram = memu->GetFC()->fceu->RAM;
    if (!(ram[xloc] == lram[xloc] &&
	  ram[xloc] == rram[xloc] &&
	  ram[xloc] == mram[xloc]) ||
	!(ram[yloc] == lram[yloc] &&
	  ram[yloc] == rram[yloc] &&
	  ram[yloc] == mram[yloc])) {
      success++;
    }
  }

  return success / (float)TEST_CONTROL_FRAMES;
}
