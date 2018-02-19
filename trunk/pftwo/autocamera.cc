
#include "autocamera.h"
#include "pftwo.h"

#include <math.h>
#include <unordered_set>
#include <mutex>
#include <atomic>

#include "../fceulib/emulator.h"
#include "../fceulib/simplefm2.h"

#include "../cc-lib/threadutil.h"
#include "../fceulib/ppu.h"

// Could also be done with a macro; maybe template parameter packs??
inline static void StepPlayer(Emulator *emu, bool is_p1, uint8 input) {
  if (is_p1) emu->Step(input, 0);
  else emu->Step(0, input);
}

inline static void StepFullPlayer(Emulator *emu, bool is_p1, uint8 input) {
  if (is_p1) emu->StepFull(input, 0);
  else emu->StepFull(0, input);
}

static void SaveEmulatorImage(Emulator *emu, const string &filename) {
  // nothing
}

// Some games make our lives harder by writing sprites to a different
// starting position on each frame. Typically, they rotate the start
// index, and then sometimes also have some fixed stride after each
// sprite is written. The sprites are written into indices mod 64, of
// course. One good reason to do this is because a maximum of 8 sprites
// can be drawn on a given scanline, so in case this is exceeded, it's
// preferable to flicker sprites (because their relative order keeps
// changing) rather than consistently drop a sprite (could be the
// player!)
//
// This routine takes two OAMs and guesses the displacement between
// them (in [0, 63]). It does this by computing the "loss" for each
// possible displacement and choosing the one with the minimal loss.
// Loss is computed by looking at the sprite in oam a and the 
// corresponding (i.e., after displacement) in oam b. This pair of
// sprites has more loss when their positions differ, when their
// attributes differ, etc.
//
// Of course, this generalizes the case when sprites are not rotated;
// i.e., displacement is zero.
//
// TODO: Contra is even worse, because it does its own manual
// "flickering"; replacing some player sprites with bullets every
// other frame, when the player is shooting. Since this isn't always
// happening, we may still be able to find good alignments (or good
// alignments for other parts of the player's sprite constellation?)
// with just the treatment below. (Indeed, it does seem to work,
// at least for finding alignments.)
int AutoCamera::BestDisplacement(const vector<uint8> &oldoam, 
				 const vector<uint8> &newoam) {
  CHECK_EQ(256, oldoam.size());
  CHECK_EQ(256, newoam.size());

  // Get pointer to the four bytes of sprite data.
  auto OAMSprite = [](const vector<uint8> &oam, int idx) {
    return &oam[idx * 4];
  };

  // Consider ignoring sprites when one of them is off screen?
  // Not sure how these are treated.
  auto SpriteLoss = [](const uint8 *a, const uint8 *b) {
    const int atile_idx = a[1];
    const int btile_idx = b[1];
    const uint8 aattr = a[2];
    const uint8 battr = b[2];
    const bool av_flip = !!(aattr & (1 << 7));
    const bool bv_flip = !!(battr & (1 << 7));
    const bool ah_flip = !!(aattr & (1 << 6));
    const bool bh_flip = !!(aattr & (1 << 6));
    const uint8 acolorbits = aattr & 3;
    const uint8 bcolorbits = battr & 3;
    const int axpos = a[3], bxpos = b[3];
    const int aypos = a[0], bypos = b[0];
    const int dx = axpos - bxpos;
    const int dy = aypos - bypos;
    // euclidean distance between positions
    const double dpos = sqrt((dx * dx) + (dy * dy));
    // linear distance between tile indices.
    // we don't just use != with the idea that often
    // sprites will animate (especially the player).
    //  - OTOH, maybe a difference of 1 should be treated as
    //    a lot more than 0.
    //  - Could consider taking minimal distance mod 256,
    //    although maybe games tend to pack sprites that
    //    are part of an animation together?
    //  - Could consider penalizing line parity differences
    //    when sprites are tall; it's probably a good
    //    heuristic that games don't tend to mix these?
    const double dtile = abs(atile_idx - btile_idx);
    const double dvflip = av_flip != bv_flip;
    const double dhflip = ah_flip != bh_flip;

    const double dcolor = acolorbits != bcolorbits;

    // note: Very low weight for h_flip because it's normal to flip
    // the player sprite when turning around, and this is something we
    // do when doing science. But all of the sprites participate in
    // an alignment, so it can still be helpful for other sprites.
    // These always have magnitude 1 or 0.
    static constexpr double HFLIP_WEIGHT = 0.1;
    static constexpr double VFLIP_WEIGHT = 2.0;
    static constexpr double COLOR_WEIGHT = 2.0;

    // Magnitudes of these are high.
    static constexpr double DIST_WEIGHT = 1.0;
    static constexpr double TILE_WEIGHT = 2.0;

    const double loss =
        DIST_WEIGHT * dpos + 
        TILE_WEIGHT * dtile +
        HFLIP_WEIGHT * dhflip +
        VFLIP_WEIGHT * dvflip +
        COLOR_WEIGHT * dcolor;

    return loss;
  };

  int bestd = -1;
  double bestloss = numeric_limits<double>::infinity();
  for (int disp = 0; disp < 64; disp++) {
    double loss = 0.0;
    for (int oldi = 0; oldi < 64; oldi++) {
      const int newi = (oldi + disp) % 64;
      const uint8 *olds = OAMSprite(oldoam, oldi);
      const uint8 *news = OAMSprite(newoam, newi);
      loss += SpriteLoss(olds, news);
    }
    // printf("Loss %d = %.2f\n", disp, loss);
    if (loss < bestloss) {
      bestd = disp;
      bestloss = loss;
    }
  }
  CHECK_GE(bestd, 0);
  // printf(" ** Best loss is %d (%.2f)\n", bestd, bestloss);
  return bestd;
}

// OAM is Object Attribute Memory, which is sprite data.
vector<uint8> AutoCamera::OAM(Emulator *emu) {
  const PPU *ppu = emu->GetFC()->ppu;
  vector<uint8> oam;
  oam.resize(256);
  memcpy(oam.data(), ppu->SPRAM, 256);
  return oam;
};

namespace {
struct InputGenerator {
  explicit InputGenerator(int id) : id(id) {}

  uint8 Next() {
    i++;
    switch (id) {
    case 0: return 0;
    case 1: return INPUT_L;
    case 2: return INPUT_R;
    case 3: return INPUT_U;
    case 4: return INPUT_D;
    case 5: return INPUT_A;
    case 6: return INPUT_B;
    case 7: return (i >> 2) & 1 ? INPUT_A : 0;
    case 8: return (i >> 3) & 1 ? 0 : INPUT_A;
    case 9: return INPUT_L | INPUT_U | ((i >> 3) & 1 ? INPUT_A : INPUT_B);
    case 10: return INPUT_R | INPUT_U | ((i >> 3) & 1 ? INPUT_B : INPUT_A);
    case 11: return ((i >> 3) & 1 ? INPUT_L : INPUT_R) |
	((i >> 2) & 1 ? INPUT_U : INPUT_D);
    default: {
      int x = id - 10;
      // In the general case, randomly combine some streams based on
      // input bits.
      uint8 out = 0;
      if (x & 1) out |= (i % 6 < 3) ? INPUT_L : 0;
      if (x & 2) out |= (i % 10 < 5) ? INPUT_R : 0;
      if (x & 4) out |= (i % 8 < 4) ? INPUT_U : 0;
      if (x & 8) out |= (i % 8 > 4) ? INPUT_D : 0;
      if (x & 16) out |= (i % 3 > 1) ? INPUT_A : 0;
      if (x & 32) out |= (i % 5 > 2) ? INPUT_B : 0;
      if (x & 64) out ^= INPUT_B;
      if (x & 128) out ^= INPUT_A;
      return out;
    }
    }
  }
  
  const int id;
  uint32 i = 0;
};
}

AutoCamera::AutoCamera(const string &game,
		       bool first_player) : first_player(first_player) {
  printf("Creating %d emulators for AutoCamera...\n", NUM_EMULATORS);
  for (int i = 0; i < NUM_EMULATORS; i++) {
    emus.push_back(Emulator::Create(game));
  }
}

AutoCamera::~AutoCamera() {
  for (Emulator *emu : emus) delete emu;
}

AutoCamera::XSprites AutoCamera::GetXSprites(
    const vector<uint8> &start) { 

  Emulator *lemu = emus[0], *nemu = emus[1], *remu = emus[2];

  // Use three emulators. In lemu, we hold only L, in nemu,
  // we press nothing, in remu, only R.
  lemu->LoadUncompressed(start);
  nemu->LoadUncompressed(start);
  remu->LoadUncompressed(start);
  
  // "Old memory" from the previous frame. We use this because it's
  // common (universal?) for sprite memory to be copied early in the
  // frame and but then updated later, so that the sprite values lag
  // behind memory values by 1 frame.
  vector<uint8> lomem = lemu->GetMemory();
  vector<uint8> nomem = nemu->GetMemory();
  vector<uint8> romem = remu->GetMemory();

  // Due to sprite rotation (see comments on BestDisplacement), we
  // also need to keep track of the OAM from the previous frame.
  vector<uint8> looam = OAM(lemu);
  vector<uint8> nooam = OAM(nemu);
  vector<uint8> rooam = OAM(remu);

  XSprites ret;

  // Total sprite displacement since the start frame.
  int total_displacement = 0;

  // We look increasingly deep until satisfying the constraints,
  // up to 45 frames.
  for (int frames = 1; frames < 45; frames++) {

    StepFullPlayer(lemu, first_player, INPUT_L);
    StepFullPlayer(nemu, first_player, 0);
    StepFullPlayer(remu, first_player, INPUT_R);
    vector<uint8> loam = OAM(lemu);
    vector<uint8> noam = OAM(nemu);
    vector<uint8> roam = OAM(remu);

    // Align OAMs to previous frame.
    const int ldisp = BestDisplacement(looam, loam);
    const int ndisp = BestDisplacement(nooam, noam);
    const int rdisp = BestDisplacement(rooam, roam);

    // If displacements aren't the same, perhaps we had a lag
    // frame on one branch but not the others, or our displacement
    // estimate is pretty noisy. We could handle this with some
    // complexity below, but it seems safer to just ditch, and
    // then the sequel is simpler.
    if (!(ldisp == ndisp && ndisp == rdisp)) {
      printf("Displacements don't agree: %d, %d, %d\n",
	     ldisp, ndisp, rdisp);
      return ret;
    }
    const int disp = ldisp;
    ret.displacements.push_back(disp);
    total_displacement += disp;

    // Now, see if any sprite has the property we want, which is
    // that left_x < none_x < right_x.

    vector<uint8> lmem = lemu->GetMemory();
    vector<uint8> nmem = nemu->GetMemory();
    vector<uint8> rmem = remu->GetMemory();

    {
      // XXX
      printf("[%d] p1: %d,%d < %d,%d < %d,%d %s%s%s\n", 
	     frames,
	     lmem[0x0334], lmem[0x031a],
	     nmem[0x0334], nmem[0x031a],
	     rmem[0x0334], rmem[0x031a],
	     lmem == nmem ? "l=n " : "l!=n ",
	     nmem == rmem ? "n=r " : "n!=r ",
	     lmem == rmem ? "l=r " : "l!=r ");
    }

    for (int absolute_s = 0; absolute_s < 64; absolute_s++) {
      // absolute_s is, in effect, the sprite id at the start frame.
      const int s = (absolute_s + total_displacement) % 64;
      const uint8 left_y = loam[s * 4 + 0];
      const uint8 none_y = noam[s * 4 + 0];
      const uint8 right_y = roam[s * 4 + 0];
      // Skip sprites that are off screen; these can't be the
      // player. (Of course, we could have just fallen into a
      // pit. We mean they can't be the player if everything is
      // going correctly.)
      // We don't skip sprites that are clipped (in the left column)
      // even if the clip mask is set, because it seems normal to
      // allow the player to walk off the left edge of the screen.
      if (left_y >= 240 || none_y >= 240 || right_y >= 240) continue;

      // XXX test that the y values are equal? That would be the most
      // robust, but of course isn't true if we initiate autocamera
      // while jumping or falling, etc.

      // Note that we'll be confused by scroll e.g. when the player is
      // on the right edge of the screen and moves right, scrolling
      // the screen--their sprite doesn't move. But we want to determine
      // the memory locations that yield screen position, so it doesn't
      // make sense to compensate for scroll. (Except perhaps as a way
      // of rejecting the current scenario for this test. But consider
      // that some games just scroll all the time!) 
      const uint8 left_x = loam[s * 4 + 3];
      const uint8 none_x = noam[s * 4 + 3];
      const uint8 right_x = roam[s * 4 + 3];
	
      if (left_x < none_x && none_x < right_x) {
	printf("[%d] Sprite %d could be player! x vals: %d < %d < %d\n",
	       frames, s, left_x, none_x, right_x);
	// Also insist that there is memory address for which
	// the sprite x value matches in all three branches.

	// Maybe allow constant offsets here?
	auto FindMems = [left_x, none_x, right_x](const vector<uint8> &lm,
						  const vector<uint8> &nm,
						  const vector<uint8> &rm) {
	  vector<pair<uint16, int>> mems;
	  for (uint16 i = 0; i < lm.size(); i++) {
	    if (left_x == lm[i] && none_x == nm[i] && right_x == rm[i]) {
	      // Note: Always using offset of zero here.
	      mems.push_back({i, 0});
	    }
	  }
	  return mems;
	};

	vector<pair<uint16, int>> mems = FindMems(lmem, nmem, rmem);
	if (false && !mems.empty()) {
	  // XXX do ANY games work this way? See if we can simplify
	  // it away. (This can cause false positives, too, in cases
	  // where we just "luck into" two consecutive frames with the
	  // same pixel values?)
	  printf("[%d]    Good! Now mems:", frames);
	  for (pair<uint16, int> m : mems)
	    printf(" %s", AddrOffset(m).c_str());
	  printf("\n");

	  ret.sprites.emplace_back(absolute_s, false, mems, vector<pair<uint16, int>>{});
	  
	} else {
	  vector<pair<uint16, int>> omems = FindMems(lomem, nomem, romem);

	  if (!omems.empty()) {
	    printf("[%d]    Good! Old mems:", frames);
	    for (pair<uint16, int> m : omems)
	      printf(" %s", AddrOffset(m).c_str());
	    printf("\n");
	    ret.sprites.emplace_back(absolute_s, true, omems, vector<pair<uint16, int>>{});
	  }
	}
      }
    }

    if (!ret.sprites.empty()) {
      return ret;
    }

    // Shift current memory to previous.
    lomem = std::move(lmem);
    nomem = std::move(nmem);
    romem = std::move(rmem);

    // Same for OAMs.
    looam = std::move(loam);
    nooam = std::move(noam);
    rooam = std::move(roam);
  }

  return {};
}

vector<AutoCamera::XYSprite> AutoCamera::FindYCoordinates(
    const vector<uint8> &uncompressed_state,
    const XSprites &xsprites,
    vector<int> *player_sprites) {

  const int x_num_frames = xsprites.displacements.size();

  // make parameter, or decide that this can always be assumed.
  const bool lagmem = true;
  
  // We're going to run a bunch of different experiments to get
  // our science data. The goal here is to have a number of memories
  // paired with sprite data, from which we can mine correlations.

  struct Science {
    // 2k of NES memory synced to the sprite data. This is from
    // the previous frame if sprite data are lagged.
    vector<uint8> mem;
    vector<uint8> oam;
    // PERF
    string displacement_string;
    int total_displacement = 0;
    /*
    Science(vector<uint8> mem, vector<uint8> oam,
	    int total_displacement) :
      mem(mem), oam(oam), total_displacement(total_displacement) {}
    */
    Science() {}
  };

  vector<Science> sciences;
  static constexpr int NUM_SEQ = 12;
  const int SEQ_LEN = x_num_frames + 8;
  // Pre-allocate so that individual threads can save their data.
  sciences.resize(NUM_SEQ * SEQ_LEN);

  // Holds an emulator and some indices to distinguish it from
  // other sequences. Writes science data to science array when
  // fed steps.
  struct Stepper {
    Stepper(bool first_player,
	    bool lagmem,
	    vector<Science> &sciences,
	    int SEQ_LEN,
	    Emulator *emu, int seq_num) :
      first_player(first_player),
      lagmem(lagmem),
      sciences(sciences), SEQ_LEN(SEQ_LEN),
      emu(emu), seq_num(seq_num) {
      prev_mem = emu->GetMemory();
      prev_oam = OAM(emu);
    }

    void Step(uint8 input) {
      CHECK(offset < SEQ_LEN);
      // Use StepFull since we will actually read PPU data.
      // (This is probably emulated for Step as well, but
      // only StepFull promises video.)
      StepFullPlayer(emu, first_player, input);
      Science *science = &sciences[seq_num * SEQ_LEN + offset];
      vector<uint8> now_mem = emu->GetMemory();
      if (lagmem) {
	science->mem = prev_mem;
      } else {
	science->mem = now_mem;
      }

      vector<uint8> now_oam = OAM(emu);
      science->oam = now_oam;
      const int disp = BestDisplacement(prev_oam, 
					now_oam);
      science->displacement_string = 
	StringPrintf("%s%d,",
		     science->displacement_string.c_str(),
		     disp);
      total_displacement += disp;
      science->total_displacement = total_displacement;

      // Shift memory to previous.
      prev_mem = std::move(now_mem);
      prev_oam = std::move(now_oam);
      
      offset++;
    }

    const bool first_player;
    const bool lagmem;
    vector<Science> &sciences;
    const int SEQ_LEN;
    // Assumes exclusive access.
    Emulator *emu;
    const int seq_num;
    int total_displacement = 0;
    int offset = 0;
    vector<uint8> prev_mem;
    vector<uint8> prev_oam;
  };
  
  auto OneSeq = [this, lagmem, &sciences, SEQ_LEN, &uncompressed_state,
		 x_num_frames](int seq) {
    Emulator *emu = emus[seq];
    emu->LoadUncompressed(uncompressed_state);
    Stepper stepper(first_player, lagmem, sciences, SEQ_LEN, emu, seq);
    InputGenerator gen{seq};
    for (int i = 0; i < SEQ_LEN; i++) stepper.Step(gen.Next());
  };

  // PERF parallelize.
  UnParallelComp(NUM_SEQ, OneSeq, 4);

  // PERF Debug only -- make sure everything was filled in.
  for (const Science &s : sciences) {
    CHECK(s.mem.size() == 2048);
    CHECK(s.oam.size() == 256);
  }

  printf("Filtering on %d sciences (%.2f MB)...\n",
	 (int)sciences.size(),
	 (sciences.size() * (2048 + 256)) / (1024.0 * 1024.0));

  // TODO: Return YSprites structure maybe?
  vector<XYSprite> withy;
  // Now whittle down x memory locations.
  for (const XYSprite &sprite : xsprites.sprites) {
    const int absolute_s = sprite.sprite_idx;
    XYSprite newsprite;
    for (const pair<uint16, int> xaddr : sprite.xmems) {
      // Check every science.
      for (const Science &science : sciences) {
	const int s = (absolute_s + science.total_displacement) % 64;
	const int mem_x = (int)science.mem[xaddr.first] + xaddr.second;
	const int sprite_x = science.oam[s * 4 + 3];
	if (mem_x != sprite_x) {
	  printf("Sprite %d (displaced %s to %d) xmem %04x eliminated since "
		 "mem_x+%d = %d and sprite_x = %d\n",
		 absolute_s, science.displacement_string.c_str(),
		 s, xaddr.first, xaddr.second, mem_x, sprite_x);
	  goto fail_xaddr;
	}
      }

      // OK, keep it!
      newsprite.xmems.push_back(xaddr);
     
    fail_xaddr:;
    }

    // Now find y coordinates for the sprite.
    if (!newsprite.xmems.empty()) {

      for (uint16 yaddr = 0; yaddr < 2048; yaddr++) {
	// We're solving for offset such that mem[yaddr] + offset = sprite.
	// Offset may be signed. We don't have a candidate offset until
	// our first comparison.
	bool have_offset = false;
	int offset = 0;
	for (const Science &science : sciences) {
	  const int s = (absolute_s + science.total_displacement) % 64;
	  // Promote to integer from uint8 so that we can do signed
	  // comparison.
	  const int actual_mem_y = science.mem[yaddr];
	  const int sprite_y = science.oam[s * 4 + 0];

	  const int this_offset = sprite_y - actual_mem_y;
	  if (!have_offset) {
	    have_offset = true;
	    offset = this_offset;
	  }
	  
	  if (this_offset != offset) {
	    if (true && yaddr == 0x84) { // XXX zelda-specific debug output
	      printf("Sprite %d ymem %04x (offset %d) eliminated since "
		     "mem_y = %02x and sprite_y = %02x (so offset %d)\n",
		     s, yaddr, offset,
		     actual_mem_y, sprite_y, this_offset);
	    }
	    goto fail_yaddr;
	  }
	}

	newsprite.ymems.push_back({yaddr, offset});

      fail_yaddr:;
      }

      if (!newsprite.ymems.empty()) {
	printf("Sprite %d has x candidates:", absolute_s);
	for (auto xaddr : newsprite.xmems)
	  printf(" %s", AddrOffset(xaddr).c_str());
	printf("\n          and y candidates:");
	for (auto yaddr : newsprite.ymems)
	  printf(" %s", AddrOffset(yaddr).c_str());
	printf("\n");
	newsprite.sprite_idx = absolute_s;
	newsprite.oldmem = lagmem;
	withy.push_back(newsprite);
      } else {
	printf("Sprite %d eliminted since there are no ymems left.\n",
	       absolute_s);
      }
    } else {
      printf("Sprite %d eliminated since there are no xmems left.\n", 
	     absolute_s);
    }
  }

  // For sprite rejection in 3D mode, it's useful to know all the sprite
  // indices that are under player control, even if we don't have source
  // memory indices for them (because e.g. they are only stored in the
  // OAMDMA src. So save these.
  if (player_sprites != nullptr) {
    player_sprites->clear();
    for (const XYSprite &sprite : withy) {
      player_sprites->push_back(sprite.sprite_idx);
    }
  }
  
  vector<XYSprite> ret;
  // Next, filter memory locations that are just source data for
  // the OAMDMA copy. Because the DMA has to be from a source address
  // of the form 0x##00-0x##FF.
  // (Note, we could actually intercept the OAMDMA write in the emulator;
  // it's not hard. But maybe prettier to do it this way?)

  for (const XYSprite &sprite : withy) {
    XYSprite newsprite;
    auto FilterByOAM = [&sciences, &sprite](
	const vector<pair<uint16, int>> &addrs,
	int oam_byte,
	vector<pair<uint16, int>> *out) {

      // Test a single addr.
      auto OK = [&sciences, &sprite, oam_byte](pair<uint16, int> addr) {
	// Wouldn't make sense for it to have an offset; OAMDMA doesn't
	// modify anything.
	if (addr.second != 0) return true;
	// Quick filter: The address would have to match the corresponding
	// coordinate in sprite data. Also, computes the DMA root address,
	// which we need for the full check anyway.
	uint16 dma_root = addr.first & ~0xFF;
	if (addr.first != dma_root + sprite.sprite_idx * 4 + oam_byte) {
	  // This coordinate of this sprite could not have come from
	  // a DMA involving this address.
	  return true;
	}

	printf("%s-coord at addr %04x for sprite %d may be from DMA "
	       "starting at %04x?\n", oam_byte ? "x" : "y",
	       addr.first, sprite.sprite_idx, dma_root);

	// If it aligns with potential DMA, check if sprite data
	// matches a DMA at that address. If so, reject.
	for (const Science &science : sciences) {
	  for (int i = 0; i < 256; i++) {
	    // PERF memcmp
	    if (science.mem[dma_root + i] != science.oam[i]) {
	      printf("But mem[%04x] = %2x whereas oam[%2x] = %2x.\n",
		     dma_root + i, science.mem[dma_root + i],
		     i, science.oam[i]);
	      return true;
	    }
	  }
	}

	// All science is consistent.
	printf("Rejected %s-coord for sprite %d at %04x because it "
	       "is consistent with just being DMA.\n",
	       oam_byte ? "x" : "y", sprite.sprite_idx, addr.first);
	return false;
      };

      for (const auto addr : addrs)
	if (OK(addr))
	  out->push_back(addr);
    };

    FilterByOAM(sprite.xmems, 3, &newsprite.xmems);
    FilterByOAM(sprite.ymems, 0, &newsprite.ymems);

    if (!newsprite.xmems.empty() && !newsprite.ymems.empty()) {
      printf("Sprite %d has x candidates:", sprite.sprite_idx);
      for (auto xaddr : newsprite.xmems)
	printf(" %s", AddrOffset(xaddr).c_str());
      printf("\n          and y candidates:");
      for (auto yaddr : newsprite.ymems)
	printf(" %s", AddrOffset(yaddr).c_str());
      printf("\n");

      newsprite.sprite_idx = sprite.sprite_idx;
      newsprite.oldmem = lagmem;
      ret.push_back(newsprite);
    }
  }
  
  return ret;
}

vector<AutoCamera::XYSprite> AutoCamera::FilterForConsequentiality(
    const vector<uint8> &uncompressed_state,
    int x_num_frames,
    const vector<XYSprite> &sprites) {

  // To do this, we'll pick a memory location m and try to
  // eliminate it because it is "caused" by other memory
  // locations. We'll do that by modifying (only) it, then
  // emulating a frame and seeing that it does not have the
  // modified value (i.e., that its value is the same as
  // if we did not modify it).
  //
  // This procedure can eliminate a sprite, by proving that its
  // position is actually derived from other memory locations.

  // To start, we want to generate a bunch of emulator states
  // so that we can run the experiments above in a bunch of
  // different scenarios.

  // PERF share with other steps
  static constexpr int NUM_EXPERIMENTS = NUM_EMULATORS;
  static_assert(NUM_EXPERIMENTS <= NUM_EMULATORS, "assumed exclusive emu");

  vector<vector<uint8>> savestates;
  GetSavestates(uncompressed_state, NUM_EXPERIMENTS, x_num_frames, &savestates);

  vector<bool> known_consequential(2048, false);
  vector<bool> known_inconsequential(2048, false);
  
  // PERF this could be parallelized. (careful because vector<bool>
  // certainly does not have thread-safe access to nearby bits!)
  //
  // Now, pick candidate memory locations.
  for (const XYSprite &sprite : sprites) {
    auto Consequential = [this, &savestates, &sprite,
			  &known_consequential, &known_inconsequential](
	pair<uint16, int> addr) {
      if (known_consequential[addr.first]) {
	printf("Already decided %04x is consequential.\n", addr.first);
	return true;
      }
      if (known_inconsequential[addr.first]) {
	printf("Already decided %04x is inconsequential.\n", addr.first);
	return false;
      }
      for (const vector<uint8> &save : savestates) {
	Emulator *emu = emus[0];
	// Perform the experiment.
	
	// First, the control:
	emu->LoadUncompressed(save);
	// Run with no input. The stimulus is the changing of
	// the memory location.
	StepFullPlayer(emu, first_player, 0);
	StepFullPlayer(emu, first_player, 0);

	// PERF The control OAM will be the same every time;
	// compute it when computing savestates.
	vector<uint8> ctrl_oam = OAM(emu);
	vector<uint8> ctrl_mem = emu->GetMemory();
	
	// Same, but modifying the memory location:
	emu->LoadUncompressed(save);
	// How to pick the value to set? We should avoid
	// putting it off screen, or generally near the edges
	// of the screen, because that could trigger issues.
	uint8 *ram = emu->GetFC()->fceu->RAM;
	const uint8 oldval = ram[addr.first];
	const uint8 newval = (oldval <= 128) ? oldval + 64 : oldval - 64;

	// Note this bypasses any RAM hooks, but I think that's
	// the right thing to do.
	ram[addr.first] = newval;

	StepFullPlayer(emu, first_player, 0);
	StepFullPlayer(emu, first_player, 0);
	vector<uint8> expt_oam = OAM(emu);
	vector<uint8> expt_mem = emu->GetMemory();

	if (ctrl_oam != expt_oam ||
	    ctrl_mem != expt_mem) {
	  known_consequential[addr.first] = true;
	  return true;
	}
      }
      known_inconsequential[addr.first] = true;
      return false;
    };
    
    for (pair<uint16, int> xaddr : sprite.xmems) {
      if (!Consequential(xaddr)) {
	printf("sprite %d: x address %s is inconsequential\n",
	       sprite.sprite_idx, AddrOffset(xaddr).c_str());
      }
    }
      
    for (pair<uint16, int> yaddr : sprite.ymems) {
      if (!Consequential(yaddr)) {
	printf("sprite %d: y address %s is inconsequential\n",
	       sprite.sprite_idx, AddrOffset(yaddr).c_str());
      }
    }
  }
  
  vector<XYSprite> ret;
  for (const XYSprite &sprite : sprites) {
    XYSprite newsprite;
    newsprite.sprite_idx = sprite.sprite_idx;
    newsprite.oldmem = sprite.oldmem;
    for (auto addr : sprite.xmems) {
      if (!known_inconsequential[addr.first]) {
	newsprite.xmems.push_back(addr);
      }
    }
    for (auto addr : sprite.ymems) {
      if (!known_inconsequential[addr.first]) {
	newsprite.ymems.push_back(addr);
      }
    }

    if (!newsprite.xmems.empty() &&
	!newsprite.ymems.empty()) {
      printf("Sprite %d survived consequentiality.\n"
	     "   x:", newsprite.sprite_idx);
      for (const auto addr : newsprite.xmems)
	printf(" %s", AddrOffset(addr).c_str());
      printf("\n   y:");
      for (const auto addr : newsprite.ymems)
	printf(" %s", AddrOffset(addr).c_str());
      printf("\n");
      
      ret.push_back(newsprite);
    }
  }

  return ret;
}

bool AutoCamera::DetectViewType(const vector<uint8> &uncompressed_state,
				int x_num_frames,
				const vector<XYSprite> &sprites,
				bool *is_top) {
  // We guess the view type as follows:
  //   - If moving the sprites up on the screen (by writing to their
  //     x,y coordinates) results in downward motion due to "gravity",
  //     then this is side view. Top view games just don't have gravity.
  //     We do this first because there are side view games (e.g. Double
  //     Dragon) where up and down do change some kind of y coordinate.
  //     (This kind of game may be very difficult to model, let alone
  //     deduce the physics of.)

  //   - If pressing up and down reliably changes the y coordinate,
  //     then we guess this is top view. (Note that there exist side
  //     view games that this would confuse, like Gradius.)

  // To distinguish gradius from zelda, we may need to mix this with
  // the camera angle detection. In most side view games, the player
  // graphic cannot "face up" or down.

  // PERF: Should reuse savestates from previous steps!
  static constexpr int NUM_EXPERIMENTS = NUM_EMULATORS;
  static_assert(NUM_EXPERIMENTS <= NUM_EMULATORS, "assumed exclusive emu");

  vector<vector<uint8>> savestates;
  GetSavestates(uncompressed_state, NUM_EXPERIMENTS, x_num_frames, &savestates);

  // Protects the following two variables.
  std::mutex m;
  int experiments = 0, successes = 0;
  
  auto OneEmu = [this, &sprites, &savestates,
		 &m, &experiments, &successes](int idx) {
    Emulator *emu = emus[idx];
    int this_experiments = 0, this_successes = 0;
    static constexpr int DROP_TIME = 24;
    for (const XYSprite &sprite : sprites) {
      for (uint8 x : {65, 128, 190}) {
	for (uint8 y : {40, 120, 157}) {
	  this_experiments++;
	  emu->LoadUncompressed(savestates[idx]);
	  uint8 *ram = emu->GetFC()->fceu->RAM;

	  // Set ALL the memory locations.
	  // Since mem[addr] + offset = loc,
	  // subtract the offset from the desired location.
	  for (const auto xaddr : sprite.xmems)
	    ram[xaddr.first] = x - xaddr.second;
	  for (const auto yaddr : sprite.ymems)
	    ram[yaddr.first] = y - yaddr.second;

	  // Let the location soak in.
	  StepPlayer(emu, first_player, 0);
	  StepPlayer(emu, first_player, 0);

	  const uint8 start_y =
	    emu->GetFC()->ppu->SPRAM[sprite.sprite_idx * 4 + 0];
	  // start y should be near the value we set.
	  if (abs((int)start_y - (int)y) > 8) {
	    printf("sprite %d expt %d,%d: "
		   "Failed to influence y coordinate (it's %02x).\n",
		   sprite.sprite_idx,
		   // (Could print addresses here)
		   x, y, start_y);
	    continue;
	  }
	  
	  // Now see if the sprite drops.
	  for (int i = 0; i < DROP_TIME; i++) {
	    StepPlayer(emu, first_player, 0);
	  }
	  const uint8 now_y =
	    emu->GetFC()->ppu->SPRAM[sprite.sprite_idx * 4 + 0];
	  // XXX maybe should set a lower bound on this; for zelda
	  // we do see a few sprites increase by a few pixels. (Maybe
	  // the player is being ejected from some block?)
	  if (now_y > start_y) {
	    #if 0
	    printf("sprite %d expt %d,%d: "
		   "Success dropping %02x to %02x.\n",
		   sprite.sprite_idx,
		   x, y, start_y, now_y);
	    #endif
	    this_successes++;
	  }
	}
      }
      MutexLock ml(&m);
      experiments += this_experiments;
      successes += this_successes;
    }
  };

  ParallelComp(NUM_EXPERIMENTS, OneEmu, 12);
  
  printf("Overall: %d successes of %d experiments = %.3f success rate.\n",
	 successes, experiments, (double)successes / experiments);

  if (successes * 2 > experiments) {
    *is_top = false;
    return true;
  }
 
  return false;
}

// Attempt to stop the player in both x/y. The player must be still
// for stop_frames frames in a row, and max_stoptime is the maximum
// number of frames to try for.
static bool BrakePlayer(Emulator *emu,
			bool first_player,
			const AutoCamera::XYSprite &sprite,
			int stop_frames, int max_stoptime) {
  CHECK(stop_frames > 0);
  uint8 lastx = emu->GetFC()->ppu->SPRAM[sprite.sprite_idx * 4 + 3];
  uint8 lasty = emu->GetFC()->ppu->SPRAM[sprite.sprite_idx * 4 + 0];
  uint32 lastscroll = emu->GetXScroll();
    
  uint8 brake_input = 0;
  int current_stop_frames = stop_frames;
  for (int i = 0; i < max_stoptime; i++) {
    StepFullPlayer(emu, first_player, brake_input);
    const uint8 nowx = emu->GetFC()->ppu->SPRAM[sprite.sprite_idx * 4 + 3];
    const uint8 nowy = emu->GetFC()->ppu->SPRAM[sprite.sprite_idx * 4 + 0];
    const uint32 nowscroll = emu->GetXScroll();

    if (nowx == lastx && nowy == lasty && nowscroll == lastscroll) {
      current_stop_frames--;
      if (current_stop_frames == 0) {
	return true;
      }
    } else {
      current_stop_frames = stop_frames;
    }

    // Rapidly tap the opposite direction to try to slow us. But beware
    // of holding it too long. Allow holding it for the first 1/4, then
    // for the next 1/4, strobing it.
    if (i < (max_stoptime >> 2) ||
	(i < (max_stoptime >> 1) && brake_input == 0)) {
      // Moving right.
      if (nowx >= lastx + 1) brake_input = INPUT_L;
      else if (nowx <= lastx - 1) brake_input = INPUT_R;
      else brake_input = 0;
    } else {
      brake_input = 0;
    }

    lastx = nowx;
    lasty = nowy;
    lastscroll = nowscroll;
  }

  return false;
}

AutoCamera::CameraStatus
AutoCamera::DetectCameraAngle(const vector<uint8> &uncompressed_state,
			      int x_num_frames,
			      const vector<XYSprite> &sprites,
			      uint16 *addr,
			      uint8 *up, uint8 *down,
			      uint8 *left, uint8 *right) {

  if (sprites.empty())
    return CAMERA_FAILED;

  const XYSprite &sprite = sprites[0];
  
  static constexpr int MAX_STOPTIME = 60;

  // Right now just doing this for one state (the current one) and
  // sprite. (The first one, arbitrarily). But, could run it over some
  // speculative savestates and intersect the results if there are too
  // many. It wouldn't be that strange for multiple bytes to predict
  // the direction with different values.
  Emulator *emu = emus[0];
  emu->LoadUncompressed(uncompressed_state);

  // In most games, the player can turn around even if he's stuck and
  // can't move. However, we do need to worry about momentum in games
  // like Mario where the player can't immediately turn around when
  // has has x velocity.
  if (!BrakePlayer(emu, first_player, sprite, 3, MAX_STOPTIME)) {
    printf("Couldn't brake player.\n");
    return CAMERA_FAILED;
  }

  SaveEmulatorImage(emu, "brake.png");
    
  const vector<uint8> stopped = emu->SaveUncompressed();

  // Test the L/R (X) axis or U/D (Y) axis. This was written with left
  // and right in mind, but works the same when called with up and down
  // below.
  auto TestAxis = [this, &stopped](
      uint8 input_less, uint8 input_more,
      const char *less, const char *more,
      vector<int> *candaddr, vector<uint8> *candl, vector<uint8> *candr) {
    Emulator *lemu = emus[0];
    Emulator *remu = emus[1];
    lemu->LoadUncompressed(stopped);
    remu->LoadUncompressed(stopped);

    // Player should be stopped (in both emulators, which have the same
    // state now). Tap left and right.

    for (int i = 0; i < 3; i++) {
      StepFullPlayer(lemu, first_player, input_less);
      StepFullPlayer(remu, first_player, input_more);
    }

    SaveEmulatorImage(lemu, StringPrintf("%s.png", less));
    SaveEmulatorImage(remu, StringPrintf("%s.png", more));
    
    // Find bytes where they're different.
    const uint8 *lram = lemu->GetFC()->fceu->RAM;
    const uint8 *rram = remu->GetFC()->fceu->RAM;

    // Now, we're looking for a single address that's different in the
    // two emulators. Three parallel vectors; the first contains the
    // candidate address, the other two are the value at that address
    // when pressing left, right.
    for (int i = 0; i < 2048; i++) {
      if (lram[i] != rram[i]) {
	candaddr->push_back(i);
	candl->push_back(lram[i]);
	candr->push_back(rram[i]);
      }
    }

    auto PrintCand = [less, more, candaddr, candl, candr](const char *when) {
      printf("%s-%s  After %s, %d candidates differences: ",
	     less, more, when,
	     (int)candaddr->size());
      for (int i = 0; i < candaddr->size(); i++) {
	printf(" %04x (%02x/%02x)", (*candaddr)[i], (*candl)[i], (*candr)[i]);
      }
      printf("\n");
    };

    PrintCand("tap");

    auto Filter = [less, more, candaddr, candl, candr](
	Emulator *facing_left, Emulator *facing_right) {
      uint8 *lram = facing_left->GetFC()->fceu->RAM;
      uint8 *rram = facing_right->GetFC()->fceu->RAM;

      vector<int> newcand;
      vector<uint8> newl, newr;

      for (int a = 0; a < candaddr->size(); a++) {
	// By invariant, candl[a] != candr[a].
	int addr = (*candaddr)[a];
	if (lram[addr] != (*candl)[a] ||
	    rram[addr] != (*candr)[a]) {
	  printf("%s-%s  Eliminated %04x because wanted %02x/%02x "
		 "but got %02x/%02x\n", less, more,
		 addr, (*candl)[a], (*candr)[a], lram[addr], rram[addr]);
	} else {
	  newcand.push_back(addr);
	  newl.push_back((*candl)[a]);
	  newr.push_back((*candr)[a]);
	}
      }

      candaddr->swap(newcand);
      candl->swap(newl);
      candr->swap(newr);
    };
 
    // Savestate after executing a few steps of nothing.
    vector<uint8> savel, saver;
    
    // Now, don't tap. Assume that we keep the same facing direction,
    // but for example that we stop animating.
    for (int i = 0; i < 12; i++) {
      StepFullPlayer(lemu, first_player, 0);
      StepFullPlayer(remu, first_player, 0);

      if (i == 3) {
	lemu->SaveUncompressed(&savel);
	remu->SaveUncompressed(&saver);
      }

      Filter(lemu, remu);
    }

    SaveEmulatorImage(lemu, StringPrintf("%swait.png", less));
    SaveEmulatorImage(remu, StringPrintf("%swait.png", more));

    PrintCand("wait filter");

    // One more filter pass. Move right on the left emu and left on
    // the right emu.
    lemu->LoadUncompressed(savel);
    remu->LoadUncompressed(saver);
    for (int i = 0; i < 3; i++) {
      // Note swapped directions.
      StepFullPlayer(lemu, first_player, input_more);
      StepFullPlayer(remu, first_player, input_less);
    }
    SaveEmulatorImage(lemu, StringPrintf("%s%s.png", less, more));
    SaveEmulatorImage(remu, StringPrintf("%s%s.png", more, less));

    // Note swapped filter. The right emu should now have the player
    // facing left, etc.
    Filter(remu, lemu);

    PrintCand("rev filter");

    for (int i = 0; i < 9; i++) {
      StepFullPlayer(lemu, first_player, 0);
      StepFullPlayer(remu, first_player, 0);
      // Note, still swapped.
      Filter(remu, lemu);
    }

    PrintCand("rev-wait filter");
  };

  vector<int> xcand, ycand;
  vector<uint8> xlval, xrval, yuval, ydval;
  TestAxis(INPUT_L, INPUT_R, "left", "right", &xcand, &xlval, &xrval);

  if (xcand.empty()) {
    printf("No x candidates; camera angle failed. :(\n");
    return CAMERA_FAILED;
  }

  TestAxis(INPUT_U, INPUT_D, "up", "down", &ycand, &yuval, &ydval);

  // We'll succeed in both directions if there's a valid intersection,
  // and we pick the lowest one. They're sorted from lowest to highest,
  // so we can do a "merge"-style intersection.
  int x = 0, y = 0;
  while (x < xcand.size() && y < ycand.size()) {
    // A valid intersection if it's the same memory location, and it
    // has distinct values for the four directions.
    int xaddr = xcand[x], yaddr = ycand[y];
    printf("Merge %04x vs %04x.\n", xaddr, yaddr);
    if (xaddr == yaddr) {
      // We already know xlval and xrval are distinct (and likewise for
      // yvals) by invariant.
      if (xlval[x] != yuval[y] && xlval[x] != ydval[y] &&
	  xrval[x] != yuval[y] && xrval[x] != ydval[y]) {
	*addr = xaddr;
	*up = yuval[y];
	*down = ydval[y];
	*left = xlval[x];
	*right = xrval[x];
	printf("Success! %04x udlr: %02x %02x %02x %02x :)\n",
	       *addr, *up, *down, *left, *right);
	return CAMERA_ALL;
      } else {
	// Skip both; addresses only appear once.
	x++;
	y++;
      }
    } else if (xaddr < yaddr) {
      // Advance left side until maybe equal.
      x++;
    } else {
      // xaddr > yaddr
      y++;
    }
  }

  printf("No valid intersection. Taking LR camera only.\n");
  *addr = xcand[0];
  *left = xlval[0];
  *right = xrval[0];
  return CAMERA_LR;
}

string AutoCamera::AddrOffset(pair<uint16, int> p) {
  if (p.second > 0) {
    return StringPrintf("%04x+%d", p.first, p.second);
  } else if (p.second < 0) {
    return StringPrintf("%04x-%d", p.first, -p.second);
  } else {
    return StringPrintf("%04x", p.first);
  }
}


void AutoCamera::GetSavestates(const vector<uint8> &uncompressed_state,
			       int num_experiments,
			       int x_num_frames,
			       vector<vector<uint8>> *savestates) {
  CHECK(num_experiments <= NUM_EMULATORS);

  savestates->resize(num_experiments);
  
  auto OneExperiment =
    [this, &uncompressed_state, x_num_frames, savestates](int id) {
    Emulator *emu = emus[id];
    emu->LoadUncompressed(uncompressed_state);
    const int nframes = x_num_frames + 12;
    InputGenerator gen{id};
    for (int i = 0; i < nframes; i++) {
      const uint8 input = gen.Next();
      StepFullPlayer(emu, first_player, input);
    }
    emu->SaveUncompressed(&(*savestates)[id]);
  };

  // PERF PARALLEL
  UnParallelComp(num_experiments, OneExperiment, 4);
}
