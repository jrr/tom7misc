#include <vector>
#include <string>
#include <set>
#include <memory>

#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "emulator.h"
#include "simplefm2.h"
#include "../cc-lib/wavesave.h"

using namespace std;

#if DISABLE_SOUND
#error "If sound is not compiled in, can't write wave!"
#endif

int main(int argc, char **argv) {
  if (argc < 3) {
    fprintf(stderr, "Usage: fm2wav romfile.nes moviefile.fm2\n"
	    "\nWrites to moviefile.wav.\n");
    return -1;
  }
  const string romfilename = argv[1];
  const string moviefilename = argv[2];

  size_t dot = moviefilename.rfind(".");
  if (dot == string::npos) {
    fprintf(stderr, "Moviefilename [%s] should end with .fm2.\n",
	    moviefilename.c_str());
    return -1;
  }
  const string wavefilename = moviefilename.substr(0, dot) + (string)".wav";
  
  std::unique_ptr<Emulator> emu{Emulator::Create(romfilename)};
  const vector<pair<uint8, uint8>> movie = SimpleFM2::ReadInputs2P(moviefilename);

  vector<uint16> samples;

  for (const pair<uint8, uint8> input : movie) {
    emu->StepFull(input.first, input.second);

    // Sound.
    vector<int16> sound;
    emu->GetSound(&sound);
    for (int16 s : sound) samples.push_back(s);
  }

  if (!WaveSave::SaveMono16(wavefilename, samples, Emulator::AUDIO_SAMPLE_RATE)) {
    fprintf(stderr, "Couldn't write to %s...\n", wavefilename.c_str());
    return -1;
  }
    
  fprintf(stderr, "Wrote %d frames of sound to %s.\n",
	  (int)movie.size(), wavefilename.c_str());
  return 0;
}
