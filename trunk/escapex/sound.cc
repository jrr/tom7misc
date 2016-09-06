
#include "sound.h"

#ifdef NOSOUND

/* dummy implementation */
bool Sound::enabled() { return false; }
void Sound::mute(bool b) { }
void Sound::init() { }
void Sound::shutdown() { }
void Sound::Play(sound_t s) { }

#else


/* real implementation */

#include "escapex.h"
#include "SDL_mixer.h"

/* declare static array sound_data */
#include "sound_decs.h"


static bool sound_available = false;
static bool sound_muted = false;

/* XXX should also halt any sound playing */
void Sound::mute(bool b) { sound_muted = b; }

void Sound::init() {
  if (audio) {
    /* XXX what frequency to use? */
    /* XXX fall back to mono if stereo fails */

    /* uses 16 bit at system byte order */
    // XXX 22KHz, really??
    if (-1 == Mix_OpenAudio(22050, MIX_DEFAULT_FORMAT, 2, 1024)) {
      printf("Can't open audio. (%s)\n", Mix_GetError());
      return;
    }

    if (-1 == Mix_AllocateChannels(4)) {
      Mix_CloseAudio();
      return;
    }

    /* load sound data into memory */
    #include "sound_load.h"
    /* XXX fail gracefully */

    sound_available = true;
  }

}

void Sound::Play(sound_t s) {
  //  printf(" avail: %d muted: %d\n", sound_available?1:0, sound_muted?1:0);
  if (sound_available && !sound_muted) {
    Mix_PlayChannel(-1, sound_data[s], 0);
  }
}

void Sound::shutdown() {
  if (sound_available) {
    Mix_CloseAudio();
  }
  sound_available = false;
}


#endif
