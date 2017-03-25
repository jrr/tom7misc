/**********************************************************
 *  paper.c, Copyright (c) 2017 Tom Murphy VII Ph.D.
 *  This copyright notice must appear in the compiled
 *  version of this program. Otherwise, please distribute
 *  freely.
 *
 *  Plays music in a simplified ABC notation, given on the
 *  command line, or one of several built-in songs.
 *
 **********************************************************/

int _putc(int); // XXX
int _out8(int, int);
int _exit();

/* XXXX */
int puts(unsigned char *s) {
  while ((int)*s != (int)0) {
    _putc((int)*s);
    s = (unsigned char*)((int)s + (int)1);
  }
  return 0;
}

unsigned char *meta_note = "Now this is the part of the data segment "
  "that stores global variables. This is actually a string constant in "
  "the program itself, so you'll see it again when I show you the source "
  "code later. We have almost 64kb of space to store stuff, although "
  "this segment is also used for the stack of local variables and "
  "arguments, and would be used for malloc as well, if it were "
  "implemented. Storing a string like this is basically free, because "
  "everything in it is printable, aside from the terminating \\0 "
  "character. At program startup, non-printable characters are "
  "overwritten by instructions in the code segment. Like, here's one: "
  "--> \xFF <-- It's stored in the data segment as a printable "
  "placeholder.";

// Adlib uses two bytes to do a "note-on", and the notes are specified
// in a somewhat complex way (octave multiplier plus frequency.) These
// tables give the upper and lower byte for each MIDI note. Computed
// by makefreq.sml.
unsigned char *upper = "\x20\x20\x20\x20\x20\x20\x20\x20!!!!!!!!!!!!"
  "\x22\x22\x22\x22\x22\x22\x22#####&&&&&&&'''''*******+++++"
  "......./////222222233333666666677777:::::::;;;;;>>>>>>>"
  "????????????????\0";
unsigned char *lower = "\xA9\xB3\xBD\xC9\xD5\xE1\xEF\xFD\x0C\x1C-?Qf{"
  "\x91\xA9\xC2\xDD\xFA\x18" "8Y}\xA3\xCB\xF6#R\x85\xBA\xF3\x18"
  "8Y}\xA3\xCB\xF6#R\x85\xBA\xF3\x18" "8Y}\xA3\xCB\xF6#R\x85\xBA"
  "\xF3\x18" "8Y}\xA3\xCB\xF6#R\x85\xBA\xF3\x18" "8Y}\xA3\xCB\xF6#R"
  "\x85\xBA\xF3\x18" "8Y}\xA3\xCB\xF6#R\x85\xBA\xF3\x18" "8Y}\xA3\xCB"
  "\xF6#R\x85\xBA\xF3\x18" "8Y}\xA3\xCB\xF6#R\x85\xBA\xF3\xFF\xFF"
  "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\0";

unsigned char *default_song =
  "ABD'B" "^F'3^F'3E'6"
  "AB_D'A" "E'3E'3D'6"
  "AB_D'A" "D'4E'2_D'2B2A2z2" "A2E'4D'4";

unsigned char *alphabet =
  "C4C4G4G4A4A4G8" "F4F4E4E4D4D4C8"
  "G4G4F4F4E4E4D8" "G4G4F4F4E4E4D8"
  "C4C4G4G4A4A4G8" "F4F4E4E4D4D4C8";

unsigned char *plumber =
  "e'e'ze'zc'e'zg'z3g2z2c'z2gz2ez2azbz^aazge'zg'a'2f'g'ze'zc'd'bz2c'z2gz2ez2azbz^aazge'zg'a'zf'g'ze'zc'd'bz4g'^f'f'^d'ze'z^gac'zac'd'z2g'^f'f'^d'ze'zc''zc''c''z5g'^f'f'^d'ze'z^gac'zac'd'z2^d'z2d'z2c'|"
  "DDzDzDDzgz3G2z2Gz2Ez2Cz2FzGz^FFzEczef2dezczABGz2Gz2Ez2Cz2FzGz^FFzEczefzdezczABGz2Cz2Gz2czFz2cczFzCz2Ez2Gczg'zg'g'zGzCz2Gz2czFz2cczFzCz^Gz2^Az2cz2GGzC";
  //  "e2e4e4c2e4g2z6G4"
  //  "cz2Gz2EzA2B^2'AA2"
  //  "G2e2g2a3fg3e4cdBz";

unsigned char *bluehair =
  "^A8z2F4^G8F3c4^A4F4^A4^G8z8"
  "^A8z2c8z2^c8z2^d8z2f8z2F4F4F4F8";

typedef struct {
  unsigned char *song;
  int idx;
  int midi_note;
  unsigned int ticksleft;
} Channel;

int Adlib(int reg, int value) {
  int i;
  _out8((int)0x0388, (int)reg);
  // We have to wait "12 cycles" after writing the port.
  for (i = 0; i < (int)12; i++) {}
  _out8((int)0x0389, (int)value);
  // And 84 cycles after writing the value. These numbers are
  // probably far too high; recall that a for loop like this
  // has to jump through every rung in the program! (i.e.,
  // A single iteration is linear in the program size.)
  for (i = 0; i < (int)84; i++) {}
  return 0;
}

int PlayNote(int ch, int midi_note) {
  // First turn note off; silence is better than weird "accidentals."
  Adlib((int)0xB0 + ch, 0x00);
  // midi_note = 128 actually accesses the terminating \0 in the
  // above strings, which is what we want to turn off the channel.
  Adlib((int)0xA0 + ch, (int)(lower[midi_note]));
  Adlib((int)0xB0 + ch, (int)(upper[midi_note]));
}

// Zero all the adlib ports, which both silences it and
// initializes it.
int Quiet() {
  int port;

  // Clear the main tones first, so that we don't hear artifacts during
  // the clearing process if a note is playing.
  Adlib((int)0xB0, (int)0x00);
  Adlib((int)0xB1, (int)0x00);
  Adlib((int)0xB2, (int)0x00);

  for (port = (int)0x01; port <= (int)0xF5; port++) {
    Adlib((int)port, (int)0x00);
  }
}

// ABC provides no standard library, so you gotta roll
// your own.
int strlen(unsigned char *s) {
  int len = 0;
  while ((int)*s != (int)0) {
    len++;
    s = (unsigned char *)((int)s + (int)1);
  }
  return len;
}

int streq(unsigned char *a, unsigned char *b) {
  int i;
  for (i = 0; /* in loop */; i++) {
    int ca = a[i], cb = b[i];
    if (ca != cb) return (int)0;
    if (ca == (int)0) return (int)1;
  }
}

// DOS command lines always start with a space, which is annoying.
// Strip that. DOS also terminates the command line with 0x0D, not
// 0x00. This function updates it in place so that we can use normal
// string routines on it.
int MakeArgString(unsigned char **argstring) {
  unsigned char *s = *argstring;
  while (*s == (int)' ') {
    s = (unsigned char *)((int)s + (int)1);
  }
  *argstring = s;

  while ((int)*s != (int)0x0D) {
    s = (unsigned char *)((int)s + (int)1);
  }
  *s = (unsigned char)0;
  return 0;
}

// We pick octave 4 as the base one; this is fairly canonical and
// benefits us since this array is all printable. Note that A4 is
// higher than C4, since octave 4 begins at the note C4. This
// array maps A...G to the corresponding MIDI note.
unsigned char *octave4 =
  "9"  // A = 57
  ";"  // B = 59
  "0"  // C = 48
  "2"  // D = 50
  "4"  // E = 52
  "5"  // F = 53
  "7"; // G = 55
// Parse a character c (must be capital A,B,C,D,E,F,G)
// and interpret any suffixes as well.
int ParseNote(unsigned char *ptr, int c, int *idx) {
  int midi;
  int offset = c - (int)'A';
  int nextc;
  midi = octave4[offset];
  for (;;) {
    nextc = (int)ptr[*idx];
    switch (nextc) {
    case '\'':
      // Up octave.
      midi += (int)12;
      break;
    case ',':
      // Down octave.
      midi -= (int)12;
      break;
    default:
      // Not suffix, so we're done (and don't consume
      // the character.)
      return midi;
    }
    *idx = *idx + (int)1;
  }
}

unsigned int ParseLength(unsigned char *ptr, int *idx) {
  int c = (int)ptr[*idx];
  if (c >= (int)'2' && c <= (int)'8') {
    int m = c - (int)'0';
    *idx = *idx + (int)1;
    return (unsigned int)200 * m;
  }
  return (unsigned int)200;
}

// Parse the song description (ptr) starting at *idx. Updates *idx to
// point after the parsed note. Updates *len to be the length in some
// unspecified for-loop unit. Returns the MIDI note to play next, or 0
// when the song is done.
int GetMidi(unsigned char *ptr, int *idx, unsigned int *len) {
  int c, midi_note;
  int sharpflat = 0;
  for (;;) {
    c = (int)(ptr[*idx]);

    // End of string literal.
    if (c == (int)0) return 0;

    _putc('[');
    _putc(c);
    _putc(']');
    // Advance to next character.
    *idx = *idx + (int)1;


    switch (c) {
    case '^':
      sharpflat++;
      break;
    case '_':
      sharpflat--;
      break;
    case '=':
      // Nothing. We assume key of C, so there are no naturals.
      break;
    case 'z':
      *len = ParseLength(ptr, idx);
      // No sound.
      return 128;
    default:
      if (c >= (int)'A' && c <= (int)'G') {
        midi_note = ParseNote(ptr, c, idx) + sharpflat;
        *len = ParseLength(ptr, idx);
        return midi_note;
      } else if (c >= (int)'a' && c <= (int)'g') {
        midi_note = ParseNote(ptr, c - (int)32, idx) + (int)12 + sharpflat;
        *len = ParseLength(ptr, idx);
        return midi_note;
      }
    }
  }
}

// Adlib has 9 channels, but they are packed in groups of
// three (c1o1, c2o1, c3o1, c1o2, c2o2, c3o2, c4o1, ...).
// So this only works for the first three channels. Not
// too hard to generalize, especially with a table.
int InitInstrument(int ch) {
  // Initialize the Adlib instrument.
  Adlib((int)0x20 + ch, 0x01); // Modulator multiple 1.
  Adlib((int)0x40 + ch, 0x10); // Modulator gain ~ 40db.
  Adlib((int)0x60 + ch, 0xF0); // Modulator attack: quick. Decay: long.
  Adlib((int)0x80 + ch, 0x77); // Modulator sustain: med. Release: med.
  Adlib((int)0x23 + ch, 0x01); // Carrier multiple to 1.
  Adlib((int)0x43 + ch, 0x00); // Carrier at max volume.
  Adlib((int)0x63 + ch, 0xF0); // Carrier attack: quick. Decay: long.
  Adlib((int)0x83 + ch, 0x77); // Carrier sustain: med. release: med.
}

// First test for known songs. After that, if we have a command line,
// use it. Otherwise, use the default song.
unsigned char *GetSong(unsigned char *cmdline) {
  if (streq(cmdline, (unsigned char *)"-alphabet")) {
    return alphabet;
  } else if (streq(cmdline, (unsigned char *)"-plumber")) {
    return plumber;
  } else if (streq(cmdline, (unsigned char *)"-bluehair")) {
    return bluehair;
  } else if (strlen(cmdline) > (int)0) {
    return cmdline;
  } else {
    return default_song;
  }
}

// Note: Doesn't check that the input is within the maximum number of
// channels!
int SplitChannels(unsigned char *song, Channel *channels) {
  int i, current_channel = 0;
  unsigned char *prevsong = song;
  for (i = (int)0; /* in loop */; i++) {
    // _putc((int)song[i]);
    int c = song[i];
    switch (c) {
    case '|':
    case '\0':
      _putc('|');
      song[i] = (unsigned char)'\0';
      {
        Channel *channel = &channels[current_channel];
        channel->song = prevsong;
        // Silence; ready for next note.
        channel->midi_note = (int)128;
        channel->ticksleft = (int)0;
        channel->idx = (int)0;
      }
      current_channel++;
      // Start after the nul-terminator byte.
      prevsong = &song[i + (int)1];
      if (c == (int)0) return current_channel;
    default:
      break;
    }
  }
}

int main(int argc, unsigned char **argv) {
  Channel channels[3];
  unsigned char *song;
  unsigned char *cmdline = *argv;
  int num_channels;

  MakeArgString(&cmdline);
  song = GetSong(cmdline);

  // Initialize channels. Note that this will just blow the
  // stack-allocated channels array if there are more than
  // two in the input string!
  num_channels = SplitChannels(song, (Channel *)&channels);

  Quiet();

  {
    int i;
    _putc('n'); _putc((int)'0' + num_channels); _putc('\n');
    for (i = 0; i < num_channels; i++) {
      _putc('c'); _putc((int)'0' + i); _putc(':');
      puts(channels[i].song);
      _putc('\n');
    }
  }

  {
    int i;
    for (i = 0; i < num_channels; i++)
      InitInstrument(i);
  }

  _putc('\n');
  for (;;) {
    int ch;
    int all_done = 1;
    // At each tick (whose rate is governed just by the time
    // it takes to do this loop), reduce each channel's ticksleft;
    // if it was (already) zero, load a new note.
    for (ch = 0; ch < num_channels; ch++) {
      Channel *channel = &channels[ch];
      int midi_note = channel->midi_note;
      if (midi_note != (int)0) {
        int ticksleft = channel->ticksleft;
        all_done = 0;
        if (ticksleft > (int)0) {
          channel->ticksleft = ticksleft - (int)1;
        } else {
          int new_note = GetMidi(channel->song, &channel->idx,
                                 &channel->ticksleft);
          channel->midi_note = new_note;
          _putc((int)'0' + ch);
          _putc((int)'a' + new_note);
          _putc('\n');
          if (new_note == (int)0) {
            // Quiet the channel -- forever!
            PlayNote(ch, (int)128);
          } else {
            PlayNote(ch, new_note);
          }
        }
      }
    }
    if (all_done) break;
  }

  Quiet();
  return 0;
}
