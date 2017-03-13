/**********************************************************
 *  paper.c, Copyright (c) 2017 Tom Murphy VII Ph.D.
 *  This copyright notice must appear in the compiled
 *  version of this program. Otherwise, please distribute
 *  freely.
 *
 *  Plays music in a simplified ABC notation, given on the
 *  command line.
 *
 **********************************************************/

int _putc(int); // XXX non-printable! don't use in paper!
int _out8(int, int);
int _exit();

unsigned char *note = "Now this is the part of the data segment that "
  "stores global variables. This is actually a string constant in "
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
  "????????????????";
unsigned char *lower = "\xA9\xB3\xBD\xC9\xD5\xE1\xEF\xFD\x0C\x1C-?Qf{"
  "\x91\xA9\xC2\xDD\xFA\x18" "8Y}\xA3\xCB\xF6#R\x85\xBA\xF3\x18"
  "8Y}\xA3\xCB\xF6#R\x85\xBA\xF3\x18" "8Y}\xA3\xCB\xF6#R\x85\xBA"
  "\xF3\x18" "8Y}\xA3\xCB\xF6#R\x85\xBA\xF3\x18" "8Y}\xA3\xCB\xF6#R"
  "\x85\xBA\xF3\x18" "8Y}\xA3\xCB\xF6#R\x85\xBA\xF3\x18" "8Y}\xA3\xCB"
  "\xF6#R\x85\xBA\xF3\x18" "8Y}\xA3\xCB\xF6#R\x85\xBA\xF3\xFF\xFF"
  "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF";

unsigned char *default_song =
  "ABD'B" "^F'3^F'3E'6"
  "AB_D'A" "E'3E'3D'6"
  "AB_D'A" "D'4E'2_D'2B2A2z2" "A2E'4D'4";

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

int PlayNote(int midi_note) {
  // First turn note off; silence is better than weird "accidentals."
  Adlib((int)0xB0, (int)0x00);
  // midi_note = 128 actually accesses the terminating \0 in the
  // above strings, which is what we want to turn off the channel.
  Adlib((int)0xA0, (int)(lower[midi_note]));
  Adlib((int)0xB0, (int)(upper[midi_note]));
}

// Zero all the adlib ports, which both silences it and
// initializes it.
int Quiet() {
  int port;

  // Clear the main tone first, so that we don't hear artifacts during
  // the clearing process if a note is playing.
  Adlib((int)0xB0, (int)0x00);

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

// DOS terminates the command line with 0x0D, not 0x00.
// This function updates it in place so that we can use
// normal string routines on it.
int MakeArgString(unsigned char *s) {
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
  "0"  // C = 48 = 0
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
    if (nextc == (int)'\'') {
      // Up octave.
      midi += (int)12;
    } else if (nextc == (int)',') {
      // Down octave.
      midi -= (int)12;
    } else {
      // Not suffix, so we're done (and don't consume
      // the character.)
      return midi;
    }
    *idx = *idx + (int)1;
  }
}

unsigned int ParseLength(unsigned char *ptr, int *idx) {
  // TODO: Multiplication would be nice...
  int c = (int)ptr[*idx];
  if (c == (int)'2') {
    *idx = *idx + (int)1;
    return 4096;
  } else if (c == (int)'3') {
    *idx = *idx + (int)1;
    return 6144;
  } else if (c == (int)'4') {
    *idx = *idx + (int)1;
    return 8192;
  } else if (c == (int)'5') {
    *idx = *idx + (int)1;
    return 10240;
  } else if (c == (int)'6') {
    *idx = *idx + (int)1;
    return 12288;
  } else if (c == (int)'7') {
    *idx = *idx + (int)1;
    return 14336;
  } else if (c == (int)'8') {
    *idx = *idx + (int)1;
    return 16384;
  }
  return 2048;
}

// Parse the song description (ptr) starting at *idx. Updates *idx to
// point after the parsed note. Returns the midi index (TODO: length,
// etc.), or 0 when the song is done.
int GetMidi(unsigned char *ptr, int *idx, unsigned int *len) {
  int c, midi_note;
  int sharpflat = 0;
  for (;;) {
    c = (int)(ptr[*idx]);

    // End of string literal.
    if (c == (int)0) return 0;
    // End of command-line argument.
    if (c == (int)0x0D) return 0;

    _putc((int)'[');
    _putc(c);
    _putc((int)']');

    // Advance to next character.
    _putc((int)'=');
    _putc((int)'0' + *idx);
    *idx = *idx + (int)1;
    _putc((int)'=');
    _putc((int)'0' + *idx);

    if (c == (int)'^') {
      sharpflat++;
    } else if (c == (int)'_') {
      sharpflat--;
    } else if (c == (int)'=') {
      // Nothing. We assume key of C, so there are no naturals.
    } else if (c >= (int)'A' && c <= (int)'G') {
      _putc('P');
      midi_note = ParseNote(ptr, c, idx) + sharpflat;
      *len = ParseLength(ptr, idx);
      return midi_note;
    } else if (c >= (int)'a' && c <= (int)'g') {
      _putc('p');
      midi_note = ParseNote(ptr, c - (int)32, idx) + (int)12 + sharpflat;
      *len = ParseLength(ptr, idx);
      return midi_note;
    } else if (c == (int)'z') {
      _putc('z');
      *len = ParseLength(ptr, idx);
      return 128;  // No sound.
    }
  }
}

int main(int argc, unsigned char **argv) {
  int song_idx = 0, j, midi_note;
  unsigned char *cmdline = *argv;
  unsigned char *song;
  MakeArgString(cmdline);

  // If we have a command line, use that. Otherwise, the default
  // song.
  if (strlen(cmdline) > (int)0) {
    song = cmdline;
  } else {
    song = default_song;
  }

  _putc((int)'0' + strlen(song));

  Quiet();

  // Initialize the Adlib instrument.
  Adlib((int)0x20, (int)0x01); // Modulator multiple 1.
  Adlib((int)0x40, (int)0x10); // Modulator gain ~ 40db.
  Adlib((int)0x60, (int)0xF0); // Modulator attack: quick. Decay: long.
  Adlib((int)0x80, (int)0x77); // Modulator sustain: med. Release: med.
  Adlib((int)0x23, (int)0x01); // Carrier multiple to 1.
  Adlib((int)0x43, (int)0x00); // Carrier at max volume.
  Adlib((int)0x63, (int)0xF0); // Carrier attack: quick. Decay: long.
  Adlib((int)0x83, (int)0x77); // Carrier sustain: med. release: med.

  for (;;) {
    unsigned int len;
    _putc((int)'0' + song_idx);
    midi_note = GetMidi(song, &song_idx, &len);
    if (midi_note == (int)0) break;
    _putc((int)'A' + midi_note);
    PlayNote(midi_note);
    for (j = (int)0; j < len; j++) {}
    _putc((int)'\n');
  }

  _putc((int)'.');

  Quiet();
  return 0;
}
