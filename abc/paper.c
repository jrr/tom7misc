/* Here's the C source code for the program that is
   the SIGBOVIK paper.

   (Highly in-progress as of 10 Mar 2017!)
*/

int _putc(int); // XXX non-printable! don't use in paper!
int _out8(int, int);

/*
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

int Adlib(int reg, int value) {
  int i;
  _out8((int)0x0388, (int)reg);
  for (i = 0; i < (int)12; i++) {}
  _out8((int)0x0389, (int)value);
  for (i = 0; i < (int)84; i++) {}
  return 0;
}

int PlayNote(int midi_note) {
  Adlib((int)0xA0, (int)(lower[midi_note]));
  Adlib((int)0xB0, (int)(upper[midi_note]));
}

int Quiet() {
  int port;

  // Zero all registers to clear sound card.
  for (port = (int)0x01; port <= (int)0xF5; port++) {
    // _putc('.');
    Adlib((int)port, (int)0x00);
  }
}
*/
int ParseNote(unsigned char *ptr, int c, int *idx) {
  (*idx) = (*idx) + (int)1;
  return (int)45 + (c - (int)'A');
}

int GetMidi(unsigned char *ptr, int *idx) {
  _putc((int)*ptr);
  return 0;
  /*
  int c;
  int sharpflat = 0;
  // Default octave.
  int octave = 4;
  for (;;) {
    c = (int)(ptr[*idx]);
    // End of string.
    // XXX use "switch" here...
    if (c == (int)0) return 0;

    _putc((int)'[');
    _putc(c);
    _putc((int)']');

    if (*idx > (int)10) return 0;

    if (c == (int)'^') sharpflat++;
    else if (c == (int)'_') sharpflat--;
    else if (c >= (int)'A' && c <= (int)'G') {
      // Read suffixes...
      return ParseNote(ptr, c, idx);
    } else if (c >= (int)'a' && c <= (int)'g') {
      octave++;
      // Now read suffixes...
      return ParseNote(ptr, c - (int)32, idx);
    }

    // Advance to next character.
    (*idx) = (*idx) + (int)1;
  }
  */
}

int main(int argc, char **argv) {
  int song_idx = 0, j, midi_note;
  unsigned char *song = "ACCAEED";
  /*
  Quiet();

  Adlib((int)0x20, (int)0x01); // Set the modulator's multiple to 1
  Adlib((int)0x40, (int)0x10); // Set the modulator's level to about 40 dB
  Adlib((int)0x60, (int)0xF0); // Modulator attack: quick; decay: long
  Adlib((int)0x80, (int)0x77); // Modulator sustain: medium; release: medium
  Adlib((int)0xA0, (int)0x98); // Set voice frequency's LSB (it'll be a D#)
  Adlib((int)0x23, (int)0x01); // Set the carrier's multiple to 1
  Adlib((int)0x43, (int)0x00); // Set the carrier to max volume (about 47 dB)
  Adlib((int)0x63, (int)0xF0); // Carrier attack: quick; decay: long
  Adlib((int)0x83, (int)0x77); // Carrier sustain: medium; release: medium
  Adlib((int)0xB0, (int)0x31); // Turn voice on; set the octave and freq MSB
  */

  for (;;) {
    _putc((int)'0' + song_idx);
    midi_note = GetMidi(song, &song_idx);
    if (!midi_note) break;
    _putc((int)'\n');
    _putc((int)'a' + midi_note);
    // PlayNote(midi_note);
    // for (j = (int)0; j < (int)1000; j++) {}
  }

  // Quiet();
  return 0;
}
