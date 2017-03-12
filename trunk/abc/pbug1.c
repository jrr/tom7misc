int _putc(int);

int GetMidi(unsigned char *ptr, int *idx) {
  int c;
  int sharpflat = 0;
  // Default octave.
  int octave = 4;
  for (;;) {
    c = (int)(ptr[*idx]);
    // End of string.
    // XXX use "switch" here...
    if (c == (int)0) return 0;

    /*
    _putc((int)'[');
    _putc(c);
    _putc((int)']');

    if (*idx > (int)10) {
      _putc((int)'!');
      return 0;
    }
    */

    if (c == (int)'^') sharpflat++;
    else if (c == (int)'_') sharpflat--;
    else if (c >= (int)'A' && c <= (int)'G') {
      _putc('o');
      (*idx) = (*idx) + (int)1;      
      return 1;
    } else if (c >= (int)'a' && c <= (int)'g') {
      octave++;
      _putc('k');
      (*idx) = (*idx) + (int)1;
      // Now read suffixes...
      // return ParseNote(ptr, c - (int)32, idx);
      return 2;
    }
    // Advance to next character.
    (*idx) = (*idx) + (int)1;
  }
}

int main(int argc, char **argv) {
  int song_idx = 0, j, midi_note;
  unsigned char *song = "A^^c_";

  for (;;) {
    // _putc((int)'0' + song_idx);
    midi_note = GetMidi(song, &song_idx);
    if (!midi_note) break;
    // _putc((int)'\n');
    // _putc((int)'a' + midi_note);
  }

  _putc((int)'!');

  return 0;
}
