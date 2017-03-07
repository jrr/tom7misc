int _putc(int);
int _out8(int, int);

int Adlib(int reg, int value) {
  int i;
  _out8((int)0x0388, (int)reg);
  for (i = 0; i < (int)12; i++) {}
  _out8((int)0x0389, (int)value);
  for (i = 0; i < (int)84; i++) {}
  return 0;
}

int main(int argc, char **argv) {
  int port;

  // XX NNO
  // _out8((int)0x0338, (int)0x01);



  // Zero all registers to clear sound card.
  for (port = (int)0x01; port <= (int)0xF5; port++) {
    // _putc('.');
    Adlib((int)port, (int)0x00);
  }
  // _putc('\n');


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
  return 0;
}
