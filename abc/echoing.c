int _putc(int);

int main(int argc, unsigned char **argv) {
  int i = 0;
  unsigned char *cmdline = *argv;
  // _putc((int)cmdline[0]);
  for (i = 0; i < (int)(0xFF - 0x81); i++) {
    int c = (int)cmdline[i];
    if (c == (int)0x0D)
     return 0;
    _putc((int)c);
  }
  _putc('\n');
  return 0;
}
