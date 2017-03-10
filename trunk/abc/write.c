int _putc(int);

int main(int argc, unsigned char **argv) {
  int i = 0;
  unsigned char *cmdline = *argv;
  cmdline[0] = (char)'o';
  cmdline[1] = (char)'k';
  cmdline[2] = (char)0;

  for (i = 0; i < (int)(0xFF - 0x81); i++) {
    int c = (int)cmdline[i];
    if (!c)
      break;
    _putc((int)c);
  }

  cmdline[0] = (char)'a';
  if ((int)cmdline[0] != (int)'a' ||
      (int)cmdline[1] != (int)'k') {
    _putc('X');
  }
  cmdline[1] = (char)'b';
  if ((int)cmdline[0] != (int)'a' ||
      (int)cmdline[1] != (int)'b') {
    _putc('Y');
  }

  _putc('!');
  return 0;
}
