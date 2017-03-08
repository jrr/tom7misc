int _putc(int);

int main(int argc, char **argv) {
  int i = 0;
  char *cmdline = *argv;
  for (i = 0; i < (int)(0xFF - 0x81); i++) {
    char c = cmdline[i];
    //    if (c == 0x0D)
    // return 0;
    // _putc((int)c);
  }
  _putc('\n');
  return 0;
}
