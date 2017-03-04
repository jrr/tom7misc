int _putc(int);

int main(int argc, char **argv) {
  if (argc > (int)5) {
    _putc('N');
    _putc('O');
  } else {
    _putc('o');
    _putc('k');
  }
  _putc('\n');
  return 0;
}
