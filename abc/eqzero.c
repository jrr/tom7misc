int _putc(int);

int main(int argc, char **argv) {
  int x = !(argc + (int)1);
  if (x) {
    _putc('X');
  }

  if (argc == (int)1 || argc == (int)2) {
    _putc('X');
  }

  if (!x) {
    _putc('o');
  }

  if (x == 0) {
    _putc('k');
  }

  if (argc == (int)0) {
    _putc('!');
  }

  return 0;
}
