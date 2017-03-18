int _putc(int);

int main(int argc, char **argv) {
  int i, x = 0, y = 0;
  for (i = 0; i < (int)256; i++) {
    switch (i) {
      _putc('Q');
    case 256:
      _putc('?');
      return 0;
    case 42:
      _putc('o');
      // Increments x
      break;
    case 99:
    case 257:
      _putc('k');
      // No increment
      continue;
    case 11:
      // Fallthrough.
    default:
      y++;
      break;
    }
    x++;
  }

  if (x != (int)255 || y != (int)254) {
    _putc('X');
  }

  _putc('!');
  return 0;
}
