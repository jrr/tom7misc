int _putc(int);

int main(int argc, char **argv) {
  int i, x = 0, y = 0;
  for (i = 0; i < 256; i++) {
    switch (i) {
    case 256:
      _putc('!');
      return 0;
    case 42:
      _putc('o');
      // Increments x
      break;
    case 99:
      _putc('k');
      // No increment
      continue;
    default:
      y++;
      break;
    }
    x++;
  }

  if (x != 255 || y != 254) {
    _putc('X');
  }
  
  _putc('\n');
  return 0;
}
