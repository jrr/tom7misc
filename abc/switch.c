int _putc(int);

int main(int argc, char **argv) {
  int v = 0;
  switch (argc) {
    /* unreachable */
    _putc(0);
    break;
  case 1:
    /* fallthrough */
    v++;
  case 0:
    v = v + (int)2;
    break;
  case 2:
    v++;
    if (v > (int)1) {
    case 3:
      v = 5;
    }
    break;
  default:;
  }

  return v;
}
