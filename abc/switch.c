int main(int argc, char **argv) {
  int v = 0;
  switch (argc) {
    /* unreachable */
    v++;
    break;
  case 1:
    /* fallthrough */
    v++;
  case 0:
    v = v * 2;
    break;
  case 2:
    v++;
    if (v > 1) {
    case 3:
      v = 5;
    }
    break;
  default:;
  }

  return v;
}
