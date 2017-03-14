int _putc(int);

int main(int argc, char **argv) {
  int ffff = (int)0 - argc;
  int oooo = argc - (int)1;
  int thirty_two = (int)31 + 1;
  _putc(((int)'o' & ffff) | oooo);
  _putc((int)'K' | thirty_two);
  _putc((int)'!' | oooo);
  return 0;
}
