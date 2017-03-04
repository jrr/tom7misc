int _putc(int);

unsigned int aaa = 0;

int SetA() {
  aaa = 0xFFEE;
  return 0;
}

int MoreTests() {
  if (aaa == (unsigned int)0) {
    _putc('N');
    _putc('O');
    return 0xFF;
  }

  if (aaa < (unsigned int)0) {
    _putc('N');
    _putc('O');
    return 0xFF;
  }

  if (aaa <= (unsigned int)0) {
    _putc('N');
    _putc('O');
    return 0xFF;
  }
}

int main(int argc, char **argv) {
  if (aaa != (unsigned int)0) {
    _putc('N');
    _putc('O');
    return 0xFF;
  }

  if (aaa > (unsigned int)0) {
    _putc('N');
    _putc('O');
    return 0xFF;
  }

  if (aaa < (unsigned int)0) {
    _putc('N');
    _putc('O');
    return 0xFF;
  }

  if (aaa == (unsigned int)0) {
    _putc('o');
  }

  SetA();

  if (aaa > (unsigned int)0) {
    _putc('k');
  }

  if (aaa >= (unsigned int)0) {
    _putc('\n');
  }

  MoreTests();

  return 0;
}
