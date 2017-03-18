int _putc(int);

int main(int argc, char **argv) {
  _putc(((int)7 << (int)4) + (- (int)1));
  _putc(((int)13 << (argc + (int)2)) | (int)3);
  _putc((int)'!' << (int)0);
  return 0;
}
