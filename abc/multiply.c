int _putc(int);

int main(int argc, char **argv) {
  int x = (((int)0x4320 + argc) * (int)0x229 + (int)0x22) & (int)0xFF;
  _putc((int)'o' * (int)argc);
  _putc(x);
  _putc((int)11 * (int)3);
  return 0;
}
