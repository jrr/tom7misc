int _putc(int);

int main(int argc, char **argv) {
  _putc(argc + (int)'o');
  _putc((int)'k' - argc);
  _putc((int)'\n' ^ argc);
  return 0;
}
