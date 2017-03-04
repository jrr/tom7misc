int _putc(int);

int Other(int a, int b, int c) {
  _putc(b);
  _putc(c);
  _putc(a);
}

int main(int argc, char **argv) {
  Other('\n', 'o', 'k');
  return 0;
}
