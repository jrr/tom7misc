int _putc(int);

static int Other() {
  return 'k';
}

int main(int argc, char **argv) {
  _putc('o');
  _putc(Other());
  _putc('!');
  return 0;
}
