int _putc(int);

static int Other(char arg) {
  return 'k';
}

int main(int argc, char **argv) {
  _putc('o');
  Other(8);
  _putc('\n');
  return 0;
}
