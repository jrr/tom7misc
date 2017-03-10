int _putc(int);

int main(int argc, unsigned char **argv) {
  int i = 0;

  char *s = "ok!";
  for (i = 0; s[i]; i++) {
    _putc((int)s[i]);
  }

  return 0;
}
