int _putc(int);

int main(int argc, char **argv) {
  int i = 0;
  for (i = (int)250; i >= (int)0; i--) {
    if (i == (int)'o' ||
        i == (int)'k' ||
        i == (int)'\n') {
      _putc(i);
    }
  }
  return 0;
}
