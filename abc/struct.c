int _putc(int);



int main(int argc, char **argv) {
  struct {
    int x;
    char c;
  } s;

  _putc(s.x);

  return 0;
}
