int _putc(int);



int main(int argc, char **argv) {
  struct {
    int x;
    char c;
  } s;

  s.x = (int)'o';
  s.c = 'k';

  _putc(s.x);
  _putc((int)(&s)->c);
  _putc((int)'!');
  return 0;
}
