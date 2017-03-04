int _putc(int);

int global_o = 'o';
int global_k;

int main(int argc, char **argv) {
  static int global_nl;
  global_nl = '\n';
  _putc(global_o);
  global_k = 'k';
  _putc(global_k);
  _putc(global_nl);
  return 0;
}
