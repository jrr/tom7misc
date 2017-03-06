int _putc(int);

int global_o = 'o';
int global_k;

int main(int argc, char **argv) {
  static int global_bang;
  global_bang = '!';
  _putc(global_o);
  global_k = 'k';
  _putc(global_k);
  _putc(global_bang);
  return 0;
}
