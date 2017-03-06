int _putc(int);

int global_o = 'o';

int Other() {
  global_o = 'k';
}

int main(int argc, char **argv) {
  _putc(global_o);
  Other();
  _putc(global_o);
  _putc(global_o = '!');
  return 0;
}
