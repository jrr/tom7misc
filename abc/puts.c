int _putc(int);
int _out8(int, int);
int _exit();

int puts(unsigned char *s) {
  while ((int)*s != (int)0) {
    _putc((int)*s);
    s = (unsigned char*)((int)s + (int)1);
  }
  return 0;
}

unsigned char *message = "ok!";

int main(int argc, unsigned char **argv) {
  puts(message);
  return 0;
}
