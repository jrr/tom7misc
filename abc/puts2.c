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

int main(int argc, unsigned char **argv) {
  unsigned char message[4];
  message[1] = 'k';
  message[0] = 'o';
  message[2] = '!';
  message[3] = '\0';
  
  puts(message);
  return 0;
}
