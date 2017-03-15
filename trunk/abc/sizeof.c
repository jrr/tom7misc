int _putc(int);

int main(int argc, char **argv) {
  _putc('m' + sizeof(int));
  _putc('j' + sizeof(unsigned char));
  _putc('!');
  return 0;
}
