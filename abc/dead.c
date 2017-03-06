int _putc(int);

static int DeadFunction() {
  return 'k';
}

static int DeadRec() {
  return DeadRec();
}

static int DeadAddr() {
  return 0;
}

int main(int argc, char **argv) {
  DeadAddr;
  _putc('o');
  _putc('k');
  _putc('!');
  return 0;
}
