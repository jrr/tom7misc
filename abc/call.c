
char global = 123;

static char Other(char arg) {
  global = 45;
  return 32;
}

int main(int argc, char **argv) {
  char t = 67;
  Other(8);
  return t + global;
}
