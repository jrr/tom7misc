int toplevel_global = 0;
char toplevel_printable = 'c';
char toplevel_global_simplified = (1 + 2) ? (char)'a' : (char)'b';

int main (int argc, char *argv) {
  toplevel_global ++;
  return (int)toplevel_printable;
}
