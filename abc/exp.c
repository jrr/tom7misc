
int main(int argc, char **argv) {
  int v = 1, i;
  for (i = 0; i < argc; i++) {
    v = v * 2;
  }
  return v;
}
