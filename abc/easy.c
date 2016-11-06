
int main(int argc, char **argv) {
  int x = 0, y;
  for (y = 0; y < 25; y++) {
    x++;
    if (x & 1) x *= 3;
    else x >>= 1;
  }
  return x;
}
