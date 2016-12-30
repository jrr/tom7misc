int main(int argc, char **argv) {
  unsigned long lll = 6;
  unsigned char c1 = 250;
  unsigned long mmm = (c1 += lll);
  // printf("%d %d %d\n", lll, c1, mmm);
  return mmm;
}
