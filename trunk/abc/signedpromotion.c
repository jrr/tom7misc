int main(int argc, char **argv) {
  signed char a = 0xfb;
  unsigned char b = 0xfb;

  // Both are promoted to int. a becomes -5; b becomes 251.
  if (a == b)
    return -1;

  return 0;
}
