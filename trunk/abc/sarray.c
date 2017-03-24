int _putc(int);
int _out8(int, int);
int _exit();

typedef struct {
  int tag;
  unsigned char a;
  unsigned char b;
  unsigned char c;
} Item;

int WriteItems(Item *items) {
  int i;
  for (i = 0; i < (int)3; i++) {
    switch (items[i].tag) {
    case 0:
      _putc(items[i].a);
      break;
    case 1:
      _putc(items[i].b);
      break;
    case 2:
      _putc(items[i].c);
      break;
    default:
      _putc('X');
    }
  }
  return 0;
}

int main(int argc, unsigned char **argv) {
  Item items[(int)3];
  items[0].tag = (int)2;
  items[0].c = 'o';

  items[2].b = '!';
  items[2].tag = (int)1;

  items[1].tag = (int)0;
  items[1].a = 'k';
  
  WriteItems(items);
  return 0;
}
