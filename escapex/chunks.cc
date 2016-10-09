
#include "base.h"
#include "chunks.h"
#include "util.h"
#include "ptrlist.h"

Chunk::Chunk(uint32 k, int32 ii) : type(CT_INT32), key(k), i(ii) {}
Chunk::Chunk(uint32 k, bool bb) : type(CT_BOOL), key(k), i(bb) {}
Chunk::Chunk(uint32 k, string ss) : type(CT_STRING), key(k), i(0), s(ss) {}

string Chunk::ToString() {
  /* key and type first, then data.
     the size of string data must be deduced from some other
     source. */
  // printf("tostring(%p): key %d, type %d, i %d, b %d, s %");

  string res = sizes((uint32)key) + sizes((uint32)type);
  switch (type) {
  case CT_BOOL:
  case CT_INT32: return res + sizes(i);
  case CT_STRING: return res + s;
  default: break;
  }
  abort();
}

Chunk *Chunk::FromString(const string &s) {
  /* no type field */
  if (s.length() < 8) return nullptr;
  uint32 idx = 0;
  uint32 key = (unsigned int)shout(4, s, idx);
  ChunkType ty = (ChunkType)shout(4, s, idx);

  switch (ty) {
  case CT_BOOL:
    if (s.length() < 12) return nullptr;
    else return new Chunk(key, (bool)shout(4, s, idx));
  case CT_INT32:
    if (s.length() < 12) return nullptr;
    else return new Chunk(key, (int)shout(4, s, idx));
  case CT_STRING:
    return new Chunk(key, (string)s.substr(8, s.length() - 8));
  default:
    return nullptr;
  }
}

std::unique_ptr<Chunks> Chunks::Create() {
  return std::make_unique<Chunks>();
}

Chunks::~Chunks() {
  while (data)
    delete PtrList<Chunk>::pop(data);
}

Chunk *Chunks::Get(uint32 k) {
  for (PtrList<Chunk> *tmp = data; tmp; tmp = tmp->next) {
    if (tmp->head->key == k) return tmp->head;
  }
  return nullptr;
}

int Chunks::Compare(Chunk *l, Chunk *r) {
  return l->key - r->key;
}

string Chunks::ToString() {
  string op; /*  = sizes(data->length()); */

  /* sort so that we change the player file less often */
  PtrList<Chunk>::sort(Chunks::Compare, data);

  /*
  {
  printf("prepass\n");
  for (PtrList<Chunk> *tmp = data; tmp; tmp = tmp->next) {
    printf("    tmp is %p next is %p key: ", tmp, tmp->next);
    printf("    .. %d\n", tmp->head->key);
  }
  }
  */

  /*
  printf("Chunk tostring:\n");
  printf("data: %p\n", data); */
  for (PtrList<Chunk> *tmp = data; tmp; tmp = tmp->next) {
    /*
      printf("    tmp is %p next is %p key: ", tmp, tmp->next);
      printf("    .. %d\n", tmp->head->key);
    */
    const string it = tmp->head->ToString();
    op = op + sizes(it.length()) + it;
  }

  return op;
}

/* exhausts the input string */
std::unique_ptr<Chunks> Chunks::FromString(const string &s) {
  uint32 idx = 0;
  PtrList<Chunk> *dat = nullptr;

  while (idx < s.length()) {
    int len = shout(4, s, idx);
    /* XXX cleanup */
    if (idx + len > s.length()) {
      printf("bad length\n");
      return nullptr;
    }
    string ch = s.substr(idx, len); idx += len;
    Chunk *c = Chunk::FromString(ch);
    /* XXX cleanup */
    if (!c) {
      printf("bad c\n");
      return nullptr;
    }
    PtrList<Chunk>::push(dat, c);
  }

  std::unique_ptr<Chunks> ck{new Chunks};
  ck->data = dat;
  return ck;
}

void Chunks::Insert(Chunk *insme) {
  for (PtrList<Chunk> *tmp = data;
       tmp; tmp = tmp->next) {

    if (insme->key == tmp->head->key) {
      delete tmp->head;
      tmp->head = insme;
      return;
    }
  }

  /* not found */
  PtrList<Chunk>::push(data, insme);
}
