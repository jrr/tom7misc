
#include <cstdint>
#include <cmath>

#include "../cc-lib/arcfour.h"
#include "../cc-lib/randutil.h"


using uint8 = uint8_t;
using uint32 = uint32_t;

static float FloatBits(ArcFour *rc) {
  static_assert(sizeof (uint32) == sizeof (float), "32-bits please");
  union {
    uint32 i;
    float f;
  } u;
  u.i = Rand32(rc);
  return u.f;
}

static float Xor4(float a, float b) {
  return 
    fmod(((a * -1.89269124e+30f) +
	  (b * -1.09500709e+35f)) * -1.14474456e-18f, 4.77664232f);
}

static void FindXor() {

  auto Code = [](float c1, float c2, float c3, float c4,
		 uint8 a, uint8 b) -> uint8 {
    float aa = a, bb = b;

    float r = fmod(((aa * c1) + (bb * c2)) * c3, c4);
    
    uint8 ret = trunc(r);
    return ret;
  };

  int bits = 2;
  
  ArcFour rc{"fluint0"};
  int best_errors = 500;
  while (best_errors > 0) {
    float c1 = FloatBits(&rc);
    float c2 = FloatBits(&rc);
    float c3 = FloatBits(&rc);

    float c4 = (float)(1 << bits) + (RandFloat(&rc) * (bits * 2.0f));
    
    int errors = 0;
    for (uint8 a = 0; (int)a < (int)(1 << bits); a++) {
      for (uint8 b = 0; (int)b < (int)(1 << bits); b++) {
	uint8 c = Code(c1, c2, c3, c4, a, b);
	if (c != (a ^ b)) {
	  errors++;
	  if (errors >= best_errors) goto again;
	}
      }
    }
    if (errors < best_errors) {
      printf("New best, %d errors: %.9g %.9g %.9g %.9g\n",
	     errors,
	     c1, c2, c3, c4);
      best_errors = errors;
    }
  again:;
  }
}


int main () {
  for (int a = 0; a < 4; a++) {
    for (int b = 0; b < 4; b++) {
      float c = Xor4((float)a, (float)b);
      printf("%d ^ %d = %d, aka %.9g\n",
	     a, b, (a ^ b), c);
    }
  }
  exit(0);

  FindXor();
}
