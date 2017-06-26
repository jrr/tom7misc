
// header file for auth module.


#ifndef LUDUS_AUTH_H
#define LUDUS_AUTH_H

#define ROL(x,y) (((x)<<(y)) | ((x) >> (32-(y))))
#define ROR(x,y) (((x)>>(y)) | ((x) << (32-(y))))

#define bit(x,y) (((x)>>(y))&1)

typedef struct {
     ulong h,
           l;
} wint;

ulong func       (ulong a, ulong k),
      kcompress  (wint  k, int i),
      subst      (ulong in),
      permute    (ulong in);
wint  enc_block  (wint in, wint key),
      dec_block  (wint in, wint key),
      keyfromtext(uchar * m);
void  print_wint (wint x);



#endif




