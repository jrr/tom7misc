#include <stdio.h>

unsigned int CSStab0[11]={5,0,1,2,3,4,0,1,2,3,4};

unsigned char CSStab1[256]=
{
  0x33,0x73,0x3b,0x26,0x63,0x23,0x6b,0x76,0x3e,0x7e,0x36,0x2b,0x6e,0x2e,0x66,0x7b,
  0xd3,0x93,0xdb,0x06,0x43,0x03,0x4b,0x96,0xde,0x9e,0xd6,0x0b,0x4e,0x0e,0x46,0x9b,
  0x57,0x17,0x5f,0x82,0xc7,0x87,0xcf,0x12,0x5a,0x1a,0x52,0x8f,0xca,0x8a,0xc2,0x1f,
  0xd9,0x99,0xd1,0x00,0x49,0x09,0x41,0x90,0xd8,0x98,0xd0,0x01,0x48,0x08,0x40,0x91,
  0x3d,0x7d,0x35,0x24,0x6d,0x2d,0x65,0x74,0x3c,0x7c,0x34,0x25,0x6c,0x2c,0x64,0x75,
  0xdd,0x9d,0xd5,0x04,0x4d,0x0d,0x45,0x94,0xdc,0x9c,0xd4,0x05,0x4c,0x0c,0x44,0x95,
  0x59,0x19,0x51,0x80,0xc9,0x89,0xc1,0x10,0x58,0x18,0x50,0x81,0xc8,0x88,0xc0,0x11,
  0xd7,0x97,0xdf,0x02,0x47,0x07,0x4f,0x92,0xda,0x9a,0xd2,0x0f,0x4a,0x0a,0x42,0x9f,
  0x53,0x13,0x5b,0x86,0xc3,0x83,0xcb,0x16,0x5e,0x1e,0x56,0x8b,0xce,0x8e,0xc6,0x1b,
  0xb3,0xf3,0xbb,0xa6,0xe3,0xa3,0xeb,0xf6,0xbe,0xfe,0xb6,0xab,0xee,0xae,0xe6,0xfb,
  0x37,0x77,0x3f,0x22,0x67,0x27,0x6f,0x72,0x3a,0x7a,0x32,0x2f,0x6a,0x2a,0x62,0x7f,
  0xb9,0xf9,0xb1,0xa0,0xe9,0xa9,0xe1,0xf0,0xb8,0xf8,0xb0,0xa1,0xe8,0xa8,0xe0,0xf1,
  0x5d,0x1d,0x55,0x84,0xcd,0x8d,0xc5,0x14,0x5c,0x1c,0x54,0x85,0xcc,0x8c,0xc4,0x15,
  0xbd,0xfd,0xb5,0xa4,0xed,0xad,0xe5,0xf4,0xbc,0xfc,0xb4,0xa5,0xec,0xac,0xe4,0xf5,
  0x39,0x79,0x31,0x20,0x69,0x29,0x61,0x70,0x38,0x78,0x30,0x21,0x68,0x28,0x60,0x71,
  0xb7,0xf7,0xbf,0xa2,0xe7,0xa7,0xef,0xf2,0xba,0xfa,0xb2,0xaf,0xea,0xaa,0xe2,0xff
};

unsigned char CSStab2[256]=
{
  0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x09,0x08,0x0b,0x0a,0x0d,0x0c,0x0f,0x0e,
  0x12,0x13,0x10,0x11,0x16,0x17,0x14,0x15,0x1b,0x1a,0x19,0x18,0x1f,0x1e,0x1d,0x1c,
  0x24,0x25,0x26,0x27,0x20,0x21,0x22,0x23,0x2d,0x2c,0x2f,0x2e,0x29,0x28,0x2b,0x2a,
  0x36,0x37,0x34,0x35,0x32,0x33,0x30,0x31,0x3f,0x3e,0x3d,0x3c,0x3b,0x3a,0x39,0x38,
  0x49,0x48,0x4b,0x4a,0x4d,0x4c,0x4f,0x4e,0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,
  0x5b,0x5a,0x59,0x58,0x5f,0x5e,0x5d,0x5c,0x52,0x53,0x50,0x51,0x56,0x57,0x54,0x55,
  0x6d,0x6c,0x6f,0x6e,0x69,0x68,0x6b,0x6a,0x64,0x65,0x66,0x67,0x60,0x61,0x62,0x63,
  0x7f,0x7e,0x7d,0x7c,0x7b,0x7a,0x79,0x78,0x76,0x77,0x74,0x75,0x72,0x73,0x70,0x71,
  0x92,0x93,0x90,0x91,0x96,0x97,0x94,0x95,0x9b,0x9a,0x99,0x98,0x9f,0x9e,0x9d,0x9c,
  0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x89,0x88,0x8b,0x8a,0x8d,0x8c,0x8f,0x8e,
  0xb6,0xb7,0xb4,0xb5,0xb2,0xb3,0xb0,0xb1,0xbf,0xbe,0xbd,0xbc,0xbb,0xba,0xb9,0xb8,
  0xa4,0xa5,0xa6,0xa7,0xa0,0xa1,0xa2,0xa3,0xad,0xac,0xaf,0xae,0xa9,0xa8,0xab,0xaa,
  0xdb,0xda,0xd9,0xd8,0xdf,0xde,0xdd,0xdc,0xd2,0xd3,0xd0,0xd1,0xd6,0xd7,0xd4,0xd5,
  0xc9,0xc8,0xcb,0xca,0xcd,0xcc,0xcf,0xce,0xc0,0xc1,0xc2,0xc3,0xc4,0xc5,0xc6,0xc7,
  0xff,0xfe,0xfd,0xfc,0xfb,0xfa,0xf9,0xf8,0xf6,0xf7,0xf4,0xf5,0xf2,0xf3,0xf0,0xf1,
  0xed,0xec,0xef,0xee,0xe9,0xe8,0xeb,0xea,0xe4,0xe5,0xe6,0xe7,0xe0,0xe1,0xe2,0xe3
};

unsigned char CSStab3[512]=
{
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,
  0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff,0x00,0x24,0x49,0x6d,0x92,0xb6,0xdb,0xff
};

unsigned char CSStab4[256]=
{
  0x00,0x80,0x40,0xc0,0x20,0xa0,0x60,0xe0,0x10,0x90,0x50,0xd0,0x30,0xb0,0x70,0xf0,
  0x08,0x88,0x48,0xc8,0x28,0xa8,0x68,0xe8,0x18,0x98,0x58,0xd8,0x38,0xb8,0x78,0xf8,
  0x04,0x84,0x44,0xc4,0x24,0xa4,0x64,0xe4,0x14,0x94,0x54,0xd4,0x34,0xb4,0x74,0xf4,
  0x0c,0x8c,0x4c,0xcc,0x2c,0xac,0x6c,0xec,0x1c,0x9c,0x5c,0xdc,0x3c,0xbc,0x7c,0xfc,
  0x02,0x82,0x42,0xc2,0x22,0xa2,0x62,0xe2,0x12,0x92,0x52,0xd2,0x32,0xb2,0x72,0xf2,
  0x0a,0x8a,0x4a,0xca,0x2a,0xaa,0x6a,0xea,0x1a,0x9a,0x5a,0xda,0x3a,0xba,0x7a,0xfa,
  0x06,0x86,0x46,0xc6,0x26,0xa6,0x66,0xe6,0x16,0x96,0x56,0xd6,0x36,0xb6,0x76,0xf6,
  0x0e,0x8e,0x4e,0xce,0x2e,0xae,0x6e,0xee,0x1e,0x9e,0x5e,0xde,0x3e,0xbe,0x7e,0xfe,
  0x01,0x81,0x41,0xc1,0x21,0xa1,0x61,0xe1,0x11,0x91,0x51,0xd1,0x31,0xb1,0x71,0xf1,
  0x09,0x89,0x49,0xc9,0x29,0xa9,0x69,0xe9,0x19,0x99,0x59,0xd9,0x39,0xb9,0x79,0xf9,
  0x05,0x85,0x45,0xc5,0x25,0xa5,0x65,0xe5,0x15,0x95,0x55,0xd5,0x35,0xb5,0x75,0xf5,
  0x0d,0x8d,0x4d,0xcd,0x2d,0xad,0x6d,0xed,0x1d,0x9d,0x5d,0xdd,0x3d,0xbd,0x7d,0xfd,
  0x03,0x83,0x43,0xc3,0x23,0xa3,0x63,0xe3,0x13,0x93,0x53,0xd3,0x33,0xb3,0x73,0xf3,
  0x0b,0x8b,0x4b,0xcb,0x2b,0xab,0x6b,0xeb,0x1b,0x9b,0x5b,0xdb,0x3b,0xbb,0x7b,0xfb,
  0x07,0x87,0x47,0xc7,0x27,0xa7,0x67,0xe7,0x17,0x97,0x57,0xd7,0x37,0xb7,0x77,0xf7,
  0x0f,0x8f,0x4f,0xcf,0x2f,0xaf,0x6f,0xef,0x1f,0x9f,0x5f,0xdf,0x3f,0xbf,0x7f,0xff
};

unsigned char CSStab5[256]=
{
  0xff,0x7f,0xbf,0x3f,0xdf,0x5f,0x9f,0x1f,0xef,0x6f,0xaf,0x2f,0xcf,0x4f,0x8f,0x0f,
  0xf7,0x77,0xb7,0x37,0xd7,0x57,0x97,0x17,0xe7,0x67,0xa7,0x27,0xc7,0x47,0x87,0x07,
  0xfb,0x7b,0xbb,0x3b,0xdb,0x5b,0x9b,0x1b,0xeb,0x6b,0xab,0x2b,0xcb,0x4b,0x8b,0x0b,
  0xf3,0x73,0xb3,0x33,0xd3,0x53,0x93,0x13,0xe3,0x63,0xa3,0x23,0xc3,0x43,0x83,0x03,
  0xfd,0x7d,0xbd,0x3d,0xdd,0x5d,0x9d,0x1d,0xed,0x6d,0xad,0x2d,0xcd,0x4d,0x8d,0x0d,
  0xf5,0x75,0xb5,0x35,0xd5,0x55,0x95,0x15,0xe5,0x65,0xa5,0x25,0xc5,0x45,0x85,0x05,
  0xf9,0x79,0xb9,0x39,0xd9,0x59,0x99,0x19,0xe9,0x69,0xa9,0x29,0xc9,0x49,0x89,0x09,
  0xf1,0x71,0xb1,0x31,0xd1,0x51,0x91,0x11,0xe1,0x61,0xa1,0x21,0xc1,0x41,0x81,0x01,
  0xfe,0x7e,0xbe,0x3e,0xde,0x5e,0x9e,0x1e,0xee,0x6e,0xae,0x2e,0xce,0x4e,0x8e,0x0e,
  0xf6,0x76,0xb6,0x36,0xd6,0x56,0x96,0x16,0xe6,0x66,0xa6,0x26,0xc6,0x46,0x86,0x06,
  0xfa,0x7a,0xba,0x3a,0xda,0x5a,0x9a,0x1a,0xea,0x6a,0xaa,0x2a,0xca,0x4a,0x8a,0x0a,
  0xf2,0x72,0xb2,0x32,0xd2,0x52,0x92,0x12,0xe2,0x62,0xa2,0x22,0xc2,0x42,0x82,0x02,
  0xfc,0x7c,0xbc,0x3c,0xdc,0x5c,0x9c,0x1c,0xec,0x6c,0xac,0x2c,0xcc,0x4c,0x8c,0x0c,
  0xf4,0x74,0xb4,0x34,0xd4,0x54,0x94,0x14,0xe4,0x64,0xa4,0x24,0xc4,0x44,0x84,0x04,
  0xf8,0x78,0xb8,0x38,0xd8,0x58,0x98,0x18,0xe8,0x68,0xa8,0x28,0xc8,0x48,0x88,0x08,
  0xf0,0x70,0xb0,0x30,0xd0,0x50,0x90,0x10,0xe0,0x60,0xa0,0x20,0xc0,0x40,0x80,0x00
};

void CSSdescramble(unsigned char *sec,unsigned char *key)
{
  unsigned int t1,t2,t3,t4,t5,t6;
  unsigned char *end=sec+0x800;

  t1=key[0]^sec[0x54]|0x100;
  t2=key[1]^sec[0x55];
  t3=(*((unsigned int *)(key+2)))^(*((unsigned int *)(sec+0x56)));
  t4=t3&7;
  t3=t3*2+8-t4;
  sec+=0x80;
  t5=0;
  while(sec!=end)
    {
      t4=CSStab2[t2]^CSStab3[t1];
      t2=t1>>1;
      t1=((t1&1)<<8)^t4;
      t4=CSStab5[t4];
      t6=(((((((t3>>3)^t3)>>1)^t3)>>8)^t3)>>5)&0xff;
      t3=(t3<<8)|t6;
      t6=CSStab4[t6];
      t5+=t6+t4;
      *sec++=CSStab1[*sec]^(t5&0xff);
      t5>>=8;
    }
}

void CSStitlekey1(unsigned char *key,unsigned char *im)
{
  unsigned int t1,t2,t3,t4,t5,t6;
  unsigned char k[5];
  int i;

  t1=im[0]|0x100;
  t2=im[1];
  t3=*((unsigned int *)(im+2));
  t4=t3&7;

  t3=t3*2+8-t4;
  t5=0;

  for(i=0;i<5;i++)
    {
      t4=CSStab2[t2]^CSStab3[t1];
      t2=t1>>1;
      t1=((t1&1)<<8)^t4;
      t4=CSStab4[t4];
      t6=(((((((t3>>3)^t3)>>1)^t3)>>8)^t3)>>5)&0xff;
      t3=(t3<<8)|t6;
      t6=CSStab4[t6];
      t5+=t6+t4;
      /*
      printf("i=%d: [%08X %08X %08X %08X %08X %08X]\n",
	     i, t1, t2, t3, t4, t5, t6);
      */
      k[i]=t5&0xff;
      t5>>=8;
    }
  /*
  printf("k: [ %02X %02X %02X %02X %02X ]\n", k[0], k[1], k[2], k[3], k[4]);
  */
  for(i=9;i>=0;i--)
    key[CSStab0[i+1]]=k[CSStab0[i+1]]^CSStab1[key[CSStab0[i+1]]]^key[CSStab0[i]];
}

void CSStitlekey2(unsigned char *key,unsigned char *im)
{
  unsigned int t1,t2,t3,t4,t5,t6;
  unsigned char k[5];
  int i;

  t1=im[0]|0x100;
  t2=im[1];
  t3=*((unsigned int *)(im+2));
  t4=t3&7;
  t3=t3*2+8-t4;
  t5=0;
  for(i=0;i<5;i++)
    {
      t4=CSStab2[t2]^CSStab3[t1];
      t2=t1>>1;
      t1=((t1&1)<<8)^t4;
      t4=CSStab4[t4];
      t6=(((((((t3>>3)^t3)>>1)^t3)>>8)^t3)>>5)&0xff;
      t3=(t3<<8)|t6;
      t6=CSStab5[t6];
      t5+=t6+t4;
      k[i]=t5&0xff;
      t5>>=8;
    }
  for(i=9;i>=0;i--)
    key[CSStab0[i+1]]=k[CSStab0[i+1]]^CSStab1[key[CSStab0[i+1]]]^key[CSStab0[i]];
}

void CSSdecrypttitlekey(unsigned char *tkey,unsigned char *dkey)
{
  int i;
  unsigned char im1[6];
  unsigned char im2[6]={0x51,0x67,0x67,0xc5,0xe0,0x00};
	
  for(i=0;i<6;i++)
    im1[i]=dkey[i];

  CSStitlekey1(im1,im2);
  CSStitlekey2(tkey,im1);
}

unsigned char getbyte() {
  int a;
  unsigned char c;
  for(;;) {
    switch (a = getc(stdin)) {
    case EOF:
      exit(0);
    case ' ':
    case '\n':
    case '\r':
    case '\t':
      continue;
    default:
      if (a >= '0' && a <= '9') c = a - '0';
      else c = 10 + ((a|32) - 'a');
      if ((a = getc(stdin)) == EOF) exit(-1);
      c<<=4;
      if (a >= '0' && a <= '9') c |= a - '0';
      else c |= 10 + ((a|32) - 'a');
      return c;
    }
  }
}

int main () {
  int i;
  unsigned char key1[6];
  unsigned char key2[6];
  unsigned char key3[6];

  unsigned char sector[2048];

  for(;;) {
    for(i=0;i<6;i++)
      key1[i] = getbyte();
    for(i=0;i<6;i++)
      key2[i] = getbyte();
    for(i=0;i<6;i++)
      key3[i] = getbyte();
    
    for(i=0;i<2048;i++)
      sector[i] = getbyte();

    CSStitlekey1(key2, key1);
    CSStitlekey2(key3, key2);

    CSSdescramble(sector, key3);

    for(i=0;i<2048;i++) {
      printf("%02X ", sector[i]);
    }
  }

  printf("\n");

  return 0;
}

