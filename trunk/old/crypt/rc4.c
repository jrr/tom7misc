#include <stdio.h>
#define s(b) ];m=S[b];S[b]=S[j];S[j]=m;
#define F for(j=x=0;x<256;x++)

unsigned char S[256],i,j,m;main(int x,char**v){int z;if(x^2){printf("%s key\n",*
v);}else{v++;z=strlen(*v);F S[x]=x;F{j+=S[x]+(*v)[x%z s(x)}for(j=i=0;EOF!=(x=
getchar());){j+=S[++i s(i)putchar((S[i]+S[j])^x);}}}
