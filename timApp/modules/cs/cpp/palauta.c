#include <stdio.h>
#include <stdlib.h>
int main(int argc,char *argv[])
{
  int i,ret=0;
  if ( argc > 1 ) ret = atoi(argv[1]);
  for ( i=0; i<argc; i++)
    printf("%2d = %s\n",i,argv[i]);
  return ret;
}