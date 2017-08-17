/* Testi.c */
/* Testaataan mjonot.c:n aliohjelmia */

#include <stdio.h>
#include "mjonot.h"

typedef struct {
 char *s;
 double d;
} tArvos;

tArvos atest[] = {
 {  "5"   , 5.00 },
 {  "5-"  , 4.75 },
 {  "5«"  , 5.5 },
 {  "5+"  , 5.25 },
 {  "5.25", 5.25 },
 {  ""    , 0.00 },
 {  "hyl" , 0.00 },
 {  "kiit", 0.00 },
 {  "5++" , 5.5  },
 {  "+"   , 0.25 },
 {  "«"   , 0.5  },
 {  "-"   , -0.25 },
 {  "-+"  , -0.25 },
 {  "-«"  , -0.5  },
 {  "-2«" , -2.5  },
 {  "-3-" , -2.75 },
 {  NULL  , 0     }
};


int main(void)
{
#if 0
  char jono[80] = "     kIssa,     ,iStuu, ";

  poista_2_tyhjat(jono);

  printf("|%s|\n",jono);
#endif

#if 1
  char *p;
  double d;
  int i;

  for (i=0; atest[i].s; i++) {
    d = jono_arvosanaksi(atest[i].s);
    p = arvosana_jonoksi(d,NULL);
    printf("'%-7s' -> %6.2lf (%5.2lf) -> '%s'\n",atest[i].s,d,d-atest[i].d,p);
  }

#endif

  return 0;
}