/* pvm.c */
/****************************************************************************/
/*
**        P  V  M . C
**
**
**  Tiedosto sis„lt„„ p„iv„m„„r„n k„sittelyyn liittyvien aliohjelmien
**  tarvitsemat yleiset aliohjelmat.
**
**  Tekij„t:          Vesa Lappalainen
**                    Ohjelmointikurssi 1991
**  Tehty:            29.10.1991
**  Muutettu:         22.11.1991/vl
**  Mit„ muutettu:    muutettu aliohjelmakirjastoksi
**
*****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include "pvm.h"

typedef int KK_tyyppi[12];


/****************************************************************************/
/* Globaalit 'vakio'-tyyliset taulukot:                                     */

char *VIIKON_PAIVAT[]={
  "sunnuntai",
  "maanantai",
  "tiistai",
  "keskiviikko",
  "torstai",
  "perjantai",
  "lauantai"
};


char *KUUKAUDET[]={
  "",                /* Tyhj„ jono paikkaan 0, jotta saadaan tammikuu 1:ksi */
  "tammikuu",
  "helmikuu",
  "maaliskuu",
  "huhtikuu",
  "toukokuu",
  "kes„kuu",
  "hein„kuu",
  "elokuu",
  "syyskuu",
  "lokakuu",
  "marraskuu",
  "joulukuu"
};

/* P„ivi„ kuukaudessa kun ei ole karkausvuosi :                             */
                       /*  1.  2.  3.  4.  5.  6.  7.  8.  9. 10. 11. 12    */
static KK_tyyppi PV_KK = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}; 

/* Kuukauden 1. p„ivien j„rj. numerot (ei kv)                               */
static KK_tyyppi KK_1  = { 1, 32, 60, 91,121,152,182,213,244,274,305,335};  

  
/****************************************************************************/
int                       /* 0 = ei karkausvuosi                            */
karkausvuosi(             /* 1 = karkausvuosi                               */
  int vuosi               /* s   Vuosi jota tutkitaan.                      */
)
/* 
** Funktiolla palautetaan onko vuosi karkausvuosi vai ei.
**
** Algoritmi: 4:ll„ jaolliset on, paitsi t„ydet vuosisadat, kuitenkin
**            4:ll„ jaolliset vuosisadat on
** Esimerkki: 1991 -> 0,  1992 -> 1, 1900 -> 0, 2000 -> 1
----------------------------------------------------------------------------*/
{
  if ( vuosi % 100 ) return (vuosi % 4 ) == 0;
  return ( vuosi % 400 ) == 0;
}


/****************************************************************************/
void                      /*                                                */
tayta_kk_1(               /*                                                */
  int kk_1[]              /* t   Taul. johon tulee kk. 1. p„ivien j„rj.nro. */
)
/* 
** Aliohjelmalla t„ytet„„n taulukko, josta selvi„„ kunkin kuukauden 1. p„iv„n
** j„rjestysnumero vuoden alusta laskien.  T„ytt” ei kark.vuoden mukaan.
**
** Globaalit: PV_KK (g)         - taulukko jossa on kuukausien pituudet
** Algoritmi: Ynn„t„„n kuukausien pituudet.
** Esimerkki: 1990 -> 1 32 60 91 ...
----------------------------------------------------------------------------*/
{
  int i;
  kk_1[0] = 1;                                        /* Tammikuu  */
  for (i=1; i<12; i++)                                /* Loput     */
    kk_1[i] = kk_1[i-1] + PV_KK[i-1];
}


/****************************************************************************/
void                /*                                                      */
tulosta_pvm(        /*                                                      */
  Pvm_tyyppi *pvm   /* s   Tulostettava p„iv„m„„r„                          */
)
/* 
** Aliohjelmalla tulostetaan p„iv„m„„r„ muodossa 5.11.1991
**
** Tulostus:  N„yt”lle
** Esimerkki: 5.11.1991
----------------------------------------------------------------------------*/
{
  printf("%d.%d.%d",pvm->pv,pvm->kk,pvm->vv);
}


/****************************************************************************/
int                       /*                                                */
selvita_1(                /* = vuoden 1. p„iv„n nro su=0, ma=1,...la=6      */
  int vuosi               /* s   Tutkittava vuosi.                          */
)
/* 
** Funktiolla palautetaan vuoden 1. p„iv„n j„rjestysnumero
**
** Algoritmi: Tunnetaan 1.1.0001 = ma (Huom! Ei ole historiallisesti oikein!)
**            Lasketaan vuosilukujen ero.
**            Jokainen vuosi siirt„„ alkup„iv„„ yhdell„ eteenp„in,
**            paitsi karkausvuodet.
----------------------------------------------------------------------------*/
{
  int ero=vuosi-1;
  return (int) ( (ero+(ero/4)-(ero/100)+(ero/400) + 1 ) % 7 );
}


/****************************************************************************/
int                       /* 0 = pvm OK                                     */
tarkista_pvm(             /* 1 = kk liian pieni                             */
                          /* 2 = kk liian suuri                             */
                          /* 3 = pv liian pieni                             */
                          /* 4 = pv liian suuri                             */
  Pvm_tyyppi *pvm         /* s   Tutkittava p„iv„m„„r„                      */  
)
/* 
** Funktiolla tutkitaan onko annettu p„iv„m„„r„ oikein.
**
** Globaalit: PV_KK (g taulukko)
** Algoritmi: Kuukausien pituudet katsotaan taulukosta PV_KK.
**            Karkausvuoden helmikuussa pit„„ muistaa lis„t„ yksi.
----------------------------------------------------------------------------*/
{

  if ( pvm->kk < 1 )  return 1;
  if ( 12 < pvm->kk ) return 2;
  if ( pvm->pv<1 )    return 3;
  if ( (PV_KK[pvm->kk-1]
        + (karkausvuosi(pvm->vv) && (pvm->kk==2) ) /* +1 kv:n helmikuussa   */
       ) < pvm->pv )  return 4;
  return 0;
}


/****************************************************************************/
int                       /*                                                */
paivan_numero(            /* =   p„iv„n numero vuoden alusta laskien.       */
  Pvm_tyyppi *pvm         /* s   Tutkittava p„iv„m„„r„                      */
)
/* 
** Funktiolla palautetaan p„iv„n numero kun 1.1 = 1 1.2 = 32 jne.
**
** Globaalit: KK_1 (g, taulukko)
** Algoritmi: Katsotaan kk:n 1. p„iv„n numero taulukosta ja
**            karkausvuonna pit„„ maaliskuusta alkaen lis„t„ yksi.
----------------------------------------------------------------------------*/
{
  return KK_1[pvm->kk-1]
         + ( karkausvuosi(pvm->vv)&&(pvm->kk>2) )
         + (pvm->pv-1);
}


/****************************************************************************/
int                       /*                                                */
muuta_viikoksi(           /* =   viikon numero                              */
  Pvm_tyyppi *pvm         /* s,t Tutkittava p„iv„m„„r„                      */
)
/* 
** Funktiolla muutetaan p„iv„m„„r„ viikonp„iv„ksi ja viikon numeroksi
** sek„ p„iv„n numeroksi.
** Virheellisest„ p„iv„m„„r„st„ palautetaan -1 ja pvm j„tet„„n 
** koskemattomaksi.
**
** Kutsuu:    tarkista_pvm
**            paivan_numero               
**            selvita_1
** Algoritmi: Viikon numero on 0, jos viikko alkaa pe, la tai su tammikuun
**            alussa.  Muuten viikon numero saadaan selville jakamalla 
**            p„iv„n juokseva j„rjestysnumero 7:ll„, kunhan muistetaan
**            siirt„„ vuoden alkua sen mukaan, mill„ p„iv„ll„ vuosi alkaa.
**            Jos vuosi alkaa                         p„iv„n numerosta
**             ma, 1.1 on vkolla 1 ja ma 8.1 vkolla 2   7 -> 0+1  8 -> 1+1
**             ti, 1.1           1 ja ma 7.1 vkolla 2   6 -> 0+1  7 -> 1+1
**             ke, 1.1           1 ja ma 6.1 vkolla 2   5 -> 0+1  5 -> 1+1
**             to, 1.1           1 ja ma 5.1 vkolla 2   4 -> 0+1  4 -> 1+1
**             pe, 1.1           0 ja ma 4.1 vkolla 1   3 -> 0+0  3 -> 1+0
**             la, 1.1           0 ja ma 3.1 vkolla 1   2 -> 0+0  2 -> 1+0
**             su, 1.1           0 ja ma 2.1 vkolla 1   1 -> 0+0  1 -> 1+0 
----------------------------------------------------------------------------*/
{
  int pv_nro,vv_1;

  if ( tarkista_pvm(pvm) ) return -1;
  pv_nro = paivan_numero(pvm);
  vv_1 = selvita_1(pvm->vv);
  if (vv_1==0) vv_1=7;                /* Muutetaan su = 7 */

  pvm->vko = ( pv_nro-1 + vv_1-1 )/7 + (1<=vv_1 && vv_1<=4);

  pvm->viikon_paiva = (pv_nro-1+vv_1)%7;
  pvm->pv_nro = pv_nro;

  return pvm->vko;
}


/****************************************************************************/
int                       /*                                                */
muuta_pvmksi(             /* =   paivan numero vuoden alusta                */
  Pvm_tyyppi *pvm         /* s,t Tutkittava p„iv„m„„r„                      */
)
/* 
** Funktiolla muutetaan p„iv„n„,viikkona ja vuotena annettu p„iv„ys muotoon   
** pp kk vv ja pv_nro
** Virheellisest„ arvoista palautetaan -1 ja pvm j„tet„„n 
** koskemattomaksi.
**
** Globaalit: PV_KK (g, taulukko)
** Kutsuu:    tarkista_pvm
**            paivan_numero               
**            selvita_1
** Algoritmi: Viikon numero on 0, jos viikko alkaa pe, la tai su tammikuun
**            alussa.  
**            Jos vuosi alkaa                         p„iv„n numerosta
**             ma, 1.1 on vkolla 1 ja ma 8.1 vkolla 2   7 -> 0+1  8 -> 1+1
**             ti, 1.1           1 ja ma 7.1 vkolla 2   6 -> 0+1  7 -> 1+1
**             ke, 1.1           1 ja ma 6.1 vkolla 2   5 -> 0+1  5 -> 1+1
**             to, 1.1           1 ja ma 5.1 vkolla 2   4 -> 0+1  4 -> 1+1
**             pe, 1.1           0 ja ma 4.1 vkolla 1   3 -> 0+0  3 -> 1+0
**             la, 1.1           0 ja ma 3.1 vkolla 1   2 -> 0+0  2 -> 1+0
**             su, 1.1           0 ja ma 2.1 vkolla 1   1 -> 0+0  1 -> 1+0 
**            Kun p„iv„n j„rj. numero on saatu selville, etsit„„n
**            sit„ kuukautta, jossa ollaan.
----------------------------------------------------------------------------*/
{
  int pv_nro,vko,vv_1,paiva,kv,kk,pv,pv_kk;

  if ( (pvm->vko<0)          ||
       (pvm->vko>53)         ||
       (pvm->viikon_paiva<0) ||
       (pvm->viikon_paiva>6) ) return -1;
    
  vv_1 = selvita_1(pvm->vv);
  if (vv_1==0) vv_1=7;                /* Muutetaan su = 7 */
  paiva = pvm->viikon_paiva;
  if (paiva==0) paiva=7;

  vko = pvm->vko - (1<=vv_1 && vv_1<=4);

  pv_nro = 7*vko - vv_1 + paiva + 1;
  if (pv_nro<0) return -1;

  kv = karkausvuosi(pvm->vv);
  if ( pv_nro-kv>366 ) return -1;

  for (pv=pv_nro,kk=0; kk<12; kk++) {
    pv_kk=PV_KK[kk]+(kv&&(kk==1));
    if ( pv <= pv_kk ) break;
    pv -=pv_kk;
  }

/* Tai:
  for (pv=pv_nro,kk=0; kk<12 && pv>(pv_kk=PV_KK[kk]+(kv&&kk==1)); kk++) 
    pv -=pv_kk;
*/
          
  pvm->pv = pv;
  pvm->kk = kk+1;
  pvm->pv_nro = pv_nro;

  return pv_nro;
}


/****************************************************************************/
void                      /*                                                */
selvita_nyky_pvm(         /*                                                */
  Pvm_tyyppi *nyky        /* t   T„m„n p„iv„n p„iv„m„„r„                    */
)
/* 
** Aliohjelmalla selvitet„„n t„m„n p„iv„n p„iv„m„„r„.
**
** Kutsuu:    <time.h>
** Tekij„:    Vesa Lappalainen
** Pvm:       5.11.1991
----------------------------------------------------------------------------*/
{
  time_t timer;
  struct tm *tblock;

  /* gets time of day  */
  timer = time(NULL);

  /* converts date/time to a structure */
  tblock = localtime(&timer);

  nyky->pv = tblock->tm_mday;
  nyky->kk = tblock->tm_mon+1;
  nyky->vv = tblock->tm_year+1900;
  muuta_viikoksi(nyky);
}


/****************************************************************************/
int                       /*                                                */
kysy_pvm(                 /* =   moneenko kentt„„n vastattiin.              */
  Pvm_tyyppi *pvm,        /* t   kysytty p„iv„m„„r„                         */
  Pvm_tyyppi *oletus      /* s   oletusarvot, mik„li kentt„„ ei anneta      */
)
/* 
** Funktiolla kysyt„„n p„iv„m„„r„.  Mik„li kaikkiin p„iv„m„„r„n kenttiin ei
** vastata, k„ytet„„n niihin oletusrvoja.  Jos p„iv„m„„r„ on virheellinen,
** palautetaan -1.
**
** Sy”tt”:    P„„tteelt„
** Tulostus:  N„ytt”lle
** Kutsuu:    muuta_viikoksi
** Tulostus:  N„ytt””n
** Esimerkki: oletus 5.11.1991, sy”tt” 12        -> 12.11.1991
**                                     3.4       ->  3.4.1991 
**                                     2.3.1841  ->  2.3.1841
----------------------------------------------------------------------------*/
{
  int palautus;
  *pvm = *oletus;
  printf("Anna p„iv„m„„r„ muodossa pp.kk.vvvv >");
  fflush(stdin);
  palautus = scanf("%d.%d.%d",&pvm->pv,&pvm->kk,&pvm->vv);
  if ( muuta_viikoksi(pvm)<0 ) return -1;
  return palautus;
}


/****************************************************************************/
int                       /* 0 = sotun p„iv„m„„r„ oikein                    */
tarkista_sotu_pvm(        /* 1 = sotun p„iv„m„„r„ v„„rin                    */
  const char *sotu        /* s   Tutkittava sotu                            */
)    
/* 
** Funktiolla tarkistetaan onko annetussa sotussa p„iv„m„„r„ oikein.
**
----------------------------------------------------------------------------*/
{
  Pvm_tyyppi synt;
  if ( strlen(sotu)<6 ) return 1;
  if ( sscanf(sotu,"%2d%2d%2d",&synt.pv,&synt.kk,&synt.vv) != 3 ) return 1;
  synt.vv += 1900;
  return tarkista_pvm(&synt);
}




