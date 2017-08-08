/****************************************************************************/
/*
**        L Y H E N N E . C
**
** Aliohjelmia lyhenteiden (glossary) k„sittelyyn.
**
** Aliohjelmat:
**    lue_lyhenne_jono       - lukee jonon p„„tteelt„ ja ottaa lyhenteet
**                             huomioon.
**    kasittele_maaritys     - ottaa lyhenteen jonosta
**    poista_lyhenne         - poistaa lyhenteen nimen perusteella
**    korvaa_kaikki          - korvaa jonon kaikki merkit lyhenteella.
**    lyhenne_help           - tulostaa lyhenteet
**    alusta lyhenteet       - lukee lyhenteet levylt„
**    talleta_lyhenteet      - tallettaa lyhenteet levylle
**    lue_lyhenne_jono       - varsinainen lukemisrutiini
**
**
**  Tekij„t:          Vesa Lappalainen
**  Tehty:            29.1.1994
**  Muutettu:
**  Mit„ muutettu:
**
**  K„ytt”:
**    Projektiin tarvitaan t„m„n lis„ksi mjonot.c
**    Yksinkertaisimmillaan ohjelmaan lis„t„„n vain kutsu
**      lue_lyhenne_jono(N_S(s));
**
**    Jos lyhenteet halutaan tallettaa ja alustaa, kutsutaan
**      alusta_lyhenteet("oma.glo");  // ohjelman alussa
**      ...
**      talleta_lyhenteet("oma.glo"); // ohjelman lopussa
**
**  Erikoimerkit (n„m„ voidaan muuttaa lyhennetiedoston 1. rivill„):
**    \{    - lyhenteen aloitus (jos puuttuu niin oletetaan jonon alkuun)
**    \}    - lyhenteen lopetus
**    \/    - lyhennenimen lopetus (jos puuttuu niin oletetaan jonon loppuun)
**    \?    - antaa avustusta eli listaa lyhenteet
**            jos t„m„ on rivill„, suoritetaan lukemisrivi aina uudelleen
**            joten t„m„n avulla voi tarkistaa mik„ tulisi sy”t”ksi.
**
**  Esimerkikki:
**  1. Jos tarvitsee sy”tt„„ useita jonoja joissa sama osa
**     esiintyy usein
**       26.1/VL-
**       27.1/VL+
**       jne..
**
**     Nyt voidaan ensimm„isen (tai jonkin muun) sy”t”n kohdalla luoda
**     uusi lyhenne (tai kumota vanha) sy”tt„m„ll„
**       26\{.1/VL\}#\/-
**     T„ll”in merkkijono # (esimerkiss„ yksi merkki) korvautuu seuraavasti
**       "#"   =>  ".1/vl"
**     Samalla varsinaiseksi sy”t”ksi menee
**       26.1/VL-
**     Nyt esim. sy”t”t korvautuvat seuraavasti:
**       27#-     =>  27.1/VL-
**       hyv 29#  =>  hyv 29.1/VL
**
**  2. Jos mallilyhenne on koko rivi, voidaan lyhenne luoda helpommin.
**     Jos esim. sy”tet„„n:
**       Matematiikka\}#m
**     saadaan lyhenne "#m" joka korvautuu sanalla "Matematiikka"
**     esimerkiksi sy”t”st„
**       #m on kivaa!
**     tulisi sy”tt”
**       Matematiikka on kivaa                                                -
**  3. K„yt„nn”ss„ lyhenteet pidet„„n pisin-ensin -j„rjestyksess„, jotta
**     samanaikaisesti voi olla k„yt”ss„ sek„ lyhenne # ett„ #m
**     ja jos #m l”ytyy, k„ytet„„n sit„.
**  4. Lyhenne poistuu antamalla lyhenteeksi tyhj„ jono.
**     Esimerkiksi edellinen Matematiikka voidaan poistaa sy”t”ll„
**      \{\}#m\/                                                              -
**  5. Lyhenteiden erikoismerkit voidaan muuttaa ohjelman tekem„„
**     lyhennetiedostoa muuttamalla
**  6. Jos sy”tt” halutaan tarkistaa, voidaan riville laittaa
**     lyhenteiden avustus:
**       #m on kivaa\?
**     =>
**       lista lyhenteist„ ja
**       (Matematiikka on kivaa) >
**     ja suluissa oleva arvo voidaan hyv„ksy„ pelk„„ RETi„ painamalla.
**
** Lyhennetiedoston muoto:
** -----------------------
**
**  1) | { | } | / | \? |
**  2) ---------------------------------------------------------------
**  3) kalle}k1
**  4) julle}j1
**  5) ---------------------------------------------------------------
**
**   1) erikoismerkit | alku | loppu | lyhenteen loppu | avustus |
**      Jos t„m„ rivi kirjoitetaan ennen ohjelman k„ytt”„, voidaan
**      erikoismerkit laittaa miksi halutaan.
**   2) ohitettava viiva, voidaan j„tt„„ pois
**   3) 1. lyhennettett„v„ asia ja mill„ se lyhennet„„n
**   4) 2.  - " -
**   5) ohitettava viiva, voidaan j„tt„„ pois
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mjonot.h"
#include "lyhenne.h"

#define LYHENNE_ALKU  "\\{"
#define LYHENNE_LOPPU "\\}"
#define LYHENNE_NIMI  "\\/"
#define LYHENNE_HELP  "\\?"

typedef struct {
  char alku[10];
  char loppu[10];
  char nimi[10];
  char help[10];
  char erotin[2];
  char merkit[10];
  int muuttunut;
} lyhenne_jonot_tyyppi;


static lyhenne_jonot_tyyppi lyhenne = {
 LYHENNE_ALKU,LYHENNE_LOPPU,LYHENNE_NIMI,LYHENNE_HELP,"|","\\",
 0
};

typedef struct {
  char lyhenne[10];
  char korvaus[80];
} lyhenne_tyyppi;


#define MAX_LYHENTEITA 20

static lyhenne_tyyppi lyhenteet[MAX_LYHENTEITA+1];

/****************************************************************************/
int lyhenne_merkkeja(const char *s)
{
  return strcspn(s,lyhenne.merkit) < strlen(s);
}

/****************************************************************************/
static int vertaa_lyhenne(const void *a, const void *b)
{
  int la = strlen( ((lyhenne_tyyppi *)a)->lyhenne );
  int lb = strlen( ((lyhenne_tyyppi *)b)->lyhenne );
  return lb - la;
}


/****************************************************************************/
static void lajittele_lyhenteet(void)
{
  qsort( (void *)lyhenteet,MAX_LYHENTEITA,
         sizeof(lyhenne_tyyppi),vertaa_lyhenne);
  lyhenne.muuttunut = 1;
}


/****************************************************************************/
static int etsi_lyhenne(const char *s)
{
  int i;
  for (i=0; lyhenteet[i].lyhenne[0]; i++)
    if ( strcmp(lyhenteet[i].lyhenne,s) == 0 ) return i;
  return -i;
}


/****************************************************************************/
static int lisaa_lyhenne(const char *s,const char *k)
{
  int i = etsi_lyhenne(s);
  if ( i < 0 ) {
    i = -i;
    if ( i >= MAX_LYHENTEITA ) return 1;
  }
  kopioi_jono(N_S(lyhenteet[i].lyhenne),s);
  kopioi_jono(N_S(lyhenteet[i].korvaus),k);
  if ( lyhenteet[i].korvaus[0] == 0 ) lyhenteet[i].lyhenne[0] = 0;
  lajittele_lyhenteet();
  return 0;
}


/****************************************************************************/
int poista_lyhenne(const char *s)
{
  int i = etsi_lyhenne(s);
  if ( i < 0 ) return 1;
  lyhenteet[i].lyhenne[0] = 0;
  lajittele_lyhenteet();
  return 0;
}


/****************************************************************************/
int kasittele_maaritys(char *s)
{
  char *pa,*pl,*pn;
  int n = 0;
  int lya = strlen(lyhenne.alku);
  int lyl = strlen(lyhenne.loppu);
  int lyn = strlen(lyhenne.nimi);
  char ent_n;

  while (1) {
    pa = strstr(s,lyhenne.alku);
    pl = strstr(s,lyhenne.loppu);
    if ( pa && pl && pa < pl ) { poista_alusta(pa,lya); pl-=lya; }
    else pa = s;
    if ( pl == NULL ) return n;
    n++;
    if ( ( pn = strstr(pl,lyhenne.nimi) ) == NULL )
      if ( ( pn = strstr(pl,lyhenne.alku) ) == NULL )
        pn = s+strlen(s);


    ent_n = *pn; *pn = 0; *pl = 0;

    if ( lisaa_lyhenne(pl+lyl,pa) ) return -1;

    *pn = ent_n; *pl = 1;

    poista_alusta(pl,(int)(pn-pl+lyn));

  }
}

/****************************************************************************/
int kasittele_maaritys_valmis(char *s)
{
  char *pa,*pl,*pn;
  int n = 0;
  int lya = strlen(lyhenne.alku);
  int lyl = strlen(lyhenne.loppu);
  int lyn = strlen(lyhenne.nimi);
  char ent_n;

  while (1) {
    pa = strstr(s,lyhenne.alku);
    pl = strstr(s,lyhenne.loppu);
    if ( pl ) pn = strstr(pl,lyhenne.nimi);
    if ( pa && pl && pa < pl && pn) { poista_alusta(pa,lya); pl-=lya; }
    else pa = s;
    if ( pl == NULL || pn == NULL ) return n;
    n++;
    if ( ( pn = strstr(pl,lyhenne.nimi) ) == NULL )
      if ( ( pn = strstr(pl,lyhenne.alku) ) == NULL )
        pn = s+strlen(s);


    ent_n = *pn; *pn = 0; *pl = 0;

    if ( lisaa_lyhenne(pl+lyl,pa) ) return -1;

    *pn = ent_n; *pl = 1;

    poista_alusta(pl,(int)(pn-pl+lyn));

  }
}

/****************************************************************************/
static int korvaa_1(char *s,int maxp)
{
  char *p;
  int i,lyl;

  for (i=0; lyhenteet[i].lyhenne[0]; i++)
    if ( ( p = strstr(s,lyhenteet[i].lyhenne) ) != NULL ) {
      lyl = strlen(lyhenne.loppu);
      if ( p-s >= lyl && strncmp(p-lyl,lyhenne.loppu,lyl) == 0 ) continue;
      poista_alusta(p,strlen(lyhenteet[i].lyhenne));
      return lisaa_alkuun(p,(int)(maxp-(p-s)),lyhenteet[i].korvaus);
    }
  return 1;
}


/****************************************************************************/
int korvaa_kaikki(char *s,int maxp)
{
  int i;
  while ( ( i = korvaa_1(s,maxp) ) == 0 );
  if ( i < 0 ) return EI_MAHDU;
  return 0;
}


/****************************************************************************/
void lyhenne_help(void)
{
  int i;
  if ( lyhenteet[0].lyhenne[0] == 0 ) return;

  printf("\n");
  printf("---------------------------------------------------------------\n");
  for (i=0;lyhenteet[i].lyhenne[0]; i++)
    printf("%-10s => %s\n",lyhenteet[i].lyhenne,lyhenteet[i].korvaus);
  printf("---------------------------------------------------------------\n");

}

/****************************************************************************/
int lue_lyhenne_jono(char *s,int maxp)
{
  int paluu;
  int apua;
  int paluuk;
  char *p;
  char apu[80]="";

  lue_jono_tyyppi vanha;
  while (1) {
    vanha = alusta_lue_jono(NULL);
    paluu = lue_jono(s,maxp);
    alusta_lue_jono(vanha);

    if ( paluu < 0 ) return paluu;
    if ( paluu == OLETUS ) {
      kopioi_jono(s,maxp,apu);
      paluu = SYOTTO_OK;
      break;
    }

    apua = 0;
    if ( ( p = strstr(s,lyhenne.help) ) != NULL ) {
      apua = 1;
      poista_alusta(p,strlen(lyhenne.help));
    }

    kasittele_maaritys(s);
    /* paluuk = */ korvaa_kaikki(s,maxp);

    if ( apua == 0 ) break;

    lyhenne_help();
    printf("(%s) >",s);
    kopioi_jono(N_S(apu),s);
  }

  if ( s[0] == 0 ) paluuk = OLETUS; else paluuk = paluu;

  return paluu == EI_MAHDU ? paluu : paluuk;
}

/****************************************************************************/
int kasittele_lyhenne_jono(char *s,int maxp,int montako)
{
  int apua;
  int paluuk;
  char *p;

  apua = 0;
  if ( ( p = strstr(s,lyhenne.help) ) != NULL ) {
    apua = 1;
    poista_alusta(p,strlen(lyhenne.help));
  }

  if ( montako == 1 )
    kasittele_maaritys_valmis(s);
  else
    kasittele_maaritys(s);

  paluuk = korvaa_kaikki(s,maxp);

  if ( apua == 0 ) return paluuk;

  lyhenne_help();

  return paluuk;
}

/****************************************************************************/
int alusta_lyhenteet(const char *nimi)
{
  FILE *f;
  char s[100]="",*p,*e;
  int j;



  f = fopen(nimi,"rt");
  if ( !f ) return 1;

  f_lue_jono(f,N_S(s));

  lyhenne.erotin[0] = s[0]; e = lyhenne.erotin;

  p = poista_tyhjat(palanen(s+1,e,&j));
  if ( p[0] ) kopioi_jono(N_S(lyhenne.alku),p);
  p = poista_tyhjat(palanen(NULL,e,&j));
  if ( p[0] ) kopioi_jono(N_S(lyhenne.loppu),p);
  p = poista_tyhjat(palanen(NULL,e,&j));
  if ( p[0] ) kopioi_jono(N_S(lyhenne.nimi),p);
  p = poista_tyhjat(palanen(NULL,e,&j));
  if ( p[0] ) kopioi_jono(N_S(lyhenne.help),p);

  while ( !feof(f) ) {
    if ( f_lue_jono(f,N_S(s)) < OLETUS ) break;
    if ( kasittele_maaritys(s) < 0 ) break;
  }

  fclose(f);

  lyhenne.muuttunut = 0;

  lyhenne.merkit[0] = lyhenne.alku[0];
  lyhenne.merkit[1] = lyhenne.loppu[0];
/*  lyhenne.merkit[2] = lyhenne.nimi[0]; */
  lyhenne.merkit[2] = lyhenne.help[0];
  lyhenne.merkit[3] = 0;

  return 0;
}

/****************************************************************************/
int talleta_lyhenteet(const char *nimi)
{
  FILE *f;
  int i;
  char format[50] = "| %s | %s | %s | %s |\n";

  if ( lyhenne.muuttunut == 0 ) return 0;

  vaihda_jonot(N_S(format),"|",lyhenne.erotin);

  f = fopen(nimi,"wt");
  if ( !f ) return 1;

  fprintf(f,format,
          lyhenne.alku,lyhenne.loppu,lyhenne.nimi,lyhenne.help);
  fprintf(f,"------------------------------------------------------------\n");
  for (i=0;lyhenteet[i].lyhenne[0]; i++)
    fprintf(f,"%s%s%s\n",lyhenteet[i].korvaus,
                      lyhenne.loppu,
                      lyhenteet[i].lyhenne);
  fprintf(f,"------------------------------------------------------------\n");

  fclose(f);

  return 0;
}


/****************************************************************************/
#ifdef USEMAIN
int main(void)
{
  char s[20]="Ka%ikk%i %k%k";
  kasittele_maaritys("kissa \\{istuu\\}%i\\/ puussa");
  kasittele_maaritys("kissa \\}k");
  while ( 1 ) {
    printf("Jono >");
    if ( lue_lyhenne_jono(N_S(s)) <= OLETUS ) break;
    printf("%s\n",s);
  }
  return 0;
}
#endif
