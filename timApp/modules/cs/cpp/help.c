/****************************************************************************/
/*
**        H E L P . C
**
** Aliohjelmia avustusten tekemiseen.
**
** Aliohjelmat:
**    alusta_help            - alustaa help-systeemin valitulle ohjelmalle
**    vapauta_help           - lopettaa helpin k‰ytˆn
**    help                   - tulostaa valitun help-tekstin jos
**                             aihe hakasuluissa.  Jollei ole, tulostetaan
**                             kutsuparametri sellaisenaan
**    help_aihe              - tulostaa valitun teksin.  Jos teksti
**                             ei ole hakasuluissa, lis‰t‰‰n ne
**    help_hakemisto         - helpin selailutilaan valitun aiheen kohdalta
**                             tarvittaessa lis‰t‰‰n aiheeseen hakasulut
**
** Tekij‰:         Vesa Lappalainen 6.12.1993
** Muutettu:       30.1.1994
** Mit‰ muutettu:  + tiedostoa etsit‰‰n
**                     1) oletushakemisto
**                     2) alkup. hakemisto.
** Muutettu:       14.1.1996
** Mit‰ muutettu:  + jos muualla k‰ytet‰‰n muunnettua lue_jonoa, niin nyt
**                   help toimii t‰m‰n kanssa paremmin.
**                 + voi paremmin hakea vajaalla hakusanalla
**
** Muutettu:      28.12.2001/vl
** Mita muutettu: Lisatty vakion DOSOUT tutkiminen.  Jos vakio maritelty
**                muutetaan ANSI-merkit OEM-merkeiksi tulostuksessa.
**
** K‰yttˆ ks. help.h
**
*/

#include <stdio.h>
#include <string.h>
#include "mjonot.h"
#include "help.h"

#define SISALLYS        "[SISƒLLYS]"
#define HELP_TARKENNIN  ".hlp"
#define AIHE_ALKU       '['
#define AIHE_LOPPU      ']'
#define AIHE_ALKU_JONO  "["
#define AIHE_LOPPU_JONO "]"
#define HELP_KOMMENTTI  ";"
#define RIVEJA          23


static char *help_nimi = NULL;
static char help_file[80];

/****************************************************************************/
/* Virheilmoitukset:                                                        */
static const char *NULL_NIMI    = "Ohjelman nimi on NULL!";
static const char *EI_TIEDOSTOA = "Ei lˆydy help-tiedostoa!";
static const char *EI_ALUSTETTU = "Help-tiedostoa ei ole alustettu!";



static int riveja=0;
/****************************************************************************/
void alusta_help_rivit(void)
{
  riveja = 0;
}

/****************************************************************************/
static int lisaa_riveja(void)
/* Lis‰t‰‰n rivej‰ ja palutetaan tieto siit‰, joko raja ylittyi.            */
{
  riveja++;
  return ( riveja >= RIVEJA );
}

/****************************************************************************/
static int stop_oli_just(void)
{
  return riveja == 0;
}

/****************************************************************************/
static void tyhjia_riveja(int n)
{
  int i;
  for (i=0; i<n; i++) printf("\n");
  alusta_help_rivit();
}

/****************************************************************************/
static int lopetus(const char *jono)
/* Palautetaan tieto onko jono == "Q" tai "q"
----------------------------------------------------------------------------*/
{
  return ( ( jono[0] == 0 ) || ( isoksi(jono[0] ) == 'Q' && jono[1] == 0 ) );
}

/****************************************************************************/
static int help_stop(int tyhjia)
{
  char apu[2]; apu[0]=0;
  if ( !stop_oli_just() ) { /* Ei kahta stoppia per‰kk‰in!                  */
    printf("Jatka RETURN >");
    lue_jono(N_S(apu));
  }
  tyhjia_riveja(tyhjia);
  apu[1]=0;
  if ( apu[0] == 0 ) return 0;
  return lopetus(apu);
}

/****************************************************************************/
static int help_printf_n(const char *p, int n)
{
  char format[10];
  if ( strncmp(p,HELP_KOMMENTTI,strlen(HELP_KOMMENTTI)) == 0 ) return 0;

  if ( strncmp(p,HELP_STOP_JONO,strlen(HELP_STOP_JONO)) == 0 )
    return help_stop(0);

  if ( n == 0 ) return 0;

  sprintf(format,"%%%d.%ds",n,n); /* format = %10.10s */
  printf(format,p);
  if ( p[n-1]=='\n' && lisaa_riveja() && help_stop(0) ) return 1;
  return 0;
}

/****************************************************************************/
static int help_printf(const char *teksti)
/* Helpin tulostus.
** Palauttaa 1 jos pit‰‰ lopettaa.
** T‰ss‰ on laskuri helpin pys‰ytt‰miseksi sopivin v‰lein.
** Siksi kaikki rivinvaihdot erotellaan erikseen tulostettavaksi.
** Jos rivi alkaa "#" ei t‰t‰ tulosteta, vaan pys‰ytet‰‰n tulostus.
** Jos rivi alkaa ";" ei sit‰ tulosteta.
----------------------------------------------------------------------------*/
{
  const char *p=teksti;
  int lf;

  while ( ( lf = paikka(p,'\n') ) >= 0 ) {
    if ( help_printf_n(p,lf+1) ) return 1;
    p += lf+1;
  }
  return help_printf_n(p,strlen(p));
}


/****************************************************************************/
/* Koska tulostus lukee tiedostosta rivej‰ liian pitk‰lle, t‰ytyy           */
/* viimeinen tulostumaton rivi olla etsimiselle jemmassa.  Muutoin          */
/* per‰kk‰isi‰ aiheita ei saada tulostettua                                 */
static char rivi[90]="";

#ifdef DOSOUT
#   include "dout.h"
#endif

/****************************************************************************/
static int tulosta_f(FILE *f)
/* Tulostetaan valmiiksi avattua help-tiedostoa nykykohdasta kunnes
** tulee seuraavan aiheen otsikko.
** Jos tulee #, pys‰ytet‰‰n tulostus
** Palautetaan 1 jos tulostus pit‰‰ lopettaa.
**
----------------------------------------------------------------------------*/
{
  while (1) {
    rivi[0] = 0;
    if ( f_lue_jono(f,N_S(rivi)) < OLETUS ) return 0;
    if ( rivi[0] == AIHE_ALKU )
      return 0; /* T‰m‰ etsi_seuraava_aihe k‰ytett‰v‰ksi */
    liita_jono(N_S(rivi),"\n");
#ifdef DOSOUT
    vaihdaOEMp(rivi);
#endif
    if ( help_printf(rivi) ) return 1;
  }
}


/****************************************************************************/
static FILE *etsi_seuraava_aihe(FILE *f, const char *aihe)
/* Etsit‰‰n avatusta tiedostosta seuraava aihetta vastaavan kohdan alku.
** Tutkitaan ensin tulosta_f j‰ljilt‰ j‰‰nyt viimeinen rivi.
** T‰h‰n pit‰isi vaihtaa pikahakutaulukon k‰yttˆ.
----------------------------------------------------------------------------*/
{
  char iaihe[90],*p;
  if ( !f ) return NULL;

  kopioi_jono(N_S(iaihe),aihe); jono_isoksi(iaihe);

  while (1) {
    if ( rivi[0]==0 && f_lue_jono(f,N_S(rivi)) < OLETUS ) {
      fclose(f);
      return NULL;
    }
    if ( ( p = strstr(rivi,HELP_KOMMENTTI) ) != NULL ) *p=0;
    jono_isoksi(poista_tyhjat(rivi));
    if ( wildmat(rivi,iaihe) == 0 ) return f;
    rivi[0] = 0; /* Jotta luetaan ihan oikeasti seuraava rivi.              */
  }
}


/****************************************************************************/
static FILE *etsi_1_aihe(const char *aihe)
/* Etsit‰‰n aihetta vastaavan kohdan alku help-tiedostosta.
** Avataan tiedosto ja liikutaan oikeaan kohtaan.  Kutsuvan
** aliohjelman velvollisuus on sitten sulkea tiedosto.
----------------------------------------------------------------------------*/
{
  FILE *f = fopen(help_nimi,"rt");
  if ( !f ) return NULL;
  rivi[0] = 0;

  return etsi_seuraava_aihe(f,aihe);
}

/****************************************************************************/
int sulje(FILE *f, int paluu)
{
  if ( f ) fclose(f);
  f = NULL;
  return paluu;
}

/****************************************************************************/
static int tulosta_aihe(const char *aihe, int viim, int tyhjia_aluksi)
/* viim          = Tuleeko pys‰hdys viimeisen aiheen j‰lkeen
** tyhjia_aluksi = montako tyhj‰‰ rivi‰ tulostuksen aluksi.  Jos != 0
**                 niin nollataan samalla rivilaskuri.
** Palautetaan 0 = OK, 1 = pit‰‰ lopettaa, 2 = aihetta ei lˆydy.
----------------------------------------------------------------------------*/
{
  int paluu;
  FILE *f = etsi_1_aihe(aihe);
  if ( !f ) return 2;

  if ( tyhjia_aluksi ) tyhjia_riveja(tyhjia_aluksi);

  while ( f ) {
    paluu = tulosta_f(f);
    f = etsi_seuraava_aihe(f,aihe);
    if ( f && help_stop(3) ) return sulje(f,1);
  }

  sulje(f,0);
  if ( viim && help_stop(3) ) return 1;
  return paluu;
}


/****************************************************************************/
static int help_virhe(const char *virhe)
{
  help_printf(virhe);
  help_printf("\n#");
  return 1;
}

/****************************************************************************/
static const char *aiheeksi(const char *aihe,const char *sis,
                            const char *loppu)
/* Lis‰t‰‰n aiheeseen sis ennen hakasulkua ja loppu hakasulun j‰lkeen
** Esim.  kissa  t  *  => [kissat]*
*/
{
  static char Aihe[60]=AIHE_ALKU_JONO;
  const char *p = aihe;
  int viim;

  if ( p[0] == AIHE_ALKU ) p++;
  kopioi_jono(Aihe+1,sizeof(Aihe)-1,p);

  viim =  strlen(Aihe)-1;
  if ( Aihe[viim] == AIHE_LOPPU ) Aihe[viim] = 0;

  liita_jono(N_S(Aihe),sis);
  liita_jono(N_S(Aihe),AIHE_LOPPU_JONO);
  liita_jono(N_S(Aihe),loppu);
  return Aihe;
}

/****************************************************************************/
int                       /* 0 = teksti lˆytyi ja tulostettiin              */
help_hakemisto(           /* muut = virhenumeroita                          */
  const char *alku        /* s   Aihe, josta tulostus aloitetaan            */
)
/* Funktiolla pyˆritet‰‰n helpin sis‰llysluetteloon perustuvaa toimintaa
** Pyˆritys aloitetaan joko valitusta aiheesta tai Sis‰llysluettelosta
** alku jonon ei ole pakko sis‰lt‰‰ []-merkkej‰, ne lis‰t‰‰n tarvittaessa
----------------------------------------------------------------------------*/
{
  static char s[60];
  const char *sis;
  int paluu;

  if ( help_nimi == NULL ) return help_virhe(EI_ALUSTETTU);

  if ( ( paluu = tulosta_aihe(alku ? aiheeksi(alku,"","") : SISALLYS,0,3) )
        == 1 )
    return 1;
  while ( lue_jono_oletus(
            "Valitse aihe (voi olla myˆs *)",0,0,s,N_S(s)) >= OLETUS )
  {
    if ( lopetus(s) ) return 1;
    if ( laske_merkit(s,"*?") > 0 ) sis = ""; else sis = "*";
    if ( tulosta_aihe(aiheeksi(s,sis,""),0,3) == 1 ) return 1;
  }
  return paluu;
}

/****************************************************************************/
int                       /* 0 = teksti lˆytyi ja tulostettiin              */
help_aihe(                /* muut = virhenumeroita                          */
  const char *aihe        /* s   Tulostettavan aiheen otsikko.              */
)
/*
** Funktiolla tulostetaan aiheen mukainen help-teksti.  Jos teksti‰
** ei lˆydy, palautetaan != 0
** Jos aihe ei ala [ -merkill‰ lis‰t‰‰n merkit.
----------------------------------------------------------------------------*/
{
  if ( help_nimi == NULL ) return help_virhe(EI_ALUSTETTU);
  if ( aihe == NULL ) return help_hakemisto(NULL);
  return tulosta_aihe(aiheeksi(aihe,"","*"),0,0);
}



/****************************************************************************/
int                       /* 0 = teksti lˆytyi ja tulostettiin              */
help(                     /* muut = virhenumeroita                          */
  const char *aihe        /* s   Tulostettavan aiheen otsikko.              */
)
/*
** Funktiolla tulostetaan aiheen mukainen help-teksti.  Jos teksti‰
** ei lˆydy, palautetaan != 0
** Jos aihe ei ala [ -merkill‰, tulostetaan teksti sellaisenaan.
** Aihe voi sis‰t‰‰ myˆs jokereita * ja ?, jolloin tulostetaan maskiin
** t‰sm‰‰v‰t aiheet.
** Jos aihe == "#", niin pys‰hdyt‰‰n.
** Jos aihe == NULL pyˆreitet‰‰n help-hakua.
----------------------------------------------------------------------------*/
{
  if ( help_nimi == NULL ) return help_virhe(EI_ALUSTETTU);
  if ( aihe == NULL ) return help_hakemisto(NULL);
  if ( aihe[0] != '[' ) { /* Jos ei aihe                                    */
    help_printf(aihe);
    return 0;
  }

  return tulosta_aihe(aihe,0,0);
}


/****************************************************************************/
void                      /*                                                */
vapauta_help(void)        /* helpin lopettamistoimet                        */
/*
** Funktiolla tehd‰‰n helpin lopettamiseksi tarvittavat toimenpiteet
** Funktioon pit‰isi lis‰t‰ pikahauktaulukon vapautus
----------------------------------------------------------------------------*/
{
  help_nimi = NULL;
}

/****************************************************************************/
int                       /* 0    = alustus onnistui                        */
alusta_help(              /* muut = virhe alustuksessa                      */
  const char *ohjelma     /* s   Ohjelman nimi (tiedosto ohjelma.HLP)       */
)
/*
** Funktiolla alustetaan avustussysteemi.
** Jos avustustiedostoa ei ole, ilmoitetaan t‰st‰ ja palutetaan virhe nro.
** Tiedostoa etsit‰‰n oletushakemistosta ja sitten ohjelman alkuper‰isest‰
** hakemistosta.
** Funktioon pit‰isi viel‰ t‰ydent‰‰ pikahakutaulukon luominen.
----------------------------------------------------------------------------*/
{
  FILE *f = NULL;
  char *p;
  int i;

  vapauta_help();
  if ( ohjelma == NULL ) return help_virhe(NULL_NIMI);
  kopioi_jono(N_S(help_file),ohjelma);
  jono_pieneksi(help_file);
  if ( ( p = strstr(help_file,".exe") ) != NULL ) *p = 0;
  liita_jono(N_S(help_file),HELP_TARKENNIN);
  help_nimi = help_file;

  for (i=strlen(help_file)-1; i>=0; i--) /* Oletushakemistossa oleva nimi   */
    if ( strchr("\\/",help_file[i]) ) { i++; break; }

  if ( i > 0 ) { /* Nimest‰ saatiin hakemisto pois, yritet‰‰n t‰t‰.         */
    f = fopen(help_file+i,"rt");
    if ( f ) help_nimi = help_file+i;
  }

  if ( !f ) f = fopen(help_nimi,"rt");
  if ( !f ) { vapauta_help(); return help_virhe(EI_TIEDOSTOA); }

  /* T‰ss‰ pit‰si t‰ytt‰‰ pikahakutaulukko */
  fclose(f);
  return 0; /* Help-lˆytyi ja alustettu! */
}
