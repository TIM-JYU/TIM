/* editlue.c */
/****************************************************************************/
/*
**        E D I T L U E . C P P
**
** Apualiohjelmia console.c:n helppoon k„ytt””nottoon.
** T„m„ tiedosto muuttaa mjonot.c:n lue_jono:n ja lue_jono_oletus
** k„ytt„m„„n editoivaa versiota EditString.
** T„ss„ on my”s toteutus omien lyhenteitten k„ytt””n (ks lyhenne.c)
**
**  Tekij„t:          Vesa Lappalainen
**                    Ohjelmointikurssi 1996-
**  Tehty:            07.01.1996
**  Muutettu:
**
**  K„ytt”:
**   0) Projektiin tarvittaan t„m„n lis„ksi
**        C\ALI\console.c
**        C\ALI\mjonot.
**        C\ALI\lyhenne.c
**      Ennen k„„nn”st„ m„„ritell„„n vakio USE_CONSOLE.
**      Mik„li ko. vakiota ei ole m„„ritelty, ei t„m„n tiedoston
**      aliohjelmat tee mit„„n, eik„ em. tiedostoja tarvitse
**      lis„t„ projektiin (ellei niit„ muuten tarvita).
**      Tiedosto ei t„llaisenaan toimi "moniajossa".
**
**   1) Ohjelman aluksi kutustaan funktiota
**        int edit_alusta(const char *lyh);
**      miss„ lyh on mahdollisen lyhennetiedoston nimi.
**
**   2) Ohjelman lopuksi kutustaan
**        int edit_vapauta(const char *lyh);
**      miss„ lyh on sen lyhennetiedoston nimi, johon ohjelman
**      aikana syntyneen lyhenteet talletetaan.
**
**   3) Oletuksena editoinnissa on kielletty 2 kertaiset v„lily”nnit.
**      Jos halutaan muita kieltoja, niin voidaan tehd„ muunosfunktio,
**      jota kutsutaan jokaisen kirjaimen painamisen j„lkeen.
**      Muunnosfunktion tulee olla tyyppi„:
**
**        typedef char *(*muunnos_funktio)(char *);
**
**      Esimerkiksi mjonot.c:n jono_isoksi on t„llainen.
**      Muunnosfunktio ei mielell„„n saa suurentaa jonon kokoa,
**      koska kokoparamteri„ ei tuoda.
**      Muunnos vaihdetaan kutsulla:
**
**        int edit_vaihda_muunnos(muunnos_funktio mf);
**
**      eli esimerkiksi
**
**        edit_vaihda_muunnos(jono_isoksi);
**
**      Vaihdettu muunnos on voimassa vain seuraavan lue_jono
**      tai lue_jono_oletus ajan. Jos mf == NULL, niin muunnoksena
**      k„ytet„„n silti ylim„„r„iset tyhj„t poistavaa muunnosta.
**
**   4) Haluttaessa voidaan my”s suoraan kutsua aliohjelmaa
**
**        int edit_lue(char *s,int maxs);
**
**      joka k„ytt„„ lukemiseen EditString-kutsua muunnettuna siten,
**      ett„ mahdolliset lyhenteet k„sitell„„n.  Tosin saman tekee
**      mjonot.c:n funktio lue_jono t„m„n tiedoston muuttamana.
**
**   5) Luettuun merkkijonoon sijoitetaan seuraavat erikoistapausmerkkijonot
**        q      jos painetaan ESC
**        ^      jos painetaan nuoli yl”s
**
**      N„it„ sijoituksia voidaan muuttaa seuraavilla kutsuilla:
**
**        int edit_tyhjenna_sijoitukset(void)
**          - tyhjent„„ sijoitustaulukon
**        int edit_lisaa_sijoitus(int key,const char *jono)
**          - lis„„ taulukkoon uuden sijoituksen
**        const char *edit_sijoitus(int key)
**          - palauttaa key:ta vastaavan sijoituksen
**
**      Jonon alkuun sallittuja merkkej„ voidaan k„sitell„ funktioilla:
**
**        const char *edit_ei_muunnosta(int i)
**          - ei_muunnosta_jonot, oletuksena:
**              i==0: q|^|.|,|$  i==1:?
**
**        int edit_muuta_ei_muunnosta(int i, const char *s)
**          - vaihtaa ei_muunosta-jonot toiseksi
**
**      Indeksi i==0 on ne jonot, jotka tutkitaan wildmat
**      ja i==1 ne jotka tutkitaan sellaisenaan
**
**   6) Jos t„t„ k„ytet„„n c++ ohjelmasta, joka tulostaa cout:iin,
**      niin projektiin on lis„tt„v„ viel„ wout.cpp, joka siirt„„
**      tulostuksen console.c:n printscr-funktiota k„ytt„v„ksi.
**      T„ll”in pit„„ my”s m„„ritell„ k„„nn”kseen cout=wout
**
**      Samoin mahdollisesti pit„„ m„„ritell„ printf=wprintf,
**      jos k„ytet„„n console.c:n toteutuksena curses-kirjastoa.
**
*/

#ifdef USE_CONSOLE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "editlue.h"
#include "console.h"
#include "mjonot.h"
#include "lyhenne.h"


/****************************************************************************/
/* Sijoitusten k„sittely:                                                   */
/****************************************************************************/

static char ei_muunnosta[2][256] = {"q|.|,|$|^","?"};

const char *edit_ei_muunnosta(int i)
{
  return ei_muunnosta[i==1];
}

int edit_muuta_ei_muunnosta(int i,const char *s)
{
  kopioi_jono(N_S(ei_muunnosta[i==1]),s);
  return 0;
}


typedef struct {
  int  key;
  char jono[50];
} tSijoitus;

#define MAX_SIJOITUS 30
typedef struct {
  int n;
  tSijoitus sijoitus[MAX_SIJOITUS];
} tSijoitukset;


tSijoitukset sijoitukset = {
  2,
  {
    { coKEY_ESC, "q" },
    { coKEY_UP , "^" },
    { 0        , ""  }
  }
};

int edit_tyhjenna_sijoitukset(void)
{
  sijoitukset.n = 0;
  return 0;
}

static int edit_sijoitus_paikka(int key)
{
  int i;
  for (i=0; i<sijoitukset.n; i++)
    if ( key == sijoitukset.sijoitus[i].key )
      return i;
  return -1;
}

int edit_lisaa_sijoitus(int key,const char *jono)
{
  tSijoitus sij;
  int paikka;
  if ( sijoitukset.n >= MAX_SIJOITUS ) return 1;
  sij.key = key;
  kopioi_jono(N_S(sij.jono),jono);
  paikka = edit_sijoitus_paikka(key);
  if ( paikka < 0 ) paikka = sijoitukset.n++;
  sijoitukset.sijoitus[paikka] = sij;
  return 0;
}

const char *edit_sijoitus(int key)
{
  int paikka = edit_sijoitus_paikka(key);
  if ( paikka < 0 ) return NULL;
  return  sijoitukset.sijoitus[paikka].jono;
}


/****************************************************************************/
/* Sijoitusten k„sittely loppu                                              */
/****************************************************************************/

static int maxS = 0;
static pEditFunc muunnos = NULL;

int edit_vaihda_muunnos(muunnos_funktio mf)
{
  muunnos = mf;
  return 0;
}

static int tee_muunnos = 1;

static char *kas_lyhenne(char *s)
{
  if ( lyhenne_merkkeja(s) ) tee_muunnos = 0;
  kasittele_lyhenne_jono(s,maxS,1);

  if ( *s && (
               joku_jono_func(s,ei_muunnosta[0],"|",wildmat) == 0 ||
               joku_jono_func(s,ei_muunnosta[1],"|",strcmp) == 0
             ) )
    return s;

  if ( tee_muunnos ) {
    poista_alku_ja_2_tyhjat(s);
    if ( muunnos ) muunnos(s);
  }
  return s;
}


int edit_lue(char *s,int maxs)
{
  int ret;
  const char *sij;
  maxS = maxs;
  tee_muunnos = 1;
  ret = EditString(s,maxs,kas_lyhenne);
  kasittele_lyhenne_jono(s,maxS,0);
  muunnos = NULL;
  printscr("\n");

  sij = edit_sijoitus(ret);
  if ( sij ) kopioi_jono(s,maxs,sij);

  return 2;
}


static int alustettu = 0;

int edit_alusta(const char *lyh)
{
  if ( alustettu++ == 0 ) InitConsole();
  alusta_lyhenteet(lyh);
  alusta_lue_jono(edit_lue);
  return 0;
}

int edit_vapauta(const char *lyh)
{
  talleta_lyhenteet(lyh);
  if ( --alustettu == 0 ) ReleaseConsole();
  return 0;
}

#endif /* USE_CONSOLE */

