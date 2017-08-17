/* ioali.c */
/****************************************************************************/
/*
** I O A L I . C
**
**
** Yleisk„ytt”isi„ IO-rutiineja mm. Kerho-ohjelman k„ytt””n
**
**  Tekij„t:          Vesa Lappalainen
**                    Ohjelmointikurssi 1991-
**  Tehty:            20.12.1991
**  Muutettu:         15.12.1995/vl
**  Mit„ muutettu:    + siirrettty omaksi yleisk„ytt”iseksi kirjoistoksi
** Muutettu:          28.12.2001/vl
** Mita muutettu:     Lisatty vakion DOSOUT tutkiminen.  Jos vakio maritelty
**                    muutetaan ANSI-merkit OEM-merkeiksi tulostuksessa.
**                    Muutos tehdään painvastoin lue_merkissa.
**
** Aliohjelmat:
** ============
**
** char lue_merkki(void);
**  - lukee yhden merkin konsolillta
**    Jos m„„ritelelty vakio USE_CONSOLE, k„ytt„„ console.c:n kutsua
**    ReadChar, muuten jos k„„nnet„„n TURBO-C:ll„, niin getch:ta tai
**    standardi tapauksessa fgets:„„, jolloin tosin joka merkin j„lkeen t„ytyy
**    painaa [Enter]
**
** char odota_nappain(const char *kelpaavat, char oletus,int merkki_isoksi);
**  - odottaa yhden merkin painamista.  Vain jonossa kelpaavat olevat
**    hyv„ksyt„„n.  Jos painetaan [Enter], palautetaan merkki oletus.
**    Jos merkki_isoksi == 1, muutetaan merkki ennen vertailua ja palautusta
**    isoksi kirjaimeksi.
**    Esim.  c = odota_nappain("KE",'K',MERKKI_ISOKSI)
**    odottaa kunnes painetaan joko K,k,E,e tai [Enter].   Jos painetaan
**    [Enter], palautetaan 'K', muuten ko. painettu merkki.
**
**    Avuksi on valmiit vakiot:
**      #define MERKKI_ISOKSI   1
**      #define MYOS_PIENET     0
**      #define KAIKKI_KELPAA   NULL
**      #define EI_OLETUSTA     0
**      #define RIVINVAIHTO     "\r\n"
**
** void odota_jotakin(void);
**  - tulostaa n„ytt””n tekstin "Paina jotakin" ja odottaa kunnes painetaan
**    mit„ tahansa n„pp„int„
**
** void ei_toimi(void);
**  - tulostaa n„ytt””n tekstin "Ei toimi viel„" ja odottaa mink„ tahansa
**    merkin painamista.
**
** int kylla_vastaus(void);
**  - Odottaa kunnes painetaan K,k,E,e tai [Enter].  [Enter]=K.
**    Jos painetaan K tai k, palautetaan 1, muuten 0.
**
** void viiva(int pit, char merkki);
**  - tulostaa n„ytt””n pit -kpl merkki„ merkki.
**    Esim. kutsulla viiva(79,'-');  printf("\n");
**    Saadaan n„ytt””n 79 merkki„ pitk„ viiva.

** void viivarivi(int pit, char merkki);
**  - kuten viiva, mutta lisaksi rivinvaihto
**    Esim. kutsulla viivarivi(79,'-');  printf("\n");
**    Saadaan n„ytt””n 79 merkki„ pitk„ viiva.
**
** void otsikko(char c,const char *teksti);
**  - Esim. kutsu otsikko('1',"Avustus");
**    tulostaa n„ytt””n:
**      1. Avustus
**      ==========
**                                (Huom! tyhj„ rivi)
**
** int ilmoitus(const char *s);
**  - tulostaa n„ytt””n tekstin s jos s != NULL, odottaa mink„ tahansa
**    n„pp„imen painamista ja palauttaa 1.
**    Jos s == NULL, ei tulostetan mit„„n, ei odoteta mit„„n ja palautetaan 0.
**    Voidaan k„ytt„„ hyvin esimerkiksi virhemerkkijonoja paluttavien
**    aliohjelmien kanssa:
**      if ( ilmoitus(lue_tiedosto("kissa.dat")) ) ....
**
** int kylla_kysymys(const char *s);
**  - tulostaa n„ytt””n tekstin s (jos s == NULL niin ""), sitten tekstin
**    (K/e) ja odottaa kunnes painetaan K,k,E,e tai [Enter] (=='K') ja
**    palauttaa 1 jos painettiin K,k tai [Enter]
**    Esim:
**      if ( kylla_kysymys("Lis„t„„nk”") ) ...
**
** int onko_tiedostoa(const char *nimi);
**  - palauttaa 1, jos on olemassa tiedosto nimi.  Muuten 0.
**
*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include "ioali.h"
#include "mjonot.h"

#ifdef USE_CONSOLE
# include "console.h"
#else
#ifdef __TURBOC__
/* Sy”tt” Turbo-C:n mukaisesti.  HUOM 2x 2_           */
# define GETCH
# include <conio.h>
#endif
#ifdef __RHIDE__
/* RHiden mukan tulee my”s conio.h  HUOM 2x 2_           */
# define GETCH
# include <conio.h>
#endif
#endif

#if ( __TURBOC__ == 0x0550 )
#pragma warn -8080 // _getch
#endif
#if ( __TURBOC__ == 0x0560 )
#pragma warn -8080 // _getch
#endif


#ifdef DOSOUT
#include "dout.h"
int ioprintf(const char *s,const char *lf)
{
  char st[500];
  strncpy(st,s,sizeof(st)); st[sizeof(st)-1] = 0;
  vaihdaOEMp(st);
  printf(st);
  return printf(lf);
}
#else
int ioprintf(const char *s,const char *lf)
{
  printf(s);
  return printf(lf);
}
#endif

char lue_merkki(void)
{
#ifdef USE_CONSOLE
   return (char)ReadChar();
#else
/* Seuraava toimii mm. Turbo C:ss„: */
#ifdef GETCH
#ifdef DOSOUT
   unsigned char c = (char)getch();
   return vaihdaAnsi(c);
#else
   return (char)getch();
#endif

# else        /* Seuraava on standardin mukainen: */
   char s[50];
   fgets(s,50,stdin);
   return s[0];
# endif
#endif
}


char odota_nappain(const char *kelpaavat, char oletus,int merkki_isoksi)
{
  char painettu;
  do {
    while ( !(painettu = lue_merkki()) );
    if ( merkki_isoksi ) painettu = isoksi(painettu);
    if ( oletus != EI_OLETUSTA && strchr(RIVINVAIHTO,painettu) )
      return oletus;
  } while ( kelpaavat != NULL && strchr(kelpaavat,painettu) == NULL );
  return painettu;
}


void odota_jotakin(void)
{
  ioprintf("Paina jotakin!","\n");
  odota_nappain(KAIKKI_KELPAA,EI_OLETUSTA,MERKKI_ISOKSI);
}


int kylla_vastaus(void)
{
  return ( odota_nappain("KE",'K',MERKKI_ISOKSI) == 'K' );
}


void ei_toimi(void)
{
  ioprintf("Ei toimi viel„! ","\n");
  odota_jotakin();
}


void viiva(int pit, char merkki)
{
  int i;
  for (i = 0; i<pit; i++)
    printf("%c",merkki);
}

void viivarivi(int pit, char merkki)
{
  viiva(pit,merkki);
  printf("\n");
}

void otsikko(char c,const char *teksti)
{
  char st[10];
  printf("\n\n\n\n");
  sprintf(st,"%c. ",c); ioprintf(st,"");
  ioprintf(teksti,"\n");
  viiva( 1+2 + strlen(teksti),'=');
  printf("\n\n");
}


/* Palauttaa 1 jos ilmoitus tulostettu, 0 jollei ilmoitusta */
int ilmoitus(const char *s)
{
  if ( s == NULL ) return 0;
  printf("\n");
  ioprintf(s,"\n");
  odota_jotakin();
  return 1;
}


/* Palauttaa 1 jos painetaan K tai ret */
int kylla_kysymys(const char *s)
{
  int kylla;
  if ( s == NULL ) s = "";
  ioprintf(s," (K/e):");
  kylla = kylla_vastaus();
  printf("\n");
  return kylla;
}


int onko_tiedostoa(const char *nimi)
{
  FILE *f;
  f = fopen(nimi,"r");
  if ( f == NULL ) return 0;
  fclose(f);
  return 1;
}



