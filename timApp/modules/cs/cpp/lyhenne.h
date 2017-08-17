/****************************************************************************/
/*
**        L Y H E N N E . H
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
**  K„ytt”:           ks. lyhenne.c
*/
#ifndef LYHENNE_H
#define LYHENNE_H
int lyhenne_merkkeja(const char *s);
int kasittele_maaritys_valmis(char *s);
int  kasittele_maaritys(char *s);
int  poista_lyhenne(const char *);
int  korvaa_kaikki(char *, int);
void lyhenne_help(void);
int kasittele_lyhenne_jono(char *s,int maxp,int montako);
int  lue_lyhenne_jono(char *, int);
int  talleta_lyhenteet(const char *nimi);
int  alusta_lyhenteet(const char *nimi);
#endif
