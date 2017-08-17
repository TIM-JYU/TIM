/* mjonotpp.h */
/****************************************************************************/
/*     M  J  O  N  O  T  P  P.  H
**
**
**   Tiedostossa on merkkien ja merkkijonojen käsittelyohjelmien yleiset
**   aliohjelmien otsikot.  Tiedosto on lähinnä muunnostiedosto string-
**   tyypille vastaavasta C-kirjastosta mjonot.c
**
**   Tekijät:          Vesa Lappalainen
**   Tehty:            02.01.1996
**   Muutettu          07.03.1996/vl
**   Mitä muutettu:    lue_jono_oletus laitettu toimimaan
**   Muutettu          06.06.2000/vl
**                     yksi lue_jono_oletus -muoto korjattu
**                     kaksi erilaista erota-funktiota
**   Muutettu          17.02.2001/vl
**                     string jono_isoksi(const string &jono)
**                     joka siis vain palauttaa muutetun jonon  
**   Muutettu          08.04.2001/vl
**                     lue_jono alustetaan kayttamaan aliohjelmaa
**                     lue_jono_getline oletuksena (ks. mjonotpp.cpp)
**   Muutettu          29.12.2001/vl
**                     luvuksi-funktiot
**   Muutettu          28.02.2002/vl
**                     luvuksi-funktiot const string &:llä
**
**  Tiedostossa on seuraavia aliohjelmia:
**    remove(st)             - poistaa tiedoston st
**    onko_tiedostoa(st)     - palauttaa 1 jos on tiedosto nimellä st
**    rename(&vanha,&uusi)   - vaihtaa tiedoston nimen
**    tarkennin_alku(&tied)  - palauttaa mistä kohti tarkennin alkaa
**                             tiedoston nimessä
**    poista_tarkennin(tied) - poistaa tarkentimen tiedostonnimestä
**    laita_tarkennin(tied,tark) - laittaa tiedoston nimen tarkentimen
**    vaihda_tarkennin(tied,tark)- vaihtaa tiedoston nimen tarkentimen
**
**    istream &lue_rivi(istream &is,char *s,int max_koko);
**    istream &lue_rivi(istream &is,string &s);
**    istream &lue_rivi(istream &is,int &i,int def=0);
**    istream &lue_rivi(istream &is,double &d,double def=0.0);
**
**  Seuraavat ovat mjonot.c:n muunnoksia string-luokalle
**  (- merk ei totetuttu erikseen string-luokalle
**     tai ei tarvitse toteuttaa)
**
**    tee_jono               - luo uuden merkkijonon jonne jono kopioidaan
**    kopioi_jono            - kopioi kork. annetun määrän merkkejä
**    liita_jono             - liittää jonon toisen perään, tulos
**                             korkeintaan max.pituus mittainen               -
** -  f_lue_jono             - lukee tiedostosta merkkijonon                  -
** -  alusta_lue_jono        - alustaa merkkijonon lukemisen                  -
** -  lue_jono               - lukee päätteeltä merkkijonon                   -
**    lue_jono_oletus        - lukee päätteeltä merkkijonon.
**                             Näyttöön tulostetaan haluttu viesti ja
**                             jonon oletusarvo, mikäli painetaan RET         -
** -  lue_kokluku_oletus     - luetaan kokonaisluku, jolle käytetään
**                             oletusarvoa mikäli heti painetaan RET          -
**    poista_alkutyhjat      - poistaa merkkijonon alussa olevat välilyönnit  -
**    poista_lopputyhjat     - poistaa merkkijonon lopussa olevat välil.      -
**    poista_2_tyhjat        - muuttaa merkkijonossa kaikki peräkkäiset
**                             välilyönnit yhdeksi välilyönniksi              -
**    poista_tyhjat          - poistaa alusta ja lopusta kaikki sekä
**                             muualta moninkertaiset välilyönnit             -
** -  isoksi                 - muuttaa kirjaimen isoksi kirjaimeksi huomioiden
**                             skandit                                        -
** -  pieneksi               - muuttaa pieneksi huomioiden skandit            -
**    jono_isoksi            - muuttaa jonon kaikki merkit isoiksi            -
**    jono_pieneksi          - muuttaa jonon kaikki merkit pieniksi           -
**    jono_alku_isoksi       - muuttaa jonon kaikki sanojen alut isoiksi
**                             ja kaikki muut pieniksi                        -
**    wildmat                - vertaa onko sana == maski, missä maskissa
**                             voi olla jokeri-merkkejä (* tai ?)             -
**    onko_samat             - ensin muutetaan jonot isoiksi ja poistetaan
**                             tyhjät ja sitten wildmat
**                             (eli "  Kalle " == "    k*  ")                 -
**    palanen                - ottaa merkkijonosta seuraavan erotinmerkkien
**                             määräämän palasen                              -
**    laske_merkit           - laskee annettujen merkkien esiintymismäärän
**                             merkkijonossa                                  -
**    paikka                 - palauttaa kirjaimen 1. indeksin merkkijonossa  -
**    tayta_valit            - täyttää syötön "A-F" muotoon "ABCDEF"          -
**    joku_jono              - vastaa kysymykseen onko "EY" joku jonoista
**                             "EU|EY|EL"                                     -
**    jono_arvosanaksi       - muuttaa merkkijonon "7-" reaaliluvuksi 6.75
**    arvosana_jonoksi       - muuttaa reaaliluvun 6.75 merkkijonoki "7-"
**    sallituissa            - paluttaa -1 jos tutkittavan jonon kaikki
**                             merkit ovat annetussa joukossa, muuten
**                             1. väärän merkin indeksin                      -
**    poista_merkit          - poistaa jonosta kaikki valitut merkit          -
**    poista_alusta          - poistaa merkkejä jonon alusta                  -
**    lisaa_alkuun           - lisää merkkejä jonon alkuun                    -
**    lisaa_merkki           - lisää merkin merkkijonon valittuun kohtaan     -
**    vaihda_jonot           - vaihtaa jonossa merkit toisiksi                -
**    muunna_C_symbolit      - muuttaa \t, \n ja \0x61 muotoiset              -
**                             C-symbolit vastaaviksi merkeiksi
**    erota                  - laittaa merkkijonon kahtia valitun merkin
**                             kohdalta
**    erota                  - laittaa merkkijonon kahtia ensimmäisen
**                             valitun merkin kohdalta
**    luvuksi                - muuttaa merkkijonon luvuksi, palauttaa tosi jos
**                             onnistuu
**--------------------------------------------------------------------------*/
#ifndef __MJONOTPP_H
#define __MJONOTPP_H
#include <iostream>
#include <cstdlib>
#include <string>
#include <cstring>
//using namespace std;
#include "mjonot.h"
#ifndef vstring
#define vstring std::string
#endif
//#define NPOS -1
//----------------------------------------------------------------------------
inline int lue_jono_oletus(const char *hopute,
                           int  oletus_alku, int  jonon_alku,
                           const char *oletus,
                           vstring &st)
// Lukee jonon päätteeltä
{
  char s[200];
  int ret=lue_jono_oletus(hopute,oletus_alku,jonon_alku,oletus,N_S(s));
  st = s;
  return ret;
}

inline int lue_jono_oletus(const char *hopute,
                           int  oletus_alku, int  jonon_alku,
                           vstring &st)
// Lukee jonon päätteeltä
{
  return lue_jono_oletus(hopute,oletus_alku,jonon_alku,st.c_str(),st);
}

inline int lue_jono_oletus(const vstring &hopute,
                           int  oletus_alku, int  jonon_alku,
                           vstring &st)
// Lukee jonon päätteeltä
{
  return lue_jono_oletus(hopute.c_str(),oletus_alku,jonon_alku,st.c_str(),st);
}

inline int lue_jono_oletus(const vstring &hopute,
                           int  oletus_alku, int  jonon_alku,
                           const vstring &oletus, vstring &st)
// Lukee jonon päätteeltä
{
  return lue_jono_oletus(hopute.c_str(),oletus_alku,jonon_alku,oletus.c_str(),st);
}

//----------------------------------------------------------------------------
inline int remove(const vstring &st)
// Poistaa tiedoston levylta.
{
  return remove(st.c_str());
}

//----------------------------------------------------------------------------
inline int rename(const vstring &vanha,const vstring &uusi) {
  return rename(vanha.c_str(),uusi.c_str());
}

int tarkennin_alku(const vstring &tied);
vstring &poista_tarkennin(vstring &tied);
vstring &laita_tarkennin(vstring &tied,const vstring &tark);
vstring &vaihda_tarkennin(vstring &tied,const vstring &tark);

std::istream &lue_rivi(std::istream &is,char *s,int max_koko);
std::istream &lue_rivi(std::istream &is,vstring &st);
std::istream &lue_rivi(std::istream &is,int &i,int def=0);
std::istream &lue_rivi(std::istream &is,double &d,double def=0.0);

//----------------------------------------------------------------------------
inline char *tee_jono(const vstring &st) {
  return tee_jono(st.c_str());
}

inline int   kopioi_jono(char *s, int k, const vstring &st) {
  return kopioi_jono(s,k,st.c_str());
}

inline int   liita_jono(char *s, int k, const vstring &st) {
  return liita_jono(s,k,st.c_str());
}

#define CFUNC_TO_CPP(func)               \
  inline vstring &func(vstring &st)        \
  {                                      \
    char *s = tee_jono(st);              \
    func(s);                             \
    st = s;                              \
    free(s);                             \
    return st;                           \
  }

CFUNC_TO_CPP(poista_alkutyhjat)
CFUNC_TO_CPP(poista_lopputyhjat)
CFUNC_TO_CPP(poista_2_tyhjat)
CFUNC_TO_CPP(poista_tyhjat)
CFUNC_TO_CPP(poista_alku_ja_2_tyhjat)
CFUNC_TO_CPP(jono_pieneksi)
CFUNC_TO_CPP(jono_isoksi)
CFUNC_TO_CPP(jono_alku_isoksi)
CFUNC_TO_CPP(jono_1_isoksi)

inline vstring jono_isoksi(const vstring &jono)
{
  vstring s(jono);
  jono_isoksi(s);
  return s;
}

inline int   wildmat(const vstring &st1, const vstring &st2) {
  return wildmat(st1.c_str(),st2.c_str());
}

inline int   onko_samat(const vstring &st1, const vstring &st2) {
  return onko_samat(st1.c_str(),st2.c_str());
}

// inline char *palanen(char *, const char *, int *);

inline int   laske_merkit(const vstring &st, const vstring &mitka) {
  return laske_merkit(st.c_str(),mitka.c_str());
}

inline int   paikka(vstring &st,char ch) {
  return paikka(st.c_str(),ch);
}


inline vstring &tayta_valit(vstring &st1,const vstring &st2,const char *s3) {
  char s[200];
  tayta_valit(s,sizeof(s),st2.c_str(),s3);
  st1 = s;
  return st1;
}

inline vstring &tayta_valit(vstring &st1,const vstring &st2,const vstring &st3)
{
  return tayta_valit(st1,st2,st3.c_str());
}


inline int   joku_jono(const vstring &st1,const vstring &st2,const vstring &st3)
{
  return joku_jono(st1.c_str(),st2.c_str(),st3.c_str());
}


inline double jono_arvosanaksi(const vstring &st) {
  return jono_arvosanaksi(st.c_str());
}


// inline vstring arvosana_jonoksi_st(double d,char *nolla);

inline int   sallituissa(const vstring &st1,const vstring &st2) {
  return sallituissa(st1.c_str(),st2.c_str());
}

inline int   sallituissa(const vstring &st1,const char *s2) {
  return sallituissa(st1.c_str(),s2);
}

inline int   sallituissa(const char *s1,const vstring &st2) {
  return sallituissa(s1,st2.c_str());
}


inline vstring &poista_merkit(vstring &st,const char *mitka) {
  char *s = tee_jono(st);
  poista_merkit(s,mitka);
  st = s;
  free(s);
  return st;
}

inline vstring &poista_merkit(vstring &st,const vstring &st2) {
  return poista_merkit(st,st2.c_str());
}

inline vstring &poista_alusta(vstring &st, int n) {
  return st.erase(0,n);
}

inline int   lisaa_alkuun(vstring &st, const char *s2) {
  st.insert(0,s2);
  return 0;
}

inline vstring &lisaa_merkki(vstring &st,char mika,int mihin) {
  char s[2]; s[0]=mika; s[1]=0;
  return st.insert(mihin,s);
}

inline vstring &vaihda_jonot(vstring &st, const char *mika, const char *milla) {
  int len = strlen(st.c_str()) + 100;
  char *s = (char *)malloc(len);
  if ( s == NULL ) return st;
  vaihda_jonot(s,len,mika,milla);
  st = s;
  free(s);
  return st;
}

inline int muunna_C_symbolit(vstring &st) {
  char *s = tee_jono(st);
  int err = muunna_C_symbolit(s);
  st = s;
  free(s);
  return err;
}


//string erota(string &jono, char merkki=' ',bool etsi_takaperin=false);
//------------------------------------------------------------------------------
inline vstring erota(vstring &jono, char merkki=' ',bool etsi_takaperin=false)
// Erottaa jonosta valitun merkin kohdalta alkuosan ja loppuosan.
// Alkuosa palautetaan funktion nimessä ja loppuosa jätetään
// jonoon.  Merkin etsimissuunta voidana valita (oletuksena alusta päin).
// Jos merkkiä ei löydy, palautetaan koko jono ja tyhjennetään jono.
// Käyttöesimerkki: olkoon aluksi string jono,s;
//  1)  jono = "123 456";  s = erota(jono);   => jono == "456"  s == "123"
//  2)  jono = "123";      s = erota(jono);   => jono == ""     s == "123"
//  3)  jono = "1 2 3";
//      while ( jono != "" ) cout << erota(jono) << ",";  => tulostaa 1,2,3,
//
{
  size_t p;
  if ( !etsi_takaperin ) p = jono.find(merkki); else p = jono.rfind(merkki);
  vstring alku = jono.substr(0,p);
  if ( p == vstring::npos ) jono = "";
  else jono.erase(0,p+1);
  return alku;
}


vstring erota(vstring &jono, const vstring &merkit,bool etsi_takaperin=false);


// luvuksi funktiot muuttavat merkkijonon luvuksi.  Jos muuttaminen
// onnistuu, palautetaa true.  Jos ei, niin luvun arvoksi sijoitetaan def
// ja palautetaan false.  Jos oletusta (def) ei ole annettu, käytetään
// oletuksena 0:aa.
inline bool luvuksi(const vstring &jono, double &d, double def=0.0)
{
  d = def;
  return std::sscanf(jono.c_str(),"%lf",&d) == 1;
}

inline bool luvuksi(const vstring &jono, int &i, int def=0)
{
  i = def;
  return std::sscanf(jono.c_str(),"%d",&i) == 1;
}

inline bool luvuksi(const vstring &jono, long &i, long def=0)
{
  i = def;
  return std::sscanf(jono.c_str(),"%ld",&i) == 1;
}


#endif  /* __MJONOTPP_H */
