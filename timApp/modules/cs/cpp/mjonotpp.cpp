/* mjonotpp.cpp */
/****************************************************************************/
/*     M  J  O  N  O  T  P  P.  C P P
**
**
**   Tiedostossa on merkkien ja merkkijonojen käsittelyohjelmien yleiset
**   aliohjelmien otsikot.  Tiedosto on lähinnä muunnostiedosto string-
**   tyypille vastaavasta C-kirjastosta mjonot.c
**
**   Tekijät:          Vesa Lappalainen
**   Tehty:            02.01.1996
**
**   Kommentit ks. mjonot.c ja mjonotpp.h
**
*****************************************************************************/

#include <string>
#include <stdio.h>
#include <iostream>
#include "mjonot.h"
#include "mjonotpp.h"

//----------------------------------------------------------------------------
int tarkennin_alku(const vstring &tied)
// Palauttaa tarkentimen alkupaikan tai -1
// Muistettava että hakemistonkin nimessa voi olla .
// Esim: oma.dat => 3
//       0123456
//       c:\oma.1\oma => -1
{
  size_t i = tied.find_last_of(":.\\"); // Mikä loytyy viimeisenä?
  if ( i == vstring::npos ) return -1;  // Jollei mikaan niin ei ole
  if ( tied[i] == '.' ) return i;       // Jos viimeinen oli . niin löytyi
  return -1;                            // Muuten ei loytynyt
}

//----------------------------------------------------------------------------
vstring &poista_tarkennin(vstring &tied)
// Poistaa mahdollisen tarkentimen
// Esim: oma.dat = oma
{
  int i = tarkennin_alku(tied);
  if ( i < 0 ) return tied;
  return tied.erase(i);
}

//----------------------------------------------------------------------------
vstring &laita_tarkennin(vstring &tied,const vstring &tark)
// Lisää tarkentimen, mikäli sitä ei vielä ole
// Esim: oma.dat  .bak   => oma.dat
//       oma      .bak   => oma.bak
{
  int i = tarkennin_alku(tied);
  if ( i >= 0 ) return tied;
  return tied += tark;
}

//----------------------------------------------------------------------------
vstring &vaihda_tarkennin(vstring &tied,const vstring &tark)
// Vaihtaa tarkentimen
// Esim: oma.dat  .bak   => oma.bak
//       oma      .bak   => oma.bak
{
  return poista_tarkennin(tied) += tark;
}

//----------------------------------------------------------------------------
std::istream &lue_rivi(std::istream &is,char *s,int max_koko)
{
#ifdef USE_CONSOLE
  if ( &is == &std::cin ) {
//  if ( is == cin ) {
    lue_jono(s,max_koko);
    return is;
  }
#endif
  is.getline(s,max_koko,'\n');
  return is;
}

//----------------------------------------------------------------------------
std::istream &lue_rivi(std::istream &is,vstring &st)
{
  char s[400]="";
  kopioi_jono(N_S(s),st.c_str());
  lue_rivi(is,s,sizeof(s));
  st = s;
  return is;
}

//----------------------------------------------------------------------------
std::istream &lue_rivi(std::istream &is,int &i,int def)
{
  char s[400]="";
  sprintf(s,"%d",def);
  lue_rivi(is,s,sizeof(s));
  i = def;
  sscanf(s,"%d",&i);
  return is;
}

//----------------------------------------------------------------------------
std::istream &lue_rivi(std::istream &is,double &d,double def)
{
  char s[400]="";
  sprintf(s,"%f",def);
  lue_rivi(is,s,sizeof(s));
  d = def;
  sscanf(s,"%lf",&d);
  return is;
}


/*
//------------------------------------------------------------------------------
vstring erota(vstring &jono, char merkki,bool etsi_takaperin)
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
*/

//------------------------------------------------------------------------------
vstring erota(vstring &jono, const vstring &merkit,bool etsi_takaperin)
// Erottaa jonosta valittujen merkkien kohdalta alkuosan ja loppuosan.
// Alkuosa palautetaan funktion nimessä ja loppuosa jätetään
// jonoon.  Merkin etsimissuunta voidana valita (oletuksena alusta päin).
// Jos merkkiä ei löydy, palautetaan koko jono ja tyhjennetään jono.
// Käyttöesimerkki: olkoon aluksi string jono,s;
//  1)  jono = "123 456";  s = erota(jono," ");   => jono == "456"  s == "123"
//  2)  jono = "123";      s = erota(jono,";");   => jono == ""     s == "123"
//  3)  jono = "1;2 3";    s = erota(jono," ;");  => jono == "2 3"  s == "1"
//
{
  size_t p;
  if ( !etsi_takaperin ) p = jono.find_first_of(merkit);
  else                   p = jono.find_last_of(merkit);
  vstring alku = jono.substr(0,p);
  if ( p == vstring::npos ) jono = "";
  else jono.erase(0,p+1);
  return alku;
}

//------------------------------------------------------------------------------
// lue_jono, joka kayttaa getline-metodia
int lue_jono_getline(char *s,int n)
{
  std::string st;
  if ( s == 0 || n <= 0 ) return -2;
  s[0] = 0;
  if ( !getline(std::cin,st) ) return -1;
  kopioi_jono(s,n,st.c_str());
  if ( st.length() >= (unsigned)n ) return 1;
  if ( st.length() == 0 ) return 0;
  return 2;
}

class cLueJono {
  public:
    cLueJono(lue_jono_tyyppi f) { alusta_lue_jono(f); }
};

// Talla globaalilla oliolla saadaan alustus tehtya automaattisesti
cLueJono globaali_luejono_alustus(lue_jono_getline);

