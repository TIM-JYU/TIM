/* dout.cpp  */
/****************************************************************************/
/*
**       D O U T . C P P
**
** Tietovirta, joka menee cout:iin mutta niin, että windows äät tulostetaan
** koodisivun 850 (OEM) mukaan.
**
**
**  Tekijät:          Vesa Lappalainen
**  Tehty:            17.02.2001
**  Muutettu:
**  Mitä muutettu:
**
**  Käyttö:
**  -------
**
** Vaihtoehto 1:
**  1) Lisää  #include "dout.h"  jokaisen sellaisen tiedoston alkuun, jossa
**     tarvitset dout -tietovirtaa.  Korvaa cout <<   dout <<
**  2) Samoin korvaa jokainen getline  dgetline-funktiolla
**  3) lisää projektiin dout.cpp
**
** Vaihtoehto 2:
**  1) Lisää  #include "dosout.h"  jokaisen sellaisen tiedoston alkuun, jossa
**     tarvitset cout -tietovirtaa.  Nämä muuttuvat nyt dout -tietovirraksi
**  2) Samoin jokainen getline muuttuu dgetline-funktioksi
**  3) lisää projektiin dout.cpp
**
** Vikoja:
**  - toteutus on puskuroimaton ja siten hieman hidas.
**
**
*****************************************************************************/

#include <iostream>
#include <string>
#include <string.h>
using namespace std;
#include "dout.h"

#ifdef EXTERNDOUT
// Ei tarvita, koska siirretty kaikki dout.h:hon

const char *OEMChars  = "™š†„”";
const char *ANSIChars = "ÅÄÖÜåäöü";

char vaihdaOEM(char c) {
  const char *p = strchr(ANSIChars,c);
  if ( p == 0 ) return c;
  int diff = (int)(p-ANSIChars);
  return *(OEMChars+diff);
}

char vaihdaAnsi(char c) {
  const char *p = strchr(OEMChars,c);
  if ( p == 0 ) return c;
  int diff = (int)(p-OEMChars);
  return *(ANSIChars+diff);
}

char *vaihdaOEMp(char *ps)
{
  char *p;
  for (p=ps;*p;p++) *p = vaihdaOEM(*p);
  return ps;
}

char *vaihdaAnsip(char *ps)
{
  char *p;
  for (p=ps;*p;p++) *p = vaihdaAnsi(*p);
  return ps;
}

istream &dgetline(istream &is,string &s,char delim)
{
  getline(is,s,delim);
  int len = s.length();
  for (int i=0; i<len; i++)
    s[i] = vaihdaAnsi(s[i]);
  return is;  
}


class dstreambuf : public streambuf {
public:
  int overflow(int c);
  dstreambuf() : streambuf() {;}
  ~dstreambuf();
};


dstreambuf::~dstreambuf()
{
  return;
}

int dstreambuf::overflow(int c)
{
  cout << vaihdaOEM(char(c));
  return 0;
}

::dstreambuf dbuf;

ostream dout(&dbuf);

#endif

#ifdef TESTI

int main(void)
{
  char i = '5';
  dout << i << '\n';
  dout << "Käki öisin möykkäilee!" << endl;
  dout << "ÅÄÖÜåäöü" << endl;
  return 0;
}

#endif
