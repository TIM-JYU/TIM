/* wout.cpp  */
/****************************************************************************/
/*
**       W O U T . C P P
**
** Tietovirta, joka menee console.c:n printscr-funktion mukaiseen paikkaan.
**
**
**  Tekij„t:          Vesa Lappalainen
**  Tehty:            07.01.1996
**  Muutettu:
**  Mit„ muutettu:
**
**  K„ytt”:
**  -------
**
**  0) M„„rittele vakio USE_CONSOLE
**  1) Lis„„  #include "wout.h"  jokaisen sellaisen tiedoston alkuun, jossa
**     tarvitset cout -tietovirtaa.  N„m„ muuttuvat nyt wout -tietovirraksi
**  2) Lis„„ projektiin wout.cpp ja console.c
**
** Vikoja:
**  - toteutus on puskuroimaton ja siten hieman hidas.
**
**
*****************************************************************************/

#include <iostream.h>
#ifndef TESTI
#include "console.h"
#else
#include <cstdio>

#endif
#include "wout.h"


class wstreambuf : public streambuf {
public:
  int overflow(int c);
  wstreambuf() : streambuf() {;}
  ~wstreambuf();
};


wstreambuf::~wstreambuf()
{
  return;
}

int wstreambuf::overflow(int c)
{
#ifndef TESTI
  char s[2];
  s[0] = char(c); s[1] = 0;
  printscr(s);
#else
  if ( ( c == '\n' ) ) printf("\r\n");
  else printf("%c",c);
#endif
  return 0;
}

::wstreambuf wbuf;

ostream wout(&wbuf);

#ifdef TESTI

int main(void)
{
  char i = '5';
  wout << i << '\n';
  wout << "Kissa istuu puussa!" << endl;
  return 0;
}

#endif
