/* dout.h  */
/****************************************************************************/
/*
**       D O U T . H
**
** Tietovirta, joka menee cout:iin mutta niin, ett‰ windows ‰‰t tulostetaan
** koodisivun 850 (OEM) mukaan.
**
**  Tekij‰t:          Vesa Lappalainen
**  Tehty:            17.02.2001
**  Komentit ja p‰ivityshistoria ks. dout.cpp
**
*****************************************************************************/

#ifndef __DOUT_H
#define __DOUT_H


#ifdef EXTERNDOUT
#ifdef __cplusplus
#include <iostream>
extern std::ostream dout;
std::istream &dgetline(std::istream &is,std::string &s,char delim='\n');
extern "C" {
#endif
char vaihdaOEM(char c);
char vaihdaAnsi(char c);
char *vaihdaOEMp(char *p);
char *vaihdaAnsip(char *p);
#ifdef __cplusplus
}
#endif

#else /*  EXTERNDOUT */

#if ( __TURBOC__ == 0x0550 )
#pragma warn -8058 // Builderin precompiled-varoitus pois p‰‰lt‰
#endif
#if ( __TURBOC__ == 0x0560 )
#pragma warn -8058 // Builderin precompiled-varoitus pois p‰‰lt‰
#endif

static const char *OEMChars  = "èéôöÜÑîÅ";
static const char *ANSIChars = "≈ƒ÷‹Â‰ˆ¸";
static const char *_p;
#define vaihdaOEM(c) ( (_p=strchr(ANSIChars,(c)) )!=NULL ? OEMChars[(int)(_p-ANSIChars)] : c )
#define vaihdaAnsi(c) ( (_p=strchr(OEMChars,(c)) )!=NULL ? ANSIChars[(int)(_p-OEMChars)] : c )
#define vaihdaOEMp(p) { int _i; for (_i=0; p[_i]; ++_i) p[_i] = vaihdaOEM(p[_i]); }
#define vaihdaANSIp(p) { int _i; for (_i=0; p[_i]; ++_i) p[_i] = vaihdaAnsi(p[_i]); }

#ifdef __cplusplus
#include <iostream>
#include <string>

#if ( __BCPLUSPLUS__ == 0x0310 )
#pragma warn -inl
#endif
static std::istream &dgetline(std::istream &is,std::string &s,char delim='\n')
{
#ifdef getline
#undef getline
  getline(is,s,delim);
#define getline dgetline
#else
  getline(is,s,delim);
#endif
  int len = s.length();
  for (int i=0; i<len; i++)
    s[i] = vaihdaAnsi(s[i]);
  return is;
}

#include <cstdio>
class dstreambuf : public std::streambuf {
  public:
  int overflow(int c) {
#ifdef cout
//#undef cout
//    std::cout << vaihdaOEM(char(c));
  std::printf("%c",vaihdaOEM(char(c)));
#define cout dout
#else
    std::cout << vaihdaOEM(char(c));
#endif
    if ( c > 9999 ) { std::string s; dgetline(std::cin,s); } // dummy to fool compiler
    return 0;
  }
  dstreambuf() : std::streambuf() {  }
};

static ::dstreambuf dbuf;

static std::ostream dout(&dbuf);
#endif /*  __cplusplus */

#endif /*  EXTERNDOUT */


#endif /* __DOUT_H    */



