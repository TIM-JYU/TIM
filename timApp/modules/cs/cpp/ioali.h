/* ioali.h */
/****************************************************************************/
/*
** I O A L I . H
**
**
** Yleisk„ytt”isi„ IO-rutiineja mm. Kerho-ohjelman k„ytt””n
** Lis„kommentit ks. IOALI.C
**
**  Tekij„t:          Vesa Lappalainen
**                    Ohjelmointikurssi 1991-
**  Tehty:            20.12.1991
**  Muutettu:         15.12.1995/vl
**  Mit„ muutettu:    + siirrettty omaksi yleisk„ytt”iseksi kirjoistoksi
**  Muutettu:         28.12.2001/vl
**  Mit„ muutettu:    + lisatty inlinena C++ versiot
**
**
*****************************************************************************/

#ifndef __IOALI_H
#define __IOALI_H

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------------------------------------------------------
** Vakiota:
----------------------------------------------------------------------------*/
#define MERKKI_ISOKSI   1
#define MYOS_PIENET     0
#define KAIKKI_KELPAA   NULL
#define EI_OLETUSTA     '\0'
#define RIVINVAIHTO     "\r\n"


char lue_merkki(void);
char odota_nappain(const char *kelpaavat, char oletus,int merkki_isoksi);
void odota_jotakin(void);
void ei_toimi(void);
int kylla_vastaus(void);
void viiva(int pit, char merkki);
void viivarivi(int pit, char merkki);
void otsikko(char c,const char *teksti);
int ilmoitus(const char *s);
int kylla_kysymys(const char *s);
int onko_tiedostoa(const char *nimi);


#ifdef __cplusplus
}
#include <string>
inline char odota_nappain(const string &kelpaavat, char oletus,int merkki_isoksi) {
  if ( kelpaavat == "" ) return odota_nappain(NULL,oletus,merkki_isoksi);
  else return odota_nappain(kelpaavat.c_str(),oletus,merkki_isoksi);
}

inline void otsikko(char c,const string &teksti) {
  otsikko(c,teksti.c_str());
}

inline int ilmoitus(const std::string &s) {
  if ( s == "" ) return 0;
  return ilmoitus(s.c_str());
}

inline int kylla_kysymys(const string &s) {
  return kylla_kysymys(s.c_str());
}

inline int onko_tiedostoa(const string &nimi) {
  return onko_tiedostoa(nimi.c_str());
}
#endif


#endif
