// editluep.h
/****************************************************************************/
/*
**        E D I T L U E P . H
**
** Apualiohjelmia console.c:n helppoon k„ytt””nottoon C++:ssa
** T„m„ tiedosto muuttaa mjonot.c:n lue_jono:n ja lue_jono_oletus
** k„ytt„m„„n editoivaa versiota EditString.
** T„ss„ on my”s toteutus omien lyhenteitten k„ytt””n (ks lyhenne.c)
** Katso tarkemmin editlue.c.
**
**  Tekij„t:          Vesa Lappalainen
**                    Ohjelmointikurssi 1996-
**  Tehty:            13.03.1996
**  Muutettu:
**
**  K„ytt”:
**   0) Projektiin tarvittaan lis„ksi
**        C\ALI\console.c
**        C\ALI\mjonot.
**        C\ALI\lyhenne.c
**        C\ALI\editlue.c
**        C\ALI\editluep.cpp   - jos lyhennetiedostoksi kelpaa lyhenne.lyh
**        C\ALI\wout.cpp       - jotta cout menee console.c:n kautta
**
**      Ennen k„„nn”st„ m„„ritell„„n vakio USE_CONSOLE.
**      Mik„li ko. vakiota ei ole m„„ritelty, ei t„m„n tiedoston
**      aliohjelmat tee mit„„n, eik„ em. tiedostoja tarvitse
**
**   0) #include "wout.h" niissa tiedostoissa joissa cout halutaan muuttaa
**   1) #include "editluep.h" ja
**      ohjelmassa maaritellaan jossakin olio
**        cEditLue editlue(lyh);
**      miss„ lyh on mahdollisen lyhennetiedoston nimi.
** tai
**   1) liitetaan editluep.cpp projektiin, jolloin ei tule lyhennetiedostoa
*/

#ifndef __EDITLUEP_H
#define __EDITLUEP_H
#include <string.h>
#include "editlue.h"
#include "wout.h"
#ifdef USE_CONSOLE
class cEditLue {
  char *lyh;
public:
  cEditLue(const char *alyh=NULL) {
    lyh = NULL;
    if ( alyh ) lyh = new char[strlen(alyh)+1];
    if ( lyh ) strcpy(lyh,alyh);
    edit_alusta(lyh);
  }
  ~cEditLue() { edit_vapauta(lyh); if ( lyh ) delete [] lyh;}
};
#endif /* USE_CONSOLE  */
#endif /* __EDITLUEP_H */
