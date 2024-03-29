/****************************************************************************/
/*     H  E  L  P  .  H
**
** Tekij�:         Vesa Lappalainen 6.12.1993
** Muutettu:
** Mit� muutettu:
**
** Tarkoituksena on, ett� avustukset voidaan kirjoittaa tiedostoon
** ohjelma.HLP, joka on muotoa:
**
**   [P��MENU]
**   ; Kommenttirivi jossa voi olla mit� vaan eik� tulostu
**   P��menusta voit valita ...
**   ...
**   [TULOSTUS]
**   Tulostus on ...
**   ...
**   # ; tarkoittaa n�yt�n pys�ytt�mist�.
**   Tulostus menee...
**   ...
**   [LIS�� UUSI]
**   Lis�t��n uusi ...
**   [t_ok]# ; jos hakasulun per�ss� on jokin merkki, ei t�m�
**   ;         otsikko l�ydy *-etsimisell�.
**   Tarkistus tekee sit� ja t�t�
**   [SIS�LLYS] ; T�m� olisi aina hyv� olla!
**   Sis�llys:
**   ...
**
**
** Avustustiedoston aihenimien "case" ei vaikuta, eli [P��Menu] == [P��MENU]
**
** Aliohjelmat:
**    alusta_help            - alustaa help-systeemin valitulle ohjelmalle
**    vapauta_help           - lopettaa helpin k�yt�n
**    help                   - tulostaa valitun help-tekstin jos
**                             aihe hakasuluissa.  Jollei ole, tulostetaan
**                             kutsuparametri sellaisenaan
**    help_aihe              - tulostaa valitun teksin.  Jos teksti
**                             ei ole hakasuluissa, lis�t��n ne
**    help_hakemisto         - helpin selailutilaan valitun aiheen kohdalta
**                             tarvittaessa lis�t��n aiheeseen hakasulut
**
**
** K�ytt�:
**   Ohjelmoijan on ennen helpin k�ytt�mist� kutsuttava (my�s avustus
**   tiedoston vaihtamiseksi):
**
**     alusta_help("Oma"); // alustaa help-tiedoston Oma.HLP
**
**   Kun helpi� ei en�� tarvita pit�� kutsua (uusi alusta_help vapauttaa
**   edellisen automaattisesti).
**
**     vapauta_help();
**
**   Kun halutaan avustusta esimerkiksi aiheesta TULOSTUS, kutsutaan
**
**     help("[TULOSTUS]");
**
**   Aihe voi olla my�s jokereita sis�lt�v� (esim. [TUL*]).  Jos aihe
**   on NULL, aloitetaan sis�llysluettelosta py�ritt�minen
**
**   Jotta helpi� olisi mahdollista k�ytt�� samanlaisena my�s ohjelman
**   sis�isille teksteille, voidaan kutsua
**
**     help("Valittava jotakin\n");
**
**   joka tulostaa helppim�isesti lainausmerkeiss� oleva tekstin.
**   Eli jos teksti ei ala [ tulostetaan teksti sellaisenaan.
**
**   Jos hakasulkuja ei haluta kirjoittaa aiheen ymp�rille kutsussa,
**   voidaan k�ytt�� muotoa
**
**     help_aihe("Tulostus"); // Lis�� tarvittaessa hakasulut
**
**   Helpissa voidaan k�ytt�� my�s hakuohjelmaa kutsulla
**
**     help_hakemisto("[TULOSTUS]"); // Aloittaa rivilaskurin alusta!
**
**   jolloin aloitetaan tulostamalla tulostksen ohjeet ja kysyt��n
**   hakusanaa.  Hakasulut lis�t��n tarvittaessa.
**
**   Jos tulee paljon help() - tulostuksia samaan kasaan, kannattaa
**   aluksi kutsua
**
**     alusta_help_rivit()
**
**   jotta pys�ytyslaskuri saadaan aloittamaan alusta.
**
**   C++ k�yt�ss� riitt�� kirjoittaa cHelp help("oma");
**   koska vapauta_help kutsutaan sitten automaattisesti helpin
**   h�vitt�j�ss�
*/
#ifndef HELP_H
#define HELP_H

#define HELP_STOP_JONO "#"
#define HELP_JONO      "?"

#ifdef __cplusplus
extern "C" {
#endif


int help(const char *);
int help_aihe(const char *);
int help_hakemisto(const char *);

int alusta_help(const char *);
void vapauta_help(void);

void alusta_help_rivit(void);

#ifdef __cplusplus
#include <string>
class cHelp {
public:
  cHelp(const char *s=NULL)           { if ( s != NULL ) alusta_help(s);  }
  cHelp(const std::string &s)         { alusta(s);                        }
  ~cHelp()                            { vapauta_help();                   }
  void alusta(const std::string &s)   { alusta_help(s.c_str());           }
  int help(const char *s)             { return ::help(s);                 }
  int help(const std::string &s)      { return ::help(s.c_str());         }
  int aihe(const char *s)             { return help_aihe(s);              }
  int aihe(const std::string &s)      { return help_aihe(s.c_str());      }
  int hakemisto(const char *s=NULL)   { return help_hakemisto(s);         }
  int hakemisto(const std::string &s) { return help_hakemisto(s.c_str()); }
  void alusta_rivit(void)             { alusta_help_rivit();              }
};

}
#endif

#endif
