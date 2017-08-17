/****************************************************************************/
/*     H  E  L  P  .  H
**
** Tekijä:         Vesa Lappalainen 6.12.1993
** Muutettu:
** Mitä muutettu:
**
** Tarkoituksena on, että avustukset voidaan kirjoittaa tiedostoon
** ohjelma.HLP, joka on muotoa:
**
**   [PÄÄMENU]
**   ; Kommenttirivi jossa voi olla mitä vaan eikä tulostu
**   Päämenusta voit valita ...
**   ...
**   [TULOSTUS]
**   Tulostus on ...
**   ...
**   # ; tarkoittaa näytön pysäyttämistä.
**   Tulostus menee...
**   ...
**   [LISÄÄ UUSI]
**   Lisätään uusi ...
**   [t_ok]# ; jos hakasulun perässä on jokin merkki, ei tämä
**   ;         otsikko löydy *-etsimisellä.
**   Tarkistus tekee sitä ja tätä
**   [SISÄLLYS] ; Tämä olisi aina hyvä olla!
**   Sisällys:
**   ...
**
**
** Avustustiedoston aihenimien "case" ei vaikuta, eli [PääMenu] == [PÄÄMENU]
**
** Aliohjelmat:
**    alusta_help            - alustaa help-systeemin valitulle ohjelmalle
**    vapauta_help           - lopettaa helpin käytön
**    help                   - tulostaa valitun help-tekstin jos
**                             aihe hakasuluissa.  Jollei ole, tulostetaan
**                             kutsuparametri sellaisenaan
**    help_aihe              - tulostaa valitun teksin.  Jos teksti
**                             ei ole hakasuluissa, lisätään ne
**    help_hakemisto         - helpin selailutilaan valitun aiheen kohdalta
**                             tarvittaessa lisätään aiheeseen hakasulut
**
**
** Käyttö:
**   Ohjelmoijan on ennen helpin käyttämistä kutsuttava (myös avustus
**   tiedoston vaihtamiseksi):
**
**     alusta_help("Oma"); // alustaa help-tiedoston Oma.HLP
**
**   Kun helpiä ei enää tarvita pitää kutsua (uusi alusta_help vapauttaa
**   edellisen automaattisesti).
**
**     vapauta_help();
**
**   Kun halutaan avustusta esimerkiksi aiheesta TULOSTUS, kutsutaan
**
**     help("[TULOSTUS]");
**
**   Aihe voi olla myös jokereita sisältävä (esim. [TUL*]).  Jos aihe
**   on NULL, aloitetaan sisällysluettelosta pyörittäminen
**
**   Jotta helpiä olisi mahdollista käyttää samanlaisena myös ohjelman
**   sisäisille teksteille, voidaan kutsua
**
**     help("Valittava jotakin\n");
**
**   joka tulostaa helppimäisesti lainausmerkeissä oleva tekstin.
**   Eli jos teksti ei ala [ tulostetaan teksti sellaisenaan.
**
**   Jos hakasulkuja ei haluta kirjoittaa aiheen ympärille kutsussa,
**   voidaan käyttää muotoa
**
**     help_aihe("Tulostus"); // Lisää tarvittaessa hakasulut
**
**   Helpissa voidaan käyttää myös hakuohjelmaa kutsulla
**
**     help_hakemisto("[TULOSTUS]"); // Aloittaa rivilaskurin alusta!
**
**   jolloin aloitetaan tulostamalla tulostksen ohjeet ja kysytään
**   hakusanaa.  Hakasulut lisätään tarvittaessa.
**
**   Jos tulee paljon help() - tulostuksia samaan kasaan, kannattaa
**   aluksi kutsua
**
**     alusta_help_rivit()
**
**   jotta pysäytyslaskuri saadaan aloittamaan alusta.
**
**   C++ käytössä riittää kirjoittaa cHelp help("oma");
**   koska vapauta_help kutsutaan sitten automaattisesti helpin
**   hävittäjässä
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
