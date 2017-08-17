/****************************************************************************/
/*     M  J  O  N  O  T .  H
**
**
**   Tiedostossa on merkkien ja merkkijonojen k‰sittelyohjelmien yleiset
**     vakiot
**     makrot
**     globaalit vakiotyyliset muuttujat
**     aliohjelmien otsikot
**
**   Tekij‰t:          Vesa Lappalainen
**   Tehty:            13.11.1991
**   Muutettu:         4.12.1993/vl
**   Mit‰ muutettu:    Lis‰tty const-m‰‰reit‰ kutsuihin, jotta
**                     aliohjelmia voidaan kutsua const-jonoilla
**                     Lis‰tty aliohjelmia.                                   -
**   Muutettu:         29.1.1994/vl
**   Mit‰ muutettu:    Korjattu wildmatin vika "" "*A" -> samat
**                     Lis‰tty poista_alusta ja lisaa_alkuun
**                     Lis‰tty mahdollisuus vaihtaa lue_jono-funktion
**                     k‰ytt‰m‰‰ kutsua (ks. alusta_lue_jono)                 -
**   Muutettu:         2.3.1994/vl
**   Mit‰ muutettu:    Onko samat korjattu muuttamaan isoiksi kirjaimiksi
**   Muutettu:         8.3.1994/vl                                            -
**   Mit‰ muutettu:    paikka palauttaa -1 jos merkki == 0
**                     lis‰tty aliohjelma lisaa_merkki
**   Muutettu:         17.12.1995/vl                                          -
**   Mit‰ muutettu:    arvosana toimimaan myˆs negatiivisilla arvoilla
**                     korjattu vika "´" -> 0
**                     Vakio USE_CONSOLE, jolla lue_jono_oletusta voidaan
**                     kielt‰‰ tulostamasta oletusarvoa, koska toivotaan
**                     lue_jonon tekev‰n t‰m‰n.                               -
**   Muutettu:         7.1.1996/vl
**   Mit‰ muutettu:    lis‰tty jonossa_vain_merkit,joku_jono_func
**                     poista_alku_ja_2_tyhjat, jono_1_isoksi
**                     makrot: KOPIOI ja LIITA
**   Muutettu:         17.2.2001/vl
**   Mit‰ muutettu:    isoksi ja pieneksi k‰sittelee sek‰ OEM ett‰ ANSI
**                     koodiston skandit => voi aiheuttaa yllatyksia
**                     OEM merkiston viivoissa ja sigmassa
** Muutettu:           28.12.2001/vl
** Mita muutettu:      Lisatty vakion DOSOUT tutkiminen.  Jos vakio maritelty
**                     muutetaan ANSI-merkit OEM-merkeiksi tulostuksessa.
**                     ja p‰invastoin lukemisessa.
**
**
*/
#ifndef MJONOT_H   /* Jottei haittaa vaikka k‰ytet‰‰n useita kertoja!       */
#define MJONOT_H
#ifdef __cplusplus
extern "C" {
#include <cstdio>
using namespace std;
#else
#include <stdio.h>
#endif

/****************************************************************************/
/*    V a k i o t                                                           */

/* Vakiot syˆtˆn onnistumiselle                                             */
#define  SYOTTO_OK       2
#define  EI_MAHDU        1
#define  OLETUS          0
#define  TIEDOSTO_LOPPU -1
#define  VIRHE_SYOTOSSA -2


/****************************************************************************/
/*    M a k r o t                                                           */

#define N_S(nimi) (nimi),(sizeof(nimi))

/*
** Makro N_S tuottaa muuttujan nimen ja koon , toisistaan erotettuna.
** sopii k‰ytett‰v‰ksi esim sellaisten aliohjelelmien kutsujen yhteydess‰
** jossa tarvitaan per‰kk‰in muuttujan nimi ja koko.
** Mik‰li muuttuja on osoite esim.
**   char p*;
**   p = malloc(50);
** k‰y huonosti, sill‰ makro palauttaa muistimallista riippuen
** joko p,2 tai p,4
**
** Sen sijaan m‰‰rittely
**   char jono[30];
** palauttaa  jono,30 kuten pit‰‰kin!
**
** Esim.
**   kopioi_jono(N_S(jono),"Kissa");    makrolla: KOPIOI(jono,"kissa")
**   liita_jono(N_S(jono)," istuu!");             LIITA(jono," istuu")
----------------------------------------------------------------------------*/

/****************************************************************************/
#define KOPIOI(mihin,mista) kopioi_jono(N_S(mihin),mista)
#define LIITA(mihin,mista)  liita_jono(N_S(mihin),mista)

/****************************************************************************/
/*    G l o b a a l i t   (vakio)muuttujat                                  */

extern const char *VALIMERKIT;     /* Merkit joiden j‰lkeen alkaa uusi sana.*/
extern const char *ARVOSANA_MERKIT;/* Merkit, jotka kelpaavat arvosanaan.   */
extern char TYHJA[128];

/****************************************************************************/
/*  A L I O H J E L M A T                                                   */
/*
**
**  Tiedostossa on seuraavia aliohjelmia:
**    tee_jono               - luo uuden merkkijonon jonne jono kopioidaan    -
**    kopioi_jono            - kopioi kork. annetun m‰‰r‰n merkkej‰           -
**    liita_jono             - liitt‰‰ jonon toisen per‰‰n, tulos
**                             korkeintaan max.pituus mittainen               -
**    f_lue_jono             - lukee tiedostosta merkkijonon                  -
**    alusta_lue_jono        - alustaa merkkijonon lukemisen                  -
**    lue_jono               - lukee p‰‰tteelt‰ merkkijonon                   -
**    lue_jono_oletus        - lukee p‰‰tteelt‰ merkkijonon.
**                             N‰yttˆˆn tulostetaan haluttu viesti ja
**                             jonon oletusarvo, mik‰li painetaan RET         -
**    lue_kokluku_oletus     - luetaan kokonaisluku, jolle k‰ytet‰‰n
**                             oletusarvoa mik‰li heti painetaan RET          -
**    poista_alkutyhjat      - poistaa merkkijonon alussa olevat v‰lilyˆnnit  -
**    poista_lopputyhjat     - poistaa merkkijonon lopussa olevat v‰lil.      -
**    poista_2_tyhjat        - muuttaa merkkijonossa kaikki per‰kk‰iset
**                             v‰lilyˆnnit yhdeksi v‰lilyˆnniksi              -
**    poista_tyhjat          - poistaa alusta ja lopusta kaikki sek‰
**                             muualta moninkertaiset v‰lilyˆnnit             -
**    poista_alku_ja_2_tyhjat- poistaa alusta ja 2x tyhj‰t                    -
**    isoksi                 - muuttaa kirjaimen isoksi kirjaimeksi huomioiden
**                             skandit                                        -
**    pieneksi               - muuttaa pieneksi huomioiden skandit            -
**    jono_isoksi            - muuttaa jonon kaikki merkit isoiksi            -
**    jono_pieneksi          - muuttaa jonon kaikki merkit pieniksi           -
**    jono_alku_isoksi       - muuttaa jonon kaikki sanojen alut isoiksi
**                             ja kaikki muut pieniksi                        -
**    jono_1_isoksi          - muuttaa jonon 1. kirjaimen isoksi              -
**    wildmat                - vertaa onko sana == maski, miss‰ maskissa
**                             voi olla jokeri-merkkej‰ (* tai ?)             -
**    onko_samat             - ensin muutetaan jonot isoiksi ja poistetaan
**                             tyhj‰t ja sitten wildmat
**                             (eli "  Kalle " == "    k*  ")                 -
**    palanen                - ottaa merkkijonosta seuraavan erotinmerkkien
**                             m‰‰r‰‰m‰n palasen                              -
**    laske_merkit           - laskee annettujen merkkien esiintymism‰‰r‰n
**                             merkkijonossa                                  -
**    paikka                 - palauttaa kirjaimen 1. indeksin merkkijonossa
**    tayta_valit            - t‰ytt‰‰ syˆtˆn "A-F" muotoon "ABCDEF"          -
**    joku_jono              - vastaa kysymykseen onko "EY" joku jonoista
**                             "EU|EY|EL"                                     -
**    joku_jono_func         - kuten edell‰, mutta vertailufunktio voidaan
**                             antaa itse, esim
**                               joku_jono("EU","E?|USA",wildmat) => 1        -
**    jono_arvosanaksi       - muuttaa merkkijonon "7-" reaaliluvuksi 6.75    -
**    arvosana_jonoksi       - muuttaa reaaliluvun 6.75 merkkijonoki "7-"     -
**    sallituissa            - paluttaa -1 jos tutkittavan jonon kaikki
**                             merkit ovat annetussa joukossa, muuten
**                             1. v‰‰r‰n merkin indeksin                      -
**    poista_merkit          - poistaa jonosta kaikki valitut merkit          -
**    poista_alusta          - poistaa merkkej‰ jonon alusta                  -
**    lisaa_alkuun           - lis‰‰ merkkej‰ jonon alkuun                    -
**    lisaa_merkki           - lis‰‰ merkin merkkijonon valittuun kohtaan     -
**    vaihda_jonot           - vaihtaa jonossa merkit toisiksi                -
**    muunna_C_symbolit      - muuttaa \t, \n ja \0x61 muotoiset              -
**                             C-symbolit vastaaviksi merkeiksi               -
**    jonossa_vain_merkit    - poistaa jonosta kaikki ne merkit, jotka
**                             eiv‰t ole valitussa joukossa                   -
**
** Tarkempi kuvaus kustakin aliohjelmasta lˆytyy tiedostosta mjonot.c.
**
** Kaikissa aliohjelmissa on varauduttu siihen, ett‰ niit‰ kutsutaan
** NULL osoittimella ja kukin niist‰  yritt‰‰ toimia t‰llˆin jotenkin
** j‰rkev‰sti.
**
*/

typedef int   (* lue_jono_tyyppi)(char *,int);
typedef int   (* str_2_fun)(const char *s1,const char *s2);
typedef char *(* muunnos_funktio)(char *);

char *tee_jono(const char *);
int   kopioi_jono(char *, int, const char *);
int   liita_jono(char *, int, const char *);

int   f_lue_jono(FILE *, char *, int);
lue_jono_tyyppi alusta_lue_jono(lue_jono_tyyppi);
int   lue_jono(char *, int);
int   lue_jono_oletus(const char *, int, int, const char *, char *, int);
int   lue_kokluku_oletus(int, int *);

char *poista_alkutyhjat(char *);
char *poista_lopputyhjat(char *);
char *poista_2_tyhjat(char *);
char *poista_tyhjat(char *);
char *poista_alku_ja_2_tyhjat(char *);

char  isoksi(char);
char  pieneksi(char);
char *jono_pieneksi(char *);
char *jono_isoksi(char *);
char *jono_alku_isoksi(char *);
char *jono_1_isoksi(char *);

int   wildmat(register const char *, register const char *);
int   onko_samat(const char *, const char *);

char *palanen(char *, const char *, int *);
int   laske_merkit(const char *,const char *);
int   paikka(const char *,char );
char *tayta_valit(char *,int ,const char *,const char *);

int   joku_jono(const char *,const char *,const char *);
int   joku_jono_func(const char *,const char *,const char *,str_2_fun);

double jono_arvosanaksi(const char *);
char *arvosana_jonoksi(double,char *);

int   sallituissa(const char *,const char *);
char *poista_merkit(char *,const char *);
char *poista_alusta(char *, int);
int   lisaa_alkuun(char *, int, const char *);
char *lisaa_merkki(char *,int ,char ,int);
char *vaihda_merkit(char *s,int n,char mika, char miksi);
char *vaihda_jonot(char *p, int n, const char *mika, const char *milla);
char CPrefix(char **p);
int muunna_C_symbolit(char *s);
char *jonossa_vain_merkit(char *s,const char *merkit);

#ifdef __cplusplus
}
#endif
#endif /* MJONOT_H */
