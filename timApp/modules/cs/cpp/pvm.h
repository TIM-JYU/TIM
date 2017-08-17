/* pvm.h */
/****************************************************************************/
/*
**        P  V  M . H
**
**
**  Tiedosto sis„lt„„ p„iv„m„„r„n k„sittelyyn liittyvien aliohjelmien
**  tarvitsemat yleiset
**    vakiot
**    tietotyypit
**    globaalit vakiotyyliset muuttujat
**    aliohjelmien otsikot
**
**  Tekij„t:          Vesa Lappalainen
**                    Ohjelmointikurssi 1991
**  Tehty:            29.10.1991
**  Muutettu:         22.11.1991/vl
**  Mit„ muutettu:    muutettu aliohjelmakirjastoksi
**
*/

#ifndef PVM_H
#define PVM_H

#ifdef __cplusplus
extern "C" {
#endif


/****************************************************************************/
/*    
**      V a k i o t                                                           
*/

/****************************************************************************/
/*    
**      T i e t o t y y p i t                                                 
**                                                                          
*/                                                                          
typedef struct {
  int pv;
  int kk;
  int vv;
  int viikon_paiva; /* 0=su, 1=ma, 2... 6=la */
  int vko;          /* Vuoden 1. on joko 0 (jos alk. pe,la,su) tai 1 */
  int pv_nro;       /* P„iv„n numero vuoden alusta laskien 1.1 = 1.  */
} Pvm_tyyppi;



/****************************************************************************/
/*    
**      G l o b a a l i t (vakio!) muuttujat                                  
*/
extern char *VIIKON_PAIVAT[]; /* Viikonp„ivien nimet suomeksi               */
extern char *KUUKAUDET[];     /* Kuukausien nimet suomeksi                  */


/****************************************************************************/
/*  
**      A L I O H J E L M A T                                                   
**
** Aliohjelmia on seuraavissa tiedostoissa:
**  viikko.c     - yleisi„ aliohjelmia p„iv„m„„rien k„sittelyyn
**    karkausvuosi           - tarkistaa onko annettu vuosi karkausvuosi
**    tulosta_pvm            - tulostaa pvm muodossa pp.kk.vvvv
**    selvita_1              - palauttaa vuoden 1. p„iv„n viikon p„iv„n
**    tarkista_pvm           - tarkistaa pvm oikeellisuuden
**    paivan_numero          - palauttaa pvm:st„ p„iv„n nro vuoden alusta
**    muuta_viikoksi         - muuttaa pvm:n muotoon viikko viikonp„iv„
**    muuta_pvmksi           - muuttaa muodon viikko viikonp„iv„ pvmksi
**    selvita_nyky_pvm       - ottaa nykyp„iv„n
**    kysy_pvm               - kysyy p„iv„m„„r„n
**    tarkista_sotu_pvm      - tarkistaa sotun p„iv„m„„r„n
*/
int karkausvuosi(int);
void tulosta_pvm(Pvm_tyyppi *);
int selvita_1(int);
int tarkista_pvm(Pvm_tyyppi *);
int paivan_numero(Pvm_tyyppi *);
int muuta_viikoksi(Pvm_tyyppi *);
int muuta_pvmksi(Pvm_tyyppi *);
void selvita_nyky_pvm(Pvm_tyyppi *);
int kysy_pvm(Pvm_tyyppi *, Pvm_tyyppi *);
int tarkista_sotu_pvm(const char *);

#ifdef __cplusplus
}
#endif



#endif
