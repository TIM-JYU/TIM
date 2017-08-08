/****************************************************************************/
/*  
**        M J O N O T . C
**
** Yleisi‰ merkkijonojen k‰sittelyyn liittyvi‰ aliohjelmia.               
**
**   Tekij‰t:          Vesa Lappalainen
**   Tehty:            13.11.1991
**   Muutoshistoria ks. mjonot.h
**
** Aliohjelmat:
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
**    paikka                 - palauttaa kirjaimen 1. indeksin merkkijonossa  -
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
**    vaihda_jonot           - vaihtaa jonossa merkit toisiksi
**                               ^ - rivin alussa tarkoittaa, etta
**                                   vaihto tehdaan vain rivin alusta         -
**    vaihda_merkit          - vaihtaa jonossa yksitt‰iset merkit
**                             toisiksi                                       -
**    muunna_C_symbolit      - muuttaa \t, \n ja \0x61 muotoiset
**                             C-symbolit vastaaviksi merkeiksi               -
**    jonossa_vain_merkit   - poistaa jonosta kaikki ne merkit, jotka
**                             eiv‰t ole valitussa joukossa                   -
**
** Kaikissa aliohjelmissa on varauduttu siihen, ett‰ niit‰ kutsutaan
** NULL osoittimella ja kukin niist‰  yritt‰‰ toimia t‰llˆin jotenkin
** j‰rkev‰sti.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "mjonot.h"


const char *VALIMERKIT=" .,-;:?!";
char TYHJAMJONO[128]="";

/****************************************************************************/
char                      /* = jonon kopion osoite                          */
*tee_jono(                /* NULL = ei voida kopioida                       */
  const char *jono        /* s   Kopioitava merkkijono                      */
)
/*
** Funktiolla varataan jonon kokoinen uusi tila ja jono kopioidaan sinne.
** Palautetaan uuden jonon osoite.
**
** Kutsuu:    malloc
----------------------------------------------------------------------------*/
{
  char *uusi;
  if ( jono == NULL ) return NULL;
  if ( (uusi = malloc(strlen(jono)+1)) == NULL ) return NULL;
  return (strcpy(uusi,jono));
}


/****************************************************************************/
int                       /*                                                */
kopioi_jono(              /* 0 = onnistui, muu = kaikki ei mahtunut         */
  char *tulos            ,/* t   Jono jonne kopioidaan                      */
  int  max_koko          ,/* s   Tuloksen maksimikoko                       */
  const char *jono        /* s   Kopioitava merkkijono                      */
)
/*
** Funktiolla kopioidaan merkkijono.  Mik‰li jono ei mahdu sille
** varattuun tilaan, katkaistaan jono.
**
** Kutsuu:    strncpy
----------------------------------------------------------------------------*/
{ /*
  strncpy(tulos,jono,max_koko);
  if ( strlen(jono)>=max_koko ) {
    tulos[max_koko-1]=0;
    return 1;
  }
  return 0;
  */
  int i;
  const char *p=jono;
  tulos[0] = 0;
  if ( jono == NULL ) return 0;
  for (i=0; *p && i<max_koko-1; i++, p++)
    tulos[i] = *p;
  tulos[i] = 0;
  return (*p);
}


/****************************************************************************/
int                       /*                                                */
liita_jono(               /* 0 = onnistui, 1 = kaikki ei mahtunut           */
  char *tulos            ,/* s,t Jono jonne liitet‰‰n                       */
  int  max_koko          ,/* s   Tuloksen maksimikoko                       */
  const char *jono        /* s   Kopioitava merkkijono                      */
)
/*
** Funktiolla liitet‰‰n  merkkijono tuloksen per‰‰n.  Mik‰li jono ei mahdu
** sille varattuun tilaan, katkaistaan jono.
----------------------------------------------------------------------------*/
{
  int i; const char *p;
  if ( jono == NULL ) return 0;
  for (i=strlen(tulos), p=jono; i<max_koko-1; i++, p++) {
    if ( !(tulos[i]=*p) ) break;
  }
  tulos[i]=0;
  return (*p != 0);
}


/****************************************************************************/
int                       /*  2 = jono syˆtettiin                           */
f_lue_jono(               /*  1 = jono ei mahtunut                          */
                          /*  0 = painettiin heti RETURN                    */
                          /* -1 = <EOF>                                     */
                          /* -2 = virhe syˆtˆss‰                            */
  FILE *f,                /* s   Tiedosto josta luetaan                     */
  char *jono,             /* t   Merkkijono johon luetaan                   */
  int  max_pituus         /* s   Luettavien merk. max.lkm+1 (tila \0)       */
)
/*
** Funktiolla luetaan tiedoston yhdelt‰ rivilt‰ korkeintaan
** max_pituus -mittainen merkkijono.  Jonoon ei tule \n -merkki‰.
** Tiedoston ollessa loppu palautetaan -1 mutta itse jonoon ei kosketa.
** Mik‰li rivill‰ ei yht‰‰n merkki‰, palautetaan 0 samoin kuin jonona.
** Rivin loppuosa, joka ei mahdu jonoon h‰vitet‰‰n.
**
** Syˆttˆ:    tiedostosta
** Tulostus:  n‰ytˆlle jos stdin
** Tekij‰:    Vesa Lappalainen
** Pvm:       9.10.1991
** Esimerkki: Kutsu f_lue_jono(f,jono,8)
**   Tiedostossa rivit:                     palauttaa
**     1234567890123456789              jonon pituus f_lue_jono   jono
**     Kissa puussa                          7          1         Kissa p\0
**     istuu                                 5          2         istuu\0
**                                           0          0         \0
**     ja ihmettelee.                        7          1         ja ihme\0
**     Joo!<EOF>                             4          2         Joo!\0
**- seuraava kutsu                           4         -1         Joo!\0
----------------------------------------------------------------------------*/
{
  int jonon_pituus,c;

  if ( jono == NULL ) return VIRHE_SYOTOSSA;

  if ( feof(f) ) return TIEDOSTO_LOPPU;

  if ( fgets(jono,max_pituus,f)==NULL ) return VIRHE_SYOTOSSA;

  jonon_pituus = strlen(jono);
  if ( jono[jonon_pituus-1]=='\n' ) { /* Poistetaan mahd. \n                */
    jonon_pituus--;
    jono[jonon_pituus]=0;

    if ( jonon_pituus==0 ) return OLETUS;
    return SYOTTO_OK;
  }

  /* Erikoistapaukset:                                                      */
  /* Jos jono loppui ja tiedosto samalla, ei tietenk‰‰n ole \n ja mahtui    */
  if ( feof(f) ) return SYOTTO_OK;
  /* Jos jonosta loppui tila samalla kun vain \n j‰i lukematta              */
  c=fgetc(f); if (c=='\n') return SYOTTO_OK;

  while ( ((c=fgetc(f))!=EOF) && (c!='\n') );    /* Poistetaan rivin loppu. */

  return EI_MAHDU;
}


static lue_jono_tyyppi lue_jono_funk = NULL;

/****************************************************************************/
lue_jono_tyyppi           /* Edellisen lukufunktion osoite                  */
alusta_lue_jono(          /*                                                */
  lue_jono_tyyppi uusi    /* s   Uuden lukufunktion osoite                  */
)
/*
** Funktiolla voidaan vaihtaa lue_jono-funktion k‰ytt‰m‰n funktion osoitetta
** Lukufunktion t‰ytyy paluttaa vastaavia arvoja kuin f_lue_jono
** ja sen parametrilistassa on jono ja sen maksimipituus (kuten lue_jono).
** Jos esimerkiksi halutaan k‰ytt‰‰ lyhenteit‰ k‰ytt‰v‰‰ lukufunktiota,
** kutsutaan jossakin kohti ohjelmaa (ennenkuin muutosta tarvitaan):
**   alusta_lue_jono(lue_lyhenne_jono);
**
** Jos v‰lill‰ halutaan lukea ehdottomasti alkuper‰isell‰ lue_jono
** funktion toteutuksella, kutustaan esim:
**   vanha = alusta_lue_jono(NULL);
**   paluu = lue_jono(s,maxp);
**   alusta_lue_jono(vanha);
**
** HUOM!  Jos t‰t‰ funktiota ei kutsuta ohjelman aikana, k‰ytet‰‰n
**        alkuper‰ist‰ lue_jono-toteutusta (eli f_lue_jono(stdin,...)
**
**
----------------------------------------------------------------------------*/
{
  lue_jono_tyyppi vanha = lue_jono_funk;
  lue_jono_funk = uusi;
  return vanha;
}

#ifdef DOSOUT
  #include "dout.h"
#endif

/****************************************************************************/
int lue_jono(char *jono,int max_pituus)
/*
** Funktiolla luetaan p‰‰tteelt‰ yhden rivin mittainen merkkijono.
** Muuten ks. f_lue_jono.
** Syˆttˆ:    p‰‰tteelt‰
** Tulostus:  n‰ytˆlle
----------------------------------------------------------------------------*/
{
  int paluu;
  if ( lue_jono_funk ) paluu = lue_jono_funk(jono,max_pituus);
  else {
    fflush(stdin);  /* TAMA EI TOIMI VALTTAMATTA KAIKISSA KONEISSA!           */
    paluu = f_lue_jono(stdin,jono,max_pituus);
  }  
#ifdef DOSOUT
  vaihdaANSIp(jono);
#endif
  return paluu;
}

/****************************************************************************/
int                       /*  2 = jono syˆtettiin                           */
lue_jono_oletus(          /*  1 = jono ei mahtunut                          */
                          /*  0 = tyhj‰ rivi                                */
                          /* -1 = <EOF>                                     */
                          /* -2 = virhe syˆtˆss‰                            */
  const char *hopute,     /* s   Teksti joka tulostetaan kysymykseksi.      */
  int  oletus_alku,       /* s   Sarake josta oletusarvo alkaa. (0=heti)    */
  int  jonon_alku,        /* s   Sarake johon > tulee. (0=heti.)            */
  const char *oletus,     /* s   Merkkijonon oletusarvo.                    */
  char *jono,             /* t   Palautettava merkkijono.                   */
  int max_pituus          /* s   Palautettavan jonon maksimipituus.         */
)
/*
** Funktiolla luetaan p‰‰tteelt‰ yhden rivin mittainen merkkijono.
** Mik‰li vastataan pelkk‰ [Ret] palautetaan oletusjono.
** Hoputteeksi tulostetaan teksti 'hopute (oletus) >'
** Negatiiviset arvot oletus_alku ja jonon_alku sarakkeilla tarkoittavat
** et‰isyytt‰ (itseisarvo) edellisest‰ merkist‰.
** Muuten ks. f_lue_jono
** Syˆttˆ:    p‰‰tteelt‰
** Tulostus:  n‰ytˆlle
** Kutsuu:    f_lue_jono
** Esimerkki:
**  Kutsu: lue_jono_oletus("Anna j‰senen nimi",19,33,"Ankka Aku",nimi,30)
**                   19            33
** Anna j‰senen nimi (Ankka Aku)   >Ankka Tupu
**               Palautetaan: 2 sek‰ nimi = "Ankka Tupu"
**
** Jos m‰‰ritelty vakio USE_CONSOLE, niin t‰llˆin oletusta ei tulosteta,
** vaan toivotaan lue_jonon olevan sellaista tyyppi‰, ett‰ se tulostaa
** alkuper‰isen arvon n‰yttˆˆn editoitavaksi (ks. alusta_lue_jono).
----------------------------------------------------------------------------*/
{
  char oletus_kopio[80]; /* Kopio oletusjonosta jotta oletus voi olla jono  */
  char hopute_kopio[80]; /* Kopio oletusjonosta jotta oletus voi olla jono  */
  int  hopute_pituus,tyhjat_h_s,palautus;

  if ( hopute == NULL ) hopute = "";
  if ( oletus == NULL ) oletus = "";

  kopioi_jono(N_S(oletus_kopio),oletus);
  kopioi_jono(N_S(hopute_kopio),hopute);

  hopute_pituus = strlen(hopute);
  /* Tyhji‰ hoputetekstin ja oletuksen vasemman sulun v‰liin                */
  tyhjat_h_s = oletus_alku - hopute_pituus - 1;
  if ( tyhjat_h_s <= 0 ) tyhjat_h_s = 1;
  if ( oletus_alku < 0 ) tyhjat_h_s = -oletus_alku;

  kopioi_jono(jono,max_pituus,oletus_kopio);
#ifdef DOSOUT
  vaihdaOEMp(hopute_kopio);
#endif
#ifndef USE_CONSOLE
  { int oletus_pituus,tyhjat_s_v;
  oletus_pituus = strlen(oletus_kopio);
  /* Tyhji‰ oletuksen oikean sulun ja v‰k‰sen v‰liin                        */
  tyhjat_s_v = jonon_alku - (hopute_pituus+tyhjat_h_s+1+oletus_pituus+1) - 1;
  if ( tyhjat_s_v <= 0 ) tyhjat_s_v = 0;
  if ( jonon_alku < 0 ) tyhjat_s_v = -jonon_alku;
  printf("%s"    "%*s"         "(%s)"       "%*s"        ">",
          hopute_kopio,tyhjat_h_s,"",oletus_kopio,tyhjat_s_v,"");
  if ( (palautus = lue_jono(jono,max_pituus)) != OLETUS ) return palautus;

  kopioi_jono(jono,max_pituus,oletus_kopio);
  }
#else
  (void)jonon_alku;
  printf("%s"    "%*s"         ">",
          hopute_kopio,tyhjat_h_s,"");
  lue_jono(jono,max_pituus); palautus = SYOTTO_OK;
  if ( strcmp(jono,oletus_kopio) != 0 ) return palautus;
#endif

  return OLETUS;
}

/****************************************************************************/
int                       /*  2 = syˆtettiin                                */
lue_kokluku_oletus(       /*  1 = annettiin liian iso syˆttˆ                */
                          /*  0 = tyhj‰ rivi                                */
                          /* -1 = <EOF>                                     */
                          /* -2 = virhe syˆtˆss‰                            */
  int oletus_luku,        /* s   Luvun oletusarvo.                          */
  int *luku               /* t   Luettu luku.                               */
)
/*
** Funktiolla luetaan kokonaisluku.
** Mik‰li painetaan heti CR tai j‰rkev‰‰ lukua ei ole, palautetaan oletusarvo.
** Kutsuu:    lue_jono
----------------------------------------------------------------------------*/
{
  int  palautus,i;
  char st[20];

  if ( luku == NULL ) return VIRHE_SYOTOSSA;

  *luku = oletus_luku;
  sprintf(st,"%d",oletus_luku);

  if ( (palautus=lue_jono(st,20)) <= OLETUS ) return palautus;

  if ( !sscanf(st,"%d",&i) ) return VIRHE_SYOTOSSA;

  *luku = i;
  return palautus;
}


/****************************************************************************/
char                      /*                                                */
*poista_alkutyhjat(       /* =   Muutettu jono                              */
  char *jono              /* t   Muutettu jono                              */
)
/*
** Funktiolla poistetaan merkkijonosta alussa olevat v‰lilyˆnnit
**
** Algoritmi: Siirret‰‰n jonoa alkutyhjien verran taaksep‰in.
** Esimerkki: "   kissa " -> "kissa "
----------------------------------------------------------------------------*/
{
  int i,l;
  if ( jono == NULL ) return TYHJAMJONO;
  for (i=0; jono[i]==' '; i++);          /* Lasketaan alkutyhjien lukum‰‰r‰ */
  l = strlen(&jono[i]);
  memmove(jono,&jono[i],l+1);
  return jono;
}


/****************************************************************************/
char                      /*                                                */
*poista_lopputyhjat(      /* =   Muutettu jono                              */
  char *jono              /* s,t Muutettu jono                              */
)
/*
** Funktiolla poistetaan merkkijonosta lopussa olevat v‰lilyˆnnit
**
** Algoritmi: Siirret‰‰n merkkijono loppumerkki 1. ei-tyhj‰n per‰‰n.
** Esimerkki: " kissa   " -> " kissa"
----------------------------------------------------------------------------*/
{
  int i;
  if ( jono == NULL ) return TYHJAMJONO;
  for (i=strlen(jono)-1; i>=0 && jono[i]==' '; i--)
    jono[i]=0;
  return jono;
}


/****************************************************************************/
char                      /*                                                */
*poista_2_tyhjat(         /* =   Muutettu jono                              */
  char *jono              /* s,t Muutettu jono                              */
)
/*
** Funktiolla poistetaan merkkijonosta moninkertaiset v‰lilyˆnnit.
**
** Algoritmi: K‰ytet‰‰n kahta osoitinta luku ja kirjoitus,
**            Merkki kopioidaan lukupaikasta kirjoituspaikkaan kun
**            ei ole moninkertaista tyhj‰‰, t‰llˆin myˆs kasvatetaan
**            kirjoituspaikkaa.
**            luku-osoitinta siirret‰‰n aina eteenp‰in.
** Esimerkki: "k    is   sa" -> "k is sa"
**               ^  ^
**               k  l
----------------------------------------------------------------------------*/
{
  int tyhjia=0;
  char *l=jono,*k=jono;
  if ( jono == NULL ) return TYHJAMJONO;

  while ( *l ) {
    if ( *l == ' ') tyhjia++;
    else tyhjia = 0;
    if ( tyhjia <= 1 ) { *k = *l; k++; }     /* Jollei 2-tyhj‰, kopioidaan! */
    l++;
  }

  *k = 0;

  return jono;
}


/****************************************************************************/
char                      /*                                                */
*poista_tyhjat(           /* =   Muutettu jono                              */
  char *jono              /* s,t Muutettu jono                              */
)
/*
** Funktiolla poistetaan merkkijonosta kaikki alku ja lopputyhj‰t
** sek‰ moninkertaiset tyhj‰t jonon keskelt‰.
**
** Kutsuu:    poista_alkutyhjat
**            poista_lopputyhjat
**            poista_2_tyhjat
** Esimerkki: "    kis   sa    " -> "kis sa"
----------------------------------------------------------------------------*/
{
  poista_alkutyhjat(jono);
  poista_lopputyhjat(jono);
  return poista_2_tyhjat(jono);
}

/****************************************************************************/
char *poista_alku_ja_2_tyhjat(char *s)
{
  poista_alkutyhjat(s);
  poista_2_tyhjat(s);
  return s;
}


/****************************************************************************/
char                      /*                                                */
isoksi(                   /* Muutetaan kirjain vast. isoksi kirjaimeksi.    */
  char c                  /* s   Muutettava kirjain.                        */
)
/*
** Funktiolla muutetaan pienet kirjaimet isoiksi.  Myˆs skandit toimivat
** Algoritmi: K‰sitell‰‰n skandit erikoistapauksena.
----------------------------------------------------------------------------*/
{
  switch (c) {
    case 'Ñ': return 'é';
    case 'î': return 'ô';
    case 'Ü': return 'è';
    case 'Å': return 'ö';
    case 'é': ;
    case 'ô': ;
    case 'è': 
    case 'ö': return c;
    case 'Â': return '≈';
    case '‰': return 'ƒ';
    case 'ˆ': return '÷';
    case '¸': return '‹';
    case '≈': ;
    case 'ƒ': ;
    case '÷':
    case '‹': return c;
  }
/* N‰in:
  if ( c<'a' ) return c;
  if ( c>'z' ) return c;

  return c-('a'-'A');
tai: */
  return (char)toupper(c);
}


/****************************************************************************/
char                      /*                                                */
pieneksi(                 /* Muutetaan kirjain vast. pieneksi kirjaimeksi.  */
  char c                  /* s   Muutettava kirjain.                        */
)
/*
** Funktiolla muutetaan isot kirjaimet pieniksi.  Myˆs skandit toimivat
** Algoritmi: K‰sitell‰‰n skandit erikoistapauksena.
----------------------------------------------------------------------------*/
{
  switch (c) {
    case 'é': return 'Ñ';
    case 'ô': return 'î';
    case 'è': return 'Ü';
    case 'ö': return 'Å';
    case 'Ñ': ;
    case 'î': ;
    case 'Ü': ;
    case 'Å': return c;
    case '≈': return 'Â';
    case 'ƒ': return '‰';
    case '÷': return 'ˆ';
    case '‹': return '¸';
    case 'Â': ;
    case '‰': ;
    case 'ˆ':
    case '¸': return c;
  }
/*
  if ( c<'A' ) return c;
  if ( c>'Z' ) return c;
  return c+('a'-'A');
*/
  return (char)tolower(c);
}


/****************************************************************************/
char                      /*                                                */
*jono_pieneksi(           /* Muutetaan jono pienill‰ kirjoitetuksi.         */
  char *jono              /* s,t Muutettava jono.                           */
)
/*
** Funktiolla muutetaan merkkijonon isot kirjaimet pieniksi.  Myˆs skandit.
** Kutsuu:    pieneksi
----------------------------------------------------------------------------*/
{
  char *p;
  if ( jono == NULL ) return TYHJAMJONO;
  for (p=jono; *p; p++) *p=pieneksi(*p);
  return jono;
}


/****************************************************************************/
char                      /*                                                */
*jono_isoksi(             /* Muutetaan jono isoilla kirjoitetuksi.          */
  char *jono              /* s,t Muutettava jono.                           */
)
/*
** Funktiolla muutetaan merkkijonon pienet kirjaimet isoiksi.  Myˆs skandit.
** Kutsuu:    isoksi
----------------------------------------------------------------------------*/
{
  char *p;
  if ( jono == NULL ) return TYHJAMJONO;
  for (p=jono; *p; p++) *p=isoksi(*p);
  return jono;
}


/****************************************************************************/
char                      /*                                                */
*jono_alku_isoksi(        /* Muutetaan jonon sanojen alut isoiksi, muut pien*/
  char *jono              /* s,t Muutettava jono.                           */
)
/*
** Funktiolla muutetaan jonon sanojen 1. kirjaimet isoiksi.  Sanan kaikki
** muut muutetaan pieniksi.
**
** Kutsuu:    isoksi
**            pieneksi
** Algoritmi: V‰limerkkien j‰lkeen sanan pituus = 0.  Muuten ++
**            Jos sanan pituus on 1 muutetaan aina
**            isoksi.  Muuten pieneksi.
** Esimerkki: "iso paha SUSI" -> "Iso Paha Susi"
----------------------------------------------------------------------------*/
{
  char *p; int sanan_pituus=0;
  if ( jono == NULL ) return TYHJAMJONO;
  for (p=jono; *p; p++) {
    if ( strchr(VALIMERKIT,*p) != NULL ) sanan_pituus = 0;
    else sanan_pituus++;
    if ( sanan_pituus == 1 ) *p=isoksi(*p);
    else *p=pieneksi(*p);
  }
  return jono;
}

/****************************************************************************/
char                      /*                                                */
*jono_1_isoksi(           /* Muutetaan jonon 1. kirjain isoiksi, muita ei   */
  char *jono              /* s,t Muutettava jono.                           */
)
/*
** Funktiolla muutetaan jonon 1. kirjain isoiksi.  Muihin ei kosketa.
**
** Kutsuu:    isoksi
** Esimerkki: "iso paha SUSI" -> "Iso paha SUSI"
----------------------------------------------------------------------------*/
{
  if ( jono == NULL ) return TYHJAMJONO;
  jono[0] = isoksi(jono[0]);
  return jono;
}

/****************************************************************************/
int                       /*                                                */
wildmat(                  /* 0 = jono t‰sm‰‰ maskiin                        */
                          /* 1 = jono ei t‰sm‰‰ maskiin                     */
  const register char *s ,/* s   Tutkittava jono                            */
  const register char *m  /* s   Maski, johon jonoa verrataan               */
)
/*
** Funktiolla tutkitaan t‰sm‰‰kˆ annettu jono verrattavaan maskiin.
** Maski saa sis‰lt‰‰ seuraavia erikoismerkkej‰:
**   * vastaa 0-n merkki‰
**   ? vastaa mit‰ tahansa yht‰ merkki‰
**
** Algoritmi: Kysymysmerkki ja tavallinen kirjain normaalisti
**            Jos tulee vastaan t‰hti joka ei ole jonon lopussa,
**            niin ongelmahan on oikeastaan
**            (koska t‰h‰n asti kaikki on ollut oikein)
**            "Onko loppujono sama kuin toisen jonon loppu JOSTAKIN
**             kohdasta alkaen"?
**            Siis kokeillaan sovittaa loppujonoa aliohjelman itsens‰
**            (rekursio) avulla kaikkiin mahdollisiin loppupaikkoihin.
** Esimerkki: s = "Kissa" m = "*ss*" -> 0
**                          = "*ss"  -> 1
** Vika alkuper‰isess‰ algoritmissa:
**   Jos m="*a" ja s="" ja s[1]!=0 (mik‰ tietysti sallittua!)
**   niin vastaus oli 0.
**   Korjattu 29.1.1994/vl muuttamalla rekursion j‰lkeinen
**             if (!*++s) return 1;
**   muotoon
**             if (!*s || !*++s) return 1;
----------------------------------------------------------------------------*/
{
  while (*m) {                    /* Jos kokeiltavaa jonoa on j‰ljell‰      */
    if (*m == '?') {              /* Jos kysymysmerkki, niin kaikki kelpaa  */
      if (!*s) return 1;          /* paitsi jos jono on loppunut!           */
    }
    else if (*m == '*') {         /* Jos * niin katsotaan onko viimeinen    */
      if (*++m)                   /* Jollei * viimeinen, niin kokeillaan    */
        while (wildmat(s, m))     /* loppujonoa jokaiseen eri paikkaan.     */
          if (!*s || !*++s) return 1;/* Jos jono loppuu kesken ei t‰sm‰‰!   */
      return 0;                   /* Muuten samat (* viimeinen tai loppujono*/
    }                             /* t‰sm‰si)                               */
    else if (*s != *m)            /* Jos samat merkit niin tietysti OK!     */
      return 1;
    s++; m++;                     /* Kummassakin jonossa eteenp‰in          */
  }

  return *s;                      /* Jos jono loppui yht‰aikaa, niin OK!    */
}


/****************************************************************************/
int                       /* 1 = erit                                       */
onko_samat(               /* 0 = samat                                      */
  const char *jono       ,/* s   Tutkittava merkkijono                      */
  const char *maski       /* s   Verrattava jono, joka saa sis‰lt‰‰ * ja ?  */
)
/*
** Funktiolla tutkitaan onko jono ja maski isoiksi muutettuina samoja.
**
** Kutsuu:    jono_isoksi
**            poista_tyhjat
**            wildmat
----------------------------------------------------------------------------*/
{
  char ijono[80],imaski[80];
  kopioi_jono(N_S(ijono),jono); kopioi_jono(N_S(imaski),maski);
  return wildmat(jono_isoksi(poista_tyhjat(ijono)),
                 jono_isoksi(poista_tyhjat(imaski)));
}


/****************************************************************************/
char                      /*                                                */
*palanen(                 /* Osoitin merkkijonon palaseen.                  */
  char *jono             ,/* s   P‰tkitt‰v‰ jono, turmeltuu!                */
  const char *erottimet  ,/* s   Merkit joiden kohdalta katkaistaan.        */
  int  *jaljella          /* t   Paljonko jonoa on viel‰ j‰ljell‰ (-1 loppu)*/
)
/*
** Funktiolla p‰tkit‰‰n merkkijonoa osiin.  1. kutsukerralla v‰litet‰‰n
** tutkittava jono ja t‰m‰n j‰lkeen seuraavilla NULL osoitin.
** Funktio vaihtaa lˆyt‰m‰ns‰ erotinmerkti NUL-merkeiksi!
**
** Muuttuu:   jono
** Algoritmi:
** Esimerkki:       012345678901234
**            jono="Aku|Ankka||12" erottimet="|"
**            1. kutsu palanen(jono,"|",&j) -> "Aku"  , j=9
**            2. kutsu palanen(NULL,"|",&j) -> "Ankka", j=3
**            3. kutsu palanen(NULL,"|",&j) -> ""     , j=2
**            4. kutsu palanen(NULL,"|",&j) -> "12"   , j=0
**            5. kutsu palanen(NULL,"|",&j) -> ""     , j=-1
----------------------------------------------------------------------------*/
{
  static char *st;
  static int p1,p2,pit;

  if (jono) {      /* 1. kutsukerta, alustetaan apumuuttujat */
    st  = jono;
    pit = strlen(jono);
    p1  = 0;
  }
  else
    p1  = p2+1;    /* Muilla kerroilla jatketaan siit‰ mihin viim. j‰‰tiin. */

  if ( p1 > pit ) {
    *jaljella = -1;
    return st+pit; /* Tyhj‰ merkkijono, kun osoitetaan jonon NUL-tavuun.    */
  }

  p2 = p1+strcspn(st+p1,erottimet);
  st[p2] = 0;
  *jaljella = pit-p2;

  return st+p1;

}

/****************************************************************************/
int                       /*                                                */
laske_merkit(             /* Merkkien lukum‰‰r‰ jonossa.                    */
  const char *jono       ,/* s   Jono josta merkkej‰ lasketaan.             */
  const char *merkit      /* s   Merkit joita etsit‰‰n.                     */
)
/*
** Funktiolla lasketaan annettujen merkkien yhteinen esiintymism‰‰r‰
** merkkijonossa.
**
** Esimerkki: jono = "Kissa" merkit = "is"  -> laske_merkit = 3
**            jono = "Kissa" merkit = "ss"  -> laske_merkit = 2
----------------------------------------------------------------------------*/
{
  int maara=0; char const *p=jono;
  if ( jono   == NULL ) return 0;
  if ( merkit == NULL ) return 0;

  while (*p) if (strchr(merkit,*p++)) maara++;
  return maara;
}


/****************************************************************************/
int                       /* -1   = ei lˆydy                                */
paikka(                   /* muut = 1. lˆytymispaikan indeksi               */
  const char *jono       ,/* s   Jono josta merkki‰ etsit‰‰n.               */
  char merkki             /* s   merkki jota etsit‰‰n                       */
)
/*
** Funktiolla palautetaan etsitt‰v‰n merkin 1. indeksi merkkijonossa.
**
** Esimerkki: jono = "Kissa"  merkki='s'  -> 2
**            jono = "Koira"  merkki='s'  -> -1
----------------------------------------------------------------------------*/
{
  const char *p;
  if ( jono == NULL ) return -1;
  if ( merkki == 0 ) return -1;
  p = strchr(jono,merkki);
  if (p) return (int)(p-jono);
  return -1;
}


/****************************************************************************/
char
*tayta_valit(           /* Palauttaa osoitteen muutettuun mjonoon.          */
  char *tulos         , /* t   Muutettu merkkijono                          */
  int max_koko        , /* s   Tulosjonon maksimikoko                       */
  const char *jono    , /* s   Muutettettava jono                           */
  const char *mista     /* s   Jono, josta t‰yttˆ otetaan.  Jos NULL, niin  */
                        /*     tarkoittaa kaikkia ASCII-merkkej‰            */
)
/*
** Funktiolla t‰ydennet‰‰n mjonossa olevat v‰lit.
**
** Tekij‰:    Vesa Lappalainen
** Pvm:       19.3.1992
**            24.3.1992  lis‰tty max_koko parametri/vl
**            11.12.1993 lis‰tty mista-jono
** Esimerkki: mista = NULL
**            jono = "1-5"  -> tulos = "12345"
**            jono ="-!"    -> tulos = " !"
**            jono ="K-"    -> tulos = "KLMNOPQRSTU..."
**            jono "1-4A-D" -> tulos "1234ABCD"
**            jono "1--5"   -> tulos "15"
**            jono "3-1"    -> tulos "31"
**            mista = "KISAT"
**            jono = "K-"   -> tulos = "KISAT"
**            jono = "VI-A" -> tulos = "VISA"
**            jono = "A-F"  -> tulos = "AF"
**            jono = "L-A"  -> tulos = "LA"
**            jono = "-A"   -> tulos = "KISA"
** Algoritmi: kopioidaan merkki kerrallaan tulosjonoon
**            kunnes tulee -
**              laitetaan tulosjonoon merkit edellinen+1 ... seuraava-1
**              jatketaan alusta kunnes jono loppuu
----------------------------------------------------------------------------*/
{
#define ONKO_LOPETUS if (t>loppu) { tulos[loppu]=0; return tulos; }
  int j=0;
  int t=0, loppu=max_koko-1;
  unsigned char edellinen=' '-1;
  int c,ed,seuraava;
  tulos[0] = 0;
  if ( tulos == NULL ) return TYHJAMJONO;
  if ( jono  == NULL ) return tulos;

  do {           /* K‰yd‰‰n jonon kaikki merkit l‰pi */
    if ( jono[j] != '-' ) {
      tulos[t++] = jono[j];
      ONKO_LOPETUS;
    }
    else {       /* Lis‰t‰‰n v‰liss‰ olevat merkit   */
      if ( mista == NULL ) { /* ASCII-t‰yttˆ         */
        seuraava = jono[j+1]; if (seuraava==0) seuraava = 256;
        for (c=edellinen+1; c<seuraava; c++) {
          tulos[t++] = (char)c;
          ONKO_LOPETUS;
        }
      }
      else { /* t‰ytet‰‰n mista-jonosta              */
        seuraava  = paikka(mista,jono[j+1]);
          if ( jono[j+1] == 0 ) seuraava = strlen(mista);
        ed = paikka(mista,edellinen);
        if ( j == 0 || ed >= 0 )
          for (c=ed+1; c<seuraava; c++) {
            tulos[t++] = mista[c];
            ONKO_LOPETUS;
          }
      }
    }            /* Lis‰t‰‰n v‰liss‰ olevat merkit   */
    edellinen = jono[j];
  } while ( jono[j++] );/* K‰yd‰‰n jonon kaikki merkit l‰pi */
  return tulos;
#undef ONKO_LOPETUS
}


/****************************************************************************/
int                       /* 0, jos jokin jonoista                          */
joku_jono_func(           /* 1  jos ei ole                                  */
  const char *s,          /* s  Tutkittava jono                             */
  const char *mista,      /* s  Jonot, joista etsit‰‰n                      */
  const char *erottimet,  /* s  Erotinmerkit                                */
  str_2_fun  vertaa       /* s  Mill‰ funktiolla jonona verrataan           */
)
/* Palautetaan 0 , jos s on jokin jonon mista osista. Osat erotetaan
** toisistaan EROTTIMET-merkeill‰.
** Esim. "EY"  "E?|USA" "|,"   -> 0
**       "SU"  "EU|EY|EL" "|," -> 1
** Kutsuu: palanen  -> ei saa k‰ytt‰‰ kesken paloittelun!
----------------------------------------------------------------------------*/
{
  if ( mista == NULL || s == NULL || erottimet == NULL ) return 1;
{
  char *m=tee_jono(mista),*p,*o=m;
  int j,paluu=1;  /* Oletus: Ei ole                                         */

  do {
    p = palanen(o,erottimet,&j);
    o = NULL;
    if ( vertaa(s,p) == 0 ) { paluu = 0; break; }
  } while ( j > 0 );
  free(m);
  return paluu;
}}


/****************************************************************************/
int                       /* 0, jos jokin jonoista                          */
joku_jono(                /* 1  jos ei ole                                  */
  const char *s,          /* s  Tutkittava jono                             */
  const char *mista,      /* s  Jonot, joista etsit‰‰n                      */
  const char *erottimet   /* s  Erotinmerkit                                */
)
/* Palautetaan 0 , jos s on jokin jonon mista osista. Osat erotetaan
** toisistaan EROTTIMET-merkeill‰.
** Esim. "EY"  "EU|EY|EL" "|," -> 0
**       "SU"  "EU|EY|EL" "|," -> 1
** Kutsuu: palanen  -> ei saa k‰ytt‰‰ kesken paloittelun!
----------------------------------------------------------------------------*/
{
  return joku_jono_func(s,mista,erottimet,strcmp);
}

/****************************************************************************/
/* arvosana.c */
/*
**  Aliohjelmia arvosanojen k‰sittelemiseksi muotojen 8.25 ja 8+
**  v‰lill‰.
**  Vesa Lappalainen 27.4.1993
*/

#include <math.h>

typedef struct Muunnos_tyyppi {
  char    merkki;  /* merkki joka jonon per‰ss‰ +,-,´ tai NUL               */
  double  ero;     /* ero kokonaislukuun      esim.  0.25                   */
  int     lisa;    /* takaisin muunnoksessa kokonaislukuun lis‰tt‰v‰ luku   */
                   /* ja muunnoksessa v‰hennett‰v‰ luku                     */
                   /*  esim. 4+  -> kok.osa 4 , merkki + ero 0.25 lisa 0    */
                   /*        5-  -> kok.osa 4 , merkki - ero 0.75 lisa 1    */
} Muunnos_tyyppi;

const                /*  01234567890123"   */
char *ARVOSANA_MERKIT = "0123456789.-+´";/* Arvosanaan sallitut merkit.     */


/****************************************************************************/
static
Muunnos_tyyppi MUUNNOS[] = { /* HT: lis‰‰ ominaisuus 5# = 5.33              */
  {'-',  0.75, 1 },          /*                      5@ = 4.66              */
  {'+',  0.25, 0 },
  {'´',  0.50, 0 },          /* puoli (HUOM! tulosteissa puoli ei v‰lt. n‰y)*/
  {  0,  0.00, 0 }           /* Taulukon loppu jos kaikki alkiot 0          */
};


/****************************************************************************/
double                    /*                                                */
jono_arvosanaksi(         /*                                                */
  const char *s           /* s   Arvosanaksi muutettava jono                */
)
/*
** Funktiolla muutetaan jono 5+ arvosanaksi 5.25 jne.
**
** Algoritmi:  1. otetaan numero-osa
**             2. jos lopussa -  v‰hennet‰‰n 1 ja lis‰t‰‰n 0.75
**                jos lopussa ´  lis‰t‰‰n 0.5
**                jos lopussa +  lis‰t‰‰n 0.25
**             (hoidetaan taulukolla)
**
** Muutettu toimimaan useammalla  per‰kk‰isell‰ +,- tai ´ -merkill‰.
**
** Esimerkki:         s    palautus
**                  ----------------
**                    5    5.00
**                    5-   4.75
**                    5´   5.5
**                    5+   5.25
**                    5.25 5.25
**                    ""   0.00
**                    hyl  0.00
**                    kiit 0.00
**                    5++  5.5
**                    +    0.25
**                    ´    0.5
**                    -    -0.25  // erikoistapaus
**                    -+   -0.25
**                    -´   -0.5
**                    -2´  -2.5
**                    -3-  -2.75
----------------------------------------------------------------------------*/
{
  double d=0.0;
  const  char *merkki;
  int    l = strlen(s);
  int    i;
  int    pit=0;
  int    etum = 1;

  if ( l == 0 ) return 0.0;
  if ( l == 1 )          /* Onko pelkk‰ +,- tai ´ */
    for (i=0; MUUNNOS[i].merkki != 0; i++ )
      if ( MUUNNOS[i].merkki == s[0] )
        return (MUUNNOS[i].ero - MUUNNOS[i].lisa);

  if ( s[0] == '-' ) { pit++; etum *= -1; } /* etumerkki pois */
  if ( s[0] == '+' ) { pit++; etum = 1; }   /* etumerkki pois */

  sscanf(s+pit,"%lf",&d);

  for (i=pit; s[i] && strchr("0123456789.",s[i]); i++,pit++);

  for ( merkki=s+pit; *merkki; merkki ++) {

    for (i=0; MUUNNOS[i].merkki != 0; i++)
      if ( MUUNNOS[i].merkki == *merkki ) break;

    d += (MUUNNOS[i].ero - MUUNNOS[i].lisa);
  }

  return etum * d;
}


/****************************************************************************/
char *                    /*                                                */
arvosana_jonoksi(         /*                                                */
  double d,               /* s   Jonoksi muutettava arvosana                */
  char   *nolla           /* s   Nollasta palautettava jono.                */
)                         /*     Jos NULL, palautetaan "0"                  */
/*
** Funktiolla muutetaan reaaliluku 5.25 jonoksi 5+ jne.
**
** HUOM!       Aliohjelman palauttamaan osoitteeseen EI SAA kopioida
**             mit‰‰n!   Osoitetta saa k‰ytt‰‰ vain tuloksen ottamiseen
**             ko. osoitteesta!
** Algoritmi:  1.  Jos 0 niin palautetaan ""
**             2.  katsotaan ero kokonaislukuun
**             3.  jos ero 0.25 l‰tk‰st‰‰n kokonaisluvun per‰‰n +
**                 jos     0.5      ´
**                 jos ero 0.75 yksi lis‰‰ kok.lukuun ja per‰‰n -
**
** Esimerkki:  palautus  <-  d
**             --------------------
**                    5    5.00
**                    5-   4.75
**                    5´   5.5
**                    5+   5.25
**                    5.25 5.25
**                   nolla 0.00
**                    +    0.25
**                    -   -0.25
**                   -2´  -2.5
----------------------------------------------------------------------------*/
{
  int    etum   = d<-0.25001 ? -1 : 1;  /* etum, -0.25 erikoistapaus!       */
  double dp     = etum*d;               /* d positiivisena                  */
  int    koko   = (int)floor(dp);
  int    i;
  double ero    = dp - koko; /* Paljonko d heitt‰‰ kokonaisluvusta.         */
  char   merkki = 0;         /* Jos merkki‰ ei lˆyd. -> jono 5 NUL NUL -> OK*/
  static char   jono[20];    /* Tarkkana jotta 20 riitt‰‰ sprintf:ss‰       */

  if ( d == 0 ) return ( nolla ? nolla : "0" );

  for (i=0; MUUNNOS[i].ero != 0.0; i++)
    if ( fabs(MUUNNOS[i].ero - ero ) <= 0.000001  ) {
      koko   += MUUNNOS[i].lisa;
      merkki  = MUUNNOS[i].merkki;
    }

  if ( koko == 0 ) /* 0-, 0+ ja 0´ erikoistapauksena */
    sprintf(jono,"%c",merkki);
  else
    sprintf(jono,"%d%c",koko,merkki);

  if ( etum < 0 && jono[0] != '-' ) lisaa_alkuun(N_S(jono),"-");
  return jono;
}


/****************************************************************************/
int                       /* -1 kaikki merkit sallituissa merkeiss‰         */
sallituissa(              /* >0 n‰in mones merkki ei sallituissa.           */
  const char *jono,       /*  s tutkittava jono                             */
  const char *sallitut    /*  s sallitut merkit                             */
)
/*
** Palautetaan -1,  mik‰li jonon jokainen kirjain on sallituissa
** muuten ensimm‰isen virheellisen merkin indeksi
----------------------------------------------------------------------------*/
{
  int i;

  if ( jono == NULL )     return -1;
  if ( sallitut == NULL ) return 0;

  for (i=0; jono[i]; i++ )
    if ( strchr(sallitut,jono[i]) == NULL ) return i;
  return -1;
}


/****************************************************************************/
char                      /*                                                */
*poista_merkit(           /* =   Muutettu jono                              */
  char *jono             ,/* s,t Muutettu jono                              */
  const char *mitka       /* s   Poistettavat merkit                        */
)
/*
** Funktiolla poistetaan merkkijonosta kaikki jonossa mitka olevat merkit
**
** Algoritmi: K‰ytet‰‰n kahta osoitinta luku ja kirjoitus,
**            Merkki kopioidaan lukupaikasta kirjoituspaikkaan kun
**            ei ole poistettava.
**            luku-osoitinta siirret‰‰n aina eteenp‰in.
** Esimerkki: "k    is   sa" -> "kissa"
**              ^   ^
**              k   l
----------------------------------------------------------------------------*/
{
  char *l=jono,*k=jono;
  if ( jono == NULL ) return TYHJAMJONO;

  while (*l) {
    if ( strchr(mitka,*l) == NULL ) { /* Jollei poistettava, kopioidaan     */
      *k=*l; k++;
    }
    l++;
  }

  *k = 0;

  return jono;
}


/****************************************************************************/
char *                    /*                                                */
poista_alusta(            /* =   Muuttunut jono                             */
  char *s,                /* s,t Jono josta poistetaan merkkej‰.            */
  int n                   /* s   Montako merkki‰ poistetaan.                */
)
/*
** Funktiolla poistetaan merkkijonosta annettu m‰‰r‰ merkkej‰.
** Ei haittaa vaikka n > jonon pituus.
**                
** Esimerkki: s = "Kivaa"  n = 2  => s = "vaa"
**
----------------------------------------------------------------------------*/
{
  int i=0;

  if ( s == NULL ) return TYHJAMJONO;
  /* Jos poistetaan enemm‰n merkkej‰ kuin jonon pituus                      */
  if ( (int)strlen(s) <= n ) { s[0] = 0; return s; }

  while ( ( s[i] = s[i+n] ) != 0 ) i++;
  return s;
}


/****************************************************************************/
int                       /* 0 = Onnistui                                   */
lisaa_alkuun(             /* muut ei mahdu                                  */
  char *s,                /* s,t Muutettava jono                            */
  int maxp,               /* s   Jonon maksimipituus                        */
  const char *k           /* s   Lis‰tt‰v‰ jono                             */
)
/*
** Funktiolla lis‰t‰‰n jonon alkuun toinen jono.
** Jos ei mahdu, s lyhenee lopusta p‰in.
**
** Esimerkki:                                         01234567890
**   s = "Kivaa",  maxp = 10, k = "T‰m‰ on "  => s = "T‰m‰ on Ki"  => -1
**
----------------------------------------------------------------------------*/
{
  int lk=strlen(k);
  int ls=strlen(s);
  int i;

  if ( k == NULL ) return 0;
  if ( s == NULL ) return -1;

  for (i=ls; i>=0; i--) { /* Tehd‰‰n tyhj‰‰ jonon alkuun                    */
    if ( i+lk >= maxp-1 ) s[maxp-1] = 0;
    else s[i+lk] = s[i];
  }

  for (i=0; k[i]; i++) {  /* Uusi jono tyhj‰n p‰‰lle                        */
    if ( i >= maxp-1 ) { s[maxp-1] = 0; break; }
    s[i] = k[i];
  }

  if ( lk+ls+1 > maxp ) return -1;
  return 0;
}


/****************************************************************************/
char *                    /*                                                */
lisaa_merkki(             /* = muuttunut jono                               */
  char *s,                /* s,t Muutettava jono                            */
  int maxp,               /* s   Jonon maksimipituus                        */
  char c,                 /* s   Lis‰tt‰v‰ merkki                           */
  int mihin               /* s   Paikka johon lis‰t‰‰n                      */
)
/*
** Funktiolla lis‰t‰‰n jonon kohtaan paikka merkki c.
** Jos ei mahdu, s lyhenee lopusta p‰in.
**
** Esimerkki:                                          01234567890
**   s = "Kiaa", maxp = 10, c = 'v', paikka=2  => s = "Kivaa"
**
----------------------------------------------------------------------------*/
{
  char lisa[2];
  lisa[0]=c; lisa[1]=0;
  if ( maxp <= mihin ) return s;
  lisaa_alkuun(s+mihin,maxp-mihin,lisa);
  return s;
}

/****************************************************************************/
char *vaihda_jonot(char *p, int n, const char *mika, const char *milla)
/*
** Jos mika-jonon alussa on ^- tarkoitetaan etta vaihto tehdaan vain rivin
** alusta.  Muuten kaikki esiintymat vaihdetaan.
**
** Esimerkki:
**    mika    milla    p              ->   p
**    "Aku"   "Tupu"   "Ankka Aku"         "Ankka Tupu"
**    "^Aku"  "Tupu"   "Ankka Aku"         "Ankka Aku"
**    "^Aku"   "Tupu"  "Aku Ankka"         "Tupu Ankka"
**    "Aku"    "Tupu"  "Aku Akunen"        "Tupu Tupunen"
**    "^Aku"   "Tupu"  "Aku Akunen"        "Tupu Akunen"
----------------------------------------------------------------------------*/
{               
  const char *miksi = (milla ? milla : "");
  int p_mika;
  int p_miksi = strlen(miksi);
  int i;
  int vain_alku = 0;
  if ( p == NULL  || mika == NULL ) return p;
  if ( mika[0] == '^' ) { mika++; vain_alku = 1; }
  p_mika = strlen(mika);

  for (i=0; p[i] && i<n && !( i > 0 && vain_alku ) ; i++) {
    if ( strncmp(p+i,mika,p_mika) != 0  ) continue;
    poista_alusta(p+i,p_mika);
    lisaa_alkuun(p+i,n-i,miksi);
    i += p_miksi-1;
  }
  return p;
}

/****************************************************************************/
char *vaihda_merkit(char *s,int n,char mika, char miksi)
{
  int i;
  if ( s == NULL ) return s;
  for (i=0; i<n; i++)
    if ( s[i] == mika ) s[i] = miksi;
  return s;
}


/****************************************************************************/
/****************************************************************************/
typedef struct {
  char from;
  char to;
} tCMuunnos;


tCMuunnos CPreChars[] = {
  { 'n' , '\n' },
  { 'r' , '\r' },
  { 't' , '\t' },
  { 'b' , '\b' },
  { 'a' , '\a' },
  { '\\', '\\' },
  { 0   , 0    }
};

/****************************************************************************/
char CPrefix(char **p)
/*
** Tekee jonolle CPrefix-muunnoksen, eli \ unohdetaan ja seuraava osa
** tulkitaan. Osoitin palautetaan siten, ett‰ jonon ohi p‰‰st‰‰n kun
** seuraavaksi lis‰t‰‰n osoitinta viel‰ yhdell‰.
** Toistaiseksi osataan vain heksaluvut, ei oktaaleja.
** Esim "\0x61"
**     *p^
**
**  => a ja "\0x61"
**             *p^
**--------------------------------------------------------------------------*/
{
  int i,j;
  char *s;
  if ( *p == NULL )  return 0;
  if ( **p != '\\' ) return **p;
  (*p)++;
  for (i=0; CPreChars[i].from; i++)
    if ( **p == CPreChars[i].from ) return CPreChars[i].to;

  if ( **p != '0' ) return **p;
  if ( *((*p)+1) != 'x' ) return 0;
  (*p) += 2; /* x:‰‰ seuraava, OK vaikka 0 */
  s = *p;
  j = 0;
  while ( isxdigit((int)**p) && j<2 ) { (*p)++; j++; }
  sscanf(s,"%2x",&i);
  (*p)--;
  return (char)i;
}

/****************************************************************************/
int muunna_C_symbolit(char *s)
{
  char *p = s;
  int i;

  if ( p == NULL ) return 0;

  for (i=0;*p;p++) {
    if ( *p == '\\' ) { s[i++] = CPrefix(&p); continue; }
    s[i++] = *p;
  }
  s[i] = 0;
  return 0;
}

/****************************************************************************/
char *jonossa_vain_merkit(char *s,const char *merkit)
{
  int n;
  if ( merkit == NULL || s == NULL ) return s;
  while ( ( n = sallituissa(s,merkit) ) >= 0 )
    poista_alusta(s+n,1);
  return s;
}



