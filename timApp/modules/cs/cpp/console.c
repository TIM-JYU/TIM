/****************************************************************************/
/*
**        C O N S O L E . C
**
** Aliohjelmia nayton ja nappaimiston kasittelyyn.
** Epastandardit aliohjelmat on keratty tahan tiedostoon.
**
** Aliohjelmat:
**    InitConsole            - alustaa terminaalin
**    ReleaseConsole         - vapauttaa terminaalin
**    ReadChar               - odottaa nappaimistolta yhta merkkia
**                             palautettavat erikoismerkit ks. console.h
**    ClearConsole           - pyyhkii ruudun
**    printscr               - tulostaa merkkijonon naytolle
**    ScrMove                - siirtyy ruudulla paikkaa x,y
**    EditString             - editoi merkkijonoa
**    GetScreenx             - x-koordinaatti naytolla
**    GetScreeny             - y-koordinaatti naytolla
**    GetScreenXY            - kursorin paikka n„yt”ll„
**    GetMaxx                - sarakkeiden maara
**    GetMaxy                - rivien maara
**    GetMaxXY               - n„yt”n koko
**
** Kaannoksessa projektissa tarvitaan myos mjonot.c
**
** Voidaan kayttaa printf:n kanssa PC:ssa sellaisenaan ja
** UNIXissa kun maaritellaan kaannokseen /Dprintf=printw
** Tosin varit eiv„t toimi printf:n kanssa, vaan jos vareja
** halutaan, pitaa kayttaa printscr-funktiota.
**
** Kirjallisuutta:
**    Jhon Strang: Programming with curses,
**                 O'Reilly & Associates, Inc. 1986 USA.
**    Jhon Strang, Tim O'Reilly and Linda Mui:
**                 Termcap and Terminfo,
**                 O'Reilly & Associates, Inc. 1988 USA.
**    B. Goodheart: UNIX Curses Explained,
**                 Prentice-Hall, Englewood Cliffs, N.J. 1991.
**    Steve Talbott: Managing Projects with Make,
**                 O'Reilly & Associates, Inc CA 1990.
**                 
**
** Muita lahteita: Systeemin manuaalit paperi ja CDROM muodossa
**                 seka man sivut esim. man curses tai man terminfo.
**                 Lisaksi tietysti curses.h tai conio.h tiedosto,
**                 joka loytyy mahdollisesti hakemistosta /usr/include.
**
** Tekija:         Vesa Lappalainen 14.2.1994
** Muutettu:       8.3.1994/vl
** Mita muutettu:  koordinaatit muutettu toimimaan 0-79,0-24
**                 aliohjelmat EditString, GetMaxx, GetMaxy
**
** Muutettu:      28.9.1994/ap
** Mita muutettu: Lisatty aliohjelmat InitConsole ja ReleaseConsole.
**                Lisatty unix-ymparistoa varten direktiivi __UNIX__
**                ja muuteettu aliohjelmat PyyhiRuutu, printscr,
**                SrcMove toimimaan curses-kirjaston kanssa.
**                SrcColor, lisatty ehto kaatajalle siten etta
**                varin muutos on musta tai valkea unix:ssa.
**                Curses paketti tarkistaa
**                ymparistomuuttujan TERMINFO. Jos muuttuja sisaltaa
**                hakemistopolun niin terminaalimaarityksia etsitaan
**                ensin kyseisesta hakemistosta ja sen jalkeen
**                oletuspaikasta /usr/lib/terminfo.
**
** Muutettu: Sun Mar 26 20:58:08 1995
** Mita muutettu: Lisatty initialisointiin keypad()-funktion kutsu.
**                Maaritetty KEY_BACKSPACE uudelleen.
**
** Muutettu:      20.10.1995/vl
** Mita muutettu: Muutettu kayttamaan Emacsin komentoja ja synonyymi-
**                taulukkoa.  Myos korjattu paikka paremmaksi EditString:in
**                kutsuessa muunnosfunktiota.
**                PyyhiRuutu nimetty ClearConsole ja
**                lue_merkki nimetty ReadChar
** Muutettu:      14.01.1996/vl
** Mita muutettu: Korjattu bugi, joka kaatoi jos EditString:iss„ oli
**                sopiva lokaali merkkijono ( vika: char s[10]; s[10] = 0!)
**
** Muutettu:      28.12.2001/vl
** Mita muutettu: Lisatty vakion DOSOUT tutkiminen.  Jos vakio maritelty
**                muutetaan ANSI-merkit OEM-merkeiksi tulostuksessa.
**
** Bugeja: Curses paketin getch-funktion paluuarvot ovat vielakin
**                riippuvaisia terminaalista. Yleisesti ottaen
**                nuolet toimii mutta delete, backspace ja funktio
**                -nappaimet ei.
**
*****************************************************************************/
#include <stdio.h>
#include <string.h>
#include "mjonot.h"
#include "console.h"

#ifdef _Windows
#define cprintf printf  
#endif


static int MAX_X = 79;
static int MAX_Y = 50;

typedef struct {
  cWORD org;  /* Mita nappainta painettu          */
  cWORD syn;  /* Miksi nappaimeksi t„ytyy muuttaa */
} tSynonym;

/* Kayttojarjestelman maarittaminen.
** Mikali kaantajalle ei suoraan anneta
** tietoa kaanettavasta systeemista
** voidaan maaritys tehda kuten alla
*/

#ifdef __RHIDE__
#  define __COLORS
#  define __TURBOC__
/*#  define cprintf printf */
#endif

#ifndef __TURBOC__

# define __UNIX__
/* # define __LINUX__ */
#endif

/* Alla kuhunkin systeemiin liittyvien
ÄÄ erityispiirteiden maarittelyt.
*/

/****************************************************************************/
/* Synonyymitaulukko nappaimistolle                                         */
# ifdef __TURBOC__
tSynonym Synonym[] = {
  {0x000A, coKEY_CR      },      /* Enter or send                  Ctrl-M   */
  {0x001B, coKEY_ESC     },      /* Esc                            Ctrl-\   */
  {0x4800, coKEY_UP      },      /* The arrow up                   Ctrl-P   */
  {0x5000, coKEY_DOWN    },      /* The arrow down                 Ctrl-N   */
/*{0x007F, coKEY_BS      },         BackSpace                      Ctrl-H   */
  {0x4B00, coKEY_LEFT    },      /* The arrow key left             Ctrl-B   */
  {0x4D00, coKEY_RIGHT   },      /* The arrow key rigth            Ctrl-F   */
  {0x4700, coKEY_HOME    },      /* Home key (upward+left arrow)   Ctrl-A   */
  {0x4F00, coKEY_END     },      /* End key                        Ctrl-E   */
  {0x007F, coKEY_C_BS    },      /* Delete line or Ctrl-BS                  */
  {0x5300, coKEY_DEL     },      /* Delete character               Ctrl-G   */
  {0x0E00, coKEY_A_BS    },      /* Undo                           Ctrl-Y   */
  {0xffff, 0             }
};
# include <conio.h>
# else /* __TURBOC__ */


# ifdef __UNIX__

# ifdef __LINUX__
# include <ncurses/ncurses.h>
tSynonym Synonym[] = {
  {0x000A   , coKEY_CR      },   /* Enter or send                  Ctrl-M   */
  {KEY_ENTER, coKEY_CR      },   /* Enter or send                  Ctrl-M   */
  {0x001B   , coKEY_ESC     },   /* Esc                            Ctrl-\   */
  {KEY_UP   , coKEY_UP      },   /* The arrow up                   Ctrl-P   */
  {KEY_DOWN , coKEY_DOWN    },   /* The arrow down                 Ctrl-N   */
  {0x007F   , coKEY_BS      },   /* BackSpace                      Ctrl-H   */
  {KEY_LEFT , coKEY_LEFT    },   /* The arrow key left             Ctrl-B   */
  {KEY_RIGHT, coKEY_RIGHT   },   /* The arrow key rigth            Ctrl-F   */
  {KEY_HOME , coKEY_HOME    },   /* Home key (upward+left arrow)   Ctrl-A   */
/*  {KEY_END  , coKEY_END     },    End key                        Ctrl-E   */
  {KEY_DL   , coKEY_C_BS    },   /* Delete line or Ctrl-BS                  */
  {KEY_DC   , coKEY_DEL     },   /* Delete character               Ctrl-G   */
/*  {0x0E00  , coKEY_A_BS    },     Undo                           Ctrl-Y   */
  {0xffff   , 0             }
};
# else /* __LINUX__ */
# include <curses.h>
tSynonym Synonym[] = {
  {0x000A   , coKEY_CR      },   /* Enter or send                  Ctrl-M   */
  {KEY_ENTER, coKEY_CR      },   /* Enter or send                  Ctrl-M   */
  {0x001B   , coKEY_ESC     },   /* Esc                            Ctrl-\   */
  {KEY_UP   , coKEY_UP      },   /* The arrow up                   Ctrl-P   */
  {KEY_DOWN , coKEY_DOWN    },   /* The arrow down                 Ctrl-N   */
  {0x007F   , coKEY_BS      },   /* BackSpace                      Ctrl-H   */
  {1863     , coKEY_BS      },   /* BackSpace, AIX                 Ctrl-H   */
  {'\b'     , coKEY_BS      },   /* BackSpace                      Ctrl-H   */
  {KEY_LEFT , coKEY_LEFT    },   /* The arrow key left             Ctrl-B   */
  {KEY_RIGHT, coKEY_RIGHT   },   /* The arrow key rigth            Ctrl-F   */
  {KEY_HOME , coKEY_HOME    },   /* Home key (upward+left arrow)   Ctrl-A   */
/*  {KEY_END  , coKEY_END     },    End key                        Ctrl-E   */
  {KEY_DL   , coKEY_C_BS    },   /* Delete line or Ctrl-BS                  */
  {KEY_DC   , coKEY_DEL     },   /* Delete character               Ctrl-G   */
/*  {0x0E00  , coKEY_A_BS    },     Undo                           Ctrl-Y   */
  {0xffff   , 0             }
};
# endif /* __LINUX__ */
# else /* __UNIX__ */

# error SYSTEM NOT DEFINED in console.c !!!
# endif /* __UNIX__ */
# endif /* __TURBOC__ */


/****************************************************************************/
static cWORD CheckSynonym(cWORD c)
{
  int i;
  for (i=0; Synonym[i].org != 0xffff; i++)
    if ( Synonym[i].org == c ) return Synonym[i].syn;
  return c;
}




/* Varien maarittely */
#ifndef __COLORS
#define __COLORS
enum COLORS {
    BLACK,          /* dark colors */
    BLUE,
    GREEN,
    CYAN,
    RED,
    MAGENTA,
    BROWN,
    LIGHTGRAY,
    DARKGRAY,       /* light colors */
    LIGHTBLUE,
    LIGHTGREEN,
    LIGHTCYAN,
    LIGHTRED,
    LIGHTMAGENTA,
    YELLOW,
    WHITE
};
#endif /* __COLORS */



/****************************************************************************/
int InitConsole(void)
/* Funktiolla alustetaan konsoli kayttokuntoon.
** Toiminta riippuu ymparistosta johon ohjelma kaannetaan.
*/
{
# ifdef __UNIX__
if (!initscr()) {
   perror("Can't init screen");
   return 1;
 }
if (cbreak()==ERR) {
   perror("Can't change c break mode");
   return 1;
 }
if (nonl()==ERR) {
   perror("Init error nonl");
   return 1;
 }
if (noecho()==ERR) {
   perror("Init error noecho");
   return 1;
 }
if (keypad(stdscr, TRUE)==ERR) {
   perror("Init error keyb");
   return 1;
 }
 raw();
 nl();
 scrollok(stdscr,1);
/* nodelay(stdscr,1); */

#if 0
 return(
  (!initscr()) ||
     cbreak()  ||
     nonl()    ||
     noecho()  ||
     keypad(stdscr, TRUE)
 );
#endif
# else
#ifdef __RHIDE__
  gppconio_init();
#endif
/*  ScrColor(BLACK); */
#endif
  ClearConsole(" ");
  GetMaxXY(&MAX_X,&MAX_Y);
  return 0;
}


/****************************************************************************/
int ReleaseConsole(void)
/* Funktiolla vapautetaan konsoli, eli
** siihen liittyvat tietorakenteet yms.
*/
{
# ifdef __UNIX__
  return(endwin());
# else
  return 0;
# endif
}

#ifdef DOSOUT
#include "dout.h"
#endif

/****************************************************************************/
cWORD ReadChar(void)   /* Nappaimistolta luettu merkki.                    */
/* Funktiolla luetaan nappaimistolta yksi merkki.  Mikali ohjelmassa
** ei ole maaritelty vakiota GETCH, vaaditaan RET-nappaimen painallus.
** Globaalit: GETCH - maaritys
** Syotto:    Nappaimisto
----------------------------------------------------------------------------*/
{
#ifdef __UNIX__
  refresh();
  return CheckSynonym((WORD) getch());
#else  /* __UNIX__ */
#ifdef __TURBOC__
/* Seuraava toimii mm. Turbo C:ssa: */
  cWORD c;
  cWORD a = 0;
  if ( ( c = (unsigned char)getch() ) == 0 ) a=(unsigned char)getch();
#ifdef DOSOUT
  c = vaihdaAnsi((unsigned char)c);
#endif
  return CheckSynonym(( (a<<8) | c));
#else /* __TURBOC__ */
/* Seuraava on standardin mukainen: */
  char s[50];
  fgets(s,50,stdin);
  return s[0];
#endif /* __TURBOC__ */
#endif /* __UNIX__ */
}


/****************************************************************************/
#define AMAX_X 500
#define AMAX_Y 200
typedef struct {
  int x;
  int y;
#ifdef __TURBOC__
#else
#ifdef __UNIX__
#else

  char scr[AMAX_Y][AMAX_X];
#endif /* __TURBOC__ */
#endif /* __UNIX__ */
} tScreen;


static tScreen Screen={0};

/****************************************************************************/
const char *ClearConsole(const char *s)
{
/*  if ( s ); */
#ifdef __TURBOC__
  (void)s;
  clrscr(); /* EPAS */

#else /* __TURBOC__ */
#ifdef __UNIX__
  clear();
#else /* __UNIX__ */
  {
    int i;
    for (i=0; i<MAX_Y; i++) printf("\n");
  }
  memset(Screen.scr,' ',AMAX_X*AMAX_Y);
#endif /* __UNIX__ */
#endif /* __TURBOC__ */
  Screen.x = 0; Screen.y = 0;
  return NULL;
}


/****************************************************************************/

#ifdef __UNIX__ /*#################*/

int printscr(const char *st)
{
/*  move(Screen.y+1, Screen.x+1); */
  addstr(st);
  GetScreenXY(&Screen.x,&Screen.y);
  return 0;
}
#else /*********__UNIX__*********/
#ifdef __TURBOC__ /*################*/


int printscr(const char *st)
{
  char s[300],*sp=s, *p;
  int j,l;
  kopioi_jono(N_S(s),st);
  vaihda_jonot(N_S(s),"%","%%");
#ifdef DOSOUT
  vaihdaOEMp(sp);
#endif
  while (1) {
    p  = palanen(sp,"\n",&j);
    l  = strlen(p);
/*    gotoxy(Screen.x+1,Screen.y+1); */
    if ( Screen.x+l >= MAX_X ) l = MAX_X - Screen.x;
    if ( l < 0 ) l = 0;
    p[l] = 0;
    cprintf(p);
    Screen.x += l;
    if ( j <= 0 ) return 0;
    cprintf("\r\n");
    sp = NULL;
    GetScreenXY(&Screen.x,&Screen.y);
  };
}
#else /*********__TURBOC__**********/

int printscr(const char *st)
{
  char s[300],*sp=s, *p;
  int j,l;
  kopioi_jono(N_S(s),st);
  while (1) {
    p  = palanen(sp,"\n",&j);
    l  = strlen(p);
    if ( Screen.x+l >= AMAX_X ) l = AMAX_X - Screen.x;
    if ( l < 0 ) l = 0;
    p[l] = 0;
    memcpy(&Screen.scr[Screen.y][Screen.x],p,l);
    Screen.x += l;
    if ( j <= 0 ) return;
    Screen.x = 0; Screen.y++;
    if ( Screen.y >= AMAX_Y-1 ) Screen.y = AMAX_Y-1;
    sp = NULL;
  };
}
#endif /*********__TURBOC__*********/
#endif /*********__UNIX__**********/


/****************************************************************************/
int ScrMove(int x,int y)
{
  if ( x > MAX_X ) x = MAX_X;
  if ( y > MAX_Y ) y = MAX_Y;
#ifdef __TURBOC__
  gotoxy(x+1,y+1);
#else /* __TURBOC__ */
#ifdef __UNIX__
  move(y+1,x+1);
  refresh();
#else /* __UNIX__ */
#error Cursor moving command not defined in console.c
  if ( x >= AMAX_X ) x = AMAX_X-1;
  if ( y >= AMAX_Y ) y = AMAX_Y-1;
#endif /* __UNIX__ */
#endif /* __TURBOC__ */
  Screen.x = x; Screen.y = y;
  return 0;
}

/****************************************************************************/
int GetMaxXY(int *mx,int *my)
{
#ifdef __TURBOC__
#ifndef _Windows
  struct text_info ti;
  gettextinfo(&ti);
  *mx = ti.screenwidth-1;
  *my = ti.screenheight-1;
  if ( *mx <= 0 ) *mx = 80;
  if ( *my <= 0 ) *my = 25;
  MAX_X = *mx;
  MAX_Y = *my;
#else /* WINDOWS */
  *mx = MAX_X;
  *my = MAX_Y;
#endif

#else

#ifdef __UNIX__
  *mx = MAX_X;
  *my = MAX_Y;
#else
  *mx = MAX_X;
  *my = MAX_Y;
#endif
#endif
  return 0;
}

/****************************************************************************/
int GetScreenXY(int *nx, int *ny)
{
  int x,y;
#ifdef __TURBOC__
  x = wherex()-1;
  y = wherey()-1;
#else
#ifdef __UNIX__
  getyx(stdscr,y,x);
  x--; y--;
#else
#error GetScreenXY not defined in console.c
#endif
#endif
  if ( x > MAX_X ) MAX_X = x;
  if ( y > MAX_Y ) MAX_Y = y;
  *nx = x; *ny = y;
  return 0;
}


/****************************************************************************/
int GetScreenx(void)
{
  int x,y;
  GetScreenXY(&x,&y);
  return x;
}

/****************************************************************************/
int GetScreeny(void)
{
  int x,y;
  GetScreenXY(&x,&y);
  return y;
}


/****************************************************************************/
int GetMaxx(void)
{
  return MAX_X;
}

/****************************************************************************/
int GetMaxy(void)
{
  return MAX_Y;
}


/****************************************************************************/
int ScrColor(int c)
{
#ifdef __TURBOC__
#ifndef _Windows
 {
  int old; struct text_info ti;
  gettextinfo(&ti);
  old = ti.attribute;
  textattr(c);
  return old;
 }
#else /* _Windows */
  return c;
#endif
#else /* __TURBOC__ */

#ifdef __UNIX__
 {
  int ret;
  static int old;
  switch(c){
    case BLACK       : /* dark colors */
    case BLUE        :
    case GREEN       :
    case CYAN        :
    case RED         :
    case MAGENTA     :
    case BROWN       :
    case LIGHTGRAY   : /* attrset(A_UNDERLINE); break; */
    case DARKGRAY    :     /* light colors */
    case LIGHTBLUE   :
    case LIGHTGREEN  :
    case LIGHTCYAN   :
    case LIGHTRED    :
    case LIGHTMAGENTA:
    case YELLOW      : attrset(A_NORMAL ); break;
    case WHITE       : attrset(A_REVERSE); break;
    default: attrset(A_STANDOUT);
  }
  ret=old;
  old=c;
  return ret;
 }
#else  /* __UNIX__ */
 {
  return c;
 }
#endif /* __UNIX__ */
#endif /* __TURBOC__ */
}



/****************************************************************************/
int EditString(char *s, int max_s, pEditFunc f)
/* Funktiolla editoidaan parametrina tuotua merkkijonoa.
** Palautetaan viimeksi painettu merkki, joko KEY_ESC tai KEY_CR
** Editointi aloitetaan nayton nykykohdasta ja jono tulostetaan
** siita alkaen.  Jono ei voi ylittaa ruudun oikeat reunaa.
** Funktioparametri f voi olla esimerkiksi jono_isoksi, jono_pieneksi
** tai vastaava, ja kutsu tehdaan jonolle jokaisen merkin painamisen
** jalkeen.
----------------------------------------------------------------------------*/
{
  int x = GetScreenx(), y = GetScreeny();
  int i = 0, first = 1, len, l, max_l = max_s, maxlen=strlen(s),ce;
  char old[AMAX_X+1];
  cWORD k;

  kopioi_jono(N_S(old),s);

  if ( max_l + x >= MAX_X ) max_l = MAX_X - x - 1;
  if ( max_l < 1 ) return coKEY_CR;
  s[max_l-1] = 0;

  while (1) {
    ce = strlen(s) - i;                /* Kuinka kaukana lopusta    */
    if ( f ) f(s);                     /* Mahdollinen merkkimuunnos */
    len = strlen(s);
    if ( len > maxlen ) maxlen = len;  /* Pisin, jossa jono kaynyt  */
    i = len - ce;                      /* Sama matka lopusta        */
    if ( i < 0 ) i = 0;                /* i:n korjaus alueelle      */
    if ( i > len ) i = len;

    ScrMove(x,y);                      /* Jonon tulostus            */
    printscr(s);
    for (l=strlen(s);l<maxlen;l++) printscr(" ");  /* Lopun tyhjays */
    ScrMove(x+i,y);                    /* Kursori paikalleen        */

    k = ReadChar();

    switch ( k ) {
      case coKEY_ESC:   ScrMove(x,y);
                        for (i=0; i<maxlen; i++) printscr(" ");
                        kopioi_jono(s,max_s,old); /* HUOM! Tahallaan seur. */
      case coKEY_UP:
      case coKEY_DOWN:
      case coKEY_CR:    ScrMove(x,y); printscr(s);                return k;
      case coKEY_A_BS:  kopioi_jono(s,max_s,old); i=0;            break;
      case coKEY_LEFT:  i--;                                      break;
      case coKEY_RIGHT: i++;                                      break;
      case coKEY_HOME:  i=0;                                      break;
      case coKEY_END:   i=MAX_X;                                  break;
      case coKEY_BS:    i--; if ( i < 0 ) break;
                        poista_alusta(s+i,1);                     break;
      case coKEY_DEL:   poista_alusta(s+i,1);                     break;
      case coKEY_C_BS:  i=0; s[0]=0;                              break;
      default: if ( ' ' <= k && k <= 255 ) { /* Tavallinen merkki */
        if ( first ) { s[0]=0; i=0; }
        lisaa_merkki(s,max_l,(char)k,i);
        i++;
        break;
      }
      continue;
    }
    first = 0;

  }

}


#ifdef TESTI  /* TESTI */
int main(void)
{
 int m;
 char s[100];
 /* WORD m; */
 char msg1[100]="kissa";
 char msg2[100]="cat";
 char str[100]="Merkkijono";
 if (InitConsole()){
    perror("Init failed!");
 }
 printw("Anna merkki\n");
 printw("a lopettaa >");
 do {
   m = ReadChar();
   sprintf(s,"%c = %04x\n",m,m);
   printscr(s);
 }  while ( m != 'a' ); 
 
 printscr(msg1);
 printscr(msg1);
 printscr(msg2);
 printscr(msg2);

 EditString(str,strlen(str),0);
 EditString(msg1,strlen(msg1),0);
 ReleaseConsole();
 return 0;
}
#endif /* TESTI */

