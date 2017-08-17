/****************************************************************************/
/*
**        C O N S O L E . H
**
** Aliohjelmia n„yt”n ja n„pp„imist”n k„sittelyyn
** Tekij„:   Vesa Lappalainen 14.2.1994
** Kommentit ks. console.c
**
*****************************************************************************/
#ifndef CONSOLE_H
#define CONSOLE_H

/* Emacs commands: */
#define coKEY_CR      '\r'     /* Enter or send                    Ctrl-M   */
#define coKEY_ESC     0x001B   /* Esc                              Ctrl-\   */
#define coKEY_UP      0x0010   /* The arrow up                     Ctrl-P   */
#define coKEY_DOWN    0x000E   /* The arrow down                   Ctrl-N   */
#define coKEY_BS      0x0008   /* BackSpace                        Ctrl-H   */
#define coKEY_LEFT    0x0002   /* The arrow key left               Ctrl-B   */
#define coKEY_RIGHT   0x0006   /* The arrow key rigth              Ctrl-F   */
#define coKEY_HOME    0x0001   /* Home key (upward+left arrow)     Ctrl-A   */
#define coKEY_END     0x0005   /* End key                          Ctrl-E   */
#define coKEY_C_BS    0x000B   /* Delete line or Ctrl-BS           Ctrl-K   */
#define coKEY_DEL     0x0004   /* Delete character                 Ctrl-D   */
#define coKEY_A_BS    0x0019   /* Undo                             Ctrl-Y   */

#ifdef __cplusplus
extern "C" {
#endif


typedef char *(*pEditFunc)(char *);
typedef unsigned int cWORD;

cWORD ReadChar(void);
const char *ClearConsole(const char *);
int printscr(const char *);
int ScrMove(int, int);
int GetScreenx(void);
int GetScreenXY(int *nx, int *ny);
int GetScreeny(void);
int GetMaxXY(int *mx,int *my);
int GetMaxx(void);
int GetMaxy(void);
int ScrColor(int c);

int InitConsole(void);
int ReleaseConsole(void);


int EditString(char *s, int max_s, pEditFunc f);

#ifdef __cplusplus
}
#endif

#endif
