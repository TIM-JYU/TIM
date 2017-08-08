/****************************************************************************/
/*
**        M E N U T . C
**
** Aliohjelmia menujen tekemiseen.
** Tekij„:         Vesa Lappalainen 25.1.1994
** Kommentit ks. menut.c
**
*****************************************************************************/
#ifndef MENUT_H
#define MENUT_H
#include "console.h"

#define Cchar const char

typedef void (*pvMenuFunc)(void);
typedef int  (*piMenuFunc)(void);

typedef union {
  piMenuFunc ifu;
  pvMenuFunc vfu;
} puMenuFunc;

typedef struct {
  char       *sitem;
  int        item;
#ifdef MENU94_9
  pvMenuFunc func;
#else
  piMenuFunc func;
#endif
} tMenuTable;

typedef struct {
  char       *sitem;
  int        item;
  int        wParam;
  tMenuTable *MenuTable;
/*  struct tMenuSystem *MenuSystem; */
} MSG;

int SetMessageParam(int, const char *);
const char *GetMessageParam(int);
const char *EditNameString(const char *s,pEditFunc f);
int PostQuitMessage(int);
int InitMenuSystem(const char *, MSG *, tMenuTable *);
char *GetInitError(int );
int ClearMenus(MSG *);
int GetMessage(MSG *);
int ScrMessage(Cchar *, int);
int ScrMessage1(Cchar *, int, Cchar *);
int ScrMessage2(Cchar *, int, Cchar *, Cchar *);
int ScrMessage3(Cchar *, int, Cchar *, Cchar *, Cchar *);
int ScrMessage4(Cchar *, int, Cchar *, Cchar *, Cchar *, Cchar *);
int ScrMessage5(Cchar *, int, Cchar *, Cchar *, Cchar *, Cchar *, Cchar *);
int ScrMessage6(Cchar *, int, Cchar *, Cchar *, Cchar *, Cchar *, Cchar *,
                Cchar *);
int SetNameParam(const char *,const char *);
const char *GetNameParam(const char *);
int SetNameParamInt(const char *,int);
int GetNameParamInt(const char *);
int SetNameParamDouble(const char *,double);
double GetNameParamDouble(const char *);
int DispatchMessage(MSG *);
int MenuRun(Cchar *, tMenuTable *);
int MenuRunArg(int argc,char *argv[],tMenuTable *MenuTable);

#endif
