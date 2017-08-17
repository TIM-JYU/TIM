/* editlue.h */
/****************************************************************************/
/*
**        E  D  I  T  L  U  E . H
**
**  Tekij„t:          Vesa Lappalainen
**                    Ohjelmointikurssi 1996-
**  Tehty:            07.01.1996
**  Kommentit ja muutoshistoria ks. editlue.c
**
*****************************************************************************/

#ifndef EDITLUE_H
#define EDITLUE_H

#include "mjonot.h"

#ifdef __cplusplus
extern "C" {
#endif


#ifdef USE_CONSOLE
const char *edit_ei_muunnosta(int i);
int edit_muuta_ei_muunnosta(int i,const char *s);

int edit_tyhjenna_sijoitukset(void);
int edit_lisaa_sijoitus(int key,const char *jono);
const char *edit_sijoitus(int key);


int edit_lue(char *s,int maxs);
int edit_vaihda_muunnos(muunnos_funktio mf);
int edit_alusta(const char *lyh);
int edit_vapauta(const char *lyh);
#else
#define edit_ei_muunnosta() jono_isoksi
#define edit_muuta_ei_muunnosta(s)

#define edit_tyhjenna_sijoitukset()
#define edit_lisaa_sijoitus(key,jono)
#define edit_sijoitus(key)


#define edit_lue(s,maxs) f_lue_jono(stdin,s,maxs)
#define edit_vaihda_muunnos(mf) 0
#define edit_alusta(lyh)  0
#define edit_vapauta(lyh) 0
#endif

#ifdef __cplusplus
}
#endif


#endif /* EDITLUE_H */
