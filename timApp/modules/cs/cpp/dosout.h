/* dosout.h  */
/****************************************************************************/
/*
**       D O S O U T . H
**
** Muuttaa cout tietovirran sellaiseksi että koodisivun 1252 ä:t tulostetaan
** koodisivun 850 (OEM) mukaan.
**
**  Tekijät:          Vesa Lappalainen
**  Tehty:            17.02.2001
**  Komentit ja päivityshistoria ks. dout.cpp
**
*****************************************************************************/
#ifdef __TURBOC__
#ifndef __DOSOUT_H
#define __DOSOUT_H

#include <iostream>

#define cout dout
#define getline dgetline
#include "dout.h"

#endif /* __DOSOUT_H    */
#endif /* __MSDOS__ */
