/* wout.h  */
/****************************************************************************/
/*
**       W O U T . H
**
** Tietovirta, joka menee console.c:n printscr-funktion mukaiseen paikkaan.
**
**
**  Tekij„t:          Vesa Lappalainen
**  Tehty:            07.01.1996
**  Komentit ja p„ivityshistoria ks. wout.cpp
**
*****************************************************************************/

#ifndef __WOUT_H
#define __WOUT_H

#ifdef USE_CONSOLE
#include <iostream>
#define cout wout

extern ostream wout;

#else  /* USE_CONSOLE */
#ifdef DOSOUT
#include "dosout.h"
#else  // DOSOUT

#define cout std::cout
#define cin std::cin
#endif // DOSOUT

#endif /* USE_CONSOLE */

#endif /* __WOUT_H    */

