/* string.cpp */
/****************************************************************************/
/*
** string-luokan osoittainen toteutus k„„nt„j„„n jossa ko. luokkaa ei ole.
**
**  Tekij„t:          Vesa Lappalainen
**                    Ohjelmointikurssi 1996
**  Tehty:            09.12.1995
*****************************************************************************/

#include <iostream.h>
#include <string>
#include <string.h>


size_t vstring::find_last_of(const char *s,size_t pos) const
{
  if ( pos >= pituus ) pos = pituus-1;
  for (int i=pos; i>=0; i--)
    if ( strchr(s,cstr[i]) != NULL ) return i;
  return npos;
}

size_t vstring::find_last_not_of(const char *s,size_t pos) const
{
  if ( pos >= pituus ) pos = pituus-1;
  for (int i=pos; i>=0; i--)
    if ( strchr(s,cstr[i]) == NULL ) return i;
  return npos;
}

//
//     456
// 0123456

size_t vstring::rfind(const char *s, size_t i) const
{
  int len = length();
  int slen = strlen(s);
  if ( i >= len-slen ) i=len-slen;
  size_t j;
  for (j=i; j != npos; j--)
    if ( strncmp(cstr+j,s,slen) == 0 ) return j;
  return npos;
}





