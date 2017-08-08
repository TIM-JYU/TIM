#include "luokat.h"
template <class TYPE>
string cLuokat<TYPE>::lue_tiedostosta(const string &tied)
{

  ifstream fi(tied.c_str());
  if ( !fi ) return TIED_EI_AUKEA;

  string rivi;
  TYPE uusi;
  while ( getline(fi,rivi) ) {
    if ( rivi == "" || rivi[0] == ';' ) continue;
    uusi.setAsString(rivi);
    lisaa(uusi);
  }

  return "";
}

