/****************************************************************************/
/* Testiosuus menut.c:t„ varten                                             */
/* Vesa Lappalainen 14.2.1994
/* Projektiin mukaan:
**  T„m„ tiedosto
**  menut.c
**  mjonot.c
**  console.c
*****************************************************************************/

#include <stdlib.h>
#include "menut.h"
#include "mjonot.h"
#include "console.h"


int talletus(void)
{
  return ScrMessage1("Dos",1,"Talletus");
}

int Apua(void)
{
  return ScrMessage2("Dos",2,"Apua","Saako sit„?");
}

int init(void)
{
  SetMessageParam(0,"Uusi");
  return ScrMessage("Dos",1);
}

int EiTunne(void)
{
  return ScrMessage("Dos",175);
}

int collect()
{
  return ScrMessage3("Init",1,"A:","\\oma","VIKAA");
}

int Ilmoitus(void)
{
  return ScrMessage("Ilmoitus",1);
}

int Nopeus(void)
{
  int n = GetNameParamInt("baud");
  return SetNameParamInt("baud",n+1);
}

int Editoi(void)
{
  char jono[40];
  kopioi_jono(N_S(jono),GetNameParam("jono"));
  ScrMessage("Editoi",1);
  EditString(N_S(jono),jono_isoksi);
  return SetNameParam("jono",jono);
}

int eDitoi(void)
{
  ScrMessage("Editoi",1);
  return EditNameString("jono2",jono_isoksi) != NULL;
}

int edItoi(void)
{
  return ScrMessage("Editoi",-1);
}


int Lopetus(void)
{
  return PostQuitMessage(0);
}


/****************************************************************************/
tMenuTable menu_table[] = {
  {"Talletus",0,talletus},
  {"Init"    ,0,init    },
  {"Collect" ,0,collect },
  {"ApuaT"   ,0,Apua    },
  {"ApuaS"   ,0,Apua    },
  {"EiTunne" ,0,EiTunne },
  {"Ilmoitus",0,Ilmoitus},
  {"Nopeus"  ,0,Nopeus  },
  {"Editoi"  ,0,Editoi  },
  {"eDitoi"  ,0,eDitoi  },
  {"edItoi"  ,0,edItoi  },
  {"Lopetus" ,0,Lopetus },
  {NULL      ,0,NULL    }
};


int main(int argc, char *argv[])
{
#pragma argsused
  return MenuRunArg(argc,argv,menu_table);
}

