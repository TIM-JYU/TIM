#ifndef streampr_h
#define streampr_h
#include <iostream>
//---------------------------------------------------------------------------
// Luokka tulostustarkkuuden asettamiseksi
class cStreamPre {
  std::ostream &os;
  std::ios::fmtflags oldf;
  int  oldp;
public:
  cStreamPre(std::ostream &aos,int npre=1,std::ios::fmtflags flags=std::ios::fmtflags(0)) : os(aos) {
    oldf = os.setf(std::ios::showpoint | std::ios::fixed | flags);
    oldp = os.precision(npre);
  }
  ~cStreamPre() { os.flags(oldf); os.precision(oldp); }
};

#endif
