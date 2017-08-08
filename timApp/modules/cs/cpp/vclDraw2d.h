//---------------------------------------------------------------------------
// vclDraw2d.h
// Koordinaatiston ja funktion piirto 2D:ss‰
// Vesa Lappalainen 31.3.2001

#ifndef vclDraw2dH
#define vclDraw2dH

#include <vcl.h>
#include <string>
using namespace std;
#include "3d.h"

typedef TCanvas TDC;

inline void MoveTo(TCanvas &dc, const TIPoint &pt) { dc.MoveTo(pt.x,pt.y); }
inline void LineTo(TCanvas &dc, const TIPoint &pt) { dc.LineTo(pt.x,pt.y); }
inline void TextOut(TCanvas &dc, const TIPoint &pt,const AnsiString &s) { dc.TextOut(pt.x,pt.y,s); }
inline void TextOut(TCanvas &dc, const TIPoint &pt,const string &s) { dc.TextOut(pt.x,pt.y,s.c_str()); }
inline void TextOut(TCanvas &dc, const TIPoint &pt,const char *s) { dc.TextOut(pt.x,pt.y,s); }
inline TSize CSize(int ix,int iy) { TSize sz; sz.cx = ix; sz.cy=iy; return sz; }

//---------------------------------------------------------------------------
//------------------------------------------------------------------------------
class cCoordinate {  // Piirt‰‰ koordinaatiston R2:ssa
//------------------------------------------------------------------------------
  cDPoint origo;
  cDRect dRect;
  cDPoint ds;             // small = pikkutikkujen v‰lit
  cDPoint db;             // big   = isojen tikkujen v‰lit
  string xformat;         // jos == "", ei piirret‰ numeroita
  string yformat;
  TSize smallTickRelSize; // suhteellinen koko, eli ikkunan koko jaettuna t‰ll‰
  TSize bigTickRelSize;
  TSize smallTickSize;    // absoluuttinen koko pikselein‰, < 0 => ei piirret‰
  TSize bigTickSize;
  bool ticksEqual;       // suhteellisella koolla ilmaistut tikut yht‰pitki‰ x/y

  void XTicks(TDC &dc, const cTMatrix &A,double dx, double dty, int iy) const;
  void YTicks(TDC &dc, const cTMatrix &A,double dy, double dtx, int ix) const;
  void XLabels(TDC &dc, const cTMatrix &A, double dx, double dty, int iy) const;
  void YLabels(TDC &dc, const cTMatrix &A, double dy, double dtx, int ix) const;
public:
  cCoordinate() : origo(0,0), dRect(), db(1.0,1.0), ds(0.1,0.1),
                  xformat("%3.1lf"), yformat("%3.1lf"), ticksEqual(true)  {
    SetTicksRelative();
  }
  void SetOrigo(const cDPoint &pt)      { origo = pt;           }
  void SetRect(const cDRect rc)         { dRect = rc;           }
  void SetSmallTicks(const cDPoint &pt) { ds = pt;              }
  void SetBigTicks(const cDPoint &pt)   { db = pt;              }
  void SetXFormat(string format)        { xformat = format;     }
  void SetYFormat(string format)        { yformat = format;     }
  void SetSmallTickRelSize(TSize sz)    { smallTickRelSize = sz;}
  void SetBigTickRelSize(TSize sz)      { bigTickRelSize = sz;  }
  void SetSmallTickSize(TSize sz)       { smallTickSize = sz;   }
  void SetBigTickSize(TSize sz)         { bigTickSize = sz;     }
  void SetTicksAbsolute(int bs=3,int ss=1) {
    if ( bs < 0 ) ss = -1;
    SetSmallTickRelSize(CSize(0,0));
    SetBigTickRelSize(CSize(0,0));
    SetSmallTickSize(CSize(ss,ss));
    SetBigTickSize(CSize(bs,bs));
  }
  void SetTicksRelative(int bs=40,int ss=100)       {
    SetSmallTickRelSize(CSize(ss,ss));
    SetBigTickRelSize(CSize(bs,bs));
    SetSmallTickSize(CSize(1,1));
    SetBigTickSize(CSize(1,1));
  }

  void SetFormat(string format="%3.1lf") { xformat = format; yformat = format; }
  void Draw(TDC &dc, const cTMatrix &A) const;
  void Draw(TDC *dc, const cTMatrix &A) const { Draw(*dc,A);    }
};


void Draw(TCanvas &dc, const cTMatrix &A, tFunc f,
          double x1, double x2, double dx=0.1, TColor col = clBlack);

inline void Draw(TCanvas *dc, const cTMatrix &A, tFunc f,
          double x1, double x2, double dx=0.1, TColor col = clBlack
          ) { Draw(*dc,A,f,x1,x2,dx,col); }

inline TIPoint operator-(TIPoint p1,TSize p2) {
  return TIPoint(p1.x-p2.cx,p1.y-p2.cy);
}

inline TIPoint operator+(TIPoint p1,TSize p2) {
  return TIPoint(p1.x+p2.cx,p1.y+p2.cy);
}


#endif
