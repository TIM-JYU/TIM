//---------------------------------------------------------------------------
// vclDraw2d.cpp
// Koordinaatiston ja funktion piirto 2D:ssä
// Vesa Lappalainen 31.3.2001
//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdio.h>
#pragma hdrstop

#include "vclDraw2d.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------
void cCoordinate::XTicks(TDC &dc, const cTMatrix &A, double dx, double dty, int iy) const
{
  if ( iy < 0 ) return;
  double x,y1,y2;
  y1  = origo.py()-dty;
  y2  = origo.py()+dty;
  TSize s = CSize(0,iy);
  TSize s2 = CSize(0,iy+1);  // Koska viiva loppuu yhden pikselin aikaisemmin
  for ( x = origo.px(); x >= dRect.p1.x-dx/10; x -= dx ) {
    MoveTo(dc,A(x,y1)+s); LineTo(dc,A(x,y2)-s2);
  }
  for ( x = origo.px(); x <= dRect.p2.x+dx/10; x += dx ) {
    MoveTo(dc,A(x,y1)+s); LineTo(dc,A(x,y2)-s2);
  }
}

void cCoordinate::YTicks(TDC &dc, const cTMatrix &A, double dy, double dtx, int ix) const
{
  if ( ix < 0 ) return;
  double y,x1,x2;
  x1  = origo.px()-dtx;
  x2  = origo.px()+dtx;
  TSize s = CSize(ix,0);
  TSize s2 = CSize(ix+1,0);
  for ( y = origo.py(); y >= dRect.p1.y-dy/10; y -= dy ) {
    MoveTo(dc,A(x1,y)-s); LineTo(dc,A(x2,y)+s2);
  }
  for ( y = origo.py(); y <= dRect.p2.y+dy/10; y+= dy ) {
    MoveTo(dc,A(x1,y)-s); LineTo(dc,A(x2,y)+s2);
  }
}

void cCoordinate::XLabels(TDC &dc, const cTMatrix &A, double dx, double dty, int iy) const
{
  if ( xformat == "" ) return;
  double x,y1;
  char s[50];

  TBrushStyle oldStyle = dc.Brush->Style;
  dc.Brush->Style = bsClear;
  TSize size;
  size.cy = -iy;

  y1  = origo.py()-dty;
  for ( x = origo.px()-dx; x >= dRect.p1.x-dx/10; x -= dx ) {
    sprintf(s,xformat.c_str(),x);
    size.cx = dc.TextWidth(s)/2;
    TextOut(dc,A(x,y1)-size,s);
  }
  for ( x = origo.px()+dx; x <= dRect.p2.x+dx/10; x += dx ) {
    sprintf(s,xformat.c_str(),x);
    size.cx = dc.TextWidth(s)/2;
    TextOut(dc,A(x,y1)-size,s);
  }
  dc.Brush->Style = oldStyle;
}

void cCoordinate::YLabels(TDC &dc, const cTMatrix &A, double dy, double dtx, int ix) const
{
  if ( yformat == "" ) return;
  double x1,y=-1;
  char s[50];
  sprintf(s,yformat.c_str(),y);

  TBrushStyle oldStyle = dc.Brush->Style;
  dc.Brush->Style = bsClear;
  x1  = origo.px()-dtx;
  for ( y = origo.py()-dy; y >= dRect.p1.y-dy/10; y -= dy ) {
    sprintf(s,yformat.c_str(),y);
    TSize size = dc.TextExtent(s);
    size.cy /= 2;
    size.cx += ix;
    TextOut(dc,A(x1,y)-size,s);
  }
  for ( y = origo.py()+dy; y <= dRect.p2.y+dy/10; y += dy ) {
    sprintf(s,yformat.c_str(),y);
    TSize size = dc.TextExtent(s);
    size.cy /= 2;
    size.cx += ix;
    TextOut(dc,A(x1,y)-size,s);
  }
  dc.Brush->Style = oldStyle;
}

void cCoordinate::Draw(TDC &dc, const cTMatrix &A) const
{
  cDPoint tsz,tbz;
  if ( smallTickRelSize.cx != 0 ) tsz.x = dRect.Width()/smallTickRelSize.cx;
  if ( smallTickRelSize.cy != 0 ) tsz.y = dRect.Height()/smallTickRelSize.cy;
  if ( bigTickRelSize.cx != 0 )   tbz.x = dRect.Width()/bigTickRelSize.cx;
  if ( bigTickRelSize.cy != 0 )   tbz.y = dRect.Height()/bigTickRelSize.cy;
  if ( ticksEqual ) {
    tsz.x = min(tsz.x,tsz.y); tsz.y = tsz.x;
    tbz.x = min(tbz.x,tbz.y); tbz.y = tbz.x;
  }


  MoveTo(dc,A(dRect.p1.x,origo.py()));
  LineTo(dc,A(dRect.p2.x,origo.py()));

  XTicks(dc,A,ds.y,tsz.y,smallTickSize.cy);
  XTicks(dc,A,db.y,tbz.y,bigTickSize.cy);
  XLabels(dc,A,db.y,tbz.y*1.2,max(bigTickSize.cy,0L));

  MoveTo(dc,A(origo.px(),dRect.p1.y));
  LineTo(dc,A(origo.px(),dRect.p2.y));

  YTicks(dc,A,ds.x,tsz.x,smallTickSize.cx);
  YTicks(dc,A,db.x,tbz.x,bigTickSize.cx);
  YLabels(dc,A,db.px(),tbz.x*1.2,max(bigTickSize.cx,0L)+2);
}

//------------------------------------------------------------------------------
void Draw(TCanvas &dc, const cTMatrix &A, tFunc f,
          double x1, double x2, double dx, TColor col
          )
//------------------------------------------------------------------------------
{
  TColor oldcol = dc.Pen->Color;
  dc.Pen->Color = col;
  MoveTo(dc,A(x1,f(x1)));
  for ( double x = x1+dx; x < x2; x += dx )
    LineTo(dc,A(x,f(x)));
  LineTo(dc,A(x2,f(x2)));
  dc.Pen->Color = oldcol;
}

