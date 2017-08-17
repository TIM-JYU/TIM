/****************/
/* 3d.cpp       */
/****************/
// Matriisit 3D-muunnoksiin
// /vl-96 & -01

#include <math.h>
#include "3d.h"

void cDRect::scan(tFunc f,double x1, double x2,double dx)
// Alustetaan suorakaide funktion f minimillä ja maksimilla
// Jos dx = 0, jaetaan väli 100 osaan
{
   double fx,x,y1=f(x2),y2=f(x2);
   if ( dx == 0 ) dx = (x2-x1)/100;
   for ( x=x1; x<x2; x+=dx ) {
     fx = f(x);
     if ( fx < y1 ) y1 = fx;
     if ( y2 < fx ) y2 = fx;
   }
   if ( p1.px() < x1 ) x1 = p1.px();
   if ( p1.py() < y1 ) y1 = p1.py();
   if ( p2.px() > x2 ) x2 = p2.px();
   if ( p2.py() > y2 ) y2 = p2.py();
   p1 = cDPoint(x1,y1);
   p2 = cDPoint(x2,y2);
}


//------------------------------------------------------------------------------
int vsize(int &i)   // Pakotetaan aina i välille [0,VSIZE-1]
{
  if ( VSIZE <= i ) i = VSIZE-1;
  if ( i < 0 ) i = 0;
  return i;
}

//------------------------------------------------------------------------------
cVector::cVector(double a0, double a1, double a2)
{ // Alustetaan vektoriksi (a0,a1,a2,1)
  for (int i=0; i<VSIZE; i++) a[i] = 0.0;
  a[0] = a0; a[1] = a1; a[2] = a2;
  a[VSIZE-1] = 1.0;
}

cVector::cVector(int i, double ai)
{
  for (int j=0; j<VSIZE; j++) a[j] = 0.0;
  a[vsize(i)] = ai;
}

double cVector::operator*(const cVector &b) const
{
  double s = 0;
  for (int i=0; i<VSIZE; i++)
    s += a[i] * b.a[i];
  return s;
}

//------------------------------------------------------------------------------
void cMatrix::ident()                        // Muutetaan yksikkömatriisiksi
{
  // Alusta 0:ia
  for (int j=0; j<VSIZE; j++)
    r[j] = cVector(j,1.0);
}

cVector cMatrix::column(int i) const         // Palautetaan i:s sarakevektori
{
  cVector c;
  vsize(i);
  for (int j=0; j<VSIZE; j++)
    c[j] = r[j][i];
  return c;
}

cMatrix operator*(const cMatrix &A, const cMatrix &B) // C = A*B
{
  cMatrix C;
  for (int j=0; j<VSIZE; j++)
    for (int i=0; i<VSIZE; i++)
      C[j][i] = A[j]*B.column(i);
  return C;
}

cMatrix &cMatrix::operator*=(const cMatrix &B)        // A *= B
{
  cMatrix C(*this * B);
  *this = C;
  return *this;
}


cVector operator*(const cMatrix &A, const cVector &x) // y = A*x
{
  cVector y;
  for (int j=0; j<VSIZE; j++)
    y[j] = A[j]*x;
  return y;
}

//------------------------------------------------------------------------------
void cTMatrix::scale(const cDRect &dRect, const TIRect &iRect,bool equal)
// Etsitään transformaatiomatriisi, joka muuttaa dRect -> iRect
{
  cTMatrix m;
  double scale,xs,ys,dxf,dyf,dxt,dyt; // s=scale, f=from,  t=to
  dxf = dRect.Width();
  dyf = dRect.Height();
  dxt = iRect.right - iRect.left;
  dyt = iRect.top - iRect.bottom;

  if ( dxf == 0 || dxt == 0) xs = 1; else xs = dxt/dxf;
  if ( dyf == 0 || dyt == 0) ys = 1; else ys = dyt/dyf;

  if ( equal ) { // Jos halutaan x/y = 1
    scale = fabs(xs); if ( fabs(ys) < scale ) scale = fabs(ys);
    xs = fabs(xs)/xs*scale;  ys = fabs(ys)/ys*scale;
  }

  m[0][0] = xs;  m[1][1] = ys;

  m[0][VSIZE-1] = (iRect.left+iRect.right)/2.0 - dRect.MidX()*xs;
  m[1][VSIZE-1] = (iRect.top+iRect.bottom)/2.0 - dRect.MidY()*ys;

  *this = m;
}

TIPoint cTMatrix::operator()(double x,double y, double z) const
{ // Kutsulla A(x,y) muutetaan reaalimaailman piste näytön pisteeksi
  cVector vr(x,y,z);        // Reaalimaailman vektori
  cVector vs(*this * vr );  // Näytöllä vastaava vektori
  return TIPoint(int(vs[0]+0.5),int(vs[1]+0.5));
}

//------------------------------------------------------------------------------
cRotMatrix::cRotMatrix(int axis,double deg) : cMatrix()
// Kierto akselin ympäri (0=x,1=y,2=z)
//   0:   1  0  0    1:   c  0 -s    2:   c -s  0
//        0  c -s         0  1  0         s  c  0
//        0  s  c         s  0  c         0  0  1
{
  double a = deg*M_PI/180.0, ca=cos(a), sa=sin(a);
  int    j[2];

  for (int i=0,ji=0; i<3; i++)  if ( i != axis ) j[ji++] = i;
  r[j[0]][j[0]] = ca;
  r[j[1]][j[1]] = ca;
  r[j[0]][j[1]] =-sa;
  r[j[1]][j[0]] =+sa;
}



