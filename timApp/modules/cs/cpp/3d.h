/****************/
/* 3d.h         */
/****************/
// Muunnosmatriisit 3d piirtämistä varten
// /vl-96

#ifndef M3D_H
#define M3D_H

//------------------------------------------------------------------------------
typedef double (*tFunc)(double x);            // R->R funktiotyyppi
typedef double (*tFunc2)(double x, double y); // R2->R funktiotyyppi
//------------------------------------------------------------------------------

class TIRect {
public:
  double left,right,top,bottom;
  TIRect() : left(0),right(1),top(0),bottom(1) {}
  TIRect(int ileft, int iright, int itop, int ibottom) : left(ileft),right(iright),top(itop),bottom(ibottom) {}
};

class TIPoint {
public:
  int x,y;
  TIPoint(int ix=0,int iy=0) : x(ix), y(iy) {}
};

//------------------------------------------------------------------------------
class cDPoint { // Reaalilukupiste
//------------------------------------------------------------------------------
public:
  double x,y;
  cDPoint (double ix=0, double iy=0) : x(ix),y(iy) {}
  double px() const { return x; }
  double py() const { return y; }
};

//------------------------------------------------------------------------------
class cDRect { // Reaaliluku suorakaide;
//------------------------------------------------------------------------------
public:
  cDPoint p1,p2;  // Vasen alanurkka ja oikea ylänurkka
  cDRect(double x1=0,double x2=1,double y1=0,double y2=1) :
    p1(x1,y1),p2(x2,y2) {}
  cDRect(tFunc f,double x1, double x2,double dx=0) { scan(f,x1,x2,dx); }
  void scan(tFunc f,double x1, double x2,double dx=0);
  double Width()  const  { return p2.px() - p1.px(); }
  double Height() const  { return p2.py() - p1.py(); }
  double MidX() const    { return ( p1.px() + p2.px() ) / 2.0; }
  double MidY() const    { return ( p1.py() + p2.py() ) / 2.0; }
  void init() { p1 = cDPoint(100000,100000); p2 = cDPoint(-100000,-100000); }
};



//------------------------------------------------------------------------------
const int VSIZE=4;  // VSIZE voi olla joko 3 (2D kuvat) tai 4 (3D kuvat)
int vsize(int &i);

//------------------------------------------------------------------------------
class cVector {
//------------------------------------------------------------------------------
protected:
  double a[VSIZE];
public:
  cVector(double a0=0.0, double a1=0.0, double a2=0.0);
  cVector(int i, double ai=1.0);
  double operator*(const cVector &b) const; // Sisätulo
  double operator[](int i) const { return a[vsize(i)]; }  // v[2]
  double &operator[](int i) { return a[vsize(i)]; }
};

//------------------------------------------------------------------------------
class cMatrix {
//------------------------------------------------------------------------------
protected:
  cVector r[VSIZE];
public:
  cMatrix() { ident(); }
  cVector &operator[](int j) { return r[vsize(j)]; }
  void ident();
  const cVector &operator[](int j) const { return r[vsize(j)]; }
  cVector column(int i) const;
  friend cMatrix operator*(const cMatrix &A, const cMatrix &B); // C = A*B
  cMatrix &operator*=(const cMatrix &B);
};

//------------------------------------------------------------------------------
class cTMatrix : public cMatrix {  // Transformaatiomatriisi
//------------------------------------------------------------------------------
public:
  cTMatrix() : cMatrix() {};                        // Yksikkömatriisi
  cTMatrix(const cMatrix &M) : cMatrix(M) {}
  cTMatrix(int axis,double deg);                    // Kierto akselin ympäri
  void scale(const cDRect &dRect, const TIRect &iRect,bool equal=true);
  TIPoint operator()(double x,double y, double z) const;
  TIPoint operator()(double x,double y) const { return operator()(x,y,0); }
};


//------------------------------------------------------------------------------
class cRotMatrix : public cMatrix {  // Rotaatiomatriisi
//------------------------------------------------------------------------------
public:
  cRotMatrix(int axis,double deg);
};


#endif
