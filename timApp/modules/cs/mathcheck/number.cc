copyright number_file( "number.cc", "Antti Valmari", 20200227 );
/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
*/

/* This file contains some data types used by MathCheck:
- a seven-valued truth value: all non-empty combinations of {false, undefined,
  true}
- a number type that uses precise rational numbers when it can, and then
  switches to intervals consisting of two doubles such that the precise value
  is between them. The value may also be undefined either as such or together
  with the two doubles, or a seven-valued truth value.
*/


/* A 7-valued truth value. In addition to the familiar "false" and "true",
there is the value "undefined" for situations like 0/0 < 1, where a predicate
cannot be evaluated because at least one of its arguments is undefined. To
make claims like "if x != 0 then x * 1/x = 1" work, "tv_F && tv_U == tv_F" and
"tv_T || tv_U == tv_T". Because numbers may be represented via an interval
that contains the precise value, the result of a comparison cannot always be
determined. For instance, if x is only known to be between 3.14 and 3.15, then
x < 3.14159 may be tv_T or tv_F but cannot be tv_U. For this reason, there
are four additional truth values for all non-empty non-singleton combinations
of F, U, and T. */

/* The values */
enum truth_val {  // the ordering of these values is important!
  tv_FUT,   // may be anything
  tv_UT,    // may be undefined or true
  tv_FT,    // may be false or true
  tv_T,     // certainly true
  tv_FU,    // may be false or undefined
  tv_U,     // certainly undefined
  tv_F      // certainly false
};

/* Basic propositional operators */
const truth_val tv_neg[] = { tv_FUT, tv_FU, tv_FT, tv_F, tv_UT, tv_U, tv_T };
const truth_val tv_conj[7][7] = {
  { tv_FUT, tv_FUT, tv_FUT, tv_FUT, tv_FU, tv_FU, tv_F },
  { tv_FUT, tv_UT , tv_FUT, tv_UT , tv_FU, tv_U , tv_F },
  { tv_FUT, tv_FUT, tv_FT , tv_FT , tv_FU, tv_FU, tv_F },
  { tv_FUT, tv_UT , tv_FT , tv_T  , tv_FU, tv_U , tv_F },
  { tv_FU , tv_FU , tv_FU , tv_FU , tv_FU, tv_FU, tv_F },
  { tv_FU , tv_U  , tv_FU , tv_U  , tv_FU, tv_U , tv_F },
  { tv_F  , tv_F  , tv_F  , tv_F  , tv_F , tv_F , tv_F }
};
inline truth_val operator !( truth_val t1 ){ return tv_neg[ t1 ]; }
truth_val operator &&( truth_val t1, truth_val t2 ){
  return tv_conj[ t1 ][ t2 ];
}
inline truth_val operator ||( truth_val t1, truth_val t2 ){
  return tv_neg[ tv_conj[ tv_neg[ t1 ] ][ tv_neg[ t2 ] ] ];
}
inline truth_val tv_def( truth_val t1 ){
  return t1 & 2 ? tv_T : t1 == tv_U ? tv_F : tv_FT;
}

/* Testing presence|absence of F|U|T */
inline bool tv_may_F( truth_val tv ){ return !( tv & 1 ); }
inline bool tv_may_U( truth_val tv ){ return !( tv & 2 ); }
inline bool tv_may_T( truth_val tv ){ return !( tv & 4 ); }
inline bool tv_no_F( truth_val tv ){ return tv & 1; }
inline bool tv_no_U( truth_val tv ){ return tv & 2; }
inline bool tv_no_T( truth_val tv ){ return tv & 4; }

/* Eliminating U */
inline truth_val U_to_F( truth_val tv ){
  return tv < tv_T ? tv_FT : tv > tv_T ? tv_F : tv_T;
}
inline truth_val U_to_T( truth_val tv ){
  return tv == tv_F ? tv_F : tv & 1 ? tv_T : tv_FT;
}

/* Let F=1, U=2, and T=4. Return the minimum / maximum content of tv. */
unsigned tv_min( truth_val tv ){
  if( tv == tv_T ){ return 4; }
  if( tv == tv_U || tv == tv_UT ){ return 2; }
  return 1;
}
unsigned tv_max( truth_val tv ){
  if( tv == tv_F ){ return 1; }
  if( tv == tv_U || tv == tv_FU ){ return 2; }
  return 4;
}


/* Reasoning operators */
bool tv_r_impl( truth_val &t1, truth_val t2 ){
  if( t1 == tv_T ){ return tv_may_T( t2 ); }
  t1 = t2; return true;
}
bool tv_r_lpmi( truth_val &t1, truth_val t2 ){
  if( tv_may_T( t1 ) ){ t1 = t2; return true; }
  if( t2 == tv_T ){ return false; }
  else{ t1 = truth_val( t2 | 4 ); return true; }
}
bool tv_r_eq( truth_val &t1, truth_val t2 ){
  if( t1 == tv_T ){ return tv_may_T( t2 ); }
  if( tv_may_T( t1 ) ){ t1 = t2; return true; }
  if( t2 == tv_T ){ return false; }
  else{ t1 = truth_val( t2 | 4 ); return true; }
}
bool tv_r_iden( truth_val &t1, truth_val t2 ){
  unsigned uu = t1 | t2;
  if( uu < 7 ){ t1 = truth_val( uu ); return true; }
  else{ return false; }
}


/* Check various aspects of the implementation of tv_type. */
void tv_check(){
  for( unsigned ii = 0; ii < 7; ++ii ){
    truth_val t1 = truth_val( ii );

    /* Properties of negation */
    if( (!!t1) != t1 ){ mc_err_print( "Double negation fails" ); return; }

    /* Properties of conjunction */
    if( ( t1 && tv_T ) != t1 ){ mc_err_print( "And T fails" ); return; }
    if( ( t1 && tv_F ) != tv_F ){ mc_err_print( "And F fails" ); return; }
    for( unsigned jj = 0; jj < 7; ++jj ){
      truth_val t2 = truth_val( jj );
      if( ( t1 && t2 ) != ( t2 && t1 ) ){
        mc_err_print( "Symmetry of and fails" ); return;
      }
      if( ( tv_may_F( t1 ) || tv_may_F( t2 ) ) != tv_may_F( t1 && t2 ) ){
        mc_err_print( "May yield F fails" ); return;
      }
      if(
        (
          ( tv_may_U( t1 ) || tv_may_U( t2 ) ) &&
          ( tv_may_U( t1 ) || tv_may_T( t1 ) ) &&
          ( tv_may_U( t2 ) || tv_may_T( t2 ) )
        ) != tv_may_U( t1 && t2 )
      ){
        mc_err_print( "May yield U fails" ); return;
      }
      if( ( tv_may_T( t1 ) && tv_may_T( t2 ) ) != tv_may_T( t1 && t2 ) ){
        mc_err_print( "May yield T fails" ); return;
      }
    }

    /* Properties of disjunction */
    if( ( t1 || tv_T ) != tv_T ){ mc_err_print( "Or T fails" ); return; }
    if( ( t1 || tv_F ) != t1 ){ mc_err_print( "Or F fails" ); return; }
    for( unsigned jj = 0; jj < 7; ++jj ){
      truth_val t2 = truth_val( jj );
      if( ( t1 || t2 ) != ( t1 || t2 ) ){
        mc_err_print( "Symmetry of or fails" ); return;
      }
      if( ( tv_may_F( t1 ) && tv_may_F( t2 ) ) != tv_may_F( t1 || t2 ) ){
        mc_err_print( "May yield F fails" ); return;
      }
      if(
        (
          ( tv_may_U( t1 ) || tv_may_U( t2 ) ) &&
          ( tv_may_U( t1 ) || tv_may_F( t1 ) ) &&
          ( tv_may_U( t2 ) || tv_may_F( t2 ) )
        ) != tv_may_U( t1 || t2 )
      ){
        mc_err_print( "May yield U fails" ); return;
      }
      if( ( tv_may_T( t1 ) && tv_may_T( t2 ) ) != tv_may_T( t1 && t2 ) ){
        mc_err_print( "May yield T fails" ); return;
      }
    }

  }
}


#include <climits>


/* Overflow-aware multiplication: returns 0 if overflow. Assumes ii != 0. */
inline unsigned ovfl_mult( unsigned ii, unsigned jj ){
  unsigned kk = ii * jj;
  return kk / ii == jj ? kk : 0;
}


/* Greatest common divisor */
inline unsigned gcd( unsigned n1, unsigned n2 ){
  while( true ){
    if( !n2 ){ return n1; }
    n1 %= n2;
    if( !n1 ){ return n2; }
    n2 %= n1;
  }
}


/* A number type that may hold a precise rational value, the value undefined,
an interval consisting of two double values, the combination of the latter and
undefined, or a seven-valued truth value. The undefined arises from 1/0, for
instance. In the third last case, the precise value is in the interval. In the
second last case, it is in the interval or the undefined. */

class number{
public:

  /* Variables */

  enum num_type {
    und,  // the undefined
    zer,  // special case for the value zero
    pos,  // positive rational
    neg,  // negative rational
    dbl,  // interval and not undefined
    dbu,  // interval or undefined
    trv   // truth value
  } type;

  union{
    unsigned r_nu;    // numerator
    double d_lo;      // start of interval of doubles
    truth_val t_val;  // truth value
  };
  union{
    unsigned r_de;    // denominator
    double d_hi;      // end of interval of doubles
  };


  /* Constructors */

  number(): type( und ) {}

  number( unsigned nu, unsigned de, bool is_neg = false ):
    type( de ? nu ? is_neg ? neg : pos : zer : und )
  {
    unsigned gg = gcd( nu, de );
    r_nu = nu / gg; r_de = de / gg;
  }

  number( int ii ): type( ii < 0 ? neg : ii ? pos : zer ){
    r_nu = ii < 0 ? -ii : ii; r_de = 1;
  }

  number( num_type type, double lo_, double hi_ ): type( type ){
    d_lo = lo_; d_hi = hi_;
  }

  number( truth_val tv ): type( trv ), t_val( tv ){}


  /* Low-level manipulation */

  /* Convert the number to dbl, if it is not dbl, dbu, or und. */
  void to_dblu(){
    switch( type ){
    case zer: type = dbl; d_lo = d_hi = 0.; return;
    case pos: case neg: {
      bool do_round = r_nu >= 0xFFffFFff || ( ( r_de - 1 ) & r_de );
      d_lo = double( r_nu ) / double( r_de );
      if( type == neg ){ d_lo = -d_lo; }
      if( do_round ){
        d_hi = nextafter( d_lo, 1/0. );
        d_lo = nextafter( d_lo, -1/0. );
      }else{ d_hi = d_lo; }
      type = dbl; return;
    }
    case trv: type = dbl; d_lo = d_hi = t_val; return;
    case dbl: case dbu:   // standardize the representation of undefined
      if( !( d_lo == d_lo ) ){ type = dbu; d_lo = -1/0.; }
      if( !( d_hi == d_hi ) ){ type = dbu; d_hi = 1/0.; }
    default: return;
    }
  }

  /* Update ends of intervals (used in implementation of arithmetic). */
  inline void set_lo_hi( double dd ){
    if( dd < d_lo ){ d_lo = dd; }
    if( dd > d_hi ){ d_hi = dd; }
  }
  inline void set_lo_hi( double dd, double rdn, double rup ){
    if( dd <= d_lo ){ d_lo = nextafter( dd, rdn ); }
    if( dd >= d_hi ){ d_hi = nextafter( dd, rup ); }
  }

  /* Replace an interval by its low|high end. */
  inline void ddn(){ if( type == dbl || type == dbu ){ d_hi = d_lo; } }
  inline void dup(){ if( type == dbl || type == dbu ){ d_lo = d_hi; } }

  /* Yield a hash value. */
  inline unsigned hash() const {
    switch( type ){
      default:
      case und: return 271828;
      case zer: return 314159;
      case pos: return r_nu ^ r_de;
      case neg: return ~( r_nu ^ r_de );
      case dbl: return unsigned( ( d_lo + d_hi ) / 2. );
      case dbu: return 141421 + unsigned( ( d_lo + d_hi ) / 2. );
      case trv: return 987654 + 101 * t_val;
    }
  }


  /* Yield the value in various types. */

  inline truth_val to_tv() const { return type == trv ? t_val : tv_FUT; }

  unsigned to_unsigned() const {
    return type == pos && r_de == 1 ? r_nu : 0;
  }

  int to_int() const {
    if( type == zer ){ return 0; }
    if( type == pos && r_nu <= INT_MAX && r_de == 1 ){ return r_nu; }
    if( type == neg && r_nu <= INT_MAX && r_de == 1 ){ return -r_nu; }
    return INT_MIN;
  }

  unsigned numer(){
    if( type == pos || type == neg ){ return r_nu; }
    else{ return 0; }
  }

  unsigned denom(){
    if( type == pos || type == neg ){ return r_de; }
    else if( type == zer ){ return 1; }
    else{ return 0; }
  }


  /* Information on type and value. If the answer is uncertain or undefined,
   isc-functions return false. */

  inline bool is_und() const { return type == und; }
  inline bool is_zer() const { return type == zer; }
  inline bool is_pos() const { return type == pos; }
  inline bool is_neg() const { return type == neg; }
  inline bool is_dbl() const { return type == dbl; }
  inline bool is_dbu() const { return type == dbu; }
  inline bool is_trv() const { return type == trv; }

  inline bool is_uns_type() const {
    return type == zer || ( type == pos && r_de == 1 );
  }

  inline bool is_int_type() const {
    if( type == zer ){ return true; }
    return ( type == pos || type == neg ) && r_de == 1;
  }

  inline bool isc_neg() const {
    return type == neg || ( type == dbl && d_hi < 0 );
  }

  inline bool isc_1() const {
    return
      ( type == pos && r_de == 1 && r_nu == 1 ) ||
      ( type == dbl && d_lo == 1. && d_hi == 1. );
  }

  inline bool nu_even() const {
    return
      type == zer || ( ( type == pos || type == neg ) && !( r_nu % 2 ) );
  }

  inline bool de_odd() const {
    return type == zer || ( ( type == pos || type == neg ) && r_de % 2 );
  }

  inline bool is_unprecise_type() const { return type == dbl || type == dbu; }

  inline bool is_prop_rat_type() const {
    return ( type == pos || type == neg ) && r_de != 1;
  }

  inline truth_val is_int() const {
    switch( type ){
    case und: case trv: return tv_U;
    case zer: return tv_T;
    case pos: case neg: return r_de == 1 ? tv_T : tv_F;
    case dbl: {
      double fl = floor( d_lo );
      if( fl < d_lo && d_hi < fl + 1. ){ return tv_F; }
      if( d_lo == d_hi ){ return fl == d_lo ? tv_T : tv_F; }
      return tv_FT;
    }
    case dbu: {
      double fl = floor( d_lo );
      if( fl < d_lo && d_hi < fl + 1. ){ return tv_FU; }
      if( d_lo == d_hi ){ return fl == d_lo ? tv_UT : tv_FU; }
      return tv_FUT;
    }
    default: return tv_FUT;
    }
  }

  inline bool is_informative(){
    switch( type ){
    case zer: case pos: case neg: return true;
    case dbl: return
      ( 0 < d_lo && d_hi - d_lo < d_lo / 1024 ) ||
      ( 0 > d_hi && d_lo - d_hi > d_hi / 1024 ) ||
      ( -1E-10 < d_lo && d_lo <= 0. && 0. <= d_hi && d_hi < 1E-10 );
    default: return false;
    }
  }


  /* Arithmetic */

  inline void negate(){
    switch( type ){
      case pos: type = neg; return;
      case neg: type = pos; return;
      case dbl: case dbu: {
        double tmp = d_lo; d_lo = -d_hi; d_hi = -tmp; return;
      }
      default: return;
    }
  }

  inline void invert(){
    switch( type ){
      case zer: case trv: type = und; return;
      case pos: case neg: {
        unsigned nn = r_nu; r_nu = r_de; r_de = nn; return;
      }
      case dbl: case dbu: {
        if( d_lo < 0. ){
          if( d_hi > 0. ){ d_lo = -1/0.; d_hi = 1/0.; type = dbu; return; }
          double tmp = d_lo;
          if( d_hi == 0. ){ d_lo = -1/0.; type = dbu; }
          else{ d_lo = nextafter( 1./d_hi, -1/0. ); }
          d_hi = nextafter( 1./tmp, 0. ); return;
        }
        if( d_lo == 0. ){
          if( d_hi == 0. ){ type = und; return; }
          if( d_hi < 1/0. ){ d_lo = nextafter( 1./d_hi, 0. ); d_hi = 1/0.; }
          type = dbu; return;
        }
        // d_lo > 0
        double tmp = d_lo;
        d_lo = nextafter( 1./d_hi, 0. );
        d_hi = nextafter( 1./tmp, 1/0. ); return;
       }
      default: return;
    }
  }

  number & operator +=( number x2 ){
    if( type == und || x2.type == und || type == trv || x2.type == trv ){
      type = und; return *this;
    }
    if( type == zer ){ *this = x2; return *this; }
    if( x2.type == zer ){ return *this; }
    if( x2.type == dbl || x2.type == dbu ){ to_dblu(); }
    if( type == dbl || type == dbu ){
      if( x2.type == dbu ){ type = dbu; }else{ x2.to_dblu(); }
      d_lo = nextafter( d_lo + x2.d_lo, -1/0. );
      d_hi = nextafter( d_hi + x2.d_hi, 1/0. ); return *this;
    }
    // type and x2.type are in {pos, neg}
    unsigned
      gg = gcd( r_de, x2.r_de ),
      d1 = r_de / gg, d2 = x2.r_de / gg,
      dd = ovfl_mult( r_de, d2 ),
      n1 = ovfl_mult( r_nu, d2 ), n2 = ovfl_mult( x2.r_nu, d1 );
    if( !( n1 && n2 && dd ) ){ to_dblu(); operator +=( x2 ); return *this; }
    if( type == x2.type ){
      if( n1 + n2 < n1 ){ to_dblu(); operator +=( x2 ); return *this; }
      r_nu = n1 + n2;
    }else if( n1 == n2 ){
      type = zer; return *this;
    }else if( n1 > n2 ){
      r_nu = n1 - n2;
    }else{
      r_nu = n2 - n1; type = x2.type;
    }
    gg = gcd( r_nu, dd ); r_nu /= gg; r_de = dd / gg;
    return *this;
  }

  number & operator -=( number x2 ){ x2.negate(); return operator +=( x2 ); }

  /* *= of two non-zero rationals */
  number & sub_prod( unsigned n2, unsigned d2, num_type t2 ){
    unsigned g1 = gcd( r_nu, d2 ), g2 = gcd( r_de, n2 );
    r_nu /= g1; d2 /= g1; r_de /= g2; n2 /= g2;
    unsigned nn = ovfl_mult( r_nu, n2 ), dd = ovfl_mult( r_de, d2 );
    if( nn && dd ){
      r_nu = nn; r_de = dd; type = type == t2 ? pos : neg; return *this;
    }
    d_lo = ( double( r_nu ) / r_de ) * ( double( n2 ) / d2 );
    if( t2 != type ){ d_lo = -d_lo; }
    d_hi = nextafter( d_lo, 1/0. );
    d_lo = nextafter( d_lo, -1/0. );
    type = dbl; return *this;
  }

  number & operator *=( number x2 ){
    if( type == und || x2.type == und || type == trv || x2.type == trv ){
      type = und; return *this;
    }
    if( type == zer ){
      if( x2.type == dbu ){ type = dbu; d_lo = d_hi = 0.; }
      return *this;
    }
    if( x2.type == zer ){
      if( type == dbu ){ d_lo = d_hi = 0.; }else{ type = zer; }
      return *this;
    }
    if( x2.type == dbl || x2.type == dbu ){ to_dblu(); }
    if( type != dbl && type != dbu ){
      return sub_prod( x2.r_nu, x2.r_de, x2.type );
    }
    if( x2.type == dbu ){ type = dbu; }else{ x2.to_dblu(); }
    double y1 = 0., y2 = 0., y3 = 0., y4 = 0.;
    if( d_lo != 0 ){
      double dir = d_lo > 0. ? 1/0. : -1/0.;
      if( x2.d_lo != 0. ){ y1 = nextafter( d_lo * x2.d_lo, -dir ); }
      if( x2.d_hi != 0. ){ y2 = nextafter( d_lo * x2.d_hi, dir ); }
    }
    if( d_hi != 0 ){
      double dir = d_hi > 0. ? 1/0. : -1/0.;
      if( x2.d_lo != 0. ){ y3 = nextafter( d_hi * x2.d_lo, -dir ); }
      if( x2.d_hi != 0. ){ y4 = nextafter( d_hi * x2.d_hi, dir ); }
    }
    if( y1 < y4 ){ d_lo = y1; d_hi = y4; }
    else{ d_lo = y4; d_hi = y1; }
    if( y2 < y3 ){
      if( y2 < d_lo ){ d_lo = y2; }
      if( y3 > d_hi ){ d_hi = y3; }
    }else{
      if( y3 < d_lo ){ d_lo = y3; }
      if( y2 > d_hi ){ d_hi = y2; }
    }
    return *this;
  }

  number & operator /=( number x2 ){
    if(
      type == und || x2.type == und || type == trv || x2.type == trv ||
      x2.type == zer
    ){ type = und; return *this; }
    if( type == zer ){
      if(
        x2.type == dbu || ( x2.type == dbl && x2.d_lo <= 0. && x2.d_hi >= 0. )
      ){ type = dbu; d_lo = d_hi = 0.; }
      return *this;
    }
    if( x2.type == dbl || x2.type == dbu ){ to_dblu(); }
    if( type == dbl || type == dbu ){ x2.invert(); return operator *=( x2 ); }
    return sub_prod( x2.r_de, x2.r_nu, x2.type );
  }


};


/* Elementary arithmetic as non-member operators */
inline number operator -( number xx ){ xx.negate(); return xx; }
inline number operator +( number x1, number x2 ){ x1 += x2; return x1; }
inline number operator -( number x1, number x2 ){ x1 -= x2; return x1; }
inline number operator *( number x1, number x2 ){ x1 *= x2; return x1; }
inline number operator /( number x1, number x2 ){ x1 /= x2; return x1; }


/* Often used constant numbers */
const number
  num0( 0 ), num1( 1 ), numu( number::und, 0., 0. ),
  numu0( number::dbu, 0., 0. ),
  numu1( number::dbu, 1., 1. ),
  nume( number::dbl, nextafter( exp(1.), 0. ), nextafter( exp(1.), 1/0. ) ),
  numpi2(
    number::dbl, nextafter( 2*atan(1.), 0. ), nextafter( 2*atan(1.), 1/0. )
  ),
  numpi(
    number::dbl, nextafter( 4*atan(1.), 0. ), nextafter( 4*atan(1.), 1/0. )
  ),
  num3pi2(
    number::dbl, nextafter( 6*atan(1.), 0. ), nextafter( 6*atan(1.), 1/0. )
  ),
  num2pi(
    number::dbl, nextafter( 8*atan(1.), 0. ), nextafter( 8*atan(1.), 1/0. )
  );


/* Yield a single double value. */
inline double to_double( number xx ){
  switch( xx.type ){
    default:
    case number::und: case number::dbu: case number::trv: return 0/0.;
    case number::zer: return 0.;
    case number::pos: return double( xx.r_nu ) / double( xx.r_de );
    case number::neg: return -double( xx.r_nu ) / double( xx.r_de );
    case number::dbl: return ( xx.d_lo + xx.d_hi ) / 2.;
  }
}


/* Absolute value */
number abs( number xx ){
  if( xx.type == number::neg ){ xx.type = number::pos; }
  else if( xx.type == number::dbl || xx.type == number::dbu ){
    if( xx.d_lo < 0. ){
      double tmp = -xx.d_lo;
      if( xx.d_hi > 0. ){
        if( xx.d_hi < tmp ){ xx.d_hi = tmp; }
        xx.d_lo = 0; return xx;
      }
      xx.d_lo = -xx.d_hi; xx.d_hi = tmp;
    }
  }
  return xx;
}


/* Floor and ceiling */

number floor( number xx ){
  if( xx.type == number::zer ){ return xx; }
  if( xx.type == number::und || xx.type == number::trv ){ return numu; }
  if( xx.type == number::pos ){
    xx.r_nu /= xx.r_de; xx.r_de = 1;
    if( !xx.r_nu ){ xx.type = number::zer; }
    return xx;
  }
  if( xx.type == number::neg ){
    if( xx.r_de != 1 ){ xx.r_nu /= xx.r_de; ++xx.r_nu; xx.r_de = 1; }
    return xx;
  }
  xx.d_lo = floor( xx.d_lo ); xx.d_hi = floor( xx.d_hi );
  if( xx.type == number::dbl && xx.d_lo == xx.d_hi ){
    bool is_neg = xx.d_lo < 0; double dd = xx.d_lo;
    if( is_neg ){ dd = -dd; }
    unsigned uu = unsigned( dd );
    if( dd != uu ){ return xx; }
    xx.type = is_neg ? number::neg : uu ? number::pos : number::zer;
    xx.r_nu = uu; xx.r_de = 1;
  }
  return xx;
}

number ceil( number xx ){
  if( xx.type == number::zer ){ return xx; }
  if( xx.type == number::und || xx.type == number::trv ){ return numu; }
  if( xx.type == number::neg ){
    xx.r_nu /= xx.r_de; xx.r_de = 1;
    if( !xx.r_nu ){ xx.type = number::zer; }
    return xx;
  }
  if( xx.type == number::pos ){
    if( xx.r_de != 1 ){ xx.r_nu /= xx.r_de; ++xx.r_nu; xx.r_de = 1; }
    return xx;
  }
  xx.d_lo = ceil( xx.d_lo ); xx.d_hi = ceil( xx.d_hi );
  if( xx.type == number::dbl && xx.d_lo == xx.d_hi ){
    bool is_neg = xx.d_lo < 0; double dd = xx.d_lo;
    if( is_neg ){ dd = -dd; }
    unsigned uu = unsigned( dd );
    if( dd != uu ){ return xx; }
    xx.type = is_neg ? number::neg : uu ? number::pos : number::zer;
    xx.r_nu = uu; xx.r_de = 1;
  }
  return xx;
}


/* Square root, power, exponent, and logarithms */

number sqrt( number xx ){
  if(
    xx.type == number::und || xx.type == number::neg || xx.type == number::trv
  ){ return numu; }
  if( xx.type == number::zer ){ return num0; }
  if( xx.type == number::pos ){
    unsigned pp = sqrt( xx.r_nu ) + .5;
    if( pp * pp == xx.r_nu ){
      unsigned qq = sqrt( xx.r_de ) + .5;
      if( qq * qq == xx.r_de ){ xx.r_nu = pp; xx.r_de = qq; return xx; }
    }
    xx.to_dblu();
  }
  if( xx.d_hi < 0. ){ return numu; }
  xx.d_hi = nextafter( sqrt( xx.d_hi ), 1/0. );
  if( xx.d_lo < 0. ){ xx.type = number::dbu; xx.d_lo = 0.; }
  else{ xx.d_lo = nextafter( sqrt( xx.d_lo ), 0. ); }
  return xx;
}

number exp( number xx ){
  if( xx.type == number::und || xx.type == number::trv ){ return numu; }
  if( xx.type == number::zer ){ return num1; }
  xx.to_dblu();
  xx.d_lo = nextafter( exp( xx.d_lo ), 0. );
  xx.d_hi = nextafter( exp( xx.d_hi ), 1/0. );
  return xx;
}

number ln( number xx ){
  if(
    xx.type == number::und || xx.type == number::zer ||
    xx.type == number::neg || xx.type == number::trv
  ){ return numu; }
  if( xx.isc_1() ){ return num0; }
  xx.to_dblu();
  if( xx.d_hi <= 0. ){ return numu; }
  if( xx.d_lo <= 0. ){ xx.type = number::dbu; xx.d_lo = -1/0.; }
  else{ xx.d_lo = nextafter( log( xx.d_lo ), -1/0. ); }
  xx.d_hi = nextafter( log( xx.d_hi ), 1/0. );
  return xx;
}

number log10( number xx ){
  if(
    xx.type == number::und || xx.type == number::zer ||
    xx.type == number::neg || xx.type == number::trv
  ){ return numu; }
  if( xx.isc_1() ){ return num0; }
  xx.to_dblu();
  if( xx.d_hi <= 0. ){ return numu; }
  if( xx.d_lo <= 0. ){ xx.type = number::dbu; xx.d_lo = -1/0.; }
  else{ xx.d_lo = nextafter( log10( xx.d_lo ), -1/0. ); }
  xx.d_hi = nextafter( log10( xx.d_hi ), 1/0. );
  return xx;
}

number log2( number xx ){
  const double
    ln2lo = nextafter( log(2), 0. ), ln2hi = nextafter( log(2), 1/0. );
  if(
    xx.type == number::und || xx.type == number::zer ||
    xx.type == number::neg || xx.type == number::trv
  ){ return numu; }
  unsigned pow2 = 0;
  if( xx.type == number::pos && xx.r_de == 1 ){
    while( !( xx.r_nu & 1 ) ){ xx.r_nu >>= 1; ++pow2; }
    if( xx.r_nu == 1 ){ return number( pow2 ); }
  }
  if( xx.type == number::dbl && xx.d_lo == 1. && xx.d_hi == 1. ){
    return num0;
  }
  xx.to_dblu();
  if( xx.d_hi <= 0. ){ return numu; }
  if( xx.d_lo <= 0. ){ xx.type = number::dbu; xx.d_lo = -1/0.; }
  else{ xx.d_lo = nextafter( log( xx.d_lo ) / ln2hi, -1/0. ); }
  xx.d_hi = nextafter( log( xx.d_hi ) / ln2lo, 1/0. );
  return xx + pow2;
}


number pow( number x1, number x2 ){

  /* Easy special cases */
  if(
    x1.type == number::und || x2.type == number::und ||
    x1.type == number::trv || x2.type == number::trv
  ){ return numu; }
  if( x1.isc_1() ){ return x2.type == number::dbu ? numu1 : num1; }
  if( x2.type == number::zer ){
    return x1.type == number::dbu ? numu1 : num1;
  }
  if( x1.type == number::zer ){
    if( x2.type == number::pos ){ return num0; }
    if( x2.type == number::neg ){ return numu; }
    if( x2.d_hi < 0. ){ return numu; }
    if( x2.d_hi == 0. ){
      return x2.d_lo < 0. || x2.type == number::dbu ? numu1 : num1;
    }
    // x2.d_hi > 0.
    if( x2.d_lo > 0. && x2.type == number::dbl ){ return x1; }
    x1.d_lo = 0.;
    x1.d_hi = x2.d_lo > 0. ? 0. : 1.;
    x1.type = x2.d_lo < 0. || x2.type == number::dbu
      ? number::dbu : number::dbl;
    return x1;
  }

  /* Explicit multiplication, if exponent is an integer near zero */
  if(
    ( x2.type == number::pos || x2.type == number::neg ) &&
    x2.r_de == 1 && x2.r_nu <= 10
  ){
    if( x2.type == number::neg ){ x1.invert(); }
    number yy = x1;
    for( unsigned ii = 1; ii < x2.r_nu; ++ii ){ yy *= x1; }
    return yy;
  }

  // If exponent is certainly rational, at least one of these is true.
  bool
    odd_de = x2.type == number::pos || x2.type == number::neg,
    odd_nu = odd_de && x2.r_nu % 2;
  odd_de = odd_de && x2.r_de % 2;

  /* General case, compute via pow( double ). */
  number yy( number::dbl, 1/0., -1/0. );
  x1.to_dblu(); x2.to_dblu();
  if( x2.d_lo <= 0. && x2.d_hi >= 0. ){ yy.set_lo_hi( 1. ); }
  if( x1.d_lo < 0. ){
    if( !odd_de && ( x2.d_lo < 0. || x2.d_hi > 0. ) ){
      yy.type = number::dbu;
    }
    if( !odd_nu || odd_de ){
      double dd = pow( -x1.d_lo, x2.d_lo );
      if( !odd_nu ){ yy.set_lo_hi( dd, 0., 1/0. ); }
      if( odd_nu == odd_de ){ yy.set_lo_hi( -dd, -1/0., 0. ); }
      dd = pow( -x1.d_lo, x2.d_hi );
      if( !odd_nu ){ yy.set_lo_hi( dd, 0., 1/0. ); }
      if( odd_nu == odd_de ){ yy.set_lo_hi( -dd, -1/0., 0. ); }
    }
    if( x1.d_hi < 0. ){
      if( !odd_de ){ yy.type = number::dbu; }
      if( !odd_nu || odd_de ){
        double dd = pow( -x1.d_hi, x2.d_lo );
        if( !odd_nu ){ yy.set_lo_hi( dd, 0., 1/0. ); }
        if( odd_nu == odd_de ){ yy.set_lo_hi( -dd, -1/0., 0. ); }
        dd = pow( -x1.d_hi, x2.d_hi );
        if( !odd_nu ){ yy.set_lo_hi( dd, 0., 1/0. ); }
        if( odd_nu == odd_de ){ yy.set_lo_hi( -dd, -1/0., 0. ); }
      }
    }
  }
  if( x1.d_hi > 0. ){
    yy.set_lo_hi( pow( x1.d_hi, x2.d_lo ), 0., 1/0. );
    yy.set_lo_hi( pow( x1.d_hi, x2.d_hi ), 0., 1/0. );
    if( x1.d_lo > 0. ){
      yy.set_lo_hi( pow( x1.d_lo, x2.d_lo ), 0., 1/0. );
      yy.set_lo_hi( pow( x1.d_lo, x2.d_hi ), 0., 1/0. );
    }
  }
  if( x1.d_lo <= 0. && x1.d_hi >= 0. ){
    if( x2.d_hi > 0. ){
      yy.set_lo_hi( 0. );
      if( x1.d_lo < 0. && x2.d_lo == 0. ){ yy.set_lo_hi( -1. ); }
    }
    if( x2.d_lo < 0. ){
      yy.type = number::dbu;
      if( x1.d_hi > 0. ){ yy.d_hi = 1/0.; }
      if( x1.d_lo < 0. ){ yy.d_lo = -1/0.; yy.d_hi = 1/0.; }
    }
  }
  if( yy.d_lo > yy.d_hi ){ yy.type = number::und; }
  return yy;

}


/* Trigonometric and hyperbolic functions */

void trig_scale( number &xx ){
  if(
    xx.type == number::und || xx.type == number::zer || xx.type == number::trv
  ){ return; }
  xx.to_dblu();
  if( xx.d_lo < -1e20 || xx.d_hi > 1e20 ){
    xx.d_lo = 0.; xx.d_hi = 7.; return;
  }
  double dd = floor( xx.d_lo / num2pi.d_hi );
  xx -= number( number::dbl, dd, dd ) * num2pi;
  while( xx.d_lo < 0 ){ xx += num2pi; }
}

number sin( number xx ){
  trig_scale( xx );
  if( xx.type == number::und || xx.type == number::zer ){ return xx; }
  if( xx.type == number::trv ){ return numu; }
  double d1 = sin( xx.d_lo ), d2 = sin( xx.d_hi );
  number yy( number::dbl,
    nextafter( d1 < d2 ? d1 : d2, -1. ), nextafter( d1 < d2 ? d2 : d1, 1. )
  );
  do{
    if( xx.d_lo <= numpi2.d_hi && xx.d_hi >= numpi2.d_lo ){ yy.d_hi = 1.; }
    if( xx.d_lo <= num3pi2.d_hi && xx.d_hi >= num3pi2.d_lo ){ yy.d_lo = -1.; }
    xx -= num2pi;
  }while( xx.d_hi >= numpi2.d_lo && ( yy.d_lo > -1. || yy.d_hi < 1. ) );
  return yy;
}

number cos( number xx ){
  trig_scale( xx );
  if( xx.type == number::und || xx.type == number::trv ){ return numu; }
  if( xx.type == number::zer ){ return num1; }
  double d1 = cos( xx.d_lo ), d2 = cos( xx.d_hi );
  number yy( number::dbl,
    nextafter( d1 < d2 ? d1 : d2, -1. ), nextafter( d1 < d2 ? d2 : d1, 1. )
  );
  do{
    if( xx.d_lo <= 0. && xx.d_hi >= 0. ){ yy.d_hi = 1.; }
    if( xx.d_lo <= numpi.d_hi && xx.d_hi >= numpi.d_lo ){ yy.d_lo = -1.; }
    xx -= num2pi;
  }while( xx.d_hi >= 0 && ( yy.d_lo > -1. || yy.d_hi < 1. ) );
  return yy;
}

number tan( number xx ){
  trig_scale( xx );
  if( xx.type == number::und || xx.type == number::zer ){ return xx; }
  if( xx.type == number::trv ){ return numu; }
  double d1 = tan( xx.d_lo ), d2 = tan( xx.d_hi );
  number yy( number::dbl,
    nextafter( d1 < d2 ? d1 : d2, -1/0. ),
    nextafter( d1 < d2 ? d2 : d1, 1/0. ) );
  do{
    if( xx.d_lo <= numpi2.d_hi && xx.d_hi >= numpi2.d_lo ){
      yy.type = number::dbu; yy.d_lo = -1/0.; yy.d_hi = 1/0.;
    }
    xx -= numpi;
  }while( xx.d_hi >= numpi2.d_lo && ( yy.d_lo > -1/0. || yy.d_hi < 1/0. ) );
  return yy;
}

number cot( number xx ){ return cos( xx ) / sin( xx ); }

number sinh( number xx ){
  if( xx.type == number::und || xx.type == number::trv ){ return numu; }
  if( xx.type == number::zer ){ return num0; }
  xx.to_dblu();
  xx.d_lo = nextafter( sinh( xx.d_lo ), -1/0. );
  xx.d_hi = nextafter( sinh( xx.d_hi ), 1/0. );
  return xx;
}

number cosh( number xx ){
  if( xx.type == number::und || xx.type == number::trv ){ return numu; }
  if( xx.type == number::zer ){ return num1; }
  xx.to_dblu();
  if( xx.d_lo <= 0 ){
    if( xx.d_hi >= 0 ){
      if( -xx.d_lo > xx.d_hi ){ xx.d_hi = -xx.d_lo; }
      xx.d_hi = nextafter( cosh( xx.d_hi ), 1/0. );
      xx.d_lo = 1.; return xx;
    }
    double yy = xx.d_lo;
    xx.d_lo = nextafter( cosh( xx.d_hi ), 1. );
    xx.d_hi = nextafter( cosh( yy ), 1/0. );
    return xx;
  }
  xx.d_lo = nextafter( cosh( xx.d_lo ), 1. );
  xx.d_hi = nextafter( cosh( xx.d_hi ), 1/0. );
  return xx;
}

number tanh( number xx ){
  if( xx.type == number::und || xx.type == number::trv ){ return numu; }
  if( xx.type == number::zer ){ return num0; }
  xx.to_dblu();
  xx.d_lo = nextafter( tanh( xx.d_lo ), -1. );
  xx.d_hi = nextafter( tanh( xx.d_hi ), 1. );
  return xx;
}


/* Factorial */
const unsigned fact_size = 200;   // maximum number of pre-computed factorials
number fact_val[ fact_size ];     // array of pre-computed factorials
unsigned fact_computed = 0;       // number of pre-computed factorials
number factorial( number nn ){
  switch( nn.type ){
  case number::zer: return 1;
  case number::dbl: case number::dbu:  //??? could approximate better
    return number( number::dbu, 1., 1/0. );
  case number::pos: {
    if( nn.r_de != 1 ){ return number::und; }
    unsigned ii = nn.r_nu;
    if( ii >= fact_size ){ ii = fact_size - 1; }
    fact_val[ 0 ] = 1;
    for( ; fact_computed < ii; ){
      ++fact_computed;
      fact_val[ fact_computed ] =
        fact_val[ fact_computed - 1 ] * fact_computed;
    }
    return fact_val[ ii ];
  }
  default: return number::und;
  }
}


/* Comparisons */

inline bool operator ==( number x1, number x2 ){
  if( x1.type != x2.type ){ return false; }
  if( x1.type == number::trv ){ return x1.t_val == x2.t_val; }
  if( x1.type == number::und || x1.type == number::zer ){ return true; }
  if( x1.type == number::dbl || x1.type == number::dbu ){
    return x1.d_lo == x2.d_lo && x1.d_hi == x2.d_hi;
  }
  return x1.r_nu == x2.r_nu && x1.r_de == x2.r_de;
}

truth_val num_eq( number x1, number x2 ){
  if(
    x1.type == number::und || x2.type == number::und ||
    x1.type == number::trv || x2.type == number::trv
  ){ return tv_U; }
  if(
    x1.type != number::dbl && x2.type != number::dbl &&
    x1.type != number::dbu && x2.type != number::dbu
  ){
    if( x1.type != x2.type ){ return tv_F; }
    if( x1.type == number::zer ){ return tv_T; }
    return x1.r_nu == x2.r_nu && x1.r_de == x2.r_de ? tv_T : tv_F;
  }
  x1.to_dblu(); x2.to_dblu();
  truth_val del_U =
    x1.type == number::dbu || x2.type == number::dbu ? tv_FUT : tv_FT;
  if( x1.d_hi < x2.d_lo ){ return truth_val( tv_FU | del_U ); }
  if( x2.d_hi < x1.d_lo ){ return truth_val( tv_FU | del_U ); }
  if( x1.d_hi == x2.d_lo && x2.d_hi == x1.d_lo ){
    return truth_val( tv_UT | del_U );
  }
  return truth_val( tv_FUT | del_U );
}

truth_val ration_lt( unsigned n1, unsigned d1, unsigned n2, unsigned d2 ){
  unsigned p1 = ovfl_mult( n1, d2 ), p2 = ovfl_mult( d1, n2 );
  if( p1 && p2 ){ return p1 < p2 ? tv_T : tv_F; }
  while( true ){
    if( n1 / d1 < n2 / d2 ){ return tv_T; }
    if( n1 / d1 > n2 / d2 ){ return tv_F; }
    n2 %= d2; if( !n2 ){ return tv_F; }
    n1 %= d1; if( !n1 ){ return tv_T; }
    if( d1 / n1 < d2 / n2 ){ return tv_F; }
    if( d1 / n1 > d2 / n2 ){ return tv_T; }
    d1 %= n1; if( !d1 ){ return tv_F; }
    d2 %= n2; if( !d2 ){ return tv_T; }
  }
}

truth_val num_lt( number x1, number x2 ){
  if(
    x1.type == number::und || x2.type == number::und ||
    x1.type == number::trv || x2.type == number::trv
  ){ return tv_U; }
  if(
    x1.type != number::dbl && x2.type != number::dbl &&
    x1.type != number::dbu && x2.type != number::dbu
  ){
    if( x1.type == number::zer ){
      return x2.type == number::pos ? tv_T : tv_F;
    }
    if( x1.type != x2.type ){
      return x1.type == number::neg ? tv_T : tv_F;
    }
    return x1.type == number::pos ?
      ration_lt( x1.r_nu, x1.r_de, x2.r_nu, x2.r_de ) :
      ration_lt( x2.r_nu, x2.r_de, x1.r_nu, x1.r_de );
  }
  x1.to_dblu(); x2.to_dblu();
  truth_val del_U =
    x1.type == number::dbu || x2.type == number::dbu ? tv_FUT : tv_FT;
  if( x1.d_hi < x2.d_lo ){ return truth_val( tv_UT | del_U ); }
  if( x2.d_hi <= x1.d_lo ){ return truth_val( tv_FU | del_U ); }
  return truth_val( tv_FUT | del_U );
}

truth_val num_lq( number x1, number x2 ){
  if(
    x1.type == number::und || x2.type == number::und ||
    x1.type == number::trv || x2.type == number::trv
  ){ return tv_U; }
  if(
    x1.type != number::dbl && x2.type != number::dbl &&
    x1.type != number::dbu && x2.type != number::dbu
  ){
    if( x1.type == number::zer ){
      return x2.type == number::neg ? tv_F : tv_T;
    }
    if( x1.type != x2.type ){
      return x1.type == number::neg ? tv_T : tv_F;
    }
    return x1.type == number::pos ?
      !ration_lt( x2.r_nu, x2.r_de, x1.r_nu, x1.r_de ) :
      !ration_lt( x1.r_nu, x1.r_de, x2.r_nu, x2.r_de );
  }
  x1.to_dblu(); x2.to_dblu();
  truth_val del_U =
    x1.type == number::dbu || x2.type == number::dbu ? tv_FUT : tv_FT;
  if( x1.d_hi <= x2.d_lo ){ return truth_val( tv_UT | del_U ); }
  if( x2.d_hi < x1.d_lo ){ return truth_val( tv_FU | del_U ); }
  return truth_val( tv_FUT | del_U );
}


/* Check some aspects of the number type. */
void num_check(){
  if( sizeof( double ) != 8 ){
    mc_err_print( "Unexpected sizeof( double )" );
  }
  number nn = factorial( 190 );
  if( !nn.is_dbl() ){ mc_err_print( "190! wrong type" ); }
  if( nn.d_lo != nextafter( 1/0., 0. ) ){
    mc_err_print( "190! wrong lo" );
  }
  if( nn.d_hi != 1/0. ){ mc_err_print( "190! wrong hi" ); }
}
