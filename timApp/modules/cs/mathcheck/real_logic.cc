copyright real_logic_cc( "real_logic.cc", "Antti Valmari", 20200313 );


/*** A data structure to represent c_0 + c_1 x_1 + ... + c_n x_n ≥ 0
  or c_0 + c_1 x_1 + ... + c_n x_n > 0 ***/

/* c_1 to c_n are called coefficients, and c_0 is the constant.
  Invariants:
    var_cnt ≤ relo_max_var
    ∀ i; var_cnt < i ≤ relo_max_var: coeff[i] = 0
  When normalized, obeys also the following invariants:
    var_cnt = 0 ∨ coeff[ var_cnt ] ≠ 0
    var_cnt = 0 → coeff[0] = 0
    var_cnt > 0 → coeff[1] ∈ ℤ, ..., coeff[ var_cnt ] ∈ ℤ
    var_cnt > 0 → gcd( coeff[ 1 ], ..., coeff[ var_cnt ] ) = 1
*/

const unsigned relo_max_var = 6;      // ≤ this many variables allowed
unsigned
  relo_var_count = 0,                 // this many variables globally in use
  relo_var_idx[ relo_max_var + 1 ];   // index here → index in main MathCheck

struct relo_ineq{
  bool strict;        // false = "≥", true = ">"
  unsigned var_cnt;   // number of variables (the n in c_n x_n)
  number coeff[ relo_max_var + 1 ];   // the c_i

  /* Constructor */
  relo_ineq(): strict( false ), var_cnt(0) {
    for( unsigned ii = 0; ii <= relo_max_var; ++ii ){ coeff[ ii ] = 0; }
  }


  /* Adds another inequality without normalizing. */
  void add( const relo_ineq & ie2 ){
    if( var_cnt < ie2.var_cnt ){ var_cnt = ie2.var_cnt; }
    for( unsigned ii = 0; ii <= var_cnt; ++ii ){
      coeff[ ii ] += ie2.coeff[ ii ];
    }
  }


  /* Subtracts another inequality without normalizing. */
  void subtract( const relo_ineq & ie2 ){
    if( var_cnt < ie2.var_cnt ){ var_cnt = ie2.var_cnt; }
    for( unsigned ii = 0; ii <= var_cnt; ++ii ){
      coeff[ ii ] -= ie2.coeff[ ii ];
    }
  }


  /* Changes direction of ≥ without normalizing. */
  inline void reverse(){
    for( unsigned ii = 0; ii <= var_cnt; ++ii ){ coeff[ ii ] = -coeff[ ii ]; }
  }


  /* Remove unnecessary last variables. */
  inline void prune_variables(){
    while( var_cnt && coeff[ var_cnt ].is_zer() ){ --var_cnt; }
  }


  /* This is executed when an inequation is put into a clause. In addition to
    obvious little things, this divides away the gcd of the coefficients. */
  void normalize(){

    /* Remove unnecessary last variables. */
    prune_variables();

    /* If no variables, convert to 0 ≥ 0 or 0 > 0. */
    if( !( var_cnt || coeff[0].is_zer() ) ){
      strict = coeff[0].is_neg(); coeff[0] = 0; return;
    }

    /* Convert the coefficients to integers. */
    unsigned gg = 1;
    for( unsigned ii = 1; ii <= var_cnt; ++ii ){
      unsigned dd = coeff[ ii ].denom();
      if( !dd ){ test_fail_compl(); return; }
      dd /= gcd( gg, dd );
      unsigned cc = gg * dd;
      if( dd != cc / gg ){ test_fail_compl(); return; }
      gg = cc;
    }
    if( gg > 1 ){
      for( unsigned ii = 0; ii <= var_cnt; ++ii ){ coeff[ ii ] *= gg; }
    }

    /* Divide away the gcd of the coefficients. */
    gg = 0;
    for( unsigned ii = 1; ii <= var_cnt && gg != 1; ++ii ){
      gg = gcd( gg, coeff[ ii ].numer() );
    }
    if( gg > 1 ){
      for( unsigned ii = 0; ii <= var_cnt; ++ii ){ coeff[ ii ] /= gg; }
    }

  }


  /* These recognize special cases. */
  inline bool is_false() const { return !var_cnt && strict; }
  inline bool is_true() const { return !var_cnt && !strict; }


  /* A mostly arbitrary order for sorting lists of inequalities that ignores
    the constants and strictness. */
  int compare_coeff( const relo_ineq & ie2 ) const {
    if( var_cnt < ie2.var_cnt ){ return -1; }
    if( var_cnt > ie2.var_cnt ){ return 1; }
    for( unsigned ii = 1; ii <= var_cnt; ++ii ){
      if( num_lt( coeff[ ii ], ie2.coeff[ ii ] ) == tv_T ){ return -1; }
      if( num_lt( ie2.coeff[ ii ], coeff[ ii ] ) == tv_T ){ return 1; }
    }
    return 0;
  }

  /* If my constant is smaller, or is the same but I am stricter, return -1.
    In the symmetric situation, return 1. Otherwise, return 0. */
  int compare_constant( const relo_ineq & ie2 ) const {
    if( num_lt( coeff[0], ie2.coeff[0] ) == tv_T ){ return -1; }
    if( num_lt( ie2.coeff[0], coeff[0] ) == tv_T ){ return 1; }
    if( strict > ie2.strict ){ return -1; }
    if( strict < ie2.strict ){ return 1; }
    return 0;
  }

  /* Logically negate. */
  inline void negate(){ reverse(); strict = !strict; }


  /* Assign a value to a variable. That vv ≠ 0 was checked in relo_DNF. */
  relo_ineq & assign( unsigned vv, number xx ){
    if( vv <= var_cnt ){
      coeff[0] += coeff[ vv ] * xx; coeff[ vv ] = 0; //normalize();
    }
    return *this;
  }


  /* Print the inequality. */
  void print() const {
    bool not_empty = false;
    if( !coeff[0].is_zer() ){ out_number( coeff[0] ); not_empty = true; }
    for( unsigned ii = 1; ii <= var_cnt; ++ii ){
      if( !coeff[ ii ].is_zer() ){
        if( not_empty ){
          out_am( ' ' );
          if( coeff[ ii ].is_pos() ){ out_op( op_plus ); }
        }
        if( coeff[ ii ] == number(-1) ){ out_op( op_minus ); }
        else if( !( coeff[ ii ] == number(1) ) ){ out_number( coeff[ ii ] ); }
        if( relo_var_idx[ ii ] != ~0u ){ out_idx_name( relo_var_idx[ ii ] ); }
        else{ out_am( '?' ); }
        not_empty = true;
      }
    }
    if( !not_empty ){ out_am( '0' ); }
    if( strict ){  out_am( " > 0" ); }else{ out_am( " >= 0" ); }
  }

};


class relo_clause{
  friend class relo_DNF;
  std::vector< relo_ineq > atoms;

public:

  /* Constructors */
  relo_clause(){}
  relo_clause( const relo_ineq & ie ){
    atoms.push_back( ie ); atoms[0].normalize();
    if( atoms[0].is_true() ){ atoms.clear(); }
  }

  /* These recognize special cases. */
  inline bool is_false() const {
    return !atoms.empty() && atoms[0].is_false();
  }
  inline bool is_true() const { return atoms.empty(); }


  /* Conjunction. Maintains order and, where able, drops subsumed atoms. */
  relo_clause & operator &=( const relo_clause & cl ){

    /* Special cases */
    if( is_false() || cl.is_true() ){ return *this; }
    if( is_true() || cl.is_false() ){ atoms = cl.atoms; return *this; }

    /* Merger */
    std::vector< relo_ineq > result;
    unsigned ii = 0, jj = 0;
    while( ii < atoms.size() && jj < cl.atoms.size() ){
      int cmp = atoms[ ii ].compare_coeff( cl.atoms[ jj ] );
      if( cmp < 0 ){ result.push_back( atoms[ ii ] ); ++ii; }
      else if( cmp > 0 ){ result.push_back( cl.atoms[ jj ] ); ++jj; }
      else if( atoms[ ii ].compare_constant( cl.atoms[ jj ] ) <= 0 ){ ++jj; }
      else{ ++ii; }
    }
    while( ii < atoms.size() ){ result.push_back( atoms[ ii ] ); ++ii; }
    while( jj < cl.atoms.size() ){ result.push_back( cl.atoms[ jj ] ); ++jj; }
    atoms = result;
    return *this;

  }


  /* Conjunction. Maintains order and, where able, drops subsumed atoms. */
  relo_clause & operator &=( relo_ineq & ie ){
    operator &=( relo_clause( ie ) ); return *this;
  }


  /* A mostly arbitrary order for sorting clauses. -2 tells that this strictly
    implies the other; 2 that the other strictly implies this; and 0 that they
    are identical. -1 and 1 are just an arbitrary ordering. */
  int compare( const relo_clause & cl ) const {
    int old = 0;
    unsigned ii = 0, jj = 0;
    while( ii < atoms.size() ){
      if( jj >= cl.atoms.size() ){ return old <= 0 ? -2 : -1; }
      int cmp = atoms[ ii ].compare_coeff( cl.atoms[ jj ] );
      if( cmp ){
        if( old == -1 ){ ++ii; continue; }
        if( old == 1 ){ ++jj; continue; }
        old = -1; ++ii; continue;
      }
      cmp = atoms[ ii ].compare_constant( cl.atoms[ jj ] );
      if( !old ){ old = cmp; }
      else if( old * cmp == -1 ){ return old; }
      if( old <= 0 ){ ++jj; }
      if( old >= 0 ){ ++ii; }
    }
    if( jj < cl.atoms.size() ){ return old >= 0 ? 2 : 1; }
    return old;
  }


  /* Eliminates variable vv. The sanity of vv was checked in relo_DNF. */
  relo_clause & eliminate( unsigned vv ){

    /* Split to vv-zero, vv-negative, and vv-positive. */
    std::vector< relo_ineq > neg, zer, pos;
    for( unsigned ii = 0; ii < atoms.size(); ++ii ){
      if( atoms[ ii ].coeff[ vv ].is_zer() ){ zer.push_back( atoms[ ii ] ); }
      else if( atoms[ ii ].coeff[ vv ].is_pos() ){
        pos.push_back( atoms[ ii ] );
      }else{ neg.push_back( atoms[ ii ] ); }
    }

    /* Make all combinations of vv-negative and vv-positive. */
    atoms.clear();
    for( unsigned ii = 0; ii < neg.size(); ++ii ){
      for( unsigned jj = 0; jj < pos.size(); ++jj ){

        /* Compute the conjunction of the negative and positive. */
        relo_ineq ie;
        ie.var_cnt = neg[ ii ].var_cnt > pos[ jj ].var_cnt ?
          neg[ ii ].var_cnt : pos[ jj ].var_cnt;
        ie.strict = neg[ ii ].strict || pos[ jj ].strict;
        number
          cn = pos[ jj ].coeff[ vv ], cp = -neg[ ii ].coeff[ vv ];
        for( unsigned kk = 0; kk <= ie.var_cnt; ++kk ){
          ie.coeff[ kk ] =
          cn * neg[ ii ].coeff[ kk ] + cp * pos[ jj ].coeff[ kk ];
        }   // cn and cp are not 0.
        //ie.normalize();

        operator &=( ie );
      }
    }

    /* Merge zer to atoms. */
    for( unsigned ii = 0; ii < zer.size(); ++ii ){ operator &=( zer[ ii ] ); }

    return *this;
  }


  /* Assign a value to a variable. The sanity of vv was checked in
    relo_DNF. */
  relo_clause & assign( unsigned vv, number xx ){
    relo_clause result;
    for( unsigned ii = 0; ii < atoms.size(); ++ii ){
      result &= atoms[ ii ].assign( vv, xx );
    }
    *this = result;
    return *this;
  }


  /* Find a value for variable vv that makes the clause true, assuming that
    all other variables have been eliminated. If impossible, return undef. */

  inline bool val_lts( number n1, number n2, bool eq ){
    return num_lt( n1, n2 ) == tv_T || ( eq && num_eq( n1, n2 ) == tv_T );
  }

  number value( unsigned vv ){
    if( atoms.empty() ){ return 0; }
    if( is_false() ){ return numu; }

    number low = numu, hig = numu;
    bool lo_strict = false, hi_strict = false;
    for( unsigned ii = 0; ii < atoms.size(); ++ii ){
      bool sign = atoms[ ii ].coeff[ vv ].is_pos();
      number result = -atoms[ ii ].coeff[0] / atoms[ ii ].coeff[ vv ];
      if( sign ){
        if( low == numu || val_lts( low, result, atoms[ ii ].strict ) ){
          low = result; lo_strict = atoms[ ii ].strict;
        }
      }else{
        if( hig == numu || val_lts( result, hig, atoms[ ii ].strict ) ){
          hig = result; hi_strict = atoms[ ii ].strict;
        }
      }
    }

    if(
      ( low == numu && hig == numu ) ||
      ( val_lts( low, 0, !lo_strict ) && val_lts( 0, hig, !hi_strict ) )
    ){ return 0; }
    if( hig == numu ){ return lo_strict ? floor( low + 1 ) : ceil( low ); }
    if( low == numu ){ return hi_strict ? ceil( hig - 1 ) : floor( hig ); }
    if( val_lts( hig, low, lo_strict || hi_strict ) ){ return numu; }
    number result = ( low + hig ) / 2;
    if( val_lts( low, floor( result ), !lo_strict ) ){
      return floor( result );
    }
    if( val_lts( ceil( result ), hig, !hi_strict ) ){ return ceil( result ); }
    return result;

  }


  void print() const {
    if( atoms.empty() ){ out_op( op_true ); return; }
    atoms[0].print();
    for( unsigned ii = 1; ii < atoms.size(); ++ii ){
      out_am( ' ' ); out_op( op_and ); out_am( ' ' ); atoms[ ii ].print();
    }
  }


};


class relo_DNF{
  std::vector< relo_clause > clauses;

public:

  /* Constructors */
  relo_DNF(){}
  relo_DNF( relo_clause cl ){
    if( !cl.is_false() ){ clauses.push_back( cl ); }
  }

  /* These recognize special cases. */
  inline bool is_false() const { return clauses.empty(); }
  inline bool is_true() const {
    return !clauses.empty() && clauses[0].is_true();
  }


  /* Disjunction. Maintains order and, where able, drops subsumed clauses. */
  relo_DNF & operator |=( const relo_DNF & df ){

    /* Special cases */
    if( is_true() || df.is_false() ){ return *this; }
    if( is_false() || df.is_true() ){ clauses = df.clauses; return *this; }

    /* Merger */
    std::vector< relo_clause > result;
    unsigned ii = 0, jj = 0;
    while( ii < clauses.size() && jj < df.clauses.size() ){
      int cmp = clauses[ ii ].compare( df.clauses[ jj ] );
      if( !cmp || cmp == 2 ){ ++jj; }
      else if( cmp == -2 ){ ++ii; }
      else if( cmp == -1 ){ result.push_back( clauses[ ii ] ); ++ii; }
      else{ result.push_back( df.clauses[ jj ] ); ++jj; }
    }
    while( ii < clauses.size() ){ result.push_back( clauses[ ii ] ); ++ii; }
    while( jj < df.clauses.size() ){
      result.push_back( df.clauses[ jj ] ); ++jj;
    }
    clauses = result;

    return *this;
  }


  relo_DNF & operator |=( const relo_clause & cl ){
    operator |=( relo_DNF( cl ) ); return *this;
  }


  /* Conjunction. Resembles cartesian product. */
  relo_DNF & operator &=( const relo_DNF & df ){

    /* Special cases */
    if( is_false() || df.is_true() ){ return *this; }
    if( is_true() || df.is_false() ){ clauses = df.clauses; return *this; }

    /* Merge every pair. */
    relo_DNF result;
    for( unsigned ii = 0; ii < clauses.size(); ++ii ){
      for( unsigned jj = 0; jj < df.clauses.size(); ++jj ){
        relo_clause cl = clauses[ ii ];
        cl &= df.clauses[ jj ]; result |= cl;
      }
    }
    *this = result;

    return *this;
  }


  /* Negation. Converts to conjunctive normal form and negates the atoms. */
  relo_DNF & negate(){

    /* Special cases */
    if( is_false() ){ clauses.resize(1); return *this; }
    if( is_true() ){ clauses.clear(); return *this; }

    /* Negate the atoms. */
    for( unsigned ii = 0; ii < clauses.size(); ++ii ){
      for( unsigned jj = 0; jj < clauses[ ii ].atoms.size(); ++jj ){
        clauses[ ii ].atoms[ jj ].negate();
      }
    }

    /* Convert to CNF. */
    relo_DNF result;
    unsigned sz = clauses.size(), work = 1;
    for( unsigned ii = 0; ii < sz; ++ii ){
      work *= clauses[ ii ].atoms.size();
    }
    if( work > 10000 ){ test_fail_compl(); return *this; }
    std::vector< unsigned > curr( sz );
    while( true ){
      relo_clause cl = clauses[0].atoms[ curr[0] ];
      for( unsigned ii = 1; ii < sz; ++ii ){
        cl &= clauses[ ii ].atoms[ curr[ ii ] ];
        if( cl.is_false() ){ break; }
      }
      result |= cl;
      unsigned ii = 0;
      while( ii < sz ){
        ++curr[ ii ];
        if( curr[ ii ] < clauses[ ii ].atoms.size() ){ break; }
        else{ curr[ ii ] = 0; ++ii; }
      }
      if( ii >= sz ){ break; }
    }
    *this = result;

    return *this;
  }


  /* Eliminate a variable. */
  relo_DNF & eliminate( unsigned vv ){
    if( !vv || vv > relo_max_var ){
      test_fail_compl(
        "You found a bug, tell AV: non-existent DNF variable"
      ); return *this;
    }
    relo_DNF result;
    for( unsigned ii = 0; ii < clauses.size(); ++ii ){
      result |= clauses[ ii ].eliminate( vv );
    }
    *this = result;
    return *this;
  }


  /* Assign a value to a variable. */
  relo_DNF & assign( unsigned vv, number xx ){
    if( !vv || vv > relo_max_var ){
      test_fail_compl(
        "You found a bug, tell AV: non-existent DNF variable"
      ); return *this;
    }
    relo_DNF result;
    for( unsigned ii = 0; ii < clauses.size(); ++ii ){
      result |= clauses[ ii ].assign( vv, xx );
    }
    *this = result;
    return *this;
  }


  /* Find a value for variable vv that makes the formula true, assuming that
    all other variables have been eliminated. If impossible, return undef. */
  number value( unsigned vv ){
    number result = numu;
    for( unsigned ii = 0; ii < clauses.size() && result == numu; ++ii ){
      result = clauses[ ii ].value( vv );
    }
    return result;
  }


  void print() const {
    if( clauses.empty() ){ out_op( op_false ); return; }
    clauses[0].print();
    for( unsigned ii = 1; ii < clauses.size(); ++ii ){
      out_am( ' ' ); out_op( op_or ); out_am( ' ' ); clauses[ ii ].print();
    }
  }


};

relo_DNF relo_DNF_false, relo_DNF_true = relo_clause();


/*
  The fraction of two expressions of the form c_0 + c_1 x_1 + ... + c_n x_n
  The denominator is used only if de.var_cnt > 0. Then its first non-zero c_i
  is made 1. That the denominator is constant 0 is indicated by de.strict =
  true. Then the fraction is defined nowhere. That strict comparison is
  needed with absolute values is denoted with nu.strict.
*/
struct relo_frac{
  relo_ineq nu, de;

  void plusminus( const relo_frac & rf, bool is_plus ){
    if( de.strict ){ return; }
    if( rf.de.strict ){ *this = rf; return; }

    if( de.var_cnt ){

      if( rf.de.var_cnt ){
        unsigned nn = de.var_cnt;
        if( nn < rf.de.var_cnt ){ nn = rf.de.var_cnt; }
        for( unsigned ii = 0; ii <= nn; ++ii ){
          if( !( de.coeff[ ii ] == rf.de.coeff[ ii ] ) ){
            test_fail_compl(); return;
          }
        }
        if( is_plus ){ nu.add( rf.nu ); }else{ nu.subtract( rf.nu ); }
        nu.prune_variables(); return;
      }

      if( rf.nu.var_cnt ){ test_fail_compl(); return; }

      number xx = rf.nu.coeff[0];
      if( !is_plus ){ xx = -xx; }
      if( nu.var_cnt < de.var_cnt ){ nu.var_cnt = de.var_cnt; }
      for( unsigned ii = 0; ii <= nu.var_cnt; ++ii ){
        nu.coeff[ ii ] += de.coeff[ ii ] * xx;
      }
      nu.prune_variables(); return;

    }

    if( rf.de.var_cnt ){
      if( nu.var_cnt ){ test_fail_compl(); return; }

      number xx = nu.coeff[0];
      de = rf.de; nu.var_cnt = de.var_cnt;
      for( unsigned ii = 0; ii <= nu.var_cnt; ++ii ){
        nu.coeff[ ii ] = de.coeff[ ii ] * xx;
      }

    }

    if( is_plus ){ nu.add( rf.nu ); }else{ nu.subtract( rf.nu ); }
    nu.prune_variables();
  }

  void add( const relo_frac & rf ){ plusminus( rf, true ); return; }

  void subtract( const relo_frac & rf ){ plusminus( rf, false ); return; }

  void multiply( const relo_frac & rf ){
    if( de.strict ){ return; }
    if( rf.de.strict ){ *this = rf; return; }
    if( ( nu.var_cnt && rf.nu.var_cnt ) || ( de.var_cnt && rf.de.var_cnt ) ){
      test_fail_compl(
        "Product of variables is not allowed in first-degree arithmetic"
      ); return;
    }
    number xx;
    if( !rf.nu.var_cnt ){ xx = rf.nu.coeff[0]; }
    else{ xx = nu.coeff[0]; nu = rf.nu; }
    for( unsigned ii = 0; ii <= nu.var_cnt; ++ii ){ nu.coeff[ ii ] *= xx; }
    if( rf.de.var_cnt ){ de = rf.de; }
    nu.prune_variables();
  }

  void divide( const relo_frac & rf ){
    if( de.strict ){ return; }
    if( rf.de.strict ){ *this = rf; return; }
    if( rf.de.var_cnt || ( de.var_cnt && rf.nu.var_cnt ) ){
      test_fail_compl(); return;
    }
    number xx;
/*
    if( rf.de.var_cnt ){
      xx = nu.coeff[0]; nu = rf.de;
      for( unsigned ii = 0; ii <= nu.var_cnt; ++ii ){ nu.coeff[ ii ] *= xx; }
    }
*/
    if( rf.nu.var_cnt ){
      de = rf.nu;
      unsigned ii = 0;
      for( ; ii <= de.var_cnt && de.coeff[ ii ].is_zer(); ++ii ){}
      if( ii > de.var_cnt ){
        de.var_cnt = 0; nu = de; de.strict = true; return;
      }
      xx = de.coeff[ ii ];
      for( ; ii <= de.var_cnt; ++ii ){ de.coeff[ ii ] /= xx; }
    }else{ xx = rf.nu.coeff[0]; }
    if( xx.is_zer() ){ de.var_cnt = 0; nu = de; de.strict = true; return; }
    for( unsigned ii = 0; ii <= nu.var_cnt; ++ii ){ nu.coeff[ ii ] /= xx; }
    //nu.prune_variables();
  }

};


/* Ensures an error message, if ee is null. */
inline bool relo_null( const expression *ee ){
  if( err_mode ){ return true; }
  if( ee ){ return false; }
  test_fail_compl( "You found a bug, tell AV: unexpected null expression" );
  return true;
}


/* Data structure for unfolding absolute values to many relations
  The relation is processed 2 ^ (the number of |...|) times, once for each
  combination of each |...| taken as such or negated. In each round,
  relo_abs_idx counts the |...|, and the corresponding bit of relo_abs_cnt
  tells whether the |...| is negated or not. */
unsigned
  relo_abs_idx = 0,   // counts from 0 to number of |...| in the relation
  relo_abs_cnt = 0;   // from 0 to 2 ^ (total number of |...| in the relation)
std::vector< relo_frac >
  relo_abs_term;      // term inside |...| that makes the condition

/* Constructs an inequation from an integer arithmetic term. */
relo_frac relo_term( const expression *ee ){
  relo_frac result;
  if( relo_null( ee ) ){ return result; }

  if( ee->opr() == op_const ){
    if( ee->val().is_uns_type() ){ result.nu.coeff[0] = ee->val(); }
    else{ test_fail( ee->val() ); }
    return result;
  }

  if( ee->opr() == op_var ){
    unsigned vv = ee->var_idx();

    /* Find or create a local index for the variable. Search from the end to
      find the innermost quantified instance. */
    bool found = false;
    for( unsigned ii = relo_var_count; ii; --ii ){
      if( relo_var_idx[ ii ] == vv ){ vv = ii; found = true; break; }
    }
    if( !found ){
      if( relo_var_count < relo_max_var ){
        relo_var_idx[ ++relo_var_count ] = vv; vv = relo_var_count;
      }else{ test_fail( err_var_cnt ); return result; }
    }

    result.nu.var_cnt = vv; result.nu.coeff[ vv ] = 1; return result;
  }

  if( ee->opr() == op_plus || ee->opr() == op_mixn ){
    if( !ee->left() ){ return relo_term( ee->right() ); }
    result = relo_term( ee->left() );
    result.add( relo_term( ee->right() ) ); return result;
  }

  if( ee->opr() == op_minus ){
    if( !ee->left() ){
      result = relo_term( ee->right() ); result.nu.reverse(); return result;
    }
    result = relo_term( ee->left() );
    result.subtract( relo_term( ee->right() ) ); return result;
  }

  if( ee->opr() == op_iprod || ee->opr() == op_vprod ){
    result = relo_term( ee->left() );
    result.multiply( relo_term( ee->right() ) );
/*
    relo_ineq ie = relo_term( ee->right() );
    if( result.var_cnt && ie.var_cnt ){
      test_fail_compl(
        "Product of variables is not allowed in first-degree arithmetic"
      ); return result;
    }
    number cc = 0;
    if( result.var_cnt ){ cc = ie.coeff[0]; }
    else{ cc = result.coeff[0]; result = ie; }
    for( unsigned ii = 0; ii <= result.var_cnt; ++ii ){
      result.coeff[ ii ] *= cc;
    }
*/
    return result;
  }

  if( ee->opr() == op_div ){
    result = relo_term( ee->left() );
    result.divide( relo_term( ee->right() ) );
/*
    if(
      ee->left() && ee->right() && ee->right()->opr() == op_const &&
      ee->right()->val().is_uns_type()
    ){
      result = relo_term( ee->left() ); number xx = ee->right()->val();
      if( xx.is_zer() ){ test_fail_compl( "Division by 0" ); }
      else{
        for( unsigned ii = 0; ii <= result.var_cnt; ++ii ){
          result.coeff[ ii ] /= xx;
        }
      }
    }else{ test_fail( ee->val() ); }
*/
    return result;
  }

  if( ee->opr() == op_abs ){
    result = relo_term( ee->right() );
    bool is_neg = relo_abs_cnt & 1u << relo_abs_idx;
    if( is_neg ){ result.nu.reverse(); }
    if( relo_abs_cnt ){
      relo_abs_term[ relo_abs_idx ] = result;
      if( is_neg ){ relo_abs_term[ relo_abs_idx ].nu.strict = true; }
    }
    else{ relo_abs_term.push_back( result ); }
    ++relo_abs_idx;
    return result;
  }

  if( ee->opr() == op_hpar ){ return relo_term( ee->right() ); }

  test_fail( ee->opr(), "first-degree arithmetic term" );
  return result;
}


/* Return the leftmost term compared in a relation chain. */
const expression * relo_l_term( const expression *ee ){
  if( !ee ){ return 0; }
  while( is_rel( ee->opr() ) ){ ee = ee->left(); }
  return ee;
}

/* Return the rightmost term compared in a relation chain. */
const expression * relo_r_term( const expression *ee ){
  if( !ee ){ return 0; }
  while( is_rel( ee->opr() ) ){ ee = ee->right(); }
  return ee;
}


/* Constructs a system of inequations from a relation chain. */
relo_DNF relo_relation( const expression *ee, bool U_becomes ){
  relo_DNF result;
  if( relo_null( ee ) ){ return result; }

  if(
    ee->opr() == op_gq || ee->opr() == op_gt || ee->opr() == op_eq ||
    ee->opr() == op_lq || ee->opr() == op_lt || ee->opr() == op_nq
  ){

    /* Process the current relation. */
    relo_abs_cnt = 0; relo_abs_term.clear();
    do{
      relo_abs_idx = 0;   // relo_term counts |...|s with this

      /* Fetch the terms compared by the current relation. */
      relo_frac rf = relo_term( relo_r_term( ee->left() ) );
      rf.subtract( relo_term( relo_l_term( ee->right() ) ) );
      relo_DNF dnf;

      /* Process division by constant 0. */
      if( rf.de.strict ){ dnf = U_becomes ? relo_DNF_true : relo_DNF_false; }

      else{
        relo_clause cl1, cl2;

        /* Build a comparison. */	
        if( ee->opr() == op_lt || ee->opr() == op_gt ){ rf.nu.strict = true; }
        if( ee->opr() == op_lq || ee->opr() == op_lt ){ rf.nu.reverse(); }
        cl1 = rf.nu;

        /* If necessary, build another comparison. */
        if( ee->opr() == op_eq || ee->opr() == op_nq ){
          rf.nu.reverse(); cl1 &= rf.nu;
        }

        /* Take the denominator into account. */
        if( rf.de.var_cnt ){
          if( ee->opr() == op_nq ){ cl2 = rf.de; }
          else{
            if( ee->opr() == op_eq ){ cl2 = cl1; }
            else{ rf.nu.reverse(); cl2 = rf.nu; }
            rf.de.strict = true; cl1 &= rf.de;
          }
          rf.de.reverse(); cl2 &= rf.de;
          dnf = cl1; dnf |= cl2;
        }else{ dnf = cl1; }
        if( ee->opr() == op_nq ){ dnf.negate(); }
 
        /* If necessary, make undefined appear as true. */
        if( U_becomes && rf.de.var_cnt ){
          rf.de.strict = false; cl1 = rf.de;
          rf.de.reverse(); cl1 &= rf.de;
          dnf |= cl1;
        }

        /* Take the absolute value conditions into account. */
        for( unsigned ii = 0; ii < relo_abs_idx; ++ii ){
          if( relo_abs_term[ ii ].de.strict ){
            if( !U_becomes ){ dnf = relo_DNF_false; }
          }else{
            relo_DNF d2 = relo_clause( relo_abs_term[ ii ].nu );
            if( relo_abs_term[ ii ].de.var_cnt ){
              relo_abs_term[ ii ].de.strict = true;
              d2 &= relo_clause( relo_abs_term[ ii ].de );
              relo_abs_term[ ii ].nu.reverse(); cl2 = relo_abs_term[ ii ].nu;
              relo_abs_term[ ii ].de.reverse(); cl2 &= relo_abs_term[ ii ].de;
              d2 |= cl2;
            }
            dnf &= d2;
          }
        }

      }

      /* Combine to the final outcome and move to next absolute option. */
      result |= dnf;
      ++relo_abs_cnt;
    }while( relo_abs_cnt < 1u << relo_abs_idx );

    /* Merge the relation with the chains on the left and right, if exist. */
    if( is_rel( ee->left()->opr() ) ){
      result &= relo_relation( ee->left(), U_becomes );
    }
    if( is_rel( ee->right()->opr() ) ){
      result &= relo_relation( ee->right(), U_becomes );
    }

    /* Return the result. */
    return result;
  }

  test_fail( ee->opr(), "first-degree arithmetic relation" );
  return result;
}


relo_DNF relo_formula( const expression *ee, bool negated, bool U_becomes ){
  relo_DNF result;
  if( relo_null( ee ) ){ return result; }

  if(
    ee->opr() == op_gq || ee->opr() == op_gt || ee->opr() == op_eq ||
    ee->opr() == op_lq || ee->opr() == op_lt || ee->opr() == op_nq
  ){
    result = relo_relation( ee, U_becomes != negated );
    if( negated ){ result.negate(); }
    return result;
  }

  if( ee->opr() == op_not ){
    return relo_formula( ee->right(), !negated, U_becomes );
  }

  if(
    ( !negated && ee->opr() == op_or ) || ( negated && ee->opr() == op_and )
  ){
    result = relo_formula( ee->left(), negated, U_becomes );
    result |= relo_formula( ee->right(), negated, U_becomes );
    return result;
  }

  if(
    ( negated && ee->opr() == op_or ) || ( !negated && ee->opr() == op_and )
  ){
    result = relo_formula( ee->left(), negated, U_becomes );
    result &= relo_formula( ee->right(), negated, U_becomes );
    return result;
  }

  if( ee->opr() == op_sand ){
    return relo_formula(
      new_expr( ee->left(), op_and,
        new_expr( new_expr( op_not, ee->left() ), op_or, ee->right() )
      ), negated, U_becomes );
  }

  if( ee->opr() == op_sor ){
    return relo_formula(
      new_expr( ee->left(), op_or,
        new_expr( new_expr( op_not, ee->left() ), op_and, ee->right() )
      ), negated, U_becomes );
  }

  if( ee->opr() == op_rarr ){
    if( negated ){
      result = relo_formula( ee->left(), false, U_becomes );
      result &= relo_formula( ee->right(), true, U_becomes );
    }else{
      result = relo_formula( ee->left(), true, U_becomes );
      result |= relo_formula( ee->right(), false, U_becomes );
    }
    return result;
  }

  if( ee->opr() == op_harr ){
    result = relo_formula( ee->left(), true, U_becomes );
    relo_DNF df = relo_formula( ee->right(), true, U_becomes );
    if( negated ){
      result &= relo_formula( ee->right(), false, U_becomes );
      df &= relo_formula( ee->left(), false, U_becomes );
      result |= df;
    }else{
      result |= relo_formula( ee->right(), false, U_becomes );
      df |= relo_formula( ee->left(), false, U_becomes );
      result &= df;
    }
    return result;
  }

  if( ee->opr() == op_exists || ee->opr() == op_forall ){
    bool sub_neg = ee->opr() == op_forall;
    unsigned vv = 0;
    for( ; vv < relo_var_count && relo_var_idx[ vv ] != ~0u; ++vv ){}
    if( vv >= relo_var_count ){
      if( relo_var_count < relo_max_var ){ vv = ++relo_var_count; }
      else{ test_fail( err_var_cnt ); return result; }
    }
    relo_var_idx[ vv ] = to_var_idx( ee->val() );
    if( negated != sub_neg ){ U_becomes = !U_becomes; }
    result = relo_formula( ee->right(), sub_neg, U_becomes );
    if( ee->left() ){
      if( sub_neg ){
        result &= relo_formula( ee->left(), !sub_neg, U_becomes );
      }else{ result &= relo_formula( ee->left(), sub_neg, U_becomes ); }
    }
    result.eliminate( vv ); relo_var_idx[ vv ] = ~0u;
    if( negated != sub_neg ){ result.negate(); }
    return result;
  }

  if( ee == expr_F || ee == expr_T ){
    if( negated == ( ee == expr_T ) ){ return relo_DNF_false; }
    else{ return relo_DNF_true; }
  }
  if( ee == expr_U ){
    return U_becomes ? relo_DNF_true : relo_DNF_false;
    //???test_fail_compl(
    //  "<b>U</b> is not allowed in first-degree arithmetic formulas"
    //);
    return result;
  }

  test_fail( ee->opr(), "first-degree arithmetic formula" );
  return result;
}


bool real_logic_root( expression *eF, expression *eT = expr_T ){
  if( !( eF && eT ) ){
    mc_err_print( "No real logic expression" ); return false;
  }

  /* Fetch the formula in a disjunctive normal form. */
  relo_var_count = 0;
  relo_DNF dnf =
    relo_formula( new_expr( eF, op_and, dom_expr ), false, false );
  dnf &= relo_formula( new_expr( eT, op_and, dom_expr ), false, true );
  if( err_mode ){ return false; }

  /* Eliminate the free variables. */
  std::vector< relo_DNF > back_dnf( relo_var_count + 1 );
  for( unsigned vv = relo_var_count; vv; --vv ){
    //??? Choose the variable to be eliminated
    if( relo_var_idx[ vv ] != ~0u ){
      back_dnf[ vv ] = dnf; dnf.eliminate( vv );
    }
  }

  /* Find a counter-example, if exists. */
  if( dnf.is_true() ){
    for( unsigned ii = 0; ii < var_f_cnt; ++ii ){ var_used[ ii ].value = 0; }
    for( unsigned vv = 1; vv <= relo_var_count; ++vv ){
      if( relo_var_idx[ vv ] != ~0u ){

        /* Assign the already chosen values to the old formula. */
        dnf = back_dnf[ vv ];
        for( unsigned jj = 1; jj < vv; ++jj ){
          if( relo_var_idx[ jj ] != ~0u ){
            dnf.assign( jj, var_used[ relo_var_idx[ jj ] ].value );
          }
        }

        /* Find a value that satisfies the clause. */
        var_used[ relo_var_idx[ vv ] ].value = dnf.value( vv );

      }
    }
    return true;
  }

  return false;
}


void real_logic_check( expression *e1, expression *e2, op_type rel_op ){
  if( !e1 || !e2 ){ mc_err_print( "No real logic expression" ); return; }
  if( rel_op == op_leq || rel_op == op_iden || rel_op == op_limp ){
    if(
      e1 != expr_T && e2 != expr_F &&
      real_logic_root( e2, new_expr( op_not, e1 ) )
    ){
      test_fail( tv_U, tv_T ); err_mode = err_none;   // store counterexample
      if( real_logic_root( new_expr( e2, op_and, new_expr( op_not, e1 ) ) ) ){
        test_fail( tv_F, tv_T );    // use F-counterexample, if exists
      }else{ err_mode = err_cmp; }  // activate stored U-counterexample
    }
  }
  if( rel_op == op_leq || rel_op == op_iden || rel_op == op_impl ){
    if(
      e2 != expr_T && e1 != expr_F &&
      real_logic_root( e1, new_expr( op_not, e2 ) )
    ){
      test_fail( tv_T, tv_U ); err_mode = err_none;   // store counterexample
      if( real_logic_root( new_expr( e1, op_and, new_expr( op_not, e2 ) ) ) ){
        test_fail( tv_T, tv_F );    // use F-counterexample, if exists
      }else{ err_mode = err_cmp; }  // activate stored U-counterexample
    }
  }
  if( rel_op == op_iden ){
    if(
      e1 != expr_T && e2 != expr_F &&
      real_logic_root( new_expr( op_not, e1 ), e2 )
    ){ test_fail( tv_F, tv_U ); }
    if(
      e2 != expr_T && e1 != expr_F &&
      real_logic_root( new_expr( op_not, e2 ), e1 )
    ){ test_fail( tv_U, tv_F ); }
  }
  if( !err_mode ){ err_mode = err_proven; }
}
