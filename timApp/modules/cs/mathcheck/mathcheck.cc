#define confuse
#define record
#include "input_output.cc"
copyright mathcheck_cc( "mathcheck.cc", "Antti Valmari", 20200306 );
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
/*
  This is a program for checking mathematical manipulations and printing them
  as a web page using AsciiMath and MathJax. The expressions may contain
  variables such as a, x, and y; functions such as log and sin; and, of
  course, basic mathematical operations +, -, * (also as nothing) and /.
  Antti Valmari (see copyright above)
*/


/* ??? Tehtavaa:
- set f(x) := ...x... ends
- jo lasketun lausekkeen arvon muistintaminen
- derivaatan maarittelemattomyyden kumoutuminen, esim. DD x x x^(1/3)
- arcusfunktiot
- yleinen logaritmi
- predikaatin tarkastus (ja rajoitetumpi domain expression)
- paattelyyn perustuva tarkastus
- yhtalon ratkaiseminen
- todistukset
- induktiotodistukset
- aksioomajarjestelmat
*/

#define mathcheck
#include <vector>
#include <cmath>


/*** Small widely usable functions ***/


/* Tells whether the argument is not a NaN. */
inline bool is_defined( double xx ){ return xx == xx; }

typedef unsigned char byte;


/*** Additional HTML and HTML-AsciiMath printing features ***/


/* Definitions for printing and rounding double numbers */
const unsigned max_digits = 7,  // maximum number of printed digits
  pow_digits = 10000000;        // 10^max_digits
const double r_eps = 10./pow_digits;      // rounding precision basic unit

/* Printing of double numbers */
void out_double( double xx, bool no_exp = false ){

  /* Process not-a-numbers, sign, and infinities. */
  if( !is_defined( xx ) ){ out_html( "undefined" ); return; }
  out_mode( om_am );
  if( xx < 0 ){ out_print( "\xE2\x88\x92" ); xx = -xx; }
  if( xx == 1/0. ){ out_print( "oo" ); return; }

  /* Process a precise integer */
  unsigned nn = unsigned( xx );
  if( nn < pow_digits && xx == double( nn ) ){
    out_print( nn ); out_print( '.' ); return;
  }

  /* Split the number to mantissa and exponent. */
  int ee = 0; unsigned dd = 0, ii;
  while( xx >= 10 ){ xx /= 10; ++ee; }
  while( xx < 1 ){ xx *= 10; --ee; }

  /* Round the number. */
  xx += 5./pow_digits;
  if( xx >= 10 ){ xx /= 10; ++ee; }

  /* Print the possible extra digits before the decimal point. */
  if( ee < int( max_digits ) || no_exp ){
    while( ee > 0 ){
      ii = unsigned( xx );
      out_print( char( ii + '0' ) ); ++dd; xx -= ii; xx *= 10; --ee;
    }
  }

  /* Print the digit before the decimal point, the point, and extra zeros. */
  if( ( -3 <= ee || no_exp ) && ee < 0 ){
    out_print( "0." ); ++ee;
    while( ee < 0 ){ out_print( '0' ); ++ee; }
  }else{
    ii = unsigned( xx );
    out_print( char( ii + '0' ) ); ++dd; xx -= ii; xx *= 10; out_print( '.' );
  }

  /* Print the digits after the decimal point excluding the extra zeros. */
  unsigned ze = 0;
  for( ; dd < max_digits; ++dd ){
    ii = unsigned( xx );
    if( !ii ){ ++ze; }
    else{
      for( ; ze; --ze ){ out_print( '0' ); }
      out_print( char( ii + '0' ) );
    }
    xx -= ii; xx *= 10;
  }

  /* Print the exponent part. */
  if( ee ){
    out_print( "* 10^(" );
    if( ee < 0 ){ out_print( "\xE2\x88\x92" ); ee = -ee; }
    out_print( unsigned( ee ) ); out_print( ')' );
  }

}


/* These functions print parts of the HTML page. */

bool pgh_broken = true;   // <=> main flow of output is broken or not started

/* Start a new paragraph, if necessary. */
void pgh_ensure(){
  if( pgh_broken ){ out_html( "\n<p>" ); pgh_broken = false; }
}

/* Print a message in a special appearance in a paragraph of its own. */
void pgh_msg( const char *s1 ){
  out_html( "\n<p class=set>" ); out_esc( s1 ); pgh_broken = true;
}


/*** Error reporting, part 1 ***/


/* err_type records the type of the current error. */
enum err_type {
  err_none,

  /* This indicates certainty that there is no error. */
  err_proven,

  /* These are only warnings. */
  err_few_tests, err_plausible,

  /* These arise as a result of a checking operation. */
  err_cmp, err_CFG_in, err_CFG_not_in, err_CFG_short, err_CFG_long,
  err_CFG_start, err_CFG_1, err_CFG_2, err_equ1, err_equ2, err_equ3, err_equ4,
  err_equ5, err_qua, err_tree,

  /* These prevent trying the main operation (e.g., comparison of exprs). */
  err_dom, err_no_var, err_compl, err_numb, err_var_cnt, err_op,

  /* When reporting these errors, print the input buffer. */
  err_inp, err_parse, err_var_type, err_var_f_cnt, err_var_q_cnt, err_combs,

  /* These cause abortion of execution as a whole. */
  err_time, err_hash

};
err_type err_mode = err_none;   // current error status
unsigned
  err_cnt = 0,  // number of reported errors
  err_pos = 0,  // position on input line for error messages
  check_level = 0;  // checking was 0 = comprehensive, 1 = good, others = weak

/* Records information on warnings and failed proofs. */
inline void err_set_warning( unsigned &level ){
  if( err_mode == err_few_tests ){ level |= 2; }
  else if( err_mode == err_plausible ){ level |= 4; }
  else if( err_mode != err_proven ){ level |= 1; }
}

/* Returns true, iff the current error does not prevent continuing checking a
  solution chain. */
inline bool err_is_mild(){
  return err_proven <= err_mode && err_mode <= err_tree;
}

/* (Some) parameters of the error messages */
const char *err_msg = "";
unsigned err_uns1 = 0, err_uns2 = 0, err_uns3 = 0;

/* Resetting the error */
inline void err_reset(){ err_mode = err_none; }

/* Setting an input error */
inline void err_set_inp( const char *msg ){
  if( !err_mode ){ err_mode = err_inp; err_msg = msg; }
}


/* These print error messages. */
void err_class(){
  if( err_mode == err_proven ){ out_html( " class=proven" ); }
  else if( err_few_tests <= err_mode && err_mode <= err_plausible ){
    out_html( " class=warn" );
  }
  else{ out_html( " class=err" ); }
}
void err_begin(){ out_html( "\n<p" ); err_class(); out_print( '>' ); }
void err_end(){ out_html( '\n' ); }
void err_print( const char *msg ){ err_begin(); out_esc( msg ); err_end(); }
void mc_err_print( const char *msg0, unsigned u1 = 0, const char *msg1 = 0 ){
  static unsigned cnt = 0;
  if( ++cnt > 2 ){ return; }
  err_begin();
  out_print( "\nWe are sorry! There apparently is a bug in MathCheck." );
  out_print( " To fix the bug, please email your input to AV" );
  out_print( " and tell him the following short message:\n<br>" );
  out_esc( msg0 );
  if( msg1 ){ out_print( u1 ); out_esc( msg1 ); }
  out_print( '\n' );
  err_end();
  err_set_inp( "The problem was detected at this point of input:" );
}


/*** Features for measuring the running time and issuing timeout ***/


#include <sys/time.h>
timeval time_start;
long time_sec, time_usec;
long const time_limit = 3;
void time_reset(){ gettimeofday( &time_start, 0 ); time_sec = time_usec = 0; }
inline void time_now(){
  timeval tm; gettimeofday( &tm, 0 );
  time_sec = tm.tv_sec - time_start.tv_sec;
  time_usec = tm.tv_usec - time_start.tv_usec;
  if( time_usec < 0 ){ time_usec += 1000000; time_sec -= 1; }
  if( time_sec >= time_limit ){ err_mode = err_time; }
}


inline bool lc(){
  static unsigned cycles = 1+1000000;
  if( --cycles ){ return true; }
  if( err_mode < err_time ){ err_mode = err_time; }
  cycles = 1; return false;
}


/*** Extended truth values and precise-when-you-can arithmetic ***/


#include "number.cc"

number err_num1;


/* Printing of numbers in general */
void out_number( number xx, bool no_exp = false ){
  if( xx.is_und() ){ out_html( "undefined" ); return; }
  if( xx.is_dbl() || xx.is_dbu() ){
    if( xx.is_dbu() ){ out_html( "undefined or " ); }
    else{
      double avg = ( xx.d_lo + xx.d_hi ) / 2.;
      if(
        (
          ( xx.d_hi < 0. || 0. < xx.d_lo ) &&
          xx.d_hi - xx.d_lo < ( avg < 0. ? -avg : avg ) * r_eps
        ) || (
          -r_eps < xx.d_lo && xx.d_lo <= 0. &&
           r_eps > xx.d_hi && xx.d_hi >= 0.
        )
      ){ out_double( avg ); return; }
    }
    out_double( xx.d_lo, no_exp );
    out_html( " &hellip; " );
    out_double( xx.d_hi, no_exp );
    return;
  }
  out_mode( om_am );
  if( xx.is_trv() ){
    truth_val t1 = xx.to_tv();
    out_print( "sf\"" );
    if( tv_may_F( t1 ) ){ out_print( 'F' ); }
    if( tv_may_U( t1 ) ){ out_print( 'U' ); }
    if( tv_may_T( t1 ) ){ out_print( 'T' ); }
    out_print( '\"' ); return;
  }
  if( xx.is_zer() ){ out_print( '0' ); return; }
  if( xx.is_neg() ){ out_print( "\xE2\x88\x92" ); }
  if( xx.r_de != 1 ){ out_print( "frac(" ); }
  out_print( xx.r_nu );
  if( xx.r_de != 1 ){
    out_print( ")(" ); out_print( xx.r_de ); out_print( ')' );
  }
}


/*** Tokens and operators ***/


/* Input tokens and their codenames
  The order must be: tkn_var, individual strings in sorted order, tkn_err, and
  other tokens! Those after tkn_eoi are alternative codenames for tokens that
  correspond to more than one operator. */
const char *tkn_str[] = {
  "variable",
#ifndef char_is_unsigned
  "¬", "Γ", "Δ", "Θ", "Λ", "Ξ", "Π", "Σ", "Φ", "Ψ", "Ω", "α", "β", "γ", "δ",
  "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "π", "ρ", "σ", "τ", "υ",
  "φ", "χ", "ψ", "ω", "ϑ", "ϕ", "ϵ", "←", "→", "↔", "↠", "⇐", "⇒", "⇔",
  "∀", "∃", "−", "√", "∧", "∨", "≠", "≡", "≤", "≥", "⋅", "⌈", "⌉", "⌊", "⌋",
#endif
  "!", "!=",
  "#(", "#)", "#*", "#/",
  "&&", "(", ")", "*", "*/", "+", ",", "-", "-->", "->>", "...", "/", "/*",
  "/**/", "/\\", ":", ";", "<", "<--", "<->", "<=", "<==", "<=>", "=", "===",
  "==>", ">", ">=",
  "AA", "Arithmetic", "Array_claim",
  "Brief_help",
  "CFG_CR_off", "CFG_CR_on", "CFG_ambiguous_off", "CFG_ambiguous_on",
  "CFG_compare", "CFG_in", "CFG_long", "CFG_not_in", "CFG_set", "CFG_set2",
  "CFG_short", "CFG_start", "CFG_start2", "CFG_tree",
  "DD", "DE", "De", "Draw_function",
  "EE", "Equation", "Expression_tree",
  "FF",
  "GA", "Ga",
  "Help",
  "LA", "La",
  "MathCheck", "Modulo",
  "OM", "Om",
  "PH", "PI", "PS", "Parse_tree", "Ph", "Pi", "Prop3_logic", "Prop_logic",
  "Ps",
  "Real_logic",
  "SI", "Si",
  "TH", "TT", "Th", "Tree_compare",
  "UU",
  "XI", "Xi",
  "[", "\\/", "]", "^", "^^", "^|", "_", "_|",
  "abs", "al", "allow_comp", "and", "arithmetic", "array_claim",
  "array_test_min", "assume",
  "b_nodes", "ban_comp", "be", "brief_help",
  "ceil", "ch", "cos", "cosh", "cot",
  "ddn", "de", "debug_off", "debug_on", "div", "draw_function", "draw_off",
  "draw_on", "dup",
  "e", "end_of_answer", "enda", "ends", "ep", "equation", "et", "exam_off",
  "exam_on", "expression_tree",
  "f_CNF", "f_DNF", "f_allow_U", "f_ban", "f_ban_der", "f_nodes",
  "f_polynomial", "f_range", "f_simplify", "f_top_opr", "fail_text", "floor",
  "forget_errors",
  "ga",
  "help", "hide_expr",
  "index", "integer", "io",
  "ka",
  "la", "ln", "log", "log2",
  "mathcheck", "mod", "modulo", "mu",
  "next_URL", "no_next_URL", "not", "nu",
  "ok_text", "om", "only_no_yes_off", "only_no_yes_on", "or", "original",
  "parse_tree", "ph", "pi", "prime", "prop3_logic", "prop3_off", "prop3_on",
  "prop_logic", "prove_off", "prove_on", "ps",
  "real", "real_logic", "reset", "rh", "root",
  "si", "sin", "sinh", "skip_error", "solve", "sqrt", "subend", "subproof",
  "sum",
  "ta", "tan", "tanh", "th", "tree_compare",
  "undef_off", "undef_on", "up",
  "var4", "ve", "verbose_off", "verbose_on", "vp", "vt", "vv",
  "xi",
  "ze",
  "|", "|^", "|_", "||",
#ifdef char_is_unsigned
  // move the ifndef-part here
#endif
  "unknown-token", "number", "decimal", "end-of-input", 0
};
enum tkn_type {
  tkn_var,
#ifndef char_is_unsigned
  tkn_Unot, tkn_UGamma, tkn_UDelta, tkn_UTheta, tkn_ULambda, tkn_UXi, tkn_UPi,
  tkn_USigma, tkn_UPhi, tkn_UPsi, tkn_UOmega, tkn_Ualpha, tkn_Ubeta,
  tkn_Ugamma, tkn_Udelta, tkn_Uepsiv, tkn_Uzeta, tkn_Ueta, tkn_Utheta,
  tkn_Uiota, tkn_Ukappa, tkn_Ulambda, tkn_Umu, tkn_Unu, tkn_Uxi, tkn_Upi,
  tkn_Urho, tkn_Usigma, tkn_Utau, tkn_Uupsi, tkn_Uphi, tkn_Uchi, tkn_Upsi,
  tkn_Uomega, tkn_Uthetav, tkn_Ustraightphi, tkn_Uepsi, tkn_Ularr, tkn_Urarr,
  tkn_Uharr, tkn_ULuka, tkn_UlArr, tkn_UrArr, tkn_UhArr, tkn_Uforall,
  tkn_Uexist, tkn_Uminus, tkn_Usqrt, tkn_Uand, tkn_Uor, tkn_Une, tkn_Uiden,
  tkn_Ule, tkn_Uge, tkn_Usdot, tkn_Ulceil, tkn_Urceil, tkn_Ulfloor,
  tkn_Urfloor,
#endif
  tkn_excl, tkn_nq,
  tkn_lP, tkn_rP, tkn_iprod, tkn_frac,
  tkn_2amp, tkn_lp, tkn_rp, tkn_ast, tkn_rc, tkn_plus, tkn_comma, tkn_minus,
  tkn_rarr, tkn_Luka, tkn_3dot, tkn_div, tkn_lc, tkn_lf, tkn_ud, tkn_colon,
  tkn_semic, tkn_lt, tkn_larr, tkn_harr, tkn_lq, tkn_lArr, tkn_hArr, tkn_eq,
  tkn_iden, tkn_rArr, tkn_gt, tkn_gq,
  tkn_AA, tkn_Arithm, tkn_Array,
  tkn_Brief_help,
  tkn_CFG_CR_off, tkn_CFG_CR_on, tkn_CFG_ambiguous_off, tkn_CFG_ambiguous_on,
  tkn_CFG_cmp, tkn_CFG_in, tkn_CFG_long, tkn_CFG_not_in, tkn_CFG_set,
  tkn_CFG_set2, tkn_CFG_short, tkn_CFG_start, tkn_CFG_start2, tkn_CFG_tree,
  tkn_DD, tkn_DE, tkn_De, tkn_Draw,
  tkn_EE, tkn_Equation, tkn_Expr_tree,
  tkn_FF,
  tkn_GA, tkn_Ga,
  tkn_Help,
  tkn_LA, tkn_La,
  tkn_MathCheck, tkn_Mod,
  tkn_OM, tkn_Om,
  tkn_PH, tkn_PI, tkn_PS, tkn_Parse, tkn_Ph, tkn_Pi, tkn_Prop3_logic,
  tkn_Prop_logic, tkn_Ps,
  tkn_Real_logic,
  tkn_SI, tkn_Si,
  tkn_TH, tkn_TT, tkn_Th, tkn_Tree_cmp,
  tkn_UU,
  tkn_XI, tkn_Xi,
  tkn_lB, tkn_du, tkn_rB, tkn_sup, tkn_2sup, tkn_rceil, tkn_sub, tkn_rfloor,
  tkn_abs, tkn_al, tkn_allow_comp, tkn_and, tkn_arithm, tkn_array,
  tkn_arr_test, tkn_assume,
  tkn_b_nodes, tkn_ban_comp, tkn_be, tkn_brief_help,
  tkn_ceil, tkn_ch, tkn_cos, tkn_cosh, tkn_cot,
  tkn_ddn, tkn_de, tkn_debug_off, tkn_debug_on, tkn_idiv, tkn_draw,
  tkn_draw_off, tkn_draw_on, tkn_dup,
  tkn_e2718, tkn_eoa, tkn_enda, tkn_ends, tkn_ep, tkn_equation, tkn_et,
  tkn_exam_off, tkn_exam_on, tkn_expr_tree,
  tkn_f_CNF, tkn_f_DNF, tkn_f_allow_U, tkn_f_ban, tkn_f_ban_der, tkn_f_nodes,
  tkn_f_polynomial, tkn_f_range, tkn_f_simplify, tkn_f_top_opr, tkn_fail_text,
  tkn_floor, tkn_forget_err,
  tkn_ga,
  tkn_help, tkn_hide_expr,
  tkn_index, tkn_integer, tkn_io,
  tkn_ka,
  tkn_la, tkn_ln, tkn_log, tkn_log2,
  tkn_mathcheck, tkn_imod, tkn_mod, tkn_mu,
  tkn_next_URL, tkn_no_next_URL, tkn_not, tkn_nu,
  tkn_ok_text, tkn_om, tkn_only_no_yes_off, tkn_only_no_yes_on, tkn_or,
  tkn_original,
  tkn_parse, tkn_ph, tkn_pi, tkn_prime, tkn_prop3_logic, tkn_prop3_off,
  tkn_prop3_on, tkn_prop_logic, tkn_prove_off, tkn_prove_on, tkn_ps,
  tkn_real, tkn_real_logic, tkn_reset, tkn_rh, tkn_root,
  tkn_si, tkn_sin, tkn_sinh, tkn_skip_errs, tkn_solve, tkn_sqrt, tkn_subend,
  tkn_subproof, tkn_sum,
  tkn_ta, tkn_tan, tkn_tanh, tkn_th, tkn_tree_cmp,
  tkn_undef_off, tkn_undef_on, tkn_up,
  tkn_var4, tkn_ve, tkn_verbose_off, tkn_verbose_on, tkn_vp, tkn_vt, tkn_vv,
  tkn_xi,
  tkn_ze,
  tkn_vbar, tkn_lceil, tkn_lfloor, tkn_2vbar,
#ifdef char_is_unsigned
  // move the ifndef-part here
#endif
  tkn_err, tkn_number, tkn_decimal, tkn_eoi, tkn_fact, tkn_def
};


/* Check token definitions. */
void tkn_check(){

  /* Check that chars are appropriately signed. If this error triggers, find
    all instances of char_is_unsigned and obey the comments. Then compile with
    char_is_unsigned defined or not defined, as appropriate. Sorry, the option
    that is not on my machine has not been tested! Good luck! */
#ifdef char_is_unsigned
  if( char(-1) < 0 )
#else
  if( char(-1) > 0 )
#endif
  {
    mc_err_print(
      "signed char / unsigned char problem, try compiler option -fsigned-char"
      " or see comments in code how to fix"
    );
    return;
  }

  /* Check that there is the right number of token strings. */
  unsigned ii = 0;
  while( tkn_str[ ii ] ){ ++ii; }
  if( ii != tkn_eoi + 1 ){
    mc_err_print( "Wrong number of token strings" ); return;
  }

  /* Check that token strings are in the correct order. */
  for( unsigned ii = tkn_var + 2; ii < tkn_err; ++ii ){
    for( unsigned jj = 0; tkn_str[ ii-1 ][ jj ]; ++jj ){
      if( tkn_str[ ii-1 ][ jj ] < tkn_str[ ii ][ jj ] ){ break; }
      if( tkn_str[ ii-1 ][ jj ] > tkn_str[ ii ][ jj ] ){
        mc_err_print( "Wrong token order ", ii, "" ); return;
      }
    }
  }
}


/* Operators and their output strings (mostly AsciiMath) */
/* op_type( 0 ) is also used to denote that valid operation lacks. */
enum op_type {
  op_const, op_mixn, op_var, op_hpar,
  op_plus, op_minus, op_iprod, op_vprod, op_div, op_pow, op_idiv, op_imod,
  op_root, op_abs, op_sqrt, op_fact,
  op_e2718, op_ln, op_log, op_log2,
  op_sin, op_cos, op_tan, op_cot, op_sinh, op_cosh, op_tanh, op_ddn, op_dup,
  op_sum, op_deriv,
  op_lt, op_lq, op_eq, op_nq, op_gt, op_gq,
  op_not, op_def, op_and, op_sand, op_or, op_sor, op_larr, op_harr, op_rarr,
  op_Luka, op_forall, op_exists, op_is_int,
  op_limp, op_leq, op_impl, op_iden,

  /* Excluding op_pi, op_lfloor, and op_lceil, the following are not in
    expressions, but only for printing tokens. The greek letters (except pi)
    are used as variable names and must be in a consecutive sequence from
    op_alpha to op_Xi. */
  op_lp, op_rp, op_lP, op_rP, op_lB, op_rB, op_lfloor, op_rfloor, op_lceil,
  op_rceil, op_sub, op_comma, op_colon, op_semic,
  op_false, op_undef, op_true,
  op_alpha, op_beta, op_chi, op_delta, op_epsilon, op_varepsilon, op_eta,
  op_gamma, op_iota, op_kappa, op_lambda, op_mu, op_nu, op_omega, op_phi,
  op_varphi, op_pi, op_psi, op_rho, op_sigma, op_tau, op_theta, op_vartheta,
  op_upsilon, op_xi, op_zeta, op_Delta, op_Gamma, op_Lambda, op_Omega, op_Phi,
  op_Pi, op_Psi, op_Sigma, op_Theta, op_Xi,

  op_err  // this must be the last operator
};
const char *op_AM[] = {
  "constant", "", "variable", "",
  "+", "\xE2\x88\x92", "", "*", "/", "^", "\\ text(div)\\ ", "mod",
  "root(...)", "|", "sqrt", "!",
  "e", "ln", "log", "log_2",
  "sin", "cos", "tan", "cot", "sinh", "cosh", "tanh", "sf\"ddn\"",
  "sf\"dup\"",
  "sum", "frac(del)(del ...)",
  "<", "<=", "=", "!=", ">", ">=",
  "not", "**", "^^", "\\ sf\"&&\"\\ ", "vv", "\\ sf\"||\"\\ ", "larr",
  "harr", "rarr", "->>", "AA", "EE", " in ZZ", "lArr", "hArr", "rArr", "-=",
  "(", ")", "#(", "#)", "[", "]", "|__", "__|", "|~", "~|", "_", ",", ":",
  ";",
  "sf\"F\"", "sf\"U\"", "sf\"T\"",
  "alpha", "beta", "chi", "delta", "epsilon", "varepsilon", "eta", "gamma",
  "iota", "kappa", "lambda", "mu", "nu", "omega", "phi", "varphi", "pi",
  "psi", "rho", "sigma", "tau", "theta", "vartheta", "upsilon", "xi", "zeta",
  "Delta", "Gamma", "Lambda", "Omega", "Phi", "Pi", "Psi", "Sigma", "Theta",
  "Xi",
  "", 0
};

/* Numbers of gif image symbols of operators */
const byte op_font[] = {
  164, 165, 166, 167, 43, 45, 32, 22, 23, 94, 184, 185, 24, 168, 24, 33, 101,
  169, 170, 186,
  171, 172, 173, 174, 175, 176, 177, 182, 183, 161, 26, 60, 1, 61, 2, 62, 3,
  4, 42, 5, 189, 6, 190, 7, 8, 9, 188, 10, 11, 178, 12, 13, 14, 187, 40, 41,
  40, 41, 91, 93, 18, 19,
  20, 21, 44, 95, 58, 59, 15, 16, 17, 128, 129, 130, 131, 132, 133, 134, 135,
  136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150,
  151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 127
};

/* Check that there is the right number of operator strings. */
void op_check(){
  unsigned ii = 0;
  while( op_AM[ ii ] ){ ++ii; }
  if( ii != op_err + 1 ){
    mc_err_print( "Wrong number of operator strings" ); return;
  }
  if( op_font[ op_err ] != 127 ){
    mc_err_print( "Wrong number of operator glyphs" ); return;
  }
}


/* The operator that corresponds to a token */
op_type op_tkn( tkn_type tkn ){
  switch( tkn ){
  case tkn_Unot: case tkn_excl: case tkn_not: return op_not;
  case tkn_UGamma: case tkn_GA: case tkn_Ga: return op_Gamma;
  case tkn_UDelta: case tkn_DE: case tkn_De: return op_Delta;
  case tkn_UTheta: case tkn_TH: case tkn_Th: return op_Theta;
  case tkn_ULambda: case tkn_LA: case tkn_La: return op_Lambda;
  case tkn_UXi: case tkn_XI: case tkn_Xi: return op_Xi;
  case tkn_UPi: case tkn_PI: case tkn_Pi: return op_Pi;
  case tkn_USigma: case tkn_SI: case tkn_Si: return op_Sigma;
  case tkn_UPhi: case tkn_PH: case tkn_Ph: return op_Phi;
  case tkn_UPsi: case tkn_PS: case tkn_Ps: return op_Psi;
  case tkn_UOmega: case tkn_OM: case tkn_Om: return op_Omega;
  case tkn_Ualpha: case tkn_al: return op_alpha;
  case tkn_Ubeta: case tkn_be: return op_beta;
  case tkn_Ugamma: case tkn_ga: return op_gamma;
  case tkn_Udelta: case tkn_de: return op_delta;
  case tkn_Uepsi: case tkn_ep: return op_epsilon;
  case tkn_Uzeta: case tkn_ze: return op_zeta;
  case tkn_Ueta: case tkn_et: return op_eta;
  case tkn_Utheta: case tkn_th: return op_theta;
  case tkn_Uiota: case tkn_io: return op_iota;
  case tkn_Ukappa: case tkn_ka: return op_kappa;
  case tkn_Ulambda: case tkn_la: return op_lambda;
  case tkn_Umu: case tkn_mu: return op_mu;
  case tkn_Unu: case tkn_nu: return op_nu;
  case tkn_Uxi: case tkn_xi: return op_xi;
  case tkn_Upi: case tkn_pi: return op_pi;
  case tkn_Urho: case tkn_rh: return op_rho;
  case tkn_Usigma: case tkn_si: return op_sigma;
  case tkn_Utau: case tkn_ta: return op_tau;
  case tkn_Uupsi: case tkn_up: return op_upsilon;
  case tkn_Uphi: case tkn_vp: return op_varphi;
  case tkn_Uchi: case tkn_ch: return op_chi;
  case tkn_Upsi: case tkn_ps: return op_psi;
  case tkn_Uomega: case tkn_om: return op_omega;
  case tkn_Uthetav: case tkn_vt: return op_vartheta;
  case tkn_Ustraightphi: case tkn_ph: return op_phi;
  case tkn_Uepsiv: case tkn_ve: return op_varepsilon;
  case tkn_Ularr: case tkn_larr: return op_larr;
  case tkn_Urarr: case tkn_rarr: return op_rarr;
  case tkn_Uharr: case tkn_harr: return op_harr;
  case tkn_ULuka: case tkn_Luka: return op_Luka;
  case tkn_UlArr: case tkn_lArr: return op_limp;
  case tkn_UrArr: case tkn_rArr: return op_impl;
  case tkn_UhArr: case tkn_hArr: return op_leq;
  case tkn_Uiden: case tkn_iden: return op_iden;
  case tkn_Uforall: case tkn_AA: return op_forall;
  case tkn_Uexist: case tkn_EE: return op_exists;
  case tkn_Uminus: case tkn_minus: return op_minus;
  case tkn_Usqrt: case tkn_sqrt: return op_sqrt;
  case tkn_Uand: case tkn_ud: case tkn_2sup: case tkn_and: return op_and;
  case tkn_2amp: return op_sand;
  case tkn_Uor: case tkn_du: case tkn_or: case tkn_vv: return op_or;
  case tkn_2vbar: return op_sor;
  case tkn_Une: case tkn_nq: return op_nq;
  case tkn_Ule: case tkn_lq: return op_lq;
  case tkn_Uge: case tkn_gq: return op_gq;
  case tkn_Usdot: case tkn_ast: return op_vprod;
  case tkn_Ulceil: case tkn_lceil: return op_lceil;
  case tkn_Urceil: case tkn_rceil: return op_rceil;
  case tkn_Ulfloor: case tkn_lfloor: return op_lfloor;
  case tkn_Urfloor: case tkn_rfloor: return op_rfloor;
  case tkn_iprod: return op_iprod;
  case tkn_lp: return op_lp;
  case tkn_rp: return op_rp;
  case tkn_plus: return op_plus;
  case tkn_comma: return op_comma;
  case tkn_div: return op_div;
  case tkn_colon: return op_colon;
  case tkn_semic: return op_semic;
  case tkn_lt: return op_lt;
  case tkn_eq: return op_eq;
  case tkn_gt: return op_gt;
  case tkn_DD: return op_deriv;
  case tkn_FF: return op_false;
  case tkn_TT: return op_true;
  case tkn_UU: return op_undef;
  case tkn_lB: return op_lB;
  case tkn_rB: return op_rB;
  case tkn_sup: return op_pow;
  case tkn_sub: return op_sub;
  case tkn_abs: case tkn_vbar: return op_abs;
  case tkn_ceil: return op_lceil;
  case tkn_cos: return op_cos;
  case tkn_cosh: return op_cosh;
  case tkn_cot: return op_cot;
  case tkn_ddn: return op_ddn;
  case tkn_idiv: return op_idiv;
  case tkn_dup: return op_dup;
  case tkn_e2718: return op_e2718;
  case tkn_floor: return op_lfloor;
  case tkn_ln: return op_ln;
  case tkn_log: return op_log;
  case tkn_log2: return op_log2;
  case tkn_imod: return op_imod;
  case tkn_root: return op_root;
  case tkn_sin: return op_sin;
  case tkn_sinh: return op_sinh;
  case tkn_sum: return op_sum;
  case tkn_tan: return op_tan;
  case tkn_tanh: return op_tanh;
  case tkn_fact: return op_fact;
  case tkn_def: return op_def;
  default: return op_err;
  }
}


/* Printing a token in a suitable form for the list of expected tokens */
void out_tkn( tkn_type tkn ){
  op_type opr = op_err;
  if(
    ( tkn_var < tkn && tkn < tkn_lP ) ||
    ( tkn_2amp <= tkn && tkn < tkn_err ) || tkn > tkn_eoi
  ){ opr = op_tkn( tkn ); }
  if( opr != op_err ){ out_am( op_AM[ opr ] ); }
  else if( tkn_var < tkn && tkn < tkn_err ){
    out_html( "<kbd>" ); out_esc( tkn_str[ tkn ] ); out_print( "</kbd>" );
  }else{ out_html( tkn_str[ tkn ] ); }
}


/* Functions for subsets of operators. */

inline bool is_sin_like( op_type opr ){
  return
    opr == op_ln || opr == op_log || opr == op_log2 ||
    opr == op_sin || opr == op_cos || opr == op_tan || opr == op_cot ||
    opr == op_sinh || opr == op_cosh || opr == op_tanh;
}

inline bool is_rel( op_type opr ){
  return
    opr == op_lt || opr == op_lq || opr == op_eq ||
    opr == op_gt || opr == op_gq || opr == op_nq;
}

inline bool is_lrel( op_type opr ){
  return opr == op_limp || opr == op_leq || opr == op_impl || opr == op_iden;
}

inline bool is_srel( op_type opr ){
  return
    opr == op_lt || opr == op_lq || opr == op_eq ||
    opr == op_gt || opr == op_gq || opr == op_nq ||
    opr == op_limp || opr == op_leq || opr == op_impl || opr == op_iden;
}

inline op_type op_rel_swap( op_type opr ){
  if( opr == op_lt ){ return op_gt; }
  if( opr == op_lq ){ return op_gq; }
  if( opr == op_gt ){ return op_lt; }
  if( opr == op_gq ){ return op_lq; }
  return opr;
}

inline bool yields_logic( op_type opr ){
  return
    opr == op_is_int || is_srel( opr ) ||
    opr == op_not || opr == op_def ||
    opr == op_and || opr == op_sand || opr == op_or || opr == op_sor ||
    opr == op_rarr || opr == op_harr || opr == op_Luka ||
    opr == op_forall || opr == op_exists;
}


/*** MathCheck variables ***/


enum var_type {
  vtp_none, vtp_R, vtp_Q, vtp_Z, vtp_P, vtp_mod, vtp_tv2, vtp_tv3, vtp_idx
};
unsigned mod_base = 0;

inline void out_type_name( var_type vt ){
  switch( vt ){
  case vtp_R: out_am( "RR" ); break;
  case vtp_Q: out_am( "QQ" ); break;
  case vtp_Z: case vtp_none: out_am( "ZZ" ); break;
  case vtp_P: out_html( "prime-number" ); break;
  case vtp_mod:
    out_am( "{" );
    if( mod_base > 1 ){ out_print( "0, " ); }
    if( mod_base > 2 ){ out_print( "1, " ); }
    if( mod_base == 4 ){ out_print( "2, " ); }
    if( mod_base > 4 ){ out_html( "&hellip;" ); out_am( ", " ); }
    out_print( mod_base - 1 ); out_print( "}" ); break;
  case vtp_tv2: out_am( "{sf\"F\", sf\"T\"}" ); break;
  case vtp_tv3: out_am( "{sf\"F\", sf\"U\", sf\"T\"}" ); break;
  case vtp_idx: out_html( "Index" ); break;
  default: out_html( "unknown-type" );
  }
}

struct variable{
  number value;
  unsigned name;  // 'a' ... 'z', 'A' ... 'Z', and 'z'+op_alpha ... 'z'+op_Xi
  unsigned dimension;
  var_type type;
  bool closed;    // true <=> qua variable is out of scope
  bool hides;     // true <=> hides another variable with the same name
};

/* Variable indices 0, ..., var_max refer to free and var_max + 1, ...,
  2 var_max + 1 and 2 var_max + 2, ..., 3 var_max + 2 to two sets of
  quantified variables. Each last index is only used for giving error
  messages. */
unsigned const var_max = 10;          // max number of free or qua variables
unsigned
  var_f_max = var_max,                // maximum number of free variables
  var_f_cnt = 0,                      // number of free variables
  var_q1_cnt = 0, var_q2_cnt = 0,     // numbers of quantified variables
  var_q_lo = var_max + 1, var_q_hi = var_max;   // index range of current qua
variable var_used[ 3*var_max + 3 ];   // the free and quantified variables
bool qua_set = false;                 // tells which set of qua vars is in use

/* Clears the current set of quantified variables. */
inline void qua_reset(){
  if( qua_set ){ var_q2_cnt = 0; }else{ var_q1_cnt = 0; }
  var_q_hi = var_q_lo - 1;
}
/* Starts to use the not current set of quantified variables. */
inline void qua_swap(){
  qua_set = !qua_set;
  if( qua_set ){
    var_q_lo = 2*var_max + 2; var_q_hi = var_q_lo + var_q2_cnt - 1;
  }else{ var_q_lo = var_max + 1; var_q_hi = var_max + var_q1_cnt; }
}


/* Checks that the name of a variable is legal. */
inline bool check_var_name( unsigned nm ){
  return
    is_ltr( nm ) ||
    ( unsigned( 'z' ) + op_alpha <= nm && nm <= unsigned( 'z' ) + op_Xi );
}


/* Prints the number nm as a variable name. */
inline void out_var_name( unsigned nm ){
  if( !check_var_name( nm ) ){
    mc_err_print( "Wrong variable name ", nm, "" ); return;
  }
  if( nm <= 'z' ){ out_am( char( nm ) ); }else{ out_am( op_AM[ nm - 'z' ] ); }
}

/* Prints an operator with an optional variable name. */
void out_op( op_type opr, unsigned vv = 0 ){
  if( opr == op_deriv ){
    out_am( "frac(del)(del " );
    if( check_var_name( vv ) ){ out_var_name( vv ); }
    else{ out_print( "..." ); }
    out_am( ')' );
  }else if( opr == op_iprod ){
    out_html( "invisible multiplication" );
  }else{
    out_am( op_AM[ opr ] );
    if( check_var_name( vv ) ){ out_print( ' ' ); out_var_name( vv ); }
  }
}

/* Prints the name of var_used[ ii ], with hiding emphasis as appropriate. */
inline void out_idx_name( unsigned ii ){
  if(
    ( var_f_cnt <= ii && ii <= var_max ) ||
    ( var_max + var_q1_cnt < ii && ii <= 2*var_max + 1 ) ||
    2*var_max + 1 + var_q2_cnt < ii
  ){ mc_err_print( "Wrong variable index ", ii, "" ); return; }
  unsigned nm = var_used[ ii ].name;
  if( !check_var_name( nm ) ){
    mc_err_print( "Wrong variable name ", nm, "" ); return;
  }
  if( var_used[ ii ].hides ){ out_html( "<span class=hides>" ); }
  if( nm <= 'z' ){ out_am( char( nm ) ); }else{ out_am( op_AM[ nm - 'z' ] ); }
  if( var_used[ ii ].hides ){ out_html( "</span>" ); out_am( "sf\"\"" ); }
}


/* Extracts a variable index from a number. */
unsigned to_var_idx( const number &xx ){
  unsigned ii = xx.to_unsigned();
  if(
    !( ii || xx.is_zer() ) ||
    ( var_f_cnt <= ii && ii <= var_max ) ||
    ( var_max + var_q1_cnt < ii && ii <= 2*var_max + 1 ) ||
    2*var_max + 1 + var_q2_cnt < ii
  ){ mc_err_print( "Non-existent variable ", ii, "" ); return 0; }
  return ii;
}

/* Extracts a variable from a number. */
inline variable &to_var( const number &xx ){
  return var_used[ to_var_idx( xx ) ];
}


/* Tell which variables hold integer values. */
bool intvar( unsigned nm ){
  return ( nm >= 'i' && nm <= 'n' ) || ( nm >= 'I' && nm <= 'N' );
}


/* The only array variable (.dimension > 0 refers to this) */
const unsigned arr_max_size = 4;    // maximum size
const int arr_extra = 3;            // number of tested indices beyond ends
number var_array[ arr_max_size ];   // values are this[ 0 ], ..., this[ ]
unsigned arr_sz_var;                // var_used[ this ].value = array size +
int                                 //   arr_lo - arr_ofst - 1
  arr_ofst = 0,                     // arr_hi = arr_lo + array size - 1
  arr_lo = 1, arr_hi = 0,           // the smallest and biggest user index
  arr_test_min = 0;                 // the smallest tested element value


/* Test value sequences for different types */
const number test_seq_R[] = {
  0, 1, 2, 3, 4, 5, 6, -1, -2, -3, -4, -5, -6, 10, -10, 20, -20, 50, -50, 100,
  -100,
  number( 1, 10 ), number( 1, 10, true ),
  number( 1, 100 ), number( 1, 100, true )
};
const unsigned test_size_R = 25;
const number test_seq_R2[] = { 0, 1, 3, -1, -3, 10, -10, 30, -30, 100, -100 };
const unsigned test_size_R2 = 11;
const int test_seq_Z[] = {
  0, 1, 2, 5, -1, -2, -5, 10, -10, 20, -20, 50, -50, 100, -100
};
const unsigned test_size_Z = 15;
const int test_seq_Z2[] = { 0, 1, 3, -1, -3, 10, -10, 30, -30, 100, -100 };
const unsigned test_size_Z2 = 11;
const int test_seq_P[] = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31 };
const unsigned test_size_P = 11;
const truth_val test_seq_tv2[] = { tv_F, tv_T };
const truth_val test_seq_tv3[] = { tv_F, tv_T, tv_U };
bool ls_var4 = false;   // allow fourth variable at the cost of less test

/* Number of test values as a function of type */
inline unsigned test_values( var_type vt ){
  switch( vt ){
  case vtp_R: case vtp_Q: return ls_var4 ? test_size_R2 : test_size_R;
  case vtp_Z: case vtp_none: return ls_var4 ? test_size_Z2 : test_size_Z;
  case vtp_P : return test_size_P;
  case vtp_mod: return mod_base;
  case vtp_tv2: return 2;
  case vtp_tv3: return 3;
  case vtp_idx: return arr_max_size + 2*arr_extra;
  default: return 1;
  }
}

/* Variables for maintaining combinations of test values */
unsigned
  var_seq[ var_max ],   // position in test sequence for the index variable
  test_combs = 1,       // number of test value combinations
  var_c_first = 0;      // the first index whose variable is used here


/* Reset the combination of values of test variables. */
void first_test_combination( bool choose_alt = false ){
  choose_alt |= ls_var4;
  for( unsigned ii = var_c_first; ii < var_f_cnt; ++ii ){
    var_seq[ ii ] = 0;
    switch( var_used[ ii ].type ){
    case vtp_R: case vtp_Q:
      var_used[ ii ].value = choose_alt ? test_seq_R2[ 0 ] : test_seq_R[ 0 ];
      break;
    case vtp_Z: case vtp_none:
      var_used[ ii ].value = choose_alt ? test_seq_Z2[ 0 ] : test_seq_Z[ 0 ];
      break;
    case vtp_P: var_used[ ii ].value = test_seq_P[ 0 ]; break;
    case vtp_mod: var_used[ ii ].value = 0; break;
    case vtp_tv2: var_used[ ii ].value = test_seq_tv2[ 0 ]; break;
    case vtp_tv3: var_used[ ii ].value = test_seq_tv3[ 0 ]; break;
    case vtp_idx: var_used[ ii ].value = 0; break;
    default: var_used[ ii ].value = 0; break;
    }
  }
}


/* Find the next combination of values of test variables. */
bool next_test_combination(
  bool choose_alt = false, bool fix_full_sequence = false
){
  time_now(); if( err_mode ){ return false; }
  choose_alt |= ls_var4;
  unsigned seq_inc = 1;
  for( unsigned ii = var_c_first; ii < var_f_cnt; ++ii ){
    var_seq[ ii ] += seq_inc;
    switch( var_used[ ii ].type ){
    case vtp_R: case vtp_Q:
      if( choose_alt ){
        var_seq[ ii ] %= test_size_R2;
        var_used[ ii ].value = test_seq_R2[ var_seq[ ii ] ]; break;
      }else{
        var_seq[ ii ] %= test_size_R;
        var_used[ ii ].value = test_seq_R[ var_seq[ ii ] ]; break;
      }
    case vtp_Z: case vtp_none:
      if( choose_alt ){
        var_seq[ ii ] %= test_size_Z2;
        var_used[ ii ].value = test_seq_Z2[ var_seq[ ii ] ]; break;
      }else{
        var_seq[ ii ] %= test_size_Z;
        var_used[ ii ].value = test_seq_Z[ var_seq[ ii ] ]; break;
      }
    case vtp_P:
      var_seq[ ii ] %= test_size_P;
      var_used[ ii ].value = test_seq_P[ var_seq[ ii ] ]; break;
    case vtp_mod:
      var_seq[ ii ] %= mod_base;
      var_used[ ii ].value = var_seq[ ii ]; break;
    case vtp_tv2:
      var_seq[ ii ] %= 2;
      var_used[ ii ].value = test_seq_tv2[ var_seq[ ii ] ]; break;
    case vtp_tv3:
      var_seq[ ii ] %= 3;
      var_used[ ii ].value = test_seq_tv3[ var_seq[ ii ] ]; break;
    case vtp_idx:
      var_seq[ ii ] %= arr_max_size + 2*arr_extra;
      var_used[ ii ].value = int( var_seq[ ii ] ) + arr_lo - arr_extra;
      break;
    default: break;
    }
    if( var_seq[ ii ] ){
      if( !fix_full_sequence ){ return true; }
      seq_inc = 0;
    }
  }
  return seq_inc == 0;
}


/* Find or create a new variable based on its name. */
unsigned find_or_add_variable(
  unsigned nm, var_type vt, bool is_new_q, bool may_create
){

  /* If an error location is in use, cancel it. If a new quantified variable
    is created, update the corresponding counter and range. */
  if( var_f_cnt > var_max ){ var_f_cnt = var_max; }
  if( qua_set ){
    if( var_q2_cnt > var_max ){ var_q2_cnt = var_max; }
    if( is_new_q ){ ++var_q2_cnt; ++var_q_hi; }
  }else{
    if( var_q1_cnt > var_max ){ var_q1_cnt = var_max; }
    if( is_new_q ){ ++var_q1_cnt; ++var_q_hi; }
  }

  /* Compute the index of the variable. */
  unsigned ii = var_q_hi; bool is_new = is_new_q;
  if( !is_new_q ){

    /* If an open quantified variable has the same name, let ii be the
      corresponding index. */
    for( ii = var_q_hi; ii >= var_q_lo; --ii ){
      if( var_used[ ii ].name == nm && !var_used[ ii ].closed ){ break; }
    }

    /* If the previous failed and a free variable has the same name, let ii be
      the corresponding index. */
    if( ii < var_q_lo ){
      for( ii = 0; ii < var_f_cnt; ++ii ){
        if( var_used[ ii ].name == nm ){ break; }
      }
    }

    /* If the two above failed, create a new free variable. */
    if( ii == var_f_cnt ){ ++var_f_cnt; is_new = true; }

  }

  /* Check that the name is legal. */
  if( !check_var_name( nm ) ){
    mc_err_print( "Illegal variable name" ); return 0;
  }

  /* Create the variable or check that the types match. */
  if( is_new ){
    if( vt == vtp_none ){
      if( intvar( nm ) ){ vt = vtp_Z; }else{ vt = vtp_R; }
    }
    var_used[ ii ].name = nm;
    var_used[ ii ].dimension = 0;
    var_used[ ii ].type = vt;
    var_used[ ii ].closed = var_used[ ii ].hides = false;
    test_combs *= test_values( vt );
    if( !may_create ){
      err_set_inp( "New variables must not be introduced here" );
    }
  }else if( var_used[ ii ].type != vt ){
    if( vt == vtp_idx && may_create && var_used[ ii ].dimension == 0 ){
      test_combs /= test_values( var_used[ ii ].type );
      test_combs *= test_values( vtp_idx );
      var_used[ ii ].type = vtp_idx;
    }else if( vt != vtp_none ){
      err_mode = err_var_type;
      err_uns1 = var_used[ ii ].name;
      err_uns2 = var_used[ ii ].type;
      err_uns3 = vt; return ii;
    }
  }

  /* If created a quantified variable, record name clash if exists. */
  if( is_new_q ){
    for( unsigned jj = 0; jj < var_f_cnt; ++jj ){
      if( var_used[ jj ].name == nm ){ var_used[ ii ].hides = true; break; }
    }
    for( unsigned jj = var_q_lo; jj < var_q_hi; ++jj ){
      if( var_used[ jj ].name == nm && !var_used[ jj ].closed ){
        var_used[ ii ].hides = true; break;
      }
    }
  }

  /* If created a free variable, record name clash if exists. */
  else if( is_new ){
    for( unsigned jj = var_q_lo; jj <= var_q_hi; ++jj ){
      if( var_used[ jj ].name == nm ){ var_used[ jj ].hides = true; }
    }
  }

  /* If too many variables are created, set an error code. */
  if( ii == var_max || ii == var_f_max ){ err_mode = err_var_f_cnt; }
  if( ii == 2*var_max + 1 || ii > 3*var_max + 1 ){ err_mode = err_var_q_cnt; }
  if( test_combs > 30375 ){ err_mode = err_combs; }

  return ii;
}


/* Close a (quantified) variable. */
inline void var_close( unsigned ii ){
  var_used[ ii ].closed = true;
  test_combs /= test_values( var_used[ ii ].type );
}


/*** Global and chain-local settings, and token separation ***/


/* Global and chain-local parsing-related and operation mode variables */
bool
  gs_debug = false,       // show debug information
  gs_draw = true,         // draw curves of expressions in error message
  gs_only_no_yes = false, // do not give full feedback on an error
  gs_prop3 = false,       // use tv_U also in propositional logic
  gs_prove = true,        // try to prove the claims
  gs_undef_check = true,  // treat undef as different from defined
  gs_verbose = false;     // true ==> print headers, version info, etc.
#ifdef exam
const bool gs_exam = true;  // examination mode: provide limited feedback
#else
bool gs_exam = false;
#endif
std::string
  gs_ok_text,           // printed if nonempty and the answer passes the check
  gs_fail_text;         // printed if nonempty and the answer fails the check
bool
  ls_allow_comp = false,  // allow below chain oprs also in exam textareas
  ls_ban_comp = false,    // disallow < <= > >= <== ==> as chain oprs
  ls_hide_expr = false,   // do not show the next expr of a solution chain
  /*ls_var4 = false,*/    // see earlier on
  ls_f_CNF = false,       // final expression must be in CNF
  ls_f_DNF = false,       // final expression must be in DNF
  ls_f_ban_U = false,     // final expression may not yield U
  ls_f_polynomial = false,  // final expression must be in polynomial form
  ls_f_simplify = false,  // final expression must have been simplified
  ls_f_range = false;     // f.e. must not contain constants outside mod_base
unsigned
  ls_b_nodes = 0,         // > 0: praise complexity of the final expression
  ls_f_nodes = 0,         // > 0: maximum complexity of the final expression
  ls_solve = 0,           // variable that must be solved
  ls_f_top_var = 0,       // the var of ls_f_top_opr, if it is AA, DD, EE
  ls_f_ban_cnt = 0;       // number of finally banned operators
unsigned const ls_f_ban_max = 30;   // maximum nr of finally banned operators
op_type
  ls_f_top_opr = op_err,  // required root of the final expr tree
  ls_f_ban_ops[ op_err ];   // finally banned operators
const unsigned URL_max = 100;       // maximum length of next problem page URL
char next_URL[ URL_max + 1 ] = {};  // URL of next problem page
bool CFG_ambiguous = false;   // warn about ambiguous grammar
extern bool CFG_ignore_CR;    // see CFG.cc

void reset_global_settings(){
  // For easier debugging, remember to not reset debug here!
  if( gs_verbose ){ pgh_msg( "Settings reset" ); }
  CFG_ambiguous = CFG_ignore_CR = gs_only_no_yes = gs_prop3 = gs_verbose =
  false;
  gs_draw = gs_prove = gs_undef_check = true;
  gs_ok_text.clear(); gs_fail_text.clear();
}


/* Token separation */
tkn_type tkn_now = tkn_eoi;   // current token
number tkn_val( 0 );          // its nonnegative integer or decimal parameter
unsigned tkn_cnt = 0;         // number of calls to get_token
void get_token(){

  /* Abort, if serious enough error. */
  if( err_mode >= err_time ){ tkn_now = tkn_eoi; return; }

  /* Update the global token variables. */
  ++tkn_cnt; tkn_val = 0;

  /* Skip spaces and newlines. */
  inp_skip_white_space();

  /* Remember token start place for potential error messages. */
  err_pos = inp_byte_now;

  /* If the input ended, return end-of-input. */
  if( !inp_chr ){ tkn_now = tkn_eoi; return; }

  /* Number constant */
  if( is_digit( inp_chr ) ){
    tkn_now = tkn_number;

    /* Read integer part. */
    unsigned nn = 0;
    do{
      unsigned ii = inp_chr - '0';
      if( ( -1u - ii ) / 10 < nn ){
        err_set_inp( "Too big number constant" ); return;
      }
      nn *= 10; nn += ii; inp_get_chr();
    }while( is_digit( inp_chr ) );
    tkn_val = number( nn, 1 );

    /* If the constant is an unsigned integer, return. */
    if( inp_chr != '.' ){ return; }
    inp_get_chr();
    if( !is_digit( inp_chr ) ){ inp_unread( 1 ); return; }

    /* Decimal number */
    tkn_now = tkn_decimal;
    number p10 = number( 1, 10 );
    while( is_digit( inp_chr ) ){
      tkn_val += p10 * ( inp_chr - '0' ); p10 /= 10; inp_get_chr();
    }
    return;

  }

  /* Find a match from tkn_str or as a single letter, if possible. */
  tkn_now = tkn_type( inp_match( &tkn_str[1], tkn_err - 1 ) );

  /* If a match was found, return it. */
  if( tkn_now != tkn_err ){ return; }

  /* Report error. */
  inp_get_chr(); err_set_inp( "Unknown token" );

}

inline void unget_token(){
  inp_revert_to( err_pos );
  if( tkn_now == tkn_err ){ err_reset(); }else{ tkn_now = tkn_err; }
}


/*** Expressions ***/


/* Precedence values: global small, logical, arithmetic, global big, forced
  parenthesis. Every segment except the last must have an even number of
  entries! */
enum {
  gp0, gp1,
  tp0, tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9,
  ap00, ap01, ap2, ap3, ap4, ap5, ap6, ap7, ap8, ap9, ap10, ap11, ap12, ap13,
  ap14, ap15, ap16, ap17,
  gpl, gpr, gp_force
};


/* Node of the expression tree or expression DAG */
class expression{
private:

  /* For a constant, val is its value as a number. For a variable, val is its
    index in var_used. Left precedences must be even and right precedences
    odd. */
  op_type opr_;                 // operand
  expression *left_, *right_;   // subexpressions
  number val_;                  // number value or variable letter
  unsigned h_val_;              // hash value (needed of sub-expressions)
  expression *h_next,           // next in hash list
    *smpl;                      // either 0 or simplified version of the expr.
  unsigned type_;               // static information (e.g., may be < 0)
  union{
    void *extra_ptr_;           // pointer for specific needs of algorithms
    unsigned extra_uns_;        // some bytes of workspace
  };

  /* Hash table */
  static const unsigned h_size = 1 << 16;
  static expression *h_tbl[], *h_free;
  static unsigned h_val(
    expression *left, op_type opr, expression *right, number val
  ){
    unsigned result = opr << 5 ^ val.hash();
    if( left ){ result ^= left->h_val_ << 1; }
    if( right ){ result ^= right->h_val_ << 2; }
    return result & ( h_size - 1 );
  }

  /* Constructor */
  expression( expression *left, op_type opr, expression *right, number val ):
    opr_( opr ), left_( left ), right_( right ), val_( val ), smpl( 0 ),
    extra_uns_( 0 )
  {}

  /* Friends */
  friend expression *new_expr( expression *, op_type, expression *, number );
  friend bool elt( expression *, expression * );
  friend expression *simplify( expression * );

public:

  static unsigned
    h_max_len,  // maximum hash list length ever
    h_burden,   // ~~ half of sum of squares of lengths of hash lists
    h_max_brdn, // maximum h_burden ever
    h_cnt;      // total number of hash nodes

  /* Reset hash table */
  static void h_reset(){
    h_burden = 0;
    for( unsigned ii = 0; ii < h_size; ++ii ){
      if( h_tbl[ ii ] ){
        expression *ee = h_tbl[ ii ];
        while( ee->h_next ){ ee = ee->h_next; }
        ee->h_next = h_free; h_free = h_tbl[ ii ]; h_tbl[ ii ] = 0;
      }
    }
  }

  /* Accessors */
  inline op_type opr() const { return opr_; }
  inline expression *left() const { return left_; }
  inline expression *right() const { return right_; }
  inline number val() const { return val_; }
  inline unsigned type() const { return type_; }
  inline unsigned extra_uns() const { return extra_uns_; }
  inline unsigned var_idx() const { return to_var_idx( val_ ); }
  inline variable &var() const { return to_var( val_ ); }


  /* Returns true iff operand is a number from the point of view of printing.
  */
  inline bool is_number(){
    return
      opr_ == op_const || opr_ == op_mixn || (
        opr_ == op_div && left_->opr_ == op_const && right_->opr_ == op_const
      );
  }


  /* Prints the expression in AsciiMath. */
  void print_AM(
    unsigned lup = gp1, unsigned rup = gp0, bool pr_in_abs = false
  ){

    /* Prepare for finding left and right precedence based on the operator. */
    unsigned lprec = gpl, rprec = gpr;  // precedences to the left and right
    bool right_par = false;     // true forces parentheses around right child
    static unsigned extra_par = 0,  // to produce ( ) around arg. of sin-like
      old_vis_cnt = 0;              // to detect that something was printed

    /* "Easy" arithmetic operators */
    if( opr_ == op_pow ){ lprec = ap16; rprec = ap15; right_par = true; }
    else if( opr_ == op_fact ){ lprec = ap16; }
    else if( opr_ == op_sqrt ){ rprec = ap13; right_par = true; }
    else if( opr_ == op_root ){ rprec = ap13; right_par = true; }
    else if( opr_ == op_div ){ lprec = ap12; rprec = ap13; }
    else if( opr_ == op_mixn ){ lprec = ap12; rprec = ap11; }
    else if( opr_ == op_vprod ){ lprec = ap4; rprec = ap5; }
    else if( opr_ == op_idiv || opr_ == op_imod ){ lprec = ap2; rprec = ap3; }
    else if( opr_ == op_plus || opr_ == op_minus ){
      lprec = left_ ? ap00 : ap2; rprec = ap01;   // binary vs. unary
    }
    else if( opr_ == op_const ){
      if(
        val_.is_neg() ||
        ( ( val_.is_dbl() || val_.is_dbu() ) && to_double( val_ ) < 0. )
      ){ lprec = ap2; rprec = ap01; }
    }

    /* Invisible multiplication */
    else if( opr_ == op_iprod ){
      lprec = ap10; rprec = ap11;
      if(
        is_sin_like( right_->opr_ ) ||
        right_->opr_ == op_ddn || right_->opr_ == op_dup
      ){ lprec = ap8; rprec = ap9; }
      else if( right_->opr_ == op_deriv ){ lprec = ap8; rprec = ap9; }
      //??? { lprec = ap6; rprec = ap7; }
      if( right_->is_number() ){ right_par = true; }
    }

    /* Hard parentheses */
    else if( opr_ == op_hpar ){ lprec = gp0; rprec = gp1; }

    /* ln, sin, etc. or derivative */
    else if(
      is_sin_like( opr_ ) || opr_ == op_ddn || opr_ == op_dup
    ){ rprec = ap9; }
    else if( opr_ == op_deriv ){ rprec = ap7; }

    /* Logical operators */
    else if( is_rel( opr_ ) || opr_ == op_is_int ){
      lprec = tp8; rprec = tp9;
    }
    else if( opr_ == op_not || opr_ == op_def ){ rprec = tp9; }
    else if( opr_ == op_and || opr_ == op_sand ){ lprec = tp6; rprec = tp7; }
    else if( opr_ == op_or || opr_ == op_sor ){ lprec = tp4; rprec = tp5; }
    else if( opr_ == op_rarr ){ lprec = tp4; rprec = tp3; }
    else if( opr_ == op_harr ){ lprec = tp2; rprec = tp3; }
    else if( opr_ == op_Luka ){ lprec = tp4; rprec = tp3; }
    else if( opr_ == op_forall || opr_ == op_exists ){ rprec = tp1; }
    else if( is_lrel( opr_ ) ){ lprec = tp0; rprec = tp1; }

    /* Decide about parentheses around the current sub-expression. It is
      important for the extra_par mechanism that this is the only place
      where a ( starting a sub-expression may be printed. */
    bool this_par =
      lprec < lup || rprec < rup || ( opr_ == op_abs && pr_in_abs );
    if( this_par ){
      if( extra_par == 2 ){
        if( old_vis_cnt == out_vis_cnt ){ out_am( '(' ); extra_par = 0; }
        else{ extra_par = 1; }
      }
      lup = gp1; rup = gp0; pr_in_abs = false; out_am( '(' );
    }
    if( extra_par == 3 ){ extra_par = 2; }
    if( right_par ){ rup = gp_force; }

    /* Special case: variable or array */
    if( opr_ == op_var ){
      out_am( ' ' ); out_idx_name( var_idx() );
      if( left_ ){
        out_am( '[' ); left_->print_AM();
        if( right_ ){ out_am( ',' ); right_->print_AM(); }
        out_am( ']' );
      }
      if( this_par ){ out_am( ')' ); }
      return;
    }

    /* Special case: fraction, root, square root, or power */
    if( opr_ == op_div || opr_ == op_root ){
      if( opr_ == op_div ){ out_am( " frac(" ); }else{ out_am( " root(" ); };
      left_->print_AM(); out_am( ")(" );
      right_->print_AM(); out_am( ')' );
      if( this_par ){ out_print( ')' ); }
      return;
    }
    if( opr_ == op_sqrt || opr_ == op_pow ){
      if( opr_ == op_sqrt ){ out_am( " sqrt(" ); }
      else{ left_->print_AM( lup, lprec, pr_in_abs ); out_am( " ^(" ); }
      right_->print_AM(); out_am( ')' );
      if( this_par ){ out_print( ')' ); }
      return;
    }

    /* Special case: absolute value */
    if( opr_ == op_abs ){
      out_am( '|' ); right_->print_AM( gp1, gp0, true ); out_am( '|' );
      if( this_par ){ out_print( ')' ); }
      return;
    }

    /* Special case: floor */
    if( opr_ == op_lfloor ){
      out_am( " |__" ); right_->print_AM(); out_am( " __|" );
      if( this_par ){ out_print( ')' ); }
      return;
    }

    /* Special case: ceiling */
    if( opr_ == op_lceil ){
      out_am( " |~" ); right_->print_AM(); out_am( " ~|" );
      if( this_par ){ out_print( ')' ); }
      return;
    }

    /* Special case: sin x, sin^2 x or similar */
    if(
      is_sin_like( opr_ ) || opr_ == op_deriv ||
      opr_ == op_ddn || opr_ == op_dup
    ){
      if( opr_ == op_deriv ){
        out_am( " frac(del)(del " ); out_idx_name( var_idx() ); out_am( ')' );
      }else{
        out_am( ' ' ); out_am( op_AM[ opr_ ] );
        if( left_ ){ out_print( "^(" ); left_->print_AM(); out_am( ')' ); }
      }
      bool old_par = extra_par; extra_par = 3; old_vis_cnt = out_vis_cnt;
      right_->print_AM( rprec, rup, pr_in_abs );
      if( !extra_par ){ out_am( ')' ); }
      extra_par = old_par;
      if( this_par ){ out_am( ')' ); }
      return;
    }

    /* Special case: factorial */
    if( opr_ == op_fact ){
      right_->print_AM( ap16 ); out_am( " !" );
      if( this_par ){ out_print( ')' ); }
      return;
    }

    /* Special case: quantifier */
    if( opr_ == op_forall || opr_ == op_exists ){
      out_am( ' ' ); out_am( op_AM[ opr_ ] );
      out_print( ' ' ); out_idx_name( var_idx() );
      if( left_ ){ out_am( ';' ); left_->print_AM(); }
      out_am( ':' ); right_->print_AM( rprec, rup, pr_in_abs );
      if( this_par ){ out_am( ')' ); }
      return;
    }

    /* Special case: is integer -predicate */
    if( opr_ == op_is_int ){
      right_->print_AM( lup, lprec, pr_in_abs ); out_am( op_AM[ opr_ ] );
      if( this_par ){ out_print( ')' ); }
      return;
    }

    /* If the left child exists, print it (perhaps in parentheses). */
    if( left_ ){ left_->print_AM( lup, lprec, pr_in_abs ); }

    /* Print a space and then the operator according to its type. */
    out_am( ' ' );
    if( opr_ == op_const ){ out_number( val_, true ); }
    else{ out_am( op_AM[ opr_ ] ); }

    /* If the right child exists, print it (perhaps in parentheses). */
    if( right_ ){ right_->print_AM( rprec, rup, pr_in_abs ); }

    if( this_par ){ out_am( ')' ); }
  }


  /* Setting the value of extra_uns */
  inline void set_extra( unsigned uu ){ extra_uns_ = uu; }
  inline void set_extra( void *pp ){ extra_ptr_ = pp; }
  void recursive_extra( unsigned uu ){
    extra_uns_ = uu;
    if( left_ ){ left_->recursive_extra( uu ); }
    if( right_ ){ right_->recursive_extra( uu ); }
  }


  /* Counts the number of nodes in the expression. */
  unsigned node_cnt(){
    unsigned result =
      opr_ == op_hpar ? 0 :
        ( is_sin_like( opr_ ) || opr_ == op_forall || opr_ == op_exists ) &&
        left_ ?
        2 : 1;
    if( left_ && opr_ != op_deriv ){ result += left_->node_cnt(); }
    if( right_ ){ result += right_->node_cnt(); }
    return result;
  }

  /* Reveals whether the expression contains the given operator. */
  bool has_opr_raw( op_type opr1 ){
    if( opr_ == opr1 ){ return true; }
    if( left_ && left_->has_opr_raw( opr1 ) ){ return true; }
    return right_ && right_->has_opr_raw( opr1 );
  }
  bool has_opr_pow(){
    if( opr_ == op_pow || ( is_sin_like( opr_ ) && left_ ) ){ return true; }
    if( left_ && left_->has_opr_pow() ){ return true; }
    return right_ && right_->has_opr_pow();
  }
  inline bool has_opr( op_type opr1 ){
    if( opr1 == op_pow ){ return has_opr_pow(); }
    else{ return has_opr_raw( opr1 ); }
  }

  /* Reveals whether the expression contains ii as not solved. */
  bool solved( unsigned ii, op_type father ){
    if( opr_ == op_var && !is_rel( father ) && var_idx() == ii ){
      return false;
    }
    if( left_ && !left_->solved( ii, opr_ ) ){ return false; }
    if( right_ && !right_->solved( ii, opr_ ) ){ return false; }
    return true;
  }

  /* Reveals whether the expression is a logical or arithm (negated) atom. */
  bool is_atom( bool may_neg ){
    if(
      opr_ == op_const || opr_ == op_mixn || opr_ == op_var ||
      is_rel( opr_ ) || opr_ == op_is_int || opr_ == op_e2718 || opr_ == op_pi
    ){ return true; }
    if( may_neg && ( opr_ == op_not || ( opr_ == op_minus && !left_ ) ) ){
      return right_->is_atom( false );
    }
    return false;
  }

  /* How about op_forall vs. op_and, and op_sum and op_exists vs. op_or??? */

  /* Reveals whether the expression is a logical or arithm clause. */
  bool is_clause(){
    if(
      opr_ == op_or || ( left_ && ( opr_ == op_plus || opr_ == op_minus ) )
    ){ return left_->is_clause() && right_->is_clause(); }
    return is_atom( true );
  }

  /* Reveals whether the expression is in conjunctive normal form. */
  bool is_CNF(){
    if( opr_ == op_and || opr_ == op_iprod || opr_ == op_vprod ){
      return left_->is_CNF() && right_->is_CNF();
    }
    return is_clause();
  }

  /* Reveals whether the expression is a logical or arithm esualc. */
  bool is_esualc(){
    if( opr_ == op_and || opr_ == op_iprod || opr_ == op_vprod ){
      return left_->is_esualc() && right_->is_esualc();
    }
    return is_atom( true );
  }

  /* Reveals whether the expression is in disjunctive normal form. */
  bool is_DNF(){
    if(
      opr_ == op_or || ( left_ && ( opr_ == op_plus || opr_ == op_minus ) )
    ){ return left_->is_DNF() && right_->is_DNF(); }
    return is_esualc();
  }

  /* Reveals whether the expression is in polynomial power form. */
  bool is_pol_pow(){
    if( opr_ == op_pow ){
      return
        left_->opr_ == op_var &&
        right_->opr_ == op_const &&
        ( right_->val_.type == number::zer || right_->val_.to_unsigned() );
    }else{ return opr_ == op_var || opr_ == op_const; }
  }

  /* Reveals whether the expression is in polynomial term form. */
  bool is_pol_term(){
    if( opr_ == op_vprod || opr_ == op_iprod ){
      return left_->is_pol_term() && right_->is_pol_pow();
    }else{ return is_pol_pow(); }
  }

  /* Reveals whether the expression is in polynomial form. */
  bool is_polynomial(){
    if( opr_ == op_plus || opr_ == op_minus ){
      if( left_ ){ return left_->is_polynomial() && right_->is_pol_term(); }
      else{ return right_->is_pol_term(); }
    }else{ return is_pol_term(); }
  }

  /* Reveals whether the constants in the expression are in the allowed
    range. */
  bool is_range(){
    if( opr_ == op_const ){
      if( val_.type == number::zer ){ return true; }
      unsigned ii = val_.to_unsigned();
      return 0 < ii && ii < mod_base;
    }
    return
      ( !left_ || left_->is_range() ) &&
      ( !right_ || right_->is_range() );
  }

};

expression *expression::h_tbl[ expression::h_size ] = {};
expression *expression::h_free = 0;
unsigned expression::h_max_len = 0, expression::h_burden = 0;
unsigned expression::h_max_brdn = 0, expression::h_cnt = 0;


/* Pseudo-constructors: use existing expression or create a new one */

expression *new_expr(
  expression *left, op_type opr, expression *right, number val = 0
){

  /* Search from the hash table. */
  unsigned ii = expression::h_val( left, opr, right, val ), l_len = 0;
  for( expression *ee = expression::h_tbl[ ii ]; ee; ee = ee->h_next ){
    ++l_len;
    if(
      ee->opr_ == opr && ee->left_ == left && ee->right_ == right &&
      ee->val_ == val
    ){ return ee; }
  }

  /* If the hash table is too big, abort returning an undefined expression. */
  expression::h_burden += l_len; ++expression::h_cnt;
  if( expression::h_max_len < l_len ){ expression::h_max_len = l_len; }
  if( expression::h_max_brdn < expression::h_burden ){
    expression::h_max_brdn = expression::h_burden;
  }
  if( expression::h_burden >= 8*expression::h_size && !val.is_und() ){
    err_mode = err_hash; return new_expr( 0, op_err, 0, numu );
  }

  /* Create a new object and put it to the hash table. */
  expression *ee = 0;
  if( expression::h_free ){
    ee = expression::h_free; expression::h_free = expression::h_free->h_next;
    ee->opr_ = opr; ee->left_ = left; ee->right_ = right; ee->val_ = val;
    ee->h_val_ = 0; ee->h_next = 0; ee->smpl = 0; ee->type_ = 0;
    ee->extra_uns_ = 0;
  }else{ ee = new expression( left, opr, right, val ); }
  ee->h_next = expression::h_tbl[ ii ]; expression::h_tbl[ ii ] = ee;
  ee->h_val_ = ii;

  /* The rest of this function sets the type of the expression. The bits of
    the type have the following meanings: //??? jarkiperaista koodaus
    0x1 : may yield 0.
    0x2 : may yield a positive value.
    0x4 : may yield a negative value.
    0x8 : may yield a non-integral value.
    0x10: may yield an irrational value, always together with 0x8.
    0x20: may yield more than one value (ie., is not necessarily a constant).
    0x40: yields a truth value (may combine to 0x20 and 0x100).
    0x80: yields a modular arithmetic value (may combine to 0x1, 0x2, 0x20 and
          0x100). //??? toteutettu vain 0x80 paalla / pois
    // 0x100: may yield an undefined value. //??? ei toteutettu
    "May yield" means here that "does not yield" is not known to be true.
    ??? should there also be a bit for the undefined value?
    Truth-valued expressions have type 0x40 or 0x60 ??? ei toimi const var.
  */

  /* Operators that yield a truth value. */
  if( yields_logic( opr ) ){ ee->type_ = 0x60; return ee; }

  /* Derivative */
  if( opr == op_deriv ){ ee->type_ = left->type_; return ee; }

  /* Remaining 0-ary operators */
  if( opr == op_const ){
    switch( val.type ){
      case number::dbu:
      case number::und: ee->type_ = 0x1F; return ee;
      case number::zer: ee->type_ = 0x1; return ee;
      case number::pos: ee->type_ = val.r_de == 1 ? 0x2 : 0xA; return ee;
      case number::neg: ee->type_ = val.r_de == 1 ? 0x4 : 0xC; return ee;
      case number::dbl:
        ee->type_ = 0x18;
        if( val.d_lo < 0. ){ ee->type_ |= 4; }
        if( val.d_hi > 0. ){ ee->type_ |= 2; }
        if( val.d_lo <= 0. && val.d_hi >= 0. ){ ee->type_ |= 1; }
        return ee;
      case number::trv: ee->type_ = 0x40; return ee;
    }
  }
  if( opr == op_mixn ){ ee->type_ = 0xB; return ee; }
  if( opr == op_e2718 || opr == op_pi ){ ee->type_ = 0x1A; return ee; }
  if( opr == op_var ){
    ee->type_ =
      ee->var().type == vtp_Z || ee->var().type == vtp_none ||
        ee->var().type == vtp_idx ? 0x27 :
      ee->var().type == vtp_P ? 0x22 :
      ee->var().type == vtp_tv2 || ee->var().type == vtp_tv3 ? 0x60 :
      ee->var().type == vtp_mod ? 0xA3 : 0x3F;
    return ee;
  }
  if( !right ){ ee->type_ = 0x3F; return ee; }

  /* Remaining unary operators */
  unsigned rt = right->type_;
  ee->type_ = rt;
  if( !left ){
    if( opr == op_hpar || opr == op_plus ){ return ee; }
    if( opr == op_minus ){
      ee->type_ &= ~0x6;
      if( rt & 0x2 ){ ee->type_ |= 0x4; }
      if( rt & 0x4 ){ ee->type_ |= 0x2; }
    }
    else if( opr == op_abs ){
      if( rt & 0x4 ){ ee->type_ &= ~0x4; ee->type_ |= 0x2; }
    }
    else if( opr == op_sqrt ){
      ee->type_ &= ~0x4;
      if( rt & 0x2 ){ ee->type_ |= 0x18; }
    }
    else if( opr == op_lfloor ){
      ee->type_ &= ~0x18;
      if( rt & 0x2 ){ ee->type_ |= 0x1; }
    }
    else if( opr == op_lceil ){
      ee->type_ &= ~0x18;
      if( rt & 0x4 ){ ee->type_ |= 0x1; }
    }
    else if( opr == op_fact ){
      ee->type_ &= ~0x1F; ee->type_ |= 0x2;
    }
    else{ ee->type_ |= 0x1F; }  // covers is_sin_like( opr ), ddn, dup
    return ee;
  }

  /* Remaining binary operators */
  unsigned lt = left->type_;
  ee->type_ |= lt;
  if( is_sin_like( opr ) ){ rt = lt; lt = right->type_ | 0x1F; opr = op_pow; }
  if( opr == op_plus ){
    ee->type_ &= ~0x7;
    if(
      ( lt & 0x1 && rt & 0x1 ) ||
      ( lt & 0x2 && rt & 0x4 ) ||
      ( lt & 0x4 && rt & 0x2 )
    ){ ee->type_ |= 0x1; }
    if( lt & 0x2 || rt & 0x2 ){ ee->type_ |= 0x2; }
    if( lt & 0x4 || rt & 0x4 ){ ee->type_ |= 0x4; }
    return ee;
  }
  else if( opr == op_minus ){
    ee->type_ &= ~0x7;
    if(
      ( lt & 0x1 && rt & 0x1 ) ||
      ( lt & 0x2 && rt & 0x2 ) ||
      ( lt & 0x4 && rt & 0x4 )
    ){ ee->type_ |= 0x1; }
    if( lt & 0x2 || rt & 0x4 ){ ee->type_ |= 0x2; }
    if( lt & 0x4 || rt & 0x2 ){ ee->type_ |= 0x4; }
    return ee;
  }
  else if( opr == op_vprod || opr == op_iprod || opr == op_div ){
    ee->type_ &= ~0x7;
    if( lt & 0x1 || ( opr != op_div && rt & 0x1 ) ){ ee->type_ |= 0x1; }
    if(
      ( lt & 0x2 && rt & 0x2 ) || ( lt & 0x4 && rt & 0x4 )
    ){ ee->type_ |= 0x2; }
    if(
      ( lt & 0x2 && rt & 0x4 ) || ( lt & 0x4 && rt & 0x2 )
    ){ ee->type_ |= 0x4; }
    if( opr == op_div ){ ee->type_ |= 0x8; }
    if( !( ee->type_ & 0x6 ) ){ ee->type_ = 0x1; }
    return ee;
  }
  else if( opr == op_pow ){
    ee->type_ = ( lt & 0x20 ) | ( rt & 0x20 );
    if( rt & 0x1 ){ ee->type_ |= 0x2; }
    if( lt & 0x1 && rt & 0x2 ){ ee->type_ |= 0x1; }
    if( lt & 0x6 ){ ee->type_ |= 0x2; }
    if(
      lt & 0x4 && rt & 0x6 &&
      !( right->opr() == op_const && right->val().nu_even() )
    ){ ee->type_ |= 0x4; }
    if( lt & 0x6 && rt & 0x4 ){ ee->type_ |= 0x8; }
    if( lt & 0x6 && rt & 0x18 ){ ee->type_ |= 0x18; }
    if( lt & 0x6 && rt & 0x6 ){ ee->type_ |= lt & 0x18; }
    if( !( ee->type_ & 0x6 ) ){ ee->type_ = 0x1; }
  }
  else if( opr == op_root ){
    if( ee->type_ & 0x6 ){ ee->type_ |= 0x18; }
  }
  else if( opr == op_idiv ){
    ee->type_ &= ~0x1F;
    if( lt & 0x1 ){ ee->type_ |= 0x1; }
    if(
      ( lt & 0x2 && rt & 0x2 ) || ( lt & 0x4 && rt & 0x4 )
    ){ ee->type_ |= 0x3; }
    if(
      ( lt & 0x2 && rt & 0x4 ) || ( lt & 0x4 && rt & 0x2 )
    ){ ee->type_ |= 0x4; }
    if( !( ee->type_ & 0x6 ) ){ ee->type_ = 0x1; }
  }
  else if( opr == op_imod ){
    ee->type_ &= ~0x7; ee->type_ |= 0x3;
  }
  else{ ee->type_ = 0x3F; }
  return ee;

}

inline expression *new_expr( op_type opr, expression *right ){
  return new_expr( 0, opr, right, 0 );
}

inline expression *new_expr( op_type opr, number val ){
  return new_expr( 0, opr, 0, val );
}

inline expression *new_expr( number val ){
  return new_expr( 0, op_const, 0, val );
}


/* Some commonly occurring constant expressions */
expression
  *expr_F = new_expr( tv_F ),
  *expr_U = new_expr( tv_U ),
  *expr_T = new_expr( tv_T ),
  *expr_FU = new_expr( tv_FU ),
  *expr_FT = new_expr( tv_FT ),
  *expr_UT = new_expr( tv_UT ),
  *expr_FUT = new_expr( tv_FUT ),
  *expr_0 = new_expr( 0 ),
  *expr_1 = new_expr( 1 ),
  *expr_m1 = new_expr( -1 ),
  *expr_2 = new_expr( 2 ),
  *expr_10 = new_expr( 10 ),
  *expr_m2 = new_expr( -2 ),
  *expr_numu = new_expr( numu ),
  *expr_e = new_expr( op_e2718, nume ),
  *expr_pi = new_expr( op_pi, numpi ),
  *expr_dummy = new_expr( op_err, numu );


/* Printing the type of an expression */
void print_type( expression *ee ){
  out_html( " type " );
  for( unsigned ii = 0x20; ii; ii >>= 1 ){
    out_print( !!( ee->type() & ii ) );
  }
}


/*** Expression-level checking features ***/


/* Two important expressions for checking solutions. */
expression
  *now_expr = 0,        // most recently processed expression
  *dom_expr = expr_T;   // the domain = legal variable value combinations


/* Values of variables in expressions */
number
  test_err[ var_max ],            // current and error msg values
  test_left = 0, test_right = 0;  // results of expressions in the error
expression *test_e1, *test_e2;    // if given, draw these in error message

void test_fail( err_type err_now ){
  if( err_mode >= err_cmp || err_mode >= err_now ){ return; }
  err_mode = err_now;
  for( unsigned ii = 0; ii < var_f_cnt; ++ii ){
    test_err[ ii ] = var_used[ ii ].value;
  }
}

inline void test_fail(
  number v1, number v2, expression *e1 = 0, expression *e2 = 0
){
  test_left = v1; test_right = v2; test_e1 = e1; test_e2 = e2;
  test_fail( err_cmp );
}

inline void test_fail_compl(
  const char *msg = "Too complicated claim or expression, or too big numbers"
){ if( err_mode < err_compl ){ err_mode = err_compl; err_msg = msg; } }

inline void test_fail(
  number xx, const char *msg =
    " is of number type that is not allowed in this problem mode"
){
  if( err_mode >= err_numb ){ return; }
  err_mode = err_numb; err_num1 = xx; err_msg = msg;
}

inline void test_fail( op_type op, const char *msg = 0 ){
  if( err_mode >= err_op ){ return; }
  err_mode = err_op; err_uns1 = op; err_msg = msg;
}


/* Arithmetic expression evaluation */
number eval_expr( expression *ee ){

  if( ee->opr() == op_const ){ return ee->val(); }
  if( ee->opr() == op_var ){
    if( !ee->left() ){ return ee->var().value; }
    number nn = eval_expr( ee->left() ) - arr_lo;
    unsigned ii = nn.to_unsigned();
    if(
      !( ii || nn.is_zer() ) || ii >= arr_max_size ||
      int( ii ) > arr_hi - arr_lo
    ){ return numu; }
    return var_array[ ii ];
    //??? puuttuu moniulotteisuus ja useampi kuin yksi taulukko
  }
  if( ee->opr() == op_hpar ){ return eval_expr( ee->right() ); }

  if( ee->opr() == op_plus ){
    if( ee->left() ){
      return eval_expr( ee->left() ) + eval_expr( ee->right() );
    }
    return eval_expr( ee->right() );
  }
  if( ee->opr() == op_minus ){
    if( ee->left() ){
      return eval_expr( ee->left() ) - eval_expr( ee->right() );
    }
    return -eval_expr( ee->right() );
  }

  if( ee->opr() == op_mixn ){
    return eval_expr( ee->left() ) + eval_expr( ee->right() );
  }

  if( ee->opr() == op_vprod || ee->opr() == op_iprod ){
    return eval_expr( ee->left() ) * eval_expr( ee->right() );
  }
  if( ee->opr() == op_div ){
    return eval_expr( ee->left() ) / eval_expr( ee->right() );
  }

  if( ee->opr() == op_idiv ){
    return floor( eval_expr( ee->left() ) / eval_expr( ee->right() ) );
  }
  if( ee->opr() == op_imod ){
    number nl = eval_expr( ee->left() ), nr = eval_expr( ee->right() );
    return nl - nr*floor( nl / nr );
  }

  if( ee->opr() == op_abs ){ return abs( eval_expr( ee->right() ) ); }
  if( ee->opr() == op_lfloor ){ return floor( eval_expr( ee->right() ) ); }
  if( ee->opr() == op_lceil ){ return ceil( eval_expr( ee->right() ) ); }
  if( ee->opr() == op_fact ){ return factorial( eval_expr( ee->right() ) ); }
  if( ee->opr() == op_sqrt ){ return sqrt( eval_expr( ee->right() ) ); }
  if( ee->opr() == op_root ){
    return pow( eval_expr( ee->right() ), 1 / eval_expr( ee->left() ) );
  }
  if( ee->opr() == op_pow ){
    return pow( eval_expr( ee->left() ), eval_expr( ee->right() ) );
  }

  if( ee->opr() == op_e2718 || ee->opr() == op_pi ){ return ee->val(); }
  if( is_sin_like( ee->opr() ) ){
    number xx = eval_expr( ee->right() );
    if( xx.is_und() ){ return numu; }
    /**/ if( ee->opr() == op_ln ){ xx = ln( xx ); }
    else if( ee->opr() == op_log ){ xx = log10( xx ); }
    else if( ee->opr() == op_log2 ){ xx = log2( xx ); }
    else if( ee->opr() == op_sin ){ xx = sin( xx ); }
    else if( ee->opr() == op_cos ){ xx = cos( xx ); }
    else if( ee->opr() == op_tan ){ xx = tan( xx ); }
    else if( ee->opr() == op_cot ){ xx = cot( xx ); }
    else if( ee->opr() == op_sinh ){ xx = sinh( xx ); }
    else if( ee->opr() == op_cosh ){ xx = cosh( xx ); }
    else if( ee->opr() == op_tanh ){ xx = tanh( xx ); }
    return ee->left() ? pow( xx, eval_expr( ee->left() ) ) : xx;
  }

  if( ee->opr() == op_ddn ){
    number xx = eval_expr( ee->right() );
    xx.ddn(); return xx;
  }
  if( ee->opr() == op_dup ){
    number xx = eval_expr( ee->right() );
    xx.dup(); return xx;
  }
  if( ee->opr() == op_sum ){ return 0; /*??????*/ }

  if( ee->opr() == op_deriv ){ return eval_expr( ee->left() ); }

  if( ee->opr() == op_err ){ return numu; }
  mc_err_print( "Wrong arithmetic operator ", ee->opr(), "" );
  return numu;
}


/* Modular arithmetic expression evaluation */

/* If you plan to change mod_b_max, beware that the constant 25 exists
  literally in an error message string, and the correctness of the mod_rho
  mechanism has only been checked up to mod_base = 25. */
unsigned const
  mod_undef = ~0u,  // result of undefined modular arithmetic operations
  mod_b_max = 25;   // maximum allowed value of mod_base

/* Tables for fast computation of inverse, powers and roots */
unsigned
  mod_inv[ mod_b_max ],
  mod_pow[ mod_b_max ][ mod_b_max ],
  mod_root[ mod_b_max ][ mod_b_max ],
  mod_rho = 0;  // used in computing powers and roots with n >= mod_base

/* Initialization of the above-mentioned tables */
void mod_initialize(){

  /* Not all roots are defined, so initialize the table to all undefined. */
  for( unsigned ii = 0; ii < mod_base; ++ii ){
    for( unsigned jj = 0; jj < mod_base; ++jj ){
      mod_root[ ii ][ jj ] = mod_undef;
    }
  }

  /* Initialize the power and root tables, and compute mod_rho */
  mod_rho = mod_base-1;
  for( unsigned ii = 0; ii < mod_base; ++ii ){
    unsigned xx = 1;
    for( unsigned jj = 0; jj < mod_base; ++jj ){
      mod_pow[ ii ][ jj ] = xx;
      if( mod_root[ xx ][ jj ] == mod_undef ){ mod_root[ xx ][ jj ] = ii; }
      xx *= ii; xx %= mod_base;
    }
    for( unsigned jj = mod_rho+1; jj--; ){
      if( mod_pow[ ii ][ jj ] == xx ){ mod_rho = jj; break; }
    }
  }

  /* Ensure that mod_root contains square roots also for small mod_base. */
  if( mod_base == 2 ){ mod_root[ 0 ][ 2 ] = 0; mod_root[ 1 ][ 2 ] = 1; }

  /* Initialize the inverse table. */
  for( unsigned ii = 0; ii < mod_base; ++ii ){
    mod_inv[ ii ] = mod_undef;
    for( unsigned jj = 1; jj < mod_base; ++jj ){
      if( mod_pow[ ii ][ jj ] == 1 ){
        mod_inv[ ii ] = mod_pow[ ii ][ jj-1 ]; break;
      }
    }
  }

}

unsigned eval_mod( expression *ee ){

  if( ee->opr() == op_const ){ return ee->val().to_unsigned() % mod_base; }
  if( ee->opr() == op_var ){
    if( !ee->left() ){ return ee->var().value.to_unsigned() % mod_base; }
    mc_err_print( "Array variables not yet implemented" ); return mod_undef;
  }
  if( ee->opr() == op_hpar ){ return eval_mod( ee->right() ); }

  if( ee->opr() == op_pow ){
    unsigned aa = eval_mod( ee->left() );
    if( aa == mod_undef ){ return mod_undef; }
    unsigned nn = eval_expr( ee->right() ).to_unsigned();
    if( nn >= mod_base ){
      nn -= mod_rho; nn %= mod_base - mod_rho; nn += mod_rho;
    }
    return mod_pow[ aa ][ nn ];
  }

  if( ee->opr() == op_root ){
    unsigned aa = eval_mod( ee->right() );
    if( aa == mod_undef ){ return mod_undef; }
    unsigned nn = eval_expr( ee->left() ).to_unsigned();
    if( nn >= mod_base ){
      nn -= mod_rho; nn %= mod_base - mod_rho; nn += mod_rho;
    }
    return mod_root[ aa ][ nn ];
  }

  if( !ee->left() && ee->right() ){
    unsigned yy = eval_mod( ee->right() );
    if( yy == mod_undef ){ return mod_undef; }
    if( ee->opr() == op_plus ){ return yy; }
    if( ee->opr() == op_minus ){ return ( mod_base - yy ) % mod_base; }
    if( ee->opr() == op_sqrt ){ return mod_root[ yy ][ 2 ]; }
  }

  if( ee->left() && ee->right() ){
    unsigned xx = eval_mod( ee->left() );
    if( xx == mod_undef ){ return mod_undef; }
    unsigned yy = eval_mod( ee->right() );
    if( yy == mod_undef ){ return mod_undef; }
    if( ee->opr() == op_mixn ){ return ( xx + yy ) % mod_base; }
    if( ee->opr() == op_plus ){ return ( xx + yy ) % mod_base; }
    if( ee->opr() == op_minus ){ return ( xx + mod_base - yy ) % mod_base; }
    if( ee->opr() == op_vprod ){ return ( xx * yy ) % mod_base; }
    if( ee->opr() == op_iprod ){ return ( xx * yy ) % mod_base; }
    if( ee->opr() == op_div ){
      yy = mod_inv[ yy ];
      if( yy == mod_undef ){ return mod_undef; }
      return ( xx * yy ) % mod_base;
    }
  }

  mc_err_print( "Wrong modular arithmetic operator" );
  return mod_undef;
}


/* Truth value expression evaluation */
bool has_var( expression *, unsigned );
void range( expression *, unsigned, int &, bool &, int &, bool & );
bool mod_rel = false;   // comparisons are over arithmetic / modular arithm.
truth_val eval_tv( expression *ee ){

  /* Constant, variable and hard parentheses */
  if( ee->opr() == op_const ){ return ee->val().to_tv(); }
  if( ee->opr() == op_var ){
    if( !ee->left() ){ return ee->var().value.to_tv(); }
    mc_err_print( "Array variables not yet implemented" ); return tv_U;
  }
  if( ee->opr() == op_hpar ){ return eval_tv( ee->right() ); }

  /* Arithmetic comparisons */
  if( is_rel( ee->opr() ) ){

    /* Modular arithmetic */
    if( mod_rel ){
      truth_val t1 = tv_T; unsigned xx;
      if( is_rel( ee->left()->opr() ) ){
        t1 = eval_tv( ee->left() );
        if( t1 == tv_F ){ return tv_F; }
        xx = eval_mod( ee->left()->right() );
      }else{ xx = eval_mod( ee->left() ); }
      if( xx == mod_undef ){ return tv_U; }
      unsigned yy = eval_mod( ee->right() );
      if( yy == mod_undef ){ return tv_U; }
      bool bb =
        ee->opr() == op_eq ? xx == yy :
        ee->opr() == op_nq ? xx != yy :
        ee->opr() == op_lt ? xx < yy :
        ee->opr() == op_lq ? xx <= yy :
        ee->opr() == op_gt ? yy < xx :
        ee->opr() == op_gq ? yy <= xx :
        false;
      return bb ? t1 : tv_F;

    /* Real number arithmetic */
    }else{
      truth_val t1 = tv_T; number xx;
      if( is_rel( ee->left()->opr() ) ){
        t1 = eval_tv( ee->left() );
        if( t1 == tv_F ){ return tv_F; }
        xx = eval_expr( ee->left()->right() );
      }else{ xx = eval_expr( ee->left() ); }
      number yy = eval_expr( ee->right() );
      if( ee->opr() == op_eq ){ return t1 && num_eq( xx, yy ); }
      if( ee->opr() == op_nq ){ return t1 && !num_eq( xx, yy ); }
      if( ee->opr() == op_lt ){ return t1 && num_lt( xx, yy ); }
      if( ee->opr() == op_lq ){ return t1 && num_lq( xx, yy ); }
      if( ee->opr() == op_gt ){ return t1 && num_lt( yy, xx ); }
      if( ee->opr() == op_gq ){ return t1 && num_lq( yy, xx ); }
    }
  }

  /* Reasoning operators; these should not be evaluated as tv_type
  if( is_lrel( ee->opr() ) ){
    truth_val t_left = tv_T, t1 = tv_F;
    if( is_lrel( ee->left()->opr() ) ){
      t_left = eval_tv( ee->left() );
      if( t_left == tv_F ){ return tv_F; }
      t1 = U_to_F( eval_tv( ee->left()->right() ) );
    }else{ t1 = U_to_F( eval_tv( ee->left() ) ); }
    truth_val t2 = U_to_F( eval_tv( ee->right() ) );
    if( ee->opr() == op_leq ){
      return t_left && !( t1 && !t2 ) && !( t2 && !t1 );
    }
    if( ee->opr() == op_impl ){ return t_left && !( t1 && !t2 ); }
    if( ee->opr() == op_limp ){ return t_left && !( t2 && !t1 ); }
  } */

  /* Is integer */
  if( ee->opr() == op_is_int ){ return eval_expr( ee->right() ).is_int(); }

  /* Boolean operators */
  if( ee->opr() == op_not ){ return !eval_tv( ee->right() ); }
  if( ee->opr() == op_def ){ return tv_def( eval_tv( ee->right() ) ); }
  if( ee->opr() == op_and ){
    truth_val t1 = eval_tv( ee->left() );
    return t1 == tv_F ? tv_F : t1 && eval_tv( ee->right() );
  }
  if( ee->opr() == op_sand ){
    truth_val t1 = eval_tv( ee->left() );
    if( t1 == tv_F || t1 == tv_U ){ return t1; }
    return t1 && ( !t1 || eval_tv( ee->right() ) );
  }
  if( ee->opr() == op_or ){
    truth_val t1 = eval_tv( ee->left() );
    return t1 == tv_T ? tv_T : t1 || eval_tv( ee->right() );
  }
  if( ee->opr() == op_sor ){
    truth_val t1 = eval_tv( ee->left() );
    if( t1 == tv_T || t1 == tv_U ){ return t1; }
    return t1 || ( !t1 && eval_tv( ee->right() ) );
  }
  if( ee->opr() == op_rarr ){
    truth_val t1 = eval_tv( ee->left() );
    return t1 == tv_F ? tv_T : !t1 || eval_tv( ee->right() );
  }
  if( ee->opr() == op_harr ){
    truth_val t1 = eval_tv( ee->left() ), t2 = eval_tv( ee->right() );
    return ( t1 && t2 ) || !( t1 || t2 );
  }
  if( ee->opr() == op_Luka ){
    truth_val t1 = eval_tv( ee->left() );
    if( t1 == tv_F ){ return tv_T; }
    truth_val t2 = eval_tv( ee->right() );
    return !t1 || t2 || !( tv_def( t1 ) || tv_def( t2 ) );
  }
  if( ee->opr() == op_forall ){
    unsigned vv = ee->var_idx();
    int lo = 0, hi = 0;
    if( var_used[ vv ].type == vtp_mod ){ hi = mod_base - 1; }
    else if( var_used[ vv ].type == vtp_idx ){
      bool lv = true, hv = true;
      if( has_var( ee, vv ) ){ range( ee, vv, lo, lv, hi, hv ); }
      else{ lo = hi = 0; }
      if( ( !lv && lo > INT_MIN ) || ( !hv && hi < INT_MAX ) ){ return tv_F; }
      if(
        lo < INT_MIN / 2 || hi > INT_MAX / 2 ||
        hi - lo > arr_hi - arr_lo + 2*arr_extra
      ){
        test_fail( err_qua ); err_uns1 = var_used[ vv ].name;
        return tv_FUT;
      }
    }else{
      mc_err_print( "Quantified variable ", vv, " has a wrong type" );
      return tv_FUT;
    }
    truth_val t1 = tv_T;
    for( int ii = lo; ii <= hi; ++ii ){
      var_used[ vv ].value = ii;
      truth_val t2 = ee->left() ? !eval_tv( ee->left() ) : tv_F;
      if( t2 != tv_T ){
        t1 = t1 && ( t2 || eval_tv( ee->right() ) );
        if( t1 == tv_F ){ break; }
      }
    }
    return t1;
  }
  if( ee->opr() == op_exists ){
    unsigned vv = ee->var_idx();
    int lo = 0, hi = 0;
    if( var_used[ vv ].type == vtp_mod ){ hi = mod_base - 1; }
    else if( var_used[ vv ].type == vtp_idx ){
      bool lv = true, hv = true;
      if( has_var( ee, vv ) ){ range( ee, vv, lo, lv, hi, hv ); }
      else{ lo = hi = 0; }
      if( ( lv && lo > INT_MIN ) || ( hv && hi < INT_MAX ) ){ return tv_T; }
      if(
        lo < INT_MIN / 2 || hi > INT_MAX / 2 ||
        hi - lo > arr_hi - arr_lo + 2*arr_extra
      ){
        test_fail( err_qua ); err_uns1 = var_used[ vv ].name;
        return tv_FUT;
      }
    }else{
      mc_err_print( "Quantified variable ", vv, " has a wrong type" );
      return tv_FUT;
    }
    truth_val t1 = tv_F;
    for( int ii = lo; ii <= hi; ++ii ){
      var_used[ vv ].value = ii;
      truth_val t2 = ee->left() ? eval_tv( ee->left() ) : tv_T;
      if( t2 != tv_F ){
        t1 = t1 || ( t2 && eval_tv( ee->right() ) );
        if( t1 == tv_T ){ break; }
      }
    }
    return t1;
  }

  mc_err_print( "Wrong truth value operator" );
  return tv_U;
}


bool fail_eq( number x1, number x2 ){
  if( x1.type == number::trv || x2.type == number::trv ){ return false; }
  if( x1.type == number::und ){
    if( x2.type == number::und ){ return false; }
    if( x2.type == number::dbl ){ x2.to_dblu(); }
    return x2.type != number::dbu && gs_undef_check;
  }
  if( x2.type == number::und ){
    if( x1.type == number::dbl ){ x1.to_dblu(); }
    return x1.type != number::dbu && gs_undef_check;
  }
  if(
    x1.type != number::dbl && x2.type != number::dbl &&
    x1.type != number::dbu && x2.type != number::dbu
  ){
    if( x1.type != x2.type ){ return true; }
    if( x1.type == number::zer ){ return false; }
    return x1.r_nu != x2.r_nu || x1.r_de != x2.r_de;
  }
  x1.to_dblu(); x2.to_dblu();
  return
    ( x1.d_hi < x2.d_lo || x2.d_hi < x1.d_lo ) &&
    ( x1.type != number::dbu || x2.type != number::dbu );
}

bool fail_lt( number x1, number x2 ){
  if( x1.type == number::trv || x2.type == number::trv ){ return false; }
  if( x1.type == number::und ){
    if( x2.type == number::und ){ return false; }
    if( x2.type == number::dbl ){ x2.to_dblu(); }
    return x2.type != number::dbu && gs_undef_check;
  }
  if( x2.type == number::und ){
    if( x1.type == number::dbl ){ x1.to_dblu(); }
    return x1.type != number::dbu && gs_undef_check;
  }
  if(
    x1.type != number::dbl && x2.type != number::dbl &&
    x1.type != number::dbu && x2.type != number::dbu
  ){
    if( x1.type == number::zer ){ return x2.type != number::pos; }
    if( x1.type != x2.type ){ return x1.type == number::pos; }
    return x1.type == number::pos ?
      ration_lt( x1.r_nu, x1.r_de, x2.r_nu, x2.r_de ) == tv_F:
      ration_lt( x2.r_nu, x2.r_de, x1.r_nu, x1.r_de ) == tv_F;
  }
  x1.to_dblu(); x2.to_dblu();
  return
    x1.d_lo >= x2.d_hi &&
    ( x1.type != number::dbu || x2.type != number::dbu );
}

bool fail_lq( number x1, number x2 ){
  if( x1.type == number::trv || x2.type == number::trv ){ return false; }
  if( x1.type == number::und ){
    if( x2.type == number::und ){ return false; }
    if( x2.type == number::dbl ){ x2.to_dblu(); }
    return x2.type != number::dbu && gs_undef_check;
  }
  if( x2.type == number::und ){
    if( x1.type == number::dbl ){ x1.to_dblu(); }
    return x1.type != number::dbu && gs_undef_check;
  }
  if(
    x1.type != number::dbl && x2.type != number::dbl &&
    x1.type != number::dbu && x2.type != number::dbu
  ){
    if( x1.type == number::zer ){ return x2.type == number::neg; }
    if( x1.type != x2.type ){ return x1.type == number::pos; }
    return x1.type == number::pos ?
      ration_lt( x2.r_nu, x2.r_de, x1.r_nu, x1.r_de ) == tv_T:
      ration_lt( x1.r_nu, x1.r_de, x2.r_nu, x2.r_de ) == tv_T;
  }
  x1.to_dblu(); x2.to_dblu();
  return
    x1.d_lo > x2.d_hi &&
    ( x1.type != number::dbu || x2.type != number::dbu );
}


/* Try to make eval_expr( e1 ) certainly bigger or certainly bigger-or-equal
  than eval_expr( e2 ). Some variables are double for better speed. */
bool try_fail_leq(
  expression *e1, expression *e2, bool equal_suffices, unsigned &tries_left
){
  number v1 = eval_expr( e1 ), v2 = eval_expr( e2 );
  if( equal_suffices ? fail_lt( v1, v2 ) : fail_lq( v1, v2 ) ){ return true; }
  double
    d1 = to_double( v1 ), d2 = to_double( v2 ),
    best_res = is_defined( d1 ) && is_defined( d2 ) ? d1 - d2 : -1/0.,
    best_d1 = 0.;

  /* Create the variables for trying other expression variable values. */
  number test_step[ var_max ] = {};   // size of step of each variable
  unsigned test_mod[ var_max ] = {};  // for making test_step grow
  bool test_inf[ var_max ] = {};      // for recognizing unlimited growth
  for( unsigned ii = var_c_first; ii < var_f_cnt; ++ii ){
    double xx = to_double( var_used[ ii ].value );
    if(
      xx <= -1 || xx >= 1 ||
      var_used[ ii ].type == vtp_Z || var_used[ ii ].type == vtp_none ||
      var_used[ ii ].type == vtp_idx || var_used[ ii ].type == vtp_P
    ){ test_step[ ii ] = 1; }
    else{ test_step[ ii ] = number( 1, 10 ); }
  }

  /* Iterate towards better values at most tries_left times. */
  for( ; tries_left; --tries_left ){
    unsigned best_ii = 0, best_dir = 0;
    for( unsigned ii = var_c_first; ii < var_f_cnt; ++ii ){
      number old_val = var_used[ ii ].value;

      /* Try one step in one direction for variable i. */
      var_used[ ii ].value = old_val + test_step[ ii ];
      truth_val t1 = eval_tv( dom_expr );
      if( t1 == tv_U ){ test_fail( err_dom ); return true; }
      if( t1 == tv_T ){
        v1 = eval_expr( e1 ); v2 = eval_expr( e2 );
        if( equal_suffices ? fail_lt( v1, v2 ) : fail_lq( v1, v2 ) ){
          return true;
        }
        d1 = to_double( v1 ); d2 = to_double( v2 );
        if( d1 - d2 > nextafter( best_res, 1/0. ) ){
          best_res = d1 - d2; best_d1 = d1; best_ii = ii; best_dir = 1;
        }
      }

      /* Try one step in the opposite direction for variable i. */
      var_used[ ii ].value = old_val - test_step[ ii ];
      t1 = eval_tv( dom_expr );
      if( t1 == tv_U ){ test_fail( err_dom ); return true; }
      if( t1 == tv_T ){
        v1 = eval_expr( e1 ); v2 = eval_expr( e2 );
        if( equal_suffices ? fail_lt( v1, v2 ) : fail_lq( v1, v2 ) ){
          return true;
        }
        d1 = to_double( v1 ); d2 = to_double( v2 );
        if( d1 - d2 > nextafter( best_res, 1/0. ) ){
          best_res = d1 - d2; best_d1 = d1; best_ii = ii; best_dir = 2;
        }
      }

      /* Restore the variable value and go to the next variable. */
      var_used[ ii ].value = old_val;
    }

    /* If some direction yields improvement, go to a best direction,
      possibly making the step bigger. */
    if( best_dir ){
      if( best_dir > 1 ){
        var_used[ best_ii ].value -= test_step[ best_ii ];
        test_mod[ best_ii ] += 9;
      }else{
        var_used[ best_ii ].value += test_step[ best_ii ];
        ++test_mod[ best_ii ];
      }
      test_mod[ best_ii ] %= 10;
      if( !test_mod[ best_ii ] ){
        test_step[ best_ii ] *= 10; test_inf[ best_ii ] = true;
        test_mod[ best_ii ] = best_dir > 1 ? 9 : 1;
      }

    /* Otherwise try to make the steps smaller and then try a precise rational
      value. Terminate, if that fails. */
    }else{
      best_dir = 1;   // used to record if any step was changed
      for( unsigned ii = var_c_first; ii < var_f_cnt; ++ii ){

        /* Try to make the step smaller. */
        number xx = var_used[ ii ].value;
        double
          yy = to_double( xx ) * ( r_eps/2 ),
          ns = to_double( test_step[ ii ] ) / 10;
        if(
          ns > r_eps/2 && ns > yy && ns > -yy &&
          ( var_used[ ii ].type == vtp_R || ns >= 1 )
        ){
          test_step[ ii ] /= number( 10 ); test_inf[ ii ] = false;
          best_dir = 0;
        }

        /* Try to convert the value of the variable to precise rational. */
        else if(
          ns != 0. && ( xx.is_pos() || xx.is_neg() ) && xx.r_de >= 100
        ){
          unsigned n1 = xx.r_nu, n2 = xx.r_de % n1;
          while( true ){
            if( n2 < 100 ){ break; }
            n1 %= n2;
            if( n1 < 100 ){ n1 = n2; break; }
            n2 %= n1;
          }
          var_used[ ii ].value.r_nu /= n1;
          var_used[ ii ].value.r_de /= n1;
          test_step[ ii ] = 0; best_dir = 0;
        }

      }
      if( best_dir ){ break; }
    }

  }

  /* Iterations led to a dead end or did not suffice, so try to give a
    plausible error indication and then give up. */
  if( !equal_suffices ){ return false; }
  for( unsigned ii = var_c_first; ii < var_f_cnt; ++ii ){
    if( test_inf[ ii ] ){ return false; }
  }
  if(
      best_res > -r_eps/2 ||
      best_res > best_d1 * r_eps/2 ||
      best_res > -best_d1 * r_eps/2
  ){ test_fail( err_plausible ); }

  return false;
}


/* By trying many value combinations of variables, check that the relation
  holds. */
void check_relation( expression *e1, expression *e2, op_type rel ){

  /* Initialize the variables. */
  first_test_combination( rel != op_eq );

  /* Try each sequence of values or until a certain error is found. */
  unsigned tries_left = 1000, good_tests = 0;
  number v1, v2;
  do{
    tries_left += 100;

    /* Report error if dom_expr is undefined. */
    truth_val t1 = eval_tv( dom_expr );
    if( t1 == tv_U ){ test_fail( err_dom ); return; }

    /* If the value combination is legal and finds an error, report it. Here
      also U is legal, because it justifies err_dom. */
    if( t1 == tv_T || t1 == tv_UT ){
      if( rel == op_eq ){
        v1 = eval_expr( e1 ); v2 = eval_expr( e2 );
        if( fail_eq( v1, v2 ) ){ test_fail( v1, v2, e1, e2 ); return; }
      }else if(
        ( rel == op_lq && try_fail_leq( e1, e2, false, tries_left ) ) ||
        ( rel == op_lt && try_fail_leq( e1, e2, true, tries_left ) ) ||
        ( rel == op_gq && try_fail_leq( e2, e1, false, tries_left ) ) ||
        ( rel == op_gt && try_fail_leq( e2, e1, true, tries_left ) )
      ){
        test_fail( eval_expr( e1 ), eval_expr( e2 ), e1, e2 ); return;
      }else{
        v1 = eval_expr( e1 ); v2 = eval_expr( e2 );
      }
      if( err_mode ){ return; }
      good_tests += v1.is_informative() && v2.is_informative();
    }

  }while( next_test_combination( rel != op_eq, rel != op_eq ) );
  if(
    ( good_tests < 7 && var_f_cnt ) ||
    ( !good_tests && !var_f_cnt && !( v1.is_und() && v2.is_und() ) )
  ){ err_mode = err_few_tests; }

}


/* By trying all value combinations of variables or until an error is found,
  check that the logical claim holds. */
void check_logic( expression *e1, expression *e2, op_type rel_op ){
  first_test_combination();
  do{
    truth_val d1 = eval_tv( dom_expr );
    truth_val v1 = eval_tv( e1 ) && d1;
    truth_val v2 = eval_tv( e2 ) && d1;
    if(
      ( rel_op == op_leq && !tv_r_eq( v1, v2 ) ) ||
      ( rel_op == op_iden && !tv_r_iden( v1, v2 ) ) ||
      ( rel_op == op_impl && !tv_r_impl( v1, v2 ) ) ||
      ( rel_op == op_limp && !tv_r_lpmi( v1, v2 ) )
    ){ test_fail( v1, v2 ); return; }
  }while( next_test_combination() );
  if( !err_mode ){ err_mode = err_proven; }
}

#include "real_logic.cc"


/*** Reasoning about expressions, common part 1 ***/


expression *simplify( expression * );


/* Miscellaneous */
inline bool known_const( expression *ee ){ return !( ee->type() & 0x20 ); }
inline bool known_int( expression *ee ){ return !( ee->type() & 0x18 ); }


/* Ordering of expressions */
bool elt( expression *e1, expression *e2 ){

  /* Null expression last */
  if( !e1 ){ return false; }
  if( !e2 ){ return true; }

  /* Compare products primarily on left, then right. Use implicit * 1. */
  if( e1->opr() == op_vprod ){
    if( e2->opr() == op_vprod ){
      if( elt( e1->left(), e2->left() ) ){ return true; }
      if( elt( e2->left(), e1->left() ) ){ return false; }
      return elt( e1->right(), e2->right() );
    }
    if( elt( e1->left(), e2 ) ){ return true; }
    if( elt( e2, e1->left() ) ){ return false; }
    return elt( e1->right(), expr_1 );
  }
  if( e2->opr() == op_vprod ){
    if( elt( e1, e2->left() ) ){ return true; }
    if( elt( e2->left(), e1 ) ){ return false; }
    return elt( expr_1, e2->right() );
  }

  /* Compare powers primarily on left, then right. Use implicit ^ 1. */
  if( e1->opr() == op_pow ){
    if( e2->opr() == op_pow ){
      if( elt( e1->left(), e2->left() ) ){ return true; }
      if( elt( e2->left(), e1->left() ) ){ return false; }
      return elt( e1->right(), e2->right() );
    }
    if( elt( e1->left(), e2 ) ){ return true; }
    if( elt( e2, e1->left() ) ){ return false; }
    return elt( e1->right(), expr_1 );
  }
  if( e2->opr() == op_pow ){
    if( elt( e1, e2->left() ) ){ return true; }
    if( elt( e2->left(), e1 ) ){ return false; }
    return elt( expr_1, e2->right() );
  }

  /* Compare comparisons primarily on left, then operator, then right. */
  if( is_rel( e1->opr() ) ){
    if( is_rel( e2->opr() ) ){
      if( elt( e1->left(), e2->left() ) ){ return true; }
      if( elt( e2->left(), e1->left() ) ){ return false; }
      if( e1->opr() < e2->opr() ){ return true; }
      if( e2->opr() < e1->opr() ){ return false; }
      return elt( e1->right(), e2->right() );
    }
    return false;
  }
  if( is_rel( e2->opr() ) ){ return true; }

  /* Compare logical relations primarily on left, then operator, then right.
    */
  if( is_lrel( e1->opr() ) ){
    if( is_lrel( e2->opr() ) ){
      if( elt( e1->left(), e2->left() ) ){ return true; }
      if( elt( e2->left(), e1->left() ) ){ return false; }
      if( e1->opr() < e2->opr() ){ return true; }
      if( e2->opr() < e1->opr() ){ return false; }
      return elt( e1->right(), e2->right() );
    }
    return false;
  }
  if( is_lrel( e2->opr() ) ){ return true; }

  /* Then compare number value */
  if( e1->opr() == op_const || e1->opr() == op_e2718 || e1->opr() == op_pi ){
    if(
      e2->opr() != op_const && e2->opr() != op_e2718 && e2->opr() != op_pi
    ){ return false; }
    number x1 = e1->val(), x2 = e2->val();
    if( x1.is_und() ){ return false; }
    if( x2.is_und() ){ return true; }
    if( x1.is_trv() ){ return false; }
    if( x2.is_trv() ){ return true; }
    if( x1.is_dbu() && !x2.is_dbu() ){ return false; }
    if( x2.is_dbu() && !x1.is_dbu() ){ return true; }
    if( x1.is_dbl() || x2.is_dbl() || x1.is_dbu() || x2.is_dbu() ){
      return to_double( x1 ) < to_double( x2 );
    } //??? is this appropriate treatment of imprecise values?
    if( x1.is_zer() ){ return false; }
    if( x2.is_zer() ){ return true; }
    if( x1.r_de > x2.r_de ){ return false; }
    if( x2.r_de > x1.r_de ){ return true; }
    if( x1.r_nu > x2.r_nu ){ return false; }
    if( x2.r_nu > x1.r_nu ){ return true; }
    return x1.type < x2.type;
  }
  if( e2->opr() == op_const || e2->opr() == op_e2718 || e2->opr() == op_pi ){
    return true;
  }

  /* Then unary expressions primarily on argument, secondarily on operator */
  if( !e1->left() && e1->right() ){
    if( elt( e1->right(), e2->right() ) ){ return true; }
    if( elt( e2->right(), e1->right() ) ){ return false; }
    return e1->opr() < e2->opr();
  }

  /* Then variable */
  if( e1->opr() == op_var ){
    if( e2->opr() != op_var ){ return false; }
    if( e1->var().name > e2->var().name ){ return false; }
    if( e2->var().name > e1->var().name ){ return true; }
    return e1->h_val_ < e2->h_val_;   // fixes the order of array expressions
  }
  if( e2->opr() == op_var ){ return true; }

  /* Fallback: just compare the hash values. */
  return e1->h_val_ < e2->h_val_;

}


/*** Reasoning about arithmetic expressions ***/


/* Arithm. reductions that need no new nodes, binary, 0 indicates failure. */
expression *arithm_easy( expression *e1, op_type opr, expression *e2 ){
  if( opr == op_plus ){
    if( e1 == expr_0 ){ return e2; }
    if( e2 == expr_0 ){ return e1; }
  }
  else if( opr == op_minus ){
    if( e2 == expr_0 ){ return e1; }
    if( e1 == e2 ){ return expr_0; }
  }
  else if( opr == op_vprod || opr == op_iprod ){
    if( e1 == expr_0 || e2 == expr_0 ){ return expr_0; }
    if( e1 == expr_1 ){ return e2; }
    if( e2 == expr_1 ){ return e1; }
  }
  else if( opr == op_div ){
    if( e2 == expr_0 ){ return expr_numu; }
    if( e1 == expr_0 ){ return expr_0; }
    if( e2 == expr_1 ){ return e1; }
  }
  else if( opr == op_pow ){
    if( e1 == expr_1 || e2 == expr_0 ){ return expr_1; }
    if( e2 == expr_1 ){ return e1; }
    if( !( e2->right() && e2->right()->type() & 0x5 ) ){
      if( e1 == expr_e && e2->opr() == op_ln ){ return e2->right(); }
      if( e1 == expr_10 && e2->opr() == op_log ){ return e2->right(); }
      if( e1 == expr_2 && e2->opr() == op_log2 ){ return e2->right(); }
    }
  }
  else if( opr == op_idiv ){
    if( e2 == expr_0 ){ return expr_numu; }
    if( e1 == expr_0 ){ return expr_0; }
    if( e2 == expr_1 && !( e1->type() & 0x18 ) ){ return e1; }
  }
  else if( opr == op_imod ){
    if( e2 == expr_0 ){ return expr_numu; }
    if( e1 == expr_0 ){ return expr_0; }
    if( e2 == expr_1 && !( e1->type() & 0x18 ) ){ return expr_0; }
  }
  return 0;
}

/* Arithm. reductions that need no new nodes, unary, 0 indicates failure. */
expression *arithm_easy( op_type opr, expression *e2 ){
  if( opr == op_hpar || opr == op_plus ){ return e2; }
  else if( opr == op_minus ){
    if( e2 == expr_0 ){ return expr_0; }
    if( e2 == expr_1 ){ return expr_m1; }
    if( e2 == expr_m1 ){ return expr_1; }
    if( e2->opr() == op_minus && !e2->left() ){ return e2->right(); }
  }
  else if( opr == op_abs ){
    if( !( e2->type() & 0x4 ) ){ return e2; }
    if( e2 == expr_0 ){ return expr_0; }
    if( e2 == expr_1 || e2 == expr_m1 ){ return expr_1; }
  }
  else if(
    opr == op_lfloor || opr == op_lceil || opr == op_ddn || opr == op_dup
  ){ if( !( e2->type() & 0x18 ) ){ return e2; } }
  else if( e2 == expr_0 ){
    if( opr == op_ln || opr == op_log || opr == op_log2 || opr == op_cot ){
      return expr_numu;
    }
    if(
      opr == op_sqrt || opr == op_sin || opr == op_tan || opr == op_sinh ||
      opr == op_tanh
    ){ return expr_0; }
    if( opr == op_cos || opr == op_cosh || opr == op_fact ){ return expr_1; }
  }
  else if( e2 == expr_1 ){
    if( opr == op_sqrt || opr == op_root || opr == op_fact ){
      return expr_1;
    }
    if( opr == op_ln || opr == op_log || opr == op_log2 ){ return expr_0; }
  }
  else if( opr == op_ln ){
    if( e2 == expr_e ){ return expr_1; }
    if( e2->opr() == op_pow && e2->left() == expr_e ){ return e2->right(); }
  }
  else if( opr == op_log ){
    if( e2 == expr_10 ){ return expr_1; }
    if( e2->opr() == op_pow && e2->left() == expr_10 ){ return e2->right(); }
  }
  else if( opr == op_log2 ){
    if( e2 == expr_2 ){ return expr_1; }
    if( e2->opr() == op_pow && e2->left() == expr_2 ){ return e2->right(); }
  }
  return 0;
}


/* These build new simplified arithmetic expressions. */

expression *new_arit( expression *e1, op_type opr, expression *e2 ){
  expression *ee = arithm_easy( e1, opr, e2 );
  if( !ee ){ ee = new_expr( e1, opr, e2 ); }
  return simplify( ee );
}

expression *new_arit( op_type opr, expression *e2 ){
  expression *ee = arithm_easy( opr, e2 );
  if( !ee ){ ee = new_expr( opr, e2 ); }
  return simplify( ee );
}

inline expression *new_arit( op_type opr, number val ){
  return simplify( new_expr( opr, val ) );
}

inline expression *new_arit( number val ){
  return simplify( new_expr( val ) );
}


/* Tries to add identical terms with optional coefficient. */
expression *combine_add( expression *e1, expression *e2 ){
  if( known_const( e1 ) && known_const( e2 ) ){
    return new_arit( e1, op_plus, e2 );
  }
  expression *f1 = e1, *f2 = e2, *g1 = expr_1, *g2 = expr_1;
  if( e1->opr() == op_vprod && known_const( e1->right() ) ){
    f1 = e1->left(); g1 = e1->right();
  }
  if( e2->opr() == op_vprod && known_const( e2->right() ) ){
    f2 = e2->left(); g2 = e2->right();
  }
  if( f1 == f2 ){
    return new_arit( f1, op_vprod, new_arit( g1, op_plus, g2 ) );
  }
  return 0;
}

/* Tries to multiply identical terms with optional exponent. */
expression *combine_mul( expression *e1, expression *e2 ){
  if( known_const( e1 ) && known_const( e2 ) ){
    return new_arit( e1, op_vprod, e2 );
  }
  expression *f1 = e1, *f2 = e2, *g1 = expr_1, *g2 = expr_1;
  if( e1->opr() == op_pow && known_const( e1->right() ) ){
    f1 = e1->left(); g1 = e1->right();
  }
  if( e2->opr() == op_pow && known_const( e2->right() ) ){
    f2 = e2->left(); g2 = e2->right();
  }
  if( f1 == f2 ){
    return new_arit( f1, op_pow, new_arit( g1, op_plus, g2 ) );
  }
  return 0;
}

/* A common interface to the two above. */
inline expression *combine_add_mul(
  expression *e1, op_type opr, expression *e2
){ return opr == op_plus ? combine_add( e1, e2 ) : combine_mul( e1, e2 ); }


/* Simplification of 0-ary, unary and binary arithmetic expressions. */
expression *arithm_simplify( expression *ee ){
  op_type opr = ee->opr(); expression *e1 = simplify( ee->left() );

  /* Derivative */
  if( opr == op_deriv ){ return e1; }

  expression *e2 = simplify( ee->right() );

  /* The easiest simplifications */
  expression *e0 =
    e1 ? arithm_easy( e1, opr, e2 ) : e2 ? arithm_easy( opr, e2 ) : 0;
  if( e0 ){ return e0; }

  /* Fully number */
  if( e2 && known_const( e2 ) && ( !e1 || known_const( e1 ) ) ){
    number xx = eval_expr( new_expr( e1, opr, e2 ) );
    if( !xx.is_dbl() && !xx.is_dbu() ){ return new_arit( xx ); }
  }

  /* Minus */
  if( opr == op_minus ){
    if( e1 ){ return new_arit( e1, op_plus, new_arit( op_minus, e2 ) ); }
    if( e2->opr() == op_plus ){
      return new_arit(
        new_arit( op_minus, e2->left() ), op_plus,
        new_arit( op_minus, e2->right() )
      );
    }
    if( e2->opr() == op_vprod ){
      return
        new_arit( e2->left(), e2->opr(), new_arit( op_minus, e2->right() ) );
    }
    return new_arit( e2, op_vprod, expr_m1 );
  }

  /* Division and invisible multiplication */
  if( opr == op_div ){
    return new_arit( e1, op_vprod, new_arit( e2, op_pow, expr_m1 ) );
  }
  if( opr == op_iprod ){ return new_arit( e1, op_vprod, e2 ); }

  /* Binary plus and multiplication */
  if( opr == op_plus || opr == op_vprod ){

    /* Apply associativity. */
    if( e2->opr() == opr ){
      return new_arit( new_arit( e1, opr, e2->left() ), opr, e2->right() );
    }

    /* Sort and combine the arguments. */
    if( e1->opr() == opr ){
      expression *e3 = combine_add_mul( e1->right(), opr, e2 );
      if( e3 ){ return new_arit( e1->left(), opr, e3 ); }
      return elt( e2, e1->right() ) ?
        new_arit( new_arit( e1->left(), opr, e2 ), opr, e1->right() ) :
        new_expr( e1, opr, e2 );
    }
    expression *e3 = combine_add_mul( e1, opr, e2 );
    if( e3 ){ return e3; }
    return elt( e2, e1 ) ? new_arit( e2, opr, e1 ) : new_expr( e1, opr, e2 );

  }

  /* Exponentiation */
  if( opr == op_pow ){
    if( known_const( e2 ) && eval_expr( e2 ).nu_even() ){
      if( e1->opr() == op_abs ){
        return new_arit( e1->right(), op_pow, e2 );
      }
      if(
        e1->opr() == op_vprod && known_const( e1->right() ) &&
        eval_expr( e1->right() ).isc_neg()
      ){ return new_arit( new_arit( e1, op_vprod, expr_m1 ), op_pow, e2 ); }
    }
    else if( e1->opr() == op_pow ){
      expression *e3 =
        known_const( e1->right() ) && eval_expr( e1->right() ).nu_even() ?
          new_arit( op_abs, e1->left() ) : e1->left();
      return new_arit( e3, op_pow, new_arit( e1->right(), op_vprod, e2 ) );
    }
  }

  /* Integer division and modulo */
  if( opr == op_idiv ){
    return new_arit( op_lfloor, new_arit( e1, op_div, e2 ) );
  }
  if( opr == op_imod ){
    return new_arit(
      e1, op_minus,
      new_arit( e2, op_vprod, new_arit( e1, op_idiv, e2 ) )
    );
  }

  /* Absolute value ??? */
  if( opr == op_abs ){
    if( e2->opr() == op_vprod ){
      return new_arit(
        new_arit( op_abs, e2->left() ), op_vprod,
        new_arit( op_abs, e2->right() )
      );
    }
    if( e2->opr() == op_pow ){
      return
        new_arit( new_arit( op_abs, e2->left() ), op_pow, e2->right() );
    }
  }

  /* Abbreviated power of sin-like function */
  if( is_sin_like( opr ) && e1 ){
    return new_arit( new_arit( opr, e2 ), op_pow, e1 );
  }

  /* Square root */
  if( opr == op_sqrt ){
    return new_arit( e2, op_pow, new_arit( number( 1, 2 ) ) );
  }

  /* General root */
  if( opr == op_root ){
    return new_arit( e2, op_pow, new_arit( e1, op_pow, expr_m1 ) );
  }

  /* Non-natural logarithms */
  if( opr == op_log ){
    return
      new_arit( new_arit( op_ln, e2 ), op_div, new_arit( op_ln, expr_10 ) );
  }
  if( opr == op_log2 ){
    return
      new_arit( new_arit( op_ln, e2 ), op_div, new_arit( op_ln, expr_2 ) );
  }

  /* Trigonometric functions */
  if( opr == op_tan ){
    return new_arit(
      new_arit( op_sin, e2 ), op_vprod,
      new_arit( new_arit( op_cos, e2 ), op_pow, expr_m1 )
    );
  }
  if( opr == op_cot ){
    return new_arit(
      new_arit( op_cos, e2 ), op_vprod,
      new_arit( new_arit( op_sin, e2 ), op_pow, expr_m1 )
    );
  }
  if( opr == op_sin && e2->opr() == op_abs ){
    return new_arit( op_abs, new_arit( op_sin, e2->right() ) );
  }
  if( opr == op_cos && e2->opr() == op_abs ){
    return new_arit( op_cos, e2->right() );
  }
  if( opr == op_sin && e2->opr() == op_vprod && e2->right() == expr_2 ){
    expression *e3 = e2->left();
    return new_arit(
      new_arit( new_arit( op_sin, e3 ), op_vprod, new_arit( op_cos, e3 ) ),
      op_vprod, expr_2
    );
  }
  if( opr == op_cos && e2->opr() == op_vprod && e2->right() == expr_2 ){
    expression *e3 = e2->left();
    return new_arit(
      new_arit(
        new_arit( new_arit( op_sin, e3 ), op_pow, expr_2 ), op_vprod, expr_m2
      ), op_plus, expr_1
    );
  }
  if( opr == op_pow && e1->opr() == op_cos && e2 == expr_2 ){
    expression *e3 = e1->right();
    return new_arit(
      new_arit(
        new_arit( new_arit( op_sin, e3 ), op_pow, expr_2 ), op_vprod, expr_m1
      ), op_plus, expr_1
    );
  }
  if( opr == op_pow && e1->opr() == op_cos && e2 == expr_m2 ){
    return new_arit( new_arit( e1, op_pow, expr_2 ), op_pow, expr_m1 );
  }

  /* Hyperbolic functions */
  if( opr == op_sinh ){
    return new_arit(
      new_arit(
        new_arit( expr_e, op_pow, e2 ),
        op_minus,
        new_arit( expr_e, op_pow, new_arit( op_minus, e2 ) )
      ), op_div, expr_2
    );
  }
  if( opr == op_cosh ){
    return new_arit(
      new_arit(
        new_arit( expr_e, op_pow, e2 ),
        op_plus,
        new_arit( expr_e, op_pow, new_arit( op_minus, e2 ) )
      ), op_div, expr_2
    );
  }
  if( opr == op_tanh ){
    return new_arit(
      new_arit(
        new_arit( expr_e, op_pow, e2 ),
        op_minus,
        new_arit( expr_e, op_pow, new_arit( op_minus, e2 ) )
      ), op_div,
      new_arit(
        new_arit( expr_e, op_pow, e2 ),
        op_plus,
        new_arit( expr_e, op_pow, new_arit( op_minus, e2 ) )
      )
    );
  }

/*????*/

//  if(//??? arithm_easy hoitaa
//    opr == op_ln && e2->opr() == op_pow && e2->left()->opr() == op_e2718
//  ){ return e2->right(); }

  return new_expr( e1, opr, e2, ee->val() );
}


/* Derivation */
expression *derivative( expression *ee, unsigned vv ){

  /* Variables and constant functions */
  if( known_const( ee ) ){ return expr_0; }
  op_type opr = ee->opr();
  if( opr == op_var ){ return ee->var_idx() == vv ? expr_1 : expr_0; }
//??? Taulukkomuuttujaa ei voi derivoida

  /* Remaining unary operators */
  expression *e1 = ee->left(), *e2 = ee->right();
  if( !e2 ){ return expr_0; }
  if( !e1 ){
    if( opr == op_plus ){ return derivative( e2, vv ); }
    if( opr == op_minus ){
      return new_arit( op_minus, derivative( e2, vv ) );
    }
    if( opr == op_abs ){
      return
        new_arit( new_arit( ee, op_div, e2 ), op_vprod,
        derivative( e2, vv ) );
    }
    if( opr == op_sqrt ){
      return new_arit(
        derivative( e2, vv ),
        op_div,
        new_arit( ee, op_vprod, expr_2 )
      );
    }
    if( opr == op_ln ){
      return
        new_arit(
          new_arit( e2, op_pow, expr_m1 ), op_vprod, derivative( e2, vv )
        );
    }
    if( opr == op_log ){
      return
        new_arit(
          new_arit(
            new_arit( e2, op_pow, expr_m1 ), op_vprod, derivative( e2, vv )
          ),
          op_div,
          new_arit( op_ln, expr_10 )
        );
    }
    if( opr == op_log2 ){
      return
        new_arit(
          new_arit(
            new_arit( e2, op_pow, expr_m1 ), op_vprod, derivative( e2, vv )
          ),
          op_div,
          new_arit( op_ln, expr_2 )
        );
    }
    if( opr == op_sin ){
      return
        new_arit( new_arit( op_cos, e2 ), op_vprod, derivative( e2, vv ) );
    }
    if( opr == op_cos ){
      return new_arit(
        new_arit( new_arit( op_sin, e2 ), op_vprod, derivative( e2, vv ) ),
        op_vprod,
        expr_m1
      );
    }
    if( opr == op_tan ){
      return new_arit(
        derivative( e2, vv ),
        op_vprod,
        new_arit( new_arit( op_cos, e2 ), op_pow, expr_m2 )
      );
    }
    if( opr == op_cot ){
      return new_arit(
        new_arit(
          derivative( e2, vv ),
          op_vprod,
          new_arit( new_arit( op_sin, e2 ), op_pow, expr_m2 )
        ),
        op_vprod,
        expr_m1
      );
    }
    if( opr == op_sinh ){
      return
        new_arit( new_arit( op_cosh, e2 ), op_vprod, derivative( e2, vv ) );
    }
    if( opr == op_cosh ){
      return
        new_arit( new_arit( op_sinh, e2 ), op_vprod, derivative( e2, vv ) );
    }
    if( opr == op_tanh ){
      return new_arit(
        derivative( e2, vv ),
        op_vprod,
        new_arit( new_arit( op_cosh, e2 ), op_pow, expr_m2 )
      );
    }
    return expr_0;
  }

  /* Remaining binary operators */
  if( opr == op_plus ){
    return new_arit( derivative( e1, vv ), op_plus, derivative( e2, vv ) );
  }
  if( opr == op_minus ){
    return new_arit( derivative( e1, vv ), op_minus, derivative( e2, vv ) );
  }
  if( opr == op_vprod || opr == op_iprod ){
    return new_arit(
      new_arit( derivative( e1, vv ), op_vprod, e2 ),
      op_plus,
      new_arit( e1, op_vprod, derivative( e2, vv ) )
    );
  }
  if( opr == op_div ){
    return new_arit(
      new_arit(
        new_arit( derivative( e1, vv ), op_vprod, e2 ),
        op_minus,
        new_arit( e1, op_vprod, derivative( e2, vv ) )
      ),
      op_div,
      new_arit( e2, op_pow, expr_2 )
    );
  }
  if( opr == op_pow ){
    return new_arit(
      ee, op_vprod, new_arit(
        new_arit( derivative( e2, vv ), op_vprod, new_arit( op_ln, e1 ) ),
        op_plus,
        new_arit( new_arit( e2, op_div, e1 ), op_vprod, derivative( e1, vv ) )
      )
    );
  }
  if( opr == op_root ){//??? presedenssi oikealle vs. pow; undefined nollassa
    return new_arit(
      new_arit( derivative( e2, vv ), op_vprod, ee ),
      op_div,
      new_arit( e1, op_vprod, e2 )
    );
  }
  if( is_sin_like( opr ) ){
    return derivative( new_arit( new_arit( opr, e2 ), op_pow, e1 ), vv );
  }
  if( opr == op_deriv ){ return derivative( e1, vv ); }

  return expr_0;
}


/*** Reasoning about logic expressions ***/


/* Logic reductions that need no new nodes, binary, 0 indicates failure. */
expression *logic_easy( expression *e1, op_type opr, expression *e2 ){
  if( opr == op_and ){
    if( e1 == expr_F || e2 == expr_F ){ return expr_F; }
    if( e1 == expr_T ){ return e2; }
    if( e2 == expr_T ){ return e1; }
    if( e1 == e2 ){ return e1; }
  }
  else if( opr == op_sand ){
    if( e1 == expr_F || e1 == expr_U ){ return e1; }
    if( e1 == expr_T ){ return e2; }
    if( e2 == expr_T ){ return e1; }
    if( e1 == e2 ){ return e1; }
  }
  else if( opr == op_or ){
    if( e1 == expr_F ){ return e2; }
    if( e2 == expr_F ){ return e1; }
    if( e1 == expr_T || e2 == expr_T ){ return expr_T; }
    if( e1 == e2 ){ return e1; }
  }
  else if( opr == op_sor ){
    if( e1 == expr_T || e1 == expr_U ){ return e1; }
    if( e1 == expr_F ){ return e2; }
    if( e2 == expr_F ){ return e1; }
    if( e1 == e2 ){ return e1; }
  }
  else if( e1 == e2 ){
    //???????? Olen tassa. Kommentoimaton nayttaa vaaralta, pois kommentoidut
    // oikeilta. e1 == e1 voi olla maarittelematon.
    if( opr == op_eq || opr == op_lq || opr == op_gq ){ return expr_T; }
    // || opr == op_leq || opr == op_limp || opr == op_impl
    //??? if( opr == op_nq || opr == op_lt || opr == op_gt ){ return expr_F; }
  }
  else if( e2 == expr_0 && !is_rel( e1->opr() ) ){
    if( opr == op_eq ){
      if( !( e1->type() & 0x1 ) ){ return expr_F; }
      if( !( e1->type() & 0x6 ) ){ return expr_T; }
    }
    else if( opr == op_nq ){
      if( !( e1->type() & 0x1 ) ){ return expr_T; }
      if( !( e1->type() & 0x6 ) ){ return expr_F; }
    }
    else if( opr == op_lt ){
      if( !( e1->type() & 0x3 ) ){ return expr_T; }
      if( !( e1->type() & 0x4 ) ){ return expr_F; }
    }
    else if( opr == op_gt ){
      if( !( e1->type() & 0x2 ) ){ return expr_F; }
      if( !( e1->type() & 0x5 ) ){ return expr_T; }
    }
    else if( opr == op_lq ){
      if( !( e1->type() & 0x2 ) ){ return expr_T; }
      if( !( e1->type() & 0x5 ) ){ return expr_F; }
    }
    else if( opr == op_gq ){
      if( !( e1->type() & 0x3 ) ){ return expr_F; }
      if( !( e1->type() & 0x4 ) ){ return expr_T; }
    }
  }

  return 0;
}

/* Logic reductions that need no new nodes, unary, 0 indicates failure. */
expression *logic_easy( op_type opr, expression *e2 ){
  if( opr == op_not ){
    if( e2 == expr_T ){ return expr_F; }
    if( e2 == expr_U || e2 == expr_FT || e2 == expr_FUT ){ return e2; }
    if( e2 == expr_F ){ return expr_T; }
    if( e2 == expr_FU ){ return expr_UT; }
    if( e2 == expr_UT ){ return expr_FU; }
    if( e2->opr() == op_not ){ return e2->right(); }
  }
  if( opr == op_def ){
    if( e2->opr() == op_def ){ return expr_T; }
  }
  if( opr == op_is_int ){
    if( known_int( e2 ) ){ return expr_T; }
    if( known_const( e2 ) ){
      switch( eval_expr( e2 ).is_int() ){
      case tv_F: return expr_F;
      case tv_U: return expr_U;
      case tv_T: return expr_T;
      case tv_FU: return expr_FU;
      case tv_FT: return expr_FT;
      case tv_UT: return expr_UT;
      case tv_FUT: return expr_FUT;
      }
    }
    if( e2->opr() == op_e2718 || e2->opr() == op_pi ){ return expr_F; }
  }
  return 0;
}


/* Builds new simplified logic expressions. */
expression *new_logic( expression *e1, op_type opr, expression *e2 ){
  expression *ee = logic_easy( e1, opr, e2 );
  if( !ee ){ ee = new_expr( e1, opr, e2 ); }
  return simplify( ee );
}

expression *new_logic( op_type opr, expression *e2 ){
  expression *ee = logic_easy( opr, e2 );
  if( !ee ){ ee = new_expr( 0, opr, e2 ); }
  return simplify( ee );
}

inline expression *l_and( expression *e1, expression *e2 ){
  return new_logic( e1, op_and, e2 );
}

inline expression *l_sand( expression *e1, expression *e2 ){
  return new_logic( e1, op_sand, e2 );
}

inline expression *l_or( expression *e1, expression *e2 ){
  return new_logic( e1, op_or, e2 );
}

inline expression *l_sor( expression *e1, expression *e2 ){
  return new_logic( e1, op_sor, e2 );
}

inline expression *l_not( expression *e2 ){ return new_logic( op_not, e2 ); }

inline expression *l_def( expression *e2 ){ return new_logic( op_def, e2 ); }

inline expression *l_eq_0( expression *e1 ){
  return new_logic( e1, op_eq, expr_0 );
}

inline expression *l_nq_0( expression *e1 ){
  return new_logic( e1, op_nq, expr_0 );
}

inline expression *l_lt_0( expression *e1 ){
  return new_logic( e1, op_lt, expr_0 );
}

inline expression *l_lq_0( expression *e1 ){
  return new_logic( e1, op_lq, expr_0 );
}

inline expression *l_gt_0( expression *e1 ){
  return new_logic( e1, op_gt, expr_0 );
}

inline expression *l_gq_0( expression *e1 ){
  return new_logic( e1, op_gq, expr_0 );
}


/* Making bit patterns 001 to 110 represent comparison operators */
op_type cmp_op[] = { op_err, op_eq, op_gt, op_gq, op_lt, op_lq, op_nq };
unsigned op_idx( op_type opr ){
  unsigned ii = 1;
  while( ii < 0x7 && opr != cmp_op[ ii ] ){ ++ii; }
  return ii;
}


/* Tries to compose e1 and e2 into one. */
expression *combine_and( expression *e1, expression *e2 ){
  if( e1->left() == e2->left() && e1->right() == e2->right() ){
    unsigned i1 = op_idx( e1->opr() );
    if( i1 < 7 ){
      unsigned i2 = op_idx( e2->opr() );
      if( i2 < 7 ){
        return i1 & i2 ?
          new_logic( e1->left(), cmp_op[ i1 & i2 ], e2->right() ) : expr_F;
      }
    }
  }
  return 0;
}


/* Tries to compose e1 or e2 into one. */
expression *combine_or( expression *e1, expression *e2 ){
  if( e1->left() == e2->left() && e1->right() == e2->right() ){
    unsigned i1 = op_idx( e1->opr() );
    if( i1 < 7 ){
      unsigned i2 = op_idx( e2->opr() );
      if( i2 < 7 ){
        return ( i1 | i2 ) == 7 ? expr_T :
          new_logic( e1->left(), cmp_op[ i1 | i2 ], e2->right() );
      }
    }
  }
  return 0;
}


/* A common interface to the two above. */
inline expression *combine_and_or(
  expression *e1, op_type opr, expression *e2
){ return opr == op_and ? combine_and( e1, e2 ) : combine_or( e1, e2 ); }


/* Simplification of 0-ary, unary and binary logic expressions. */
expression *logic_simplify( expression *ee ){

  expression *e1 = simplify( ee->left() ), *e2 = simplify( ee->right() );
  op_type opr = ee->opr();

  /* The easiest simplifications */
  expression *e0 =
    e1 ? logic_easy( e1, opr, e2 ) : e2 ? logic_easy( opr, e2 ) : 0;
  if( e0 ){ return e0; }

  /* Negation */
  if( opr == op_not ){
    // logic_easy handled double negation
    if( e2->opr() == op_and ){
      return l_or( l_not( e2->left() ), l_not( e2->right() ) );
    }
    if( e2->opr() == op_or ){
      return l_and( l_not( e2->left() ), l_not( e2->right() ) );
    }
    if( e2->opr() == op_eq ){
      return new_logic( e2->left(), op_nq, e2->right() );
    }
    if( e2->opr() == op_nq ){
      return new_logic( e2->left(), op_eq, e2->right() );
    }
    if( e2->opr() == op_lt ){
      return new_logic( e2->left(), op_gq, e2->right() );
    }
    if( e2->opr() == op_gt ){
      return new_logic( e2->left(), op_lq, e2->right() );
    }
    if( e2->opr() == op_lq ){
      return new_logic( e2->left(), op_gt, e2->right() );
    }
    if( e2->opr() == op_gq ){
      return new_logic( e2->left(), op_lt, e2->right() );
    }
    return new_expr( op_not, e2 );
  }

  /* Is defined */
  if( opr == op_def ){
    if( e2->opr() == op_not ){ return new_logic( op_def, e2->right() ); }
    return new_expr( op_def, e2 );
  }

  /* Conjunction and disjunction */
  if( opr == op_and || opr == op_or ){

    /* Apply associativity. */
    if( e2->opr() == opr ){
      return new_logic( new_logic( e1, opr, e2->left() ), opr, e2->right() );
    }

    /* Sort and combine the arguments. */
    if( e1->opr() == opr ){
      expression *e3 = combine_and_or( e1->right(), opr, e2 );
      if( e3 ){ return new_logic( e1->left(), opr, e3 ); }
      return elt( e2, e1->right() ) ?
        new_logic( new_logic( e1->left(), opr, e2 ), opr, e1->right() ) :
        new_expr( e1, opr, e2 );
    }
    expression *e3 = combine_and_or( e1, opr, e2 );
    if( e3 ){ return e3; }
    return elt( e2, e1 ) ? new_logic( e2, opr, e1 ) : new_expr( e1, opr, e2 );

  }

  /* Short-circuit conjunction and disjunction */
  if( opr == op_sand ){ return l_and( e1, l_or( l_not( e1 ), e2 ) ); }
  if( opr == op_sor ){ return l_or( e1, l_and( l_not( e1 ), e2 ) ); }

  /* Implication and equivalence */
  if( opr == op_rarr ){ return l_or( l_not( e1 ), e2 ); }
  if( opr == op_harr ){
    return l_or( l_and( e1, e2 ), l_and( l_not( e1 ), l_not( e2 ) ) );
  }
  if( opr == op_Luka ){
    return l_or( l_or( l_not( e1 ), e2 ),
        l_not( l_or( l_def( e1 ), l_def( e2 ) ) )
      );
  }

  /* Relations */
  if( is_rel( opr ) ){

    /* Relation chains */
    if( is_rel( e1->opr() ) ){
      return l_and( e1, new_logic( ee->left()->right(), opr, e2 ) );
    }

    /* Normalize right hand side. */
    if( !known_const( e2 ) ){
      return new_logic( new_arit( e1, op_minus, e2 ), opr, expr_0 );
    }

    /* Move additive constants to the right. */
    if( e1->opr() == op_plus && known_const( e1->right() ) ){
      return
        new_logic( e1->left(), opr, new_arit( e2, op_minus, e1->right() ) );
    }

    /* Remove an unnecessary factor on the left. */

    /* A common special case */
    if( e2 == expr_0 ){

      /* Multiplication on left */
      if( e1->opr() == op_vprod ){
        if( opr == op_eq ){
          return l_or( l_eq_0( e1->left() ), l_eq_0( e1->right() ) );
        }
        if( opr == op_nq ){
          return l_and( l_nq_0( e1->left() ), l_nq_0( e1->right() ) );
        }
        if( opr == op_lt ){   //??? Does this produce too big expressions?
          return l_or(
            l_and( l_lt_0( e1->left() ), l_gt_0( e1->right() ) ),
            l_and( l_gt_0( e1->left() ), l_lt_0( e1->right() ) )
          );
        }
        if( opr == op_gt ){   //??? Does this produce too big expressions?
          return l_or(
            l_and( l_gt_0( e1->left() ), l_gt_0( e1->right() ) ),
            l_and( l_lt_0( e1->left() ), l_lt_0( e1->right() ) )
          );
        }
        if( opr == op_lq ){   //??? Does this produce too big expressions?
          return l_or(
            l_and( l_lq_0( e1->left() ), l_gq_0( e1->right() ) ),
            l_and( l_gq_0( e1->left() ), l_lq_0( e1->right() ) )
          );
        }
        if( opr == op_gq ){   //??? Does this produce too big expressions?
          return l_or(
            l_and( l_gq_0( e1->left() ), l_gq_0( e1->right() ) ),
            l_and( l_lq_0( e1->left() ), l_lq_0( e1->right() ) )
          );
        }
      }

      /* Power on left */
      if( e1->opr() == op_pow ){
        if( opr == op_eq ){
          return l_and( l_eq_0( e1->left() ), l_nq_0( e1->right() ) );
        }
        if( opr == op_nq ){
          return l_or( l_nq_0( e1->left() ), l_eq_0( e1->right() ) );
        }
      }

    }

  }

  return new_expr( e1, ee->opr(), e2, ee->val() );
}


/*** Reasoning about expressions, common part 2 ***/


/* Interface to simplification: processes 0-expressions, implements
  memoization, and splits according to result type. */
expression *simplify( expression *ee ){
  if( !ee ){ return 0; }
  if( !ee->smpl ){
    ee->smpl = ee;  // prevents infinite recursion
    if( yields_logic( ee->opr() ) ){ ee->smpl = logic_simplify( ee ); }
    else{ ee->smpl = arithm_simplify( ee ); }
  }
  return ee->smpl;
}


/* The domain of power computation */
expression *pow_dom( expression *e1, expression *e2 ){
  expression *e3 = l_or( l_nq_0( e1 ), l_gq_0( e2 ) );
  if( known_const( e2 ) && eval_expr( e2 ).de_odd() ){ return e3; }
  return l_and( e3, l_or( l_gq_0( e1 ), new_logic( op_is_int, e2 ) ) );
}


/* Computation of the domain of an expression. */
expression *domain( expression *ee ){
  op_type opr = ee->opr();
  expression
    *e1 = ee->left(), *e2 = ee->right(),
    *dom = e1 ? domain( e1 ) : expr_T;
  if( opr == op_deriv ){ return dom; }
  if( e2 ){ dom = l_and( dom, domain( e2 ) ); }
  if( opr == op_div || opr == op_idiv || opr == op_imod ){
    return l_and( dom, l_nq_0( e2 ) );
  }
  if( opr == op_mixn ){
    return e2->right()->val().is_zer() ? expr_F : expr_T;
  }
  if( opr == op_pow ){ return l_and( dom, pow_dom( e1, e2 ) ); }
  if( opr == op_sqrt ){ return l_and( dom, l_gq_0( e2 ) ); }
  if( opr == op_root ){
    return l_and( dom, pow_dom( e2, new_expr( e1, op_pow, expr_m1 ) ) );
  }
  if( opr == op_fact ){
    return l_and( dom, l_and( l_gq_0( e2 ), new_logic( op_is_int, e2 ) ) );
  }
  if( is_sin_like( opr ) ){
    if( opr == op_ln || opr == op_log || opr == op_log2 ){
      dom = l_and( dom, l_gt_0( e2 ) );
    }
    // sin, cos, sinh, cosh, and tanh are always defined
    if( opr == op_tan ){
      dom = l_and( dom, l_nq_0( new_expr( op_cos, e2 ) ) );
    }
    if( opr == op_cot ){
      dom = l_and( dom, l_nq_0( new_expr( op_sin, e2 ) ) );
    }
    if( e1 ){ dom = l_and( dom, pow_dom( new_expr( opr, e2 ), e1 ) ); }
    return dom;
  }
  return dom;
}


/*** Input parsing ***/


/* Problem types */
enum pb_type {
  pb_arithm, pb_array_claim, pb_CFG_compare, pb_CFG_in, pb_CFG_not_in,
  pb_equation, pb_modulo, pb_prop_logic, pb_real_logic, pb_tree_compare,
  pb_MathCheck, pb_brief_help, pb_help,
  pb_CFG_long, pb_CFG_set, pb_CFG_set2, pb_CFG_short, pb_CFG_start,
  pb_CFG_start2, pb_CFG_tree, pb_draw_function, pb_expression_tree
} pb_mode = pb_arithm;


/* Set of tokens that were tried in vain */
const unsigned parse_max = 30;    // maximum nr of tokens in parse error msg
unsigned parse_cnt = 0;           // nr of tokens in parse error message
tkn_type parse_tkns[ parse_max ]; // tokens in parse error message
static unsigned parse_dones = 0;  // number of processed tokens


/* Setting a parse error, if no error mode is on */
inline void parse_error(){
  if( !err_mode ){ err_mode = err_parse; }
}

/* Trying a token */
bool parse_is( tkn_type tkn ){
  if( err_mode ){ return false; }

  /* If matches, record that fact, reset tried tokens, and return. */
  if( tkn_now == tkn ){
    parse_cnt = 0; parse_dones = tkn_cnt; return true;
  }

  /* If tkn_now has matched, do not add tkn to tried tokens. */
  if( parse_dones == tkn_cnt ){ return false; }

  /* Collect tried tokens (without duplicates) for printing error message. */
  for( unsigned ii = 0; ii < parse_cnt && ii < parse_max; ++ii ){
    if( parse_tkns[ ii ] == tkn ){ return false; }
  }
  if( parse_cnt < parse_max ){ parse_tkns[ parse_cnt ] = tkn; }
  ++parse_cnt; return false;

}

/* Try more than one token. Only expect the first one in error messages. */
inline bool parse_is( tkn_type tk1, tkn_type tk2 ){
  if( err_mode ){ return false; }
  if( tkn_now == tk2 ){ parse_cnt = 0; parse_dones = tkn_cnt; return true; }
  return parse_is( tk1 );
}
inline bool parse_is( tkn_type tk1, tkn_type tk2, tkn_type tk3 ){
  if( err_mode ){ return false; }
  if( tkn_now == tk2 || tkn_now == tk3 ){
    parse_cnt = 0; parse_dones = tkn_cnt; return true;
  }
  return parse_is( tk1 );
}
inline bool parse_is(
  tkn_type tk1, tkn_type tk2, tkn_type tk3, tkn_type tk4
){
  if( err_mode ){ return false; }
  if( tkn_now == tk2 || tkn_now == tk3 || tkn_now == tk4 ){
    parse_cnt = 0; parse_dones = tkn_cnt; return true;
  }
  return parse_is( tk1 );
}
inline bool parse_is(
  tkn_type tk1, tkn_type tk2, tkn_type tk3, tkn_type tk4, tkn_type tk5
){
  if( err_mode ){ return false; }
  if( tkn_now == tk2 || tkn_now == tk3 || tkn_now == tk4 || tkn_now == tk5 ){
    parse_cnt = 0; parse_dones = tkn_cnt; return true;
  }
  return parse_is( tk1 );
}

/* Insist that the next token is tkn. */
void parse_pass( tkn_type tkn ){
  if( !err_mode && parse_is( tkn ) ){ get_token(); }else{ parse_error(); }
}

/* Insist that the next token is tk1 or tk2, but only mention tk1 in error
  messages. */
void parse_pass( tkn_type tk1, tkn_type tk2 ){
  if( !err_mode && ( parse_is( tk1, tk2 ) ) ){ get_token(); }
  else{ parse_error(); }
}


/* Recognizes sin-like function tokens. */
inline bool parse_sin_like(){
  return
    parse_is( tkn_ln ) || parse_is( tkn_log ) || parse_is( tkn_log2 ) ||
    parse_is( tkn_sin ) || parse_is( tkn_cos ) || parse_is( tkn_tan ) ||
    parse_is( tkn_cot ) ||
    parse_is( tkn_sinh ) || parse_is( tkn_cosh ) || parse_is( tkn_tanh );
}


/* Greek letters that may be variable names. */
bool is_greek_var(){
  if(
    tkn_now == tkn_UGamma || tkn_now == tkn_UDelta || tkn_now == tkn_UTheta ||
    tkn_now == tkn_ULambda || tkn_now == tkn_UXi || tkn_now == tkn_UPi ||
    tkn_now == tkn_USigma || tkn_now == tkn_UPhi || tkn_now == tkn_UPsi ||
    tkn_now == tkn_UOmega || tkn_now == tkn_Ualpha || tkn_now == tkn_Ubeta ||
    tkn_now == tkn_Ugamma || tkn_now == tkn_Udelta || tkn_now == tkn_Uepsiv ||
    tkn_now == tkn_Uzeta || tkn_now == tkn_Ueta || tkn_now == tkn_Utheta ||
    tkn_now == tkn_Uiota || tkn_now == tkn_Ukappa || tkn_now == tkn_Ulambda ||
    tkn_now == tkn_Umu || tkn_now == tkn_Unu || tkn_now == tkn_Uxi ||
    tkn_now == tkn_Urho || tkn_now == tkn_Usigma || tkn_now == tkn_Utau ||
    tkn_now == tkn_Uupsi || tkn_now == tkn_Uphi || tkn_now == tkn_Uchi ||
    tkn_now == tkn_Upsi || tkn_now == tkn_Uomega || tkn_now == tkn_Uthetav ||
    tkn_now == tkn_Ustraightphi || tkn_now == tkn_Uepsi ||
    tkn_now == tkn_al || tkn_now == tkn_be || tkn_now == tkn_ga ||
    tkn_now == tkn_de || tkn_now == tkn_ep || tkn_now == tkn_ve ||
    tkn_now == tkn_ze || tkn_now == tkn_et || tkn_now == tkn_th ||
    tkn_now == tkn_vt || tkn_now == tkn_io || tkn_now == tkn_ka ||
    tkn_now == tkn_la || tkn_now == tkn_mu || tkn_now == tkn_nu ||
    tkn_now == tkn_xi || tkn_now == tkn_rh || tkn_now == tkn_si ||
    tkn_now == tkn_ta || tkn_now == tkn_up || tkn_now == tkn_ph ||
    tkn_now == tkn_vp || tkn_now == tkn_ch || tkn_now == tkn_ps ||
    tkn_now == tkn_om || tkn_now == tkn_GA || tkn_now == tkn_Ga ||
    tkn_now == tkn_DE || tkn_now == tkn_De || tkn_now == tkn_TH ||
    tkn_now == tkn_Th || tkn_now == tkn_LA || tkn_now == tkn_La ||
    tkn_now == tkn_XI || tkn_now == tkn_Xi || tkn_now == tkn_PI ||
    tkn_now == tkn_Pi || tkn_now == tkn_SI || tkn_now == tkn_Si ||
    tkn_now == tkn_PH || tkn_now == tkn_Ph || tkn_now == tkn_PS ||
    tkn_now == tkn_Ps || tkn_now == tkn_OM || tkn_now == tkn_Om
  ){ parse_cnt = 0; parse_dones = tkn_cnt; return true; }
  return false;
}


/* Recognizes a variable name. To not pollute the list of expected tokens,
  greek letters are not put into it. */
inline unsigned parse_is_var_name(){
  if( parse_is( tkn_var ) ){ return inp_ltr; }
  if( is_greek_var() ){ return unsigned( 'z' ) + op_tkn( tkn_now ); }
  return 0;
}


/* Recognizes the tokens that can follow a solution chain. */
bool parse_is_top_tkn(){
  if( parse_is( tkn_eoi ) ){ return true; }
  if( inp_ff_exam ){ return false; }
  return
    tkn_now == tkn_forget_err || parse_is( tkn_eoa ) ||
    parse_is( tkn_arithm ) || parse_is( tkn_Arithm ) ||
    parse_is( tkn_array ) || parse_is( tkn_Array ) ||
    parse_is( tkn_brief_help ) || parse_is( tkn_Brief_help ) ||
    parse_is( tkn_CFG_cmp ) || parse_is( tkn_CFG_in ) ||
    parse_is( tkn_CFG_long ) || parse_is( tkn_CFG_not_in ) ||
    parse_is( tkn_CFG_set ) || parse_is( tkn_CFG_set2 ) ||
    parse_is( tkn_CFG_short ) || parse_is( tkn_CFG_start ) ||
    parse_is( tkn_CFG_start2 ) || parse_is( tkn_CFG_tree ) ||
    parse_is( tkn_draw ) || parse_is( tkn_Draw ) ||
    parse_is( tkn_equation ) || parse_is( tkn_Equation ) ||
    parse_is( tkn_expr_tree ) || parse_is( tkn_Expr_tree ) ||
    parse_is( tkn_help ) || parse_is( tkn_Help ) ||
    parse_is( tkn_mathcheck ) || parse_is( tkn_MathCheck ) ||
    parse_is( tkn_mod ) || parse_is( tkn_Mod ) ||
    parse_is( tkn_parse ) || parse_is( tkn_Parse ) ||
    parse_is( tkn_prop3_logic ) || parse_is( tkn_Prop3_logic ) ||
    parse_is( tkn_prop_logic ) || parse_is( tkn_Prop_logic ) ||
    parse_is( tkn_real_logic ) || parse_is( tkn_Real_logic ) ||
    parse_is( tkn_tree_cmp ) || parse_is( tkn_Tree_cmp );
}


/* Comments */
void parse_comment( bool new_pg = false ){
  while( parse_is( tkn_lc, tkn_lf ) ){
    if( err_mode ){ return; }

    /* Process an empty comment. */
    if( tkn_now == tkn_lf ){
      if( new_pg ){ out_html( "\n<p>" ); }
      pgh_broken = true; get_token(); new_pg = true; continue;
    }

    /* Ban deconfusion, except if tkn_lc was confused. */
    if( inp_cnf_cnt < 3 ){ inp_deconfuse_off(); }

    /* Start a comment. */
    out_html( '\n' );
    if( new_pg ){ out_print( "<p>" ); }
    out_print( "<span class=comment>" );

    /* Scan through the comment. */
    while( true ){

      /* Process ordinary chars, #-commands, and end-of-comment. */
      if( !inp_chr ){ tkn_now = tkn_eoi; break; }
      else if( inp_chr == '*' ){
        inp_get_chr();
        if( inp_chr == '/' ){ inp_get_chr(); tkn_now = tkn_rc; break; }
        else{ out_html( '*' ); }
      }
      else if( inp_chr == '\n' ){ out_html( ' ' ); inp_get_chr(); }
      else if( inp_chr == '#' && !inp_ff_exam ){
        inp_get_chr(); out_html( inp_chr ); inp_get_chr();
      }
      else if( inp_chr != '`' ){ out_esc( inp_chr ); inp_get_chr(); }

      /* Process MathCheck or AsciiMath notation. */
      else{
        inp_get_chr();
        while( true ){
          if( inp_chr == ' ' || inp_chr == '\n' ){ out_am( ' ' ); }
          inp_skip_white_space();
          if( inp_chr == '`' ){ inp_get_chr(); tkn_now = tkn_err; break; }
          get_token();
          if( tkn_now == tkn_rc || tkn_now == tkn_eoi ){ break; }
          if( tkn_now == tkn_number || tkn_now == tkn_decimal ){
            out_mode( om_am ); inp_revert_to( err_pos );
            while( is_digit( inp_chr ) ){
              out_print( inp_chr ); inp_get_chr();
            }
            if( inp_chr == '.' ){ out_print( '.' ); inp_get_chr(); }
            while( is_digit( inp_chr ) ){
              out_print( inp_chr ); inp_get_chr();
            }
          }
          else if( tkn_now == tkn_var || tkn_now == tkn_err ){
            unget_token(); out_am( inp_chr ); inp_get_chr();
          }
          else{ out_am( ' ' ); out_tkn( tkn_now ); }
        }
        out_mode( om_html );
        if( tkn_now == tkn_rc || tkn_now == tkn_eoi ){ break; }
      }

    }

    /* Stop a comment. */
    out_html( "</span>\n" ); pgh_broken = true;
    if( tkn_now != tkn_rc ){ err_set_inp( "End-of-comment */ missing" ); }
    inp_deconfuse_on();
    get_token(); new_pg = true;

  }
}


/* Recognizes begin-comment and setting tokens. To not pollute the list of
  expected tokens, only begin-comment and linefeed are added to it. */
bool is_setting_tkn(){
  if( parse_is( tkn_lc, tkn_lf ) ){ return true; }
  if( inp_ff_exam ){ return false; }
  if(
    tkn_now == tkn_debug_off || tkn_now == tkn_debug_on ||
    tkn_now == tkn_CFG_CR_off || tkn_now == tkn_CFG_CR_on ||
    tkn_now == tkn_CFG_ambiguous_off || tkn_now == tkn_CFG_ambiguous_on ||
    tkn_now == tkn_draw_off || tkn_now == tkn_draw_on ||
    tkn_now == tkn_exam_off || tkn_now == tkn_exam_on ||
    tkn_now == tkn_only_no_yes_off || tkn_now == tkn_only_no_yes_on ||
    tkn_now == tkn_prop3_off || tkn_now == tkn_prop3_on ||
    tkn_now == tkn_prove_off || tkn_now == tkn_prove_on ||
    tkn_now == tkn_undef_off || tkn_now == tkn_undef_on ||
    tkn_now == tkn_verbose_off || tkn_now == tkn_verbose_on ||
    tkn_now == tkn_allow_comp || tkn_now == tkn_b_nodes ||
    tkn_now == tkn_ban_comp || tkn_now == tkn_var4 ||
    tkn_now == tkn_f_CNF || tkn_now == tkn_f_DNF ||
    tkn_now == tkn_f_allow_U ||
    tkn_now == tkn_f_ban || tkn_now == tkn_f_ban_der ||
    tkn_now == tkn_f_nodes || tkn_now == tkn_f_polynomial ||
    tkn_now == tkn_f_range || tkn_now == tkn_f_simplify ||
    tkn_now == tkn_f_top_opr || tkn_now == tkn_fail_text ||
    tkn_now == tkn_forget_err || tkn_now == tkn_hide_expr ||
    tkn_now == tkn_index || tkn_now == tkn_integer ||
    tkn_now == tkn_prime || tkn_now == tkn_real ||
    tkn_now == tkn_next_URL || tkn_now == tkn_no_next_URL ||
    tkn_now == tkn_ok_text || tkn_now == tkn_reset ||
    tkn_now == tkn_skip_errs || tkn_now == tkn_solve
  ){ parse_cnt = 0; parse_dones = tkn_cnt; return true; }
  return false;
}


/* Print a mode description. */
void print_CNF(){
  if( pb_mode == pb_arithm || pb_mode == pb_tree_compare ){
    out_html( "a product of sums" );
  }else{ out_html( "in conjunctive normal form" ); }
}
void print_DNF(){
  if( pb_mode == pb_arithm || pb_mode == pb_tree_compare ){
    out_html( "a sum of products" );
  }else{ out_html( "in disjunctive normal form" ); }
}


/* Read a feedback text. */
void parse_feedback_text( std::string &text ){
  text.clear();
  while( inp_chr == ' ' ){ inp_get_chr(); }
  bool not_first = false;
  while( inp_chr ){
    if( inp_chr == '\n' ){
      inp_get_chr();
      if( inp_chr == '\n' ){ break; }
      if( not_first ){ text.push_back( ' ' ); }
    }
    not_first = true;
    if( inp_chr == '&' ){ text += "&amp;"; }
    else if( inp_chr == '<' ){ text += "&lt;"; }
    else if( inp_chr == '>' ){ text += "&gt;"; }
    else if( inp_chr == '\"' ){ text += "&quot;"; }
    else{
      if( inp_chr == '#' ){ inp_get_chr(); }
      if( inp_chr ){ text.push_back( inp_chr ); }
    }
    inp_get_chr();
  }
}


/* Global and chain-local settings */
void parse_settings( bool new_pg = false ){
  if( err_mode ){ return; }
  bool matched = false;
  do{
    matched = false;

    /* In the exam textareas, only comments are allowed. */
    if( parse_is( tkn_lc, tkn_lf ) ){
      parse_comment( new_pg ); matched = true;
    }
    if( inp_ff_exam ){ continue; }

    /* On-off settings */
    if( tkn_now == tkn_debug_off ){
      pgh_msg( "Debug mode off" );
      gs_debug = false; get_token(); matched = true;
    }
    if( tkn_now == tkn_debug_on ){
      pgh_msg( "Debug mode on" ); gs_debug = true;
      get_token(); matched = true;
    }
    if( tkn_now == tkn_CFG_ambiguous_off ){
      if( gs_verbose ){ pgh_msg( "Ambiguous grammar is not warned about" ); }
      CFG_ambiguous = false; get_token(); matched = true;
    }
    if( tkn_now == tkn_CFG_ambiguous_on ){
      if( gs_verbose ){ pgh_msg( "Ambiguous grammar is warned about" ); }
      CFG_ambiguous = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_CFG_CR_off ){
      if( gs_verbose ){ pgh_msg( "CR is banned in CFG strings" ); }
      CFG_ignore_CR = false; get_token(); matched = true;
    }
    if( tkn_now == tkn_CFG_CR_on ){
      if( gs_verbose ){ pgh_msg( "CR is ignored in CFG strings" ); }
      CFG_ignore_CR = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_draw_off ){
      if( gs_verbose ){ pgh_msg( "Curve drawing mode off" ); }
      gs_draw = false; get_token(); matched = true;
    }
    if( tkn_now == tkn_draw_on ){
      if( gs_verbose ){ pgh_msg( "Curve drawing mode on" ); }
      gs_draw = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_exam_off ){
#ifndef exam
      if( gs_verbose ){ pgh_msg( "Examination mode off" ); }
      gs_exam = false;
#endif
      get_token(); matched = true;
    }
    if( tkn_now == tkn_exam_on ){
#ifndef exam
      if( gs_verbose ){ pgh_msg( "Examination mode on" ); }
      gs_exam = true;
#endif
      get_token(); matched = true;
    }
    if( tkn_now == tkn_only_no_yes_off ){
      if( gs_verbose ){ pgh_msg( "Full feedback on an error is given" ); }
      gs_only_no_yes = false; get_token(); matched = true;
    }
    if( tkn_now == tkn_only_no_yes_on ){
      if( gs_verbose ){
        pgh_msg( "Restricted feedback on an error is given" );
      }
      gs_only_no_yes = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_prop3_off ){
      if( gs_verbose ){
        pgh_msg( "The undefined value is not used in propositional logic" );
      }
      gs_prop3 = false; get_token(); matched = true;
    }
    if( tkn_now == tkn_prop3_on ){
      if( gs_verbose ){
        pgh_msg( "The undefined value is used also in propositional logic" );
      }
      gs_prop3 = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_prove_off ){
      if( gs_verbose ){ pgh_msg( "Prove off" ); }
      gs_prove = false; get_token(); matched = true;
    }
    if( tkn_now == tkn_prove_on ){
      if( gs_verbose ){ pgh_msg( "Prove on" ); }
      gs_prove = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_reset ){
      reset_global_settings(); get_token(); matched = true;
    }
    if( tkn_now == tkn_undef_off ){
      if( gs_verbose ){ pgh_msg( "Ignore undefined" ); }
      gs_undef_check = false; get_token(); matched = true;
    }
    if( tkn_now == tkn_undef_on ){
      if( gs_verbose ){ pgh_msg( "Detect undefined" ); }
      gs_undef_check = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_verbose_off ){
      if( gs_verbose ){ pgh_msg( "Verbose off" ); }
      gs_verbose = false; get_token(); matched = true;
    }
    if( tkn_now == tkn_verbose_on ){
      pgh_msg( "Verbose on" );
      gs_verbose = true; get_token(); matched = true;
    }

    /* Other global settings */
    if( tkn_now == tkn_allow_comp ){
      if( gs_verbose ){
        pgh_msg( "The relation chain may contain " );
        out_print( "&lt;, &le;, &gt;, and &ge; or &lArr; and &rArr;" );
      }
      ls_allow_comp = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_ban_comp ){
      if( gs_verbose ){
        pgh_msg( "The relation chain must not contain " );
        out_print( "&lt;, &le;, &gt;, and &ge; or &lArr; and &rArr;" );
      }
      ls_ban_comp = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_var4 ){
      if( gs_verbose ){
        pgh_msg( "More variables allowed, at the cost of fewer test values" );
      }
      ls_var4 = true; get_token(); matched = true;
    }

    /* Local settings */
    if( !ls_b_nodes && tkn_now == tkn_b_nodes ){
      get_token();
      if( parse_is( tkn_number ) ){ ls_b_nodes = tkn_val.to_unsigned(); }
      if( !ls_b_nodes ){
        err_set_inp( "A positive integer is needed here" ); return;
      }else if( ls_f_nodes && ls_f_nodes < ls_b_nodes ){
        err_set_inp( "The b_nodes value must be at most the f_nodes value" );
        return;
      }else if( gs_verbose ){
        pgh_msg( "Additional praise is given, if the complexity of the final"
          " expression is at most " );
        out_print( ls_b_nodes );
      }
      get_token(); matched = true;
    }
    if( tkn_now == tkn_f_CNF ){
      if( gs_verbose ){
        pgh_msg( "The final expression must be " ); print_CNF();
      }
      ls_f_CNF = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_f_DNF ){
      if( gs_verbose ){
        pgh_msg( "The final expression must be " ); print_DNF();
      }
      ls_f_DNF = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_f_allow_U ){
      if( gs_verbose ){
        pgh_msg( "The final expression is allowed to yield " );
        out_html( "<b>U</b>" );
      }
      ls_f_ban_U = false; get_token(); matched = true;
    }
    if( tkn_now == tkn_f_polynomial ){
      if( gs_verbose ){
        pgh_msg( "The final expression must be in polynomial form" );
      }
      ls_f_polynomial = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_f_simplify ){
      if( gs_verbose ){
        pgh_msg( "The final expression must be in simplified form" );
      }
      ls_f_simplify = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_f_range ){
      if( gs_verbose ){
        pgh_msg( "The constants in the final expression must be between 0 "
          "and modulus minus 1" );
      }
      ls_f_range = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_f_ban ){
      if( gs_verbose ){ pgh_msg( "The final expression must not contain" ); }
      get_token();
      while( !parse_is( tkn_semic ) && tkn_now != tkn_eoi ){
        op_type opr = op_tkn( tkn_now );
        if( gs_verbose ){ out_html( ' ' ); out_op( opr ); }
        if( ls_f_ban_cnt >= ls_f_ban_max ){
          err_set_inp( "Too many operators banned" ); return;
        }
        ls_f_ban_ops[ ls_f_ban_cnt ] = opr; ++ls_f_ban_cnt; get_token();
      }
      get_token(); matched = true;
    }
    if( tkn_now == tkn_f_ban_der ){
      if( gs_verbose ){
        pgh_msg(
          "The final expression must not contain `frac(del)(del ...)`"
        );
      }
      if( ls_f_ban_cnt >= ls_f_ban_max ){
        err_set_inp( "Too many operators banned" ); return;
      }
      ls_f_ban_ops[ ls_f_ban_cnt ] = op_deriv; ++ls_f_ban_cnt;
      get_token(); matched = true;
    }
    if( !ls_f_nodes && tkn_now == tkn_f_nodes ){
      get_token();
      if( parse_is( tkn_number ) ){ ls_f_nodes = tkn_val.to_unsigned(); }
      if( !ls_f_nodes ){
        err_set_inp( "A positive integer is needed here" ); return;
      }else if( ls_f_nodes < ls_b_nodes ){
        err_set_inp( "The f_nodes value must be at least the b_nodes value" );
        return;
      }else if( gs_verbose ){
        pgh_msg( "The complexity of the final expression must be at most " );
        out_print( ls_f_nodes );
      }
      get_token(); matched = true;
    }
    if( ls_f_top_opr == op_err && tkn_now == tkn_f_top_opr ){
      get_token(); ls_f_top_opr = op_tkn( tkn_now );
      if(
        ls_f_top_opr == op_deriv ||
        ls_f_top_opr == op_forall || ls_f_top_opr == op_exists
      ){
        get_token(); ls_f_top_var = parse_is_var_name();
        if( !ls_f_top_var ){ err_set_inp( "A variable is needed here" ); }
      }
      if( err_mode ){ return; }
      if( gs_verbose ){
        pgh_msg( "The top operator of the final expression must be " );
        out_op( ls_f_top_opr, ls_f_top_var );
      }
      get_token(); matched = true;
    }
    if( tkn_now == tkn_forget_err ){
      pgh_msg( "Error counter reset" ); err_cnt = 0;
      get_token(); matched = true;
    }
    if( tkn_now == tkn_hide_expr ){
      ls_hide_expr = true; get_token(); matched = true;
    }
    if( tkn_now == tkn_next_URL ){
      unsigned ii = 0;
      inp_skip_white_space();
      while( ' ' < inp_chr && inp_chr < '\x7F' ){
        if( ii >= URL_max ){ mc_err_print( "Too long next URL" ); return; }
        next_URL[ ii ] = inp_chr; inp_get_chr(); ++ii;
      }
      next_URL[ ii ] = '\0'; get_token(); matched = true;
    }
    if( tkn_now == tkn_no_next_URL ){
      next_URL[0] = ' '; next_URL[1] = '\0'; get_token(); matched = true;
    }
    if(
      tkn_now == tkn_index || tkn_now == tkn_integer || tkn_now == tkn_real ||
      tkn_now == tkn_prime
    ){
      var_type vt =
        tkn_now == tkn_integer ? vtp_Z : tkn_now == tkn_prime ? vtp_P :
        tkn_now == tkn_index ? vtp_idx : vtp_R;
      get_token();
      unsigned vv = parse_is_var_name();
      if( !vv ){ err_set_inp( "A variable is needed here" ); }
      if( err_mode ){ return; }
      find_or_add_variable( vv, vt, false, true );
      if( gs_verbose ){
        pgh_msg( "The type of " ); out_var_name( vv );
        out_html( " is " ); out_type_name( vt );
      }
      get_token(); matched = true;
    }
    if( tkn_now == tkn_fail_text ){
      parse_feedback_text( gs_fail_text ); get_token(); matched = true;
    }
    if( tkn_now == tkn_ok_text ){
      parse_feedback_text( gs_ok_text ); get_token(); matched = true;
    }
    if( !ls_solve && tkn_now == tkn_solve ){
      get_token(); ls_solve = parse_is_var_name();
      if( !ls_solve ){ err_set_inp( "A variable is needed here" ); }
      if( err_mode ){ return; }
      if( gs_verbose ){
        pgh_msg( "The final answer must be with respect to " );
        out_var_name( ls_solve );
      }
      get_token(); matched = true;
    }

  }while( matched );
}


/* Parsing arithmetic structures
  The resulting expression tree is determined by the operation of these
  functions. The goal is that all widely used notation is allowed (such as
  sin^2 x + cos^2 x), and the expression tree always matches the usual
  interpretation of the notation. Ordinary parentheses are not present in the
  expression tree but hard parentheses are, that is, unnecessary ordinary
  parentheses are dropped. The left and right precedences are for printing
  parentheses for AsciiMath. AsciiMath parentheses rules are unusual at
  places and sin 2x = 2 sin x cos x caused trouble, so some trickery has been
  used.
    When logic parsing encounters a left (possibly hard) parenthesis, it may
  start either a logic or an arithmetic expression. If it is arithmetic, that
  fact can be determined only when the corresponding right parenthesis is
  encountered. This problem is solved by parsing the expression as a logic
  expression and, if it turns out arithmetic, storing the result to par_expr
  and calling arithmetic parsing, delivering par_expr to parse_arithm_atom as
  already done work. parse_pred_or_expr does that. It calls arithmetic parsing
  via the usual chain of parsing functions so that when parse_arithm_atom
  returns via parse_expression, parse_logic_atom can continue with the result.
*/

typedef unsigned pm_type;
expression *par_expr = 0;   // see explanation above
expression *parse_expression( pm_type );
expression *parse_inv_prod( pm_type );
expression *parse_arithm_atom( pm_type );
expression *parse_fact( pm_type );
const pm_type       // bits for choosing alternative sub-grammars
  pm_int = 1,       // arithmetic is on integers
  pm_mod = 2,       // arithmetic is on modular numbers
  pm_tv = 4,        // no arithmetic but truth value computation instead
  pm_in_abs = 8,    // inside absolute value function |...| without ()
  pm_in_func = 16,  // inside sin, cos, ln, etc. without ()
  pm_in_der = 32,   // inside derivative
  pm_no_var = 64,   // variables must not occur
  pm_no_new_free = 128, // previously unseen free variables must not occur
//pm_no_comp = 256, // <, ..., ==> must not occur as relation chain oprs
  pm_was_not = 512, // negat. has just occurred (so parentheses must come now)
  pm_smpl_idx = 1024, // compar. chain of int, const, -const, or int +- const
  pm_no_qua = 2048, // quantifiers must not occur
  pm_not_in = ~( pm_in_abs | pm_in_func | pm_was_not );

bool parse_is_var( var_type vt, pm_type pm_now, bool is_new_q = false ){
  if( err_mode || ( pm_now & pm_no_var ) ){ return false; }
  bool may_create = is_new_q || !( pm_now & pm_no_new_free );
  unsigned nm = parse_is_var_name();
  if( nm ){
    tkn_val = find_or_add_variable( nm, vt, is_new_q, may_create );
    return true;
  }
  return false;
}

/* Note: pm_now affects the indices, not the variable itself. */
expression *parse_var( pm_type pm_now ){
  if( err_mode ){ return new_expr( op_var, tkn_val ); }
  number val = tkn_val; expression *e1 = 0, *e2 = 0;
  unsigned ad = to_var( val ).dimension;
  get_token();
  if( ad ){
    pm_now &= pm_not_in & ~( pm_mod | pm_tv ); pm_now |= pm_int;
    parse_pass( tkn_lB ); e1 = parse_expression( pm_now );
    if( ad > 1 ){
      parse_pass( tkn_comma ); e2 = parse_expression( pm_now );
    }
    parse_pass( tkn_rB );
  }else if( tkn_now == tkn_lB ){
    err_set_inp( "This variable is not an array" );
  }
  return new_expr( e1, op_var, e2, val );
}

void parse_ensure_real( unsigned vv ){
  if( var_used[ vv ].type != vtp_R ){
    err_set_inp( "This variable must be of real number type" );
  }
}

void parse_ensure_integer( unsigned vv ){
  if(
    var_used[ vv ].type != vtp_Z && var_used[ vv ].type != vtp_P &&
    var_used[ vv ].type != vtp_idx
  ){ err_set_inp( "This variable must be of integer type" ); }
}

void parse_ban_array( unsigned vv ){
  if( var_used[ vv ].dimension ){
    err_set_inp( "Array variable is not allowed here" );
  }
}


/* Recognizes prefix function and operator tokens (except unary - and +). */
bool parse_is_prefix( pm_type pm_now ){
  if( pm_now & ( pm_mod | pm_in_func ) ){ return false; }
  return
    ( !( pm_now & pm_int ) && ( parse_sin_like() || parse_is( tkn_DD ) ) ) ||
    ( !( pm_now & ( pm_int | pm_in_der ) ) &&
      ( parse_is( tkn_ddn ) || parse_is( tkn_dup ) ) ) ||
    tkn_now == tkn_abs || tkn_now == tkn_floor || tkn_now == tkn_ceil;
}


expression *parse_func_arg( pm_type pm_now ){
  if( parse_is( tkn_lp ) || parse_is( tkn_lP ) ){
    return parse_arithm_atom( pm_now & pm_not_in );
  }else if( parse_is_prefix( pm_now ) ){
    return parse_arithm_atom( pm_now );
  }else{
    return parse_inv_prod( pm_now | pm_in_func );
  }
}//??? Mita taman jalkeen saa tulla?


expression *parse_arithm_atom( pm_type pm_now ){

  /* If parse_pred_or_expr delivered an expression, use it. */
  if( par_expr ){ expression *ee = par_expr; par_expr = 0; return ee; }

  /* Unsigned integer (plus fraction) number constant */
  if( parse_is( tkn_number ) ){
    expression *e1 = new_expr( tkn_val );
    get_token();
    if( pm_now & pm_int || !parse_is( tkn_number ) ){ return e1; }

    /* Fractional part */
    expression *e2 = new_expr( tkn_val );
    get_token(); parse_pass( tkn_div );
    expression *e3 = new_expr( tkn_val );
    parse_pass( tkn_number );
    return new_expr( e1, op_mixn, new_expr( e2, op_div, e3 ) );

  }

  /* Decimal number constant */
  if( !( pm_now & ( pm_int | pm_mod ) ) && parse_is( tkn_decimal ) ){
    expression *ee = new_expr( tkn_val ); get_token(); return ee;
  }

  /* Variable */
  if(
    ( !( pm_now & pm_mod ) && parse_is_var( vtp_none, pm_now ) ) ||
    ( pm_now & pm_mod && parse_is_var( vtp_mod, pm_now ) )
  ){ return parse_var( pm_now ); }

  /* Ordinary and hard parentheses */
  if( parse_is( tkn_lp ) ){
    get_token();
    expression *ee = parse_expression( pm_now & pm_not_in );
    parse_pass( tkn_rp ); return ee;
  }
  if( parse_is( tkn_lP ) ){
    get_token();
    expression *ee = new_expr(
      op_hpar, parse_expression( pm_now & pm_not_in )
    );
    parse_pass( tkn_rP ); return ee;
  }

  /* Explicit fraction division */
  if( !( pm_now & pm_int ) && parse_is( tkn_frac ) ){
    get_token();
    parse_pass( tkn_lp );
    expression *e1 = parse_expression( pm_now & pm_not_in );
    parse_pass( tkn_rp ); parse_pass( tkn_lp );
    expression *e2 = parse_expression( pm_now & pm_not_in );
    parse_pass( tkn_rp );
    return new_expr( e1, op_div, e2 );
  }

  /* Absolute value */
  if( !( pm_now & pm_mod ) && parse_is( tkn_vbar ) ){
    get_token();
    expression *ee = new_expr( op_abs, parse_expression( pm_in_abs ) );
    parse_pass( tkn_vbar ); return ee;
  }
  if( !( pm_now & ( pm_mod | pm_in_func ) ) && tkn_now == tkn_abs ){
    get_token(); return new_expr( op_abs, parse_func_arg( pm_now ) );
  }

  /* Floor */
  if(
    !( pm_now & ( pm_mod | pm_in_der ) )
    && parse_is( tkn_lfloor, tkn_Ulfloor )
  ){
    get_token();
    expression *ee =
      new_expr( op_lfloor, parse_expression( pm_now & pm_not_in & ~pm_int ) );
    parse_pass( tkn_rfloor, tkn_Urfloor ); return ee;
  }
  if(
    !( pm_now & ( pm_mod | pm_in_func | pm_in_der ) ) && tkn_now == tkn_floor
  ){
    get_token();
    return new_expr( op_lfloor, parse_func_arg( pm_now & ~pm_int ) );
  }

  /* Ceiling */
  if(
    !( pm_now & ( pm_mod | pm_in_der ) ) && parse_is( tkn_lceil, tkn_Ulceil )
  ){
    get_token();
    expression *ee =
      new_expr( op_lceil, parse_expression( pm_now & pm_not_in & ~pm_int ) );
    parse_pass( tkn_rceil, tkn_Urceil ); return ee;
  }
  if(
    !( pm_now & ( pm_mod | pm_in_func | pm_in_der ) ) && tkn_now == tkn_ceil
  ){
    get_token();
    return new_expr( op_lceil, parse_func_arg( pm_now & ~pm_int ) );
  }

  /* Square root */
  if( !( pm_now & pm_int ) && parse_is( tkn_sqrt, tkn_Usqrt ) ){
    get_token(); return new_expr( op_sqrt, parse_func_arg( pm_now ) );
  }

  /* General root */
  if( !( pm_now & pm_int ) && parse_is( tkn_root ) ){
    get_token();
    expression *e1 = expr_dummy;
    bool was_par = parse_is( tkn_lp );
    if( was_par ){ get_token(); }
    if( parse_is( tkn_number ) ){
      e1 = new_expr( tkn_val );
      if( !tkn_val.to_unsigned() ){
        err_set_inp( "Here the number must be a positive integer" );
        return new_expr( e1, op_root, expr_dummy );
      }
      get_token();
    }else if( !( pm_now & pm_mod ) && parse_is_var( vtp_none, pm_now ) ){
      parse_ensure_integer( to_var_idx( tkn_val ) );
      e1 = parse_var( pm_now );
    }else{ parse_error(); }
    if( was_par ){ parse_pass( tkn_rp ); }
    return new_expr( e1, op_root, parse_func_arg( pm_now ) );
  }

  /* Sin-like functions (logarithms, trigonometric, hyperbolic) */
  if( !( pm_now & ( pm_int | pm_mod | pm_in_func ) ) && parse_sin_like() ){
    op_type opr = op_tkn( tkn_now ); get_token();
    expression *e1 = 0;
    if( parse_is( tkn_sup ) ){
      get_token();
      e1 = parse_fact( pm_now & pm_not_in );
    }
    return new_expr( e1, opr, parse_func_arg( pm_now ) );
  }

  /* Double down and up */
  if(
    !( pm_now & ( pm_int | pm_mod | pm_in_func | pm_in_der ) ) &&
    ( parse_is( tkn_ddn ) || parse_is( tkn_dup ) )
  ){
    op_type opr = op_tkn( tkn_now ); get_token();
    return new_expr( opr, parse_func_arg( pm_now ) );
  }

  /* Base of natural logarithm */
  if( !( pm_now & ( pm_int | pm_mod ) ) && parse_is( tkn_e2718 ) ){
    get_token(); return expr_e;
  }

  /* Pi */
  if( !( pm_now & ( pm_int | pm_mod ) ) && parse_is( tkn_pi, tkn_Upi ) ){
    get_token(); return expr_pi;
  }

  /* Derivative */
  if( !( pm_now & ( pm_int | pm_mod | pm_in_func ) ) && parse_is( tkn_DD ) ){
    get_token();
    if( !parse_is_var( vtp_R, pm_now ) ){ parse_error(); return expr_dummy; }
    unsigned vv = to_var_idx( tkn_val );
    parse_ensure_real( vv ); parse_ban_array( vv );
    if( err_mode ){ return new_expr( op_var, tkn_val ); }
    number val = tkn_val;
    get_token();
    expression *ee = parse_inv_prod( pm_now | pm_in_der );
    return new_expr( derivative( ee, vv ), op_deriv, ee, val );
  }

  parse_error(); return expr_dummy;
}

/* Factorial ! *///??? Pitaa rajata kokonaislukuargumentteihin
expression *parse_fact( pm_type pm_now ){
  expression *ee = parse_arithm_atom( pm_now );
  while( !( ee->type() & 0x18 ) && tkn_now == tkn_excl ){
    get_token(); ee = new_expr( op_fact, ee );
  }
  parse_is( tkn_fact );   // record "!" as an expected token
  return ee;
}

/* Power ^ */
expression *parse_pow( pm_type pm_now ){
  expression *ee = parse_fact( pm_now );
  if( !parse_is( tkn_sup ) ){ return ee; }
  get_token();
  if( !( pm_now & pm_mod ) ){
    return new_expr( ee, op_pow, parse_pow( pm_now & pm_not_in ) );
  }
  if( parse_is( tkn_number ) ){
    number val = tkn_val;
    if( val.is_int_type() ){ get_token(); }
    else{ err_set_inp( "Here the number must be an integer" ); }
    return new_expr( ee, op_pow, new_expr( val ) );
  }
  parse_error(); return ee;
}

/* Division */
expression *parse_div( pm_type pm_now ){
  expression *ee = parse_pow( pm_now );
  if( ( pm_now & pm_int ) || !parse_is( tkn_div ) ){ return ee; }
  get_token(); return new_expr( ee, op_div, parse_pow( pm_now ) );
}

/* Unwritten multiplication */
expression *parse_inv_prod( pm_type pm_now ){
  expression *ee = parse_div( pm_now );
  while(
    ( !( pm_now & pm_mod ) && parse_is_var( vtp_none, pm_now ) ) ||
    ( pm_now & pm_mod && parse_is_var( vtp_mod, pm_now ) ) ||
    parse_is( tkn_lp ) || parse_is( tkn_lP ) ||
    ( !( pm_now & ( pm_mod | pm_in_abs ) ) && parse_is( tkn_vbar ) ) ||
    ( !( pm_now & pm_int ) &&
      ( parse_is( tkn_frac ) || parse_is( tkn_sqrt, tkn_Usqrt ) ||
        parse_is( tkn_root ) )
      ) ||
    parse_is_prefix( pm_now ) ||
    ( !( pm_now & ( pm_int | pm_mod ) ) &&
      ( parse_is( tkn_e2718 ) || parse_is( tkn_pi, tkn_Upi ) ) ) ||
    ( !( pm_now & ( pm_mod | pm_in_der ) ) &&
      ( parse_is( tkn_lfloor, tkn_Ulfloor ) ||
        parse_is( tkn_lceil, tkn_Ulceil ) ) ) ||
    tkn_now == tkn_iprod
  ){
    if( err_mode ){ return new_expr( op_var, tkn_val ); }
    if( tkn_now == tkn_iprod ){ get_token(); }
    ee = new_expr( ee, op_iprod, parse_div( pm_now ) );
  }
  return ee;
}

/* Star multiplication * */
expression *parse_prod( pm_type pm_now ){
  expression *ee = parse_inv_prod( pm_now );
  while( parse_is( tkn_ast, tkn_Usdot ) ){
    get_token(); ee = new_expr( ee, op_vprod, parse_inv_prod( pm_now ) );
  }
  return ee;
}

/* Integer division div and modulo mod */
expression *parse_div_mod( pm_type pm_now ){
  expression *ee = parse_prod( pm_now );
  while(
    !( pm_now & pm_mod ) && ( parse_is( tkn_idiv ) || parse_is( tkn_imod ) ) )
  {
    op_type opr = op_tkn( tkn_now ); get_token();
    ee = new_expr( ee, opr, parse_prod( pm_now ) );
  }
  return ee;
}

/* Unary + and - */
expression *parse_sign( pm_type pm_now ){
  if(
    !par_expr && ( parse_is( tkn_plus ) || parse_is( tkn_Uminus, tkn_minus ) )
  ){
    op_type opr = op_tkn( tkn_now ); get_token();
    return new_expr( opr, parse_sign( pm_now ) );
  }
  return parse_div_mod( pm_now );
}

/* Addition + and subtraction - */
expression *parse_sum( pm_type pm_now ){
  expression *ee = parse_sign( pm_now );
  while( parse_is( tkn_plus ) || parse_is( tkn_Uminus, tkn_minus ) ){
    op_type opr = op_tkn( tkn_now ); get_token();
    ee = new_expr( ee, opr, parse_sign( pm_now ) );
  }
  return ee;
}

/* Expression */
expression *parse_expression( pm_type pm_now ){ return parse_sum( pm_now ); }
expression *parse_arithmetic( pm_type pm_now ){
  par_expr = 0; return parse_expression( pm_now );
}


/* Simple index suitable for arrays */
expression *parse_simple_index( pm_type pm_now, bool & allow_const ){
  expression *e1 = expr_dummy;

  /* If parse_pred_or_expr delivered an expression, use it. */
  if( par_expr ){ e1 = par_expr; par_expr = 0; return e1; }

  /* Variable with optional +- value */
  if(
    ( !( pm_now & pm_mod ) && parse_is_var( vtp_idx, pm_now ) ) ||
    ( pm_now & pm_mod && parse_is_var( vtp_mod, pm_now ) )
  ){
    unsigned vv = to_var_idx( tkn_val );
    parse_ban_array( vv ); e1 = new_expr( op_var, tkn_val );
    if( !err_mode ){ get_token(); }
    if(
      allow_const &&
      ( parse_is( tkn_plus ) || parse_is( tkn_Uminus, tkn_minus ) )
    ){
      op_type opr = op_tkn( tkn_now ); expression *e2 = expr_dummy;
      get_token();
      if( parse_is( tkn_number ) ){ e2 = new_expr( tkn_val ); get_token(); }
      else{ parse_error(); }
      e1 = new_expr( e1, opr, e2 );
    }
    allow_const = e1->opr() == op_var; return e1;
  }

  /* Value or minus value, if they are allowed */
  if( allow_const ){
    allow_const = false;
    if( parse_is( tkn_number ) ){
      e1 = new_expr( tkn_val ); get_token(); return e1;
    }
    if( parse_is( tkn_Uminus, tkn_minus ) ){
      get_token();
      if( parse_is( tkn_number ) ){ e1 = new_expr( tkn_val ); get_token(); }
      else{ parse_error(); }
      return new_expr( op_minus, e1 );
    }
  }

  parse_error(); return e1;
}


/* Parsing a predicate */

expression *parse_pred_or_expr( pm_type );

expression *parse_predicate( pm_type pm_now ){
  par_expr = 0;
  expression *ee = parse_pred_or_expr( pm_now );
  if( par_expr ){ parse_error(); }
  return ee;
}


/* Atomic predicate */
expression *parse_logic_atom( pm_type pm_now ){

  /* Truth value constants */
  if( parse_is( tkn_FF ) ){ get_token(); return expr_F; }
  if( parse_is( tkn_UU ) ){ get_token(); return expr_U; }
  if( parse_is( tkn_TT ) ){ get_token(); return expr_T; }

  /* Truth-valued variable */
  if(
    pm_now & pm_tv && parse_is_var( gs_prop3 ? vtp_tv3 : vtp_tv2, pm_now )
  ){ return parse_var( pm_now ); }

  /* Quantifier */
  if(
    !( pm_now & pm_no_qua ) &&
    ( parse_is( tkn_AA, tkn_Uforall ) || parse_is( tkn_EE, tkn_Uexist ) )
  ){
    op_type qq = op_tkn( tkn_now ); number val = 0; unsigned ii = 0;
    get_token();
    if(
      ( pm_now & pm_mod && parse_is_var( vtp_mod, pm_now, true ) ) ||
      ( !( pm_now & pm_mod ) && parse_is_var( vtp_idx, pm_now, true ) )
    ){
      ii = to_var_idx( tkn_val );
      parse_ban_array( ii );
      if( err_mode ){ return new_expr( op_var, tkn_val ); }
      val = tkn_val; get_token();
    }else{ parse_error(); return expr_dummy; }
    expression *ee = 0;
    if( parse_is( tkn_semic ) ){
      get_token();
      ee = parse_predicate( ( pm_now & pm_not_in ) | pm_smpl_idx );
    }
    parse_pass( tkn_colon );
    ee = new_expr( ee, qq, parse_predicate( pm_now & pm_not_in ), val );
    var_close( ii ); return ee;
  }

  /* Ordinary and hard parentheses */
  if( parse_is( tkn_lp ) ){
    get_token();
    expression *ee = parse_pred_or_expr( pm_now & ~pm_was_not );
    if( !par_expr ){ parse_pass( tkn_rp ); return ee; }
    if( pm_now & pm_was_not ){ parse_error(); }
    else{ parse_pass( tkn_rp ); }   // control goes to comparison chain
  }else if( parse_is( tkn_lP ) ){
    get_token();
    expression *ee = parse_pred_or_expr( pm_now & ~pm_was_not );
    if( !par_expr ){ parse_pass( tkn_rP ); return new_expr( op_hpar, ee ); }
    par_expr = new_expr( op_hpar, par_expr );
    if( pm_now & pm_was_not ){ parse_error(); }
    else{ parse_pass( tkn_rP ); }   // control goes to comparison chain
  }

  /* Negation must not be followed by a comparison without parentheses. If the
    problem mode is propositional logic, there must be no comparison chain. */
  if( pm_now & ( pm_was_not | pm_tv ) ){ parse_error(); return expr_dummy; }

  /* Comparison chain */
  bool allow_const = true;
  op_type rel_op = op_type( 0 );
  expression *e1 = pm_now & pm_smpl_idx ?
    parse_simple_index( pm_now, allow_const ) :
    parse_expression( pm_now & pm_not_in );
  while(
    parse_is( tkn_eq ) || parse_is( tkn_nq, tkn_Une ) ||
    parse_is( tkn_lt ) || parse_is( tkn_lq, tkn_Ule ) ||
    parse_is( tkn_gt ) || parse_is( tkn_gq, tkn_Uge )
  ){
    rel_op = op_tkn( tkn_now ); get_token();
    expression *e2 = pm_now & pm_smpl_idx ?
      parse_simple_index( pm_now, allow_const ) :
      parse_expression( pm_now & pm_not_in );
    e1 = new_expr( e1, rel_op, e2 );
  }

  /* If got an expression and no comparison, deliver it via par_expr. */
  if( !rel_op && !err_mode ){ par_expr = e1; }

  return e1;
}


expression *parse_not( pm_type pm_now ){
  if( parse_is( tkn_not, tkn_excl, tkn_Unot ) || tkn_now == tkn_ast ){
    op_type opr = tkn_now == tkn_ast ? op_def : op_not;
    get_token();
    expression *ee = new_expr( opr, parse_not( pm_now | pm_was_not ) );
    if( par_expr ){ parse_error(); }
    return ee;
  }else{ parse_is( tkn_def ); }   // record "**" as an expected token
  return parse_logic_atom( pm_now );
}

expression *parse_and( pm_type pm_now ){
  expression *ee = parse_not( pm_now );
  while(
    !par_expr && parse_is( tkn_2sup, tkn_ud, tkn_2amp, tkn_and, tkn_Uand )
  ){
    op_type opr = tkn_now == tkn_2amp ? op_sand : op_and;
    get_token(); ee = new_expr( ee, opr, parse_not( pm_now ) );
    if( par_expr ){ parse_error(); }
  }
  return ee;
}

expression *parse_or( pm_type pm_now ){
  expression *ee = parse_and( pm_now );
  while(
    !par_expr && parse_is( tkn_vv, tkn_du, tkn_2vbar, tkn_or, tkn_Uor )
  ){
    op_type opr = tkn_now == tkn_2vbar ? op_sor : op_or;
    get_token(); ee = new_expr( ee, opr, parse_and( pm_now ) );
    if( par_expr ){ parse_error(); }
  }
  return ee;
}

expression *parse_impl( pm_type pm_now ){
  expression *ee = parse_or( pm_now );
  if(
    par_expr ||
    !( parse_is( tkn_rarr, tkn_Urarr ) || parse_is( tkn_Luka, tkn_ULuka ) )
  ){ return ee; }
  op_type opr = op_tkn( tkn_now );
  get_token(); ee = new_expr( ee, opr, parse_impl( pm_now ) );
  if( par_expr ){ parse_error(); }
  return ee;
}

expression *parse_ekv( pm_type pm_now ){
  expression *ee = parse_impl( pm_now );
  while( !par_expr && parse_is( tkn_harr, tkn_Uharr ) ){
    get_token(); ee = new_expr( ee, op_harr, parse_impl( pm_now ) );
    if( par_expr ){ parse_error(); }
  }
  return ee;
}

expression *parse_pred_or_expr( pm_type pm_now ){
  return parse_ekv( pm_now );
}


/*** Error reporting, part 2 ***/


#include "draw.cc"

bool draw_ok(){
  if( !draw_err ){ return true; }
  out_html( "\n<p>Sorry, I cannot draw it! " ); out_print( draw_err );
  draw_err = 0;
  return false;
}

#include "CFG.cc"


/* Print the equals or approximates sign depending on the type of xx. */
void print_eq_or_aprx( bool is_unprecise ){
  if( is_unprecise ){ out_am( "~~" ); }else{ out_am( '=' ); }
}


/* Print the variable values with which the error was found. */
void print_err_var_vals( const char *msg = "when " ){
  if( !var_f_cnt ){ return; }
  out_html( ' ' ); out_esc( msg );
  for( unsigned ii = 0; ii < var_f_cnt; ++ii ){
    if( ii ){
      if( ii == var_f_cnt - 1 ){ out_html( " and " ); }
      else{ out_html( ", " ); }
    }
    out_idx_name( ii );
    if( var_used[ ii ].dimension ){
      int sz_m1 = arr_hi - arr_lo;
      bool is_unprecise = false;
      for( int jj = 0; jj <= sz_m1; ++jj ){
        if( var_array[ jj ].is_unprecise_type() ){
          is_unprecise = true; break;
        }
      }
      print_eq_or_aprx( is_unprecise ); out_am( " [" );
      if( sz_m1 >= 0 ){ out_number( var_array[ 0 ] ); }
      for( int jj = 1; jj <= sz_m1; ++jj ){
        out_am( ", " ); out_number( var_array[ jj ] );
      }
      out_am( ']' );
    }else{
      print_eq_or_aprx( test_err[ ii ].is_unprecise_type() );
      out_number( test_err[ ii ] );
      if( test_err[ ii ].is_prop_rat_type() ){
        out_am( "~~" ); out_double( to_double( test_err[ ii ] ) );
      }
    }
  }
}


/* Print a table row of the form str = xx. */
void result_table_row( const char *str, number xx ){
  out_html( "<tr><td>" ); out_esc( str ); out_print( "</td><td>" );
  print_eq_or_aprx( xx.is_unprecise_type() );
  out_html( "</td><td class=right>" ); out_number( xx );
  if( xx.is_prop_rat_type() ){
    out_am( "~~" ); out_double( to_double( xx ) );
  }
  out_html( "</td></tr>\n" );
}


/* Generic error report */
expression *err_expr = 0;   // expression parameter of error messages
void err_report(){
  if( !err_mode ){ return; }
  ++err_cnt; check_level = 1;
  const char *mc_err_msg = 0;
  err_begin();
  switch( err_mode ){
    case err_inp:
      out_esc( err_msg );
      break;
    case err_parse:
      out_html( "Got " ); out_tkn( tkn_now );
      out_html( ", but expected any of these:" );
      for( unsigned ii = 0; ii < parse_cnt && ii < parse_max; ++ii ){
        out_html( ' ' ); out_tkn( parse_tkns[ ii ] );
      }
      if( parse_cnt > parse_max ){
        out_html( " and at most " ); out_print( parse_cnt - parse_max );
        out_print( " more" );
      }
      break;
    case err_var_type:
      out_html( "Variable type mismatch: Earlier on " );
      out_var_name( err_uns1 ); out_am( " in " );
      out_type_name( var_type( err_uns2 ) );
      out_html( ", now " );
      out_var_name( err_uns1 ); out_am( " in " );
      out_type_name( var_type( err_uns3 ) );
      break;
    case err_var_f_cnt:
      out_html( "Too many free variables" );
      for( unsigned ii = 0; ii < var_f_cnt; ++ii ){
        out_html( ' ' ); out_idx_name( ii );
      }
      break;
    case err_var_q_cnt:
      out_html( "Too many quantified variables" );
      for( unsigned ii = var_q_lo; ii <= var_q_hi; ++ii ){
        out_html( ' ' ); out_idx_name( ii );
      }
      break;
    case err_combs:
      out_html( "Too many variables (of big types)" );
      for( unsigned ii = 0; ii < var_f_cnt; ++ii ){
        out_html( ' ' ); out_idx_name( ii );
      }
      for( unsigned ii = var_q_lo; ii <= var_q_hi; ++ii ){
        out_html( ' ' ); out_idx_name( ii );
      }
      break;
    case err_hash: mc_err_msg = "MathCheck ran out of memory!"; break;
    case err_time:
      out_html( "Run time limit exceeded" );
      break;
    case err_dom:
      out_html( "Assumption is undefined" ); print_err_var_vals();
      break;
    case err_no_var:
      out_html(
        "The expressions must contain one variable (for the horizontal axis)"
      );
      break;
    case err_compl:
      out_html( err_msg );
      break;
    case err_numb:
      out_html( err_msg );
      out_number( err_num1 );
      break;
    case err_var_cnt:
      out_html( "Too many variables" );
      for( unsigned ii = 0; ii < var_f_cnt; ++ii ){
        out_html( ' ' ); out_idx_name( ii );
      }
      for( unsigned ii = var_q_lo; ii <= var_q_hi; ++ii ){
        out_html( ' ' ); out_idx_name( ii );
      }
      break;
    case err_op:
      out_op( op_type( err_uns1 ) );
      out_html( " was used in a wrong context" );
      if( err_msg ){ out_html( ": " ); out_html( err_msg ); }
      break;
    case err_CFG_in:
      out_html( '\"' ); out_hard( err_msg );
      out_print( "\" is in the language <i>" ); out_print( CFG::start0 );
      out_print( "</i>" );
      break;
    case err_CFG_not_in:
      out_html( '\"' ); out_hard( err_msg );
      out_print( "\" is not in the language <i>" ); out_print( CFG::start0 );
      out_print( "</i>" );
      break;
    case err_CFG_short:
      out_html( '\"' ); out_hard( err_msg );
      out_print( "\" is too short, minimum length is " );
      out_print( CFG_short );
      break;
    case err_CFG_long:
      out_html( '\"' ); out_hard( err_msg );
      out_print( "\" is too long, maximum length is " );
      out_print( CFG_long );
      break;
    case err_CFG_start:
      out_html( "Wrong start symbol " ); out_print( CFG::start1 );
      out_print( ", must be " ); out_print( CFG::start0 );
      break;
    case err_CFG_1:
      out_html( '\"' ); out_hard( err_msg );
      out_print( "\" belongs to the first language but not the second" );
      break;
    case err_CFG_2:
      out_html( '\"' ); out_hard( err_msg );
      out_print( "\" belongs to the second language but not the first" );
      break;
    case err_cmp:
      out_html( "Relation does not hold" );
      if( gs_only_no_yes ){ break; }
      print_err_var_vals();
      out_html( "\n<table" ); err_class(); out_print( ">\n" );
      result_table_row( "left", test_left );
      result_table_row( "right", test_right );
      out_html( "</table>" );
      if( test_e1 && test_e2 && gs_draw ){

        /* Choose the variable used on the horizontal axis. */
        unsigned vv = var_c_first;
        while( vv < var_f_cnt && var_used[ vv ].type != vtp_R ){ ++vv; }
        if( vv >= var_f_cnt ){ break; }

        /* Choose the horizontal axis range. In addition to a sample / pixel,
          there is a sample before the first, between any two, and after the
          last pixel, to decide how high a line to draw for the pixel. So the
          leftmost pixel corresponds to samples 0, 1, and 2, the next to
          samples 2, 3, and 4, and so on. */
        const unsigned
          nn = 803,                 // 1 + 2 * width of the image in pixels
          piuw = ( nn - 3 ) / 20;   // pixels in one unit width
        int i_left = ceil( var_used[ vv ].value ).to_int();
        if( i_left == INT_MIN ){ break; }
        if( -18 < i_left && i_left < -9 ){ i_left = -19; }
        else if( 9 < i_left && i_left < 20 ){ i_left = -1; }
        else{ i_left -= 10; }

        /* Compute the location of the y-axis. */
        unsigned x_zero = ~0u;
        if( -20 <= i_left && i_left <= 0 ){ x_zero = -i_left * piuw + 1; }

        /* Compute the y-values. */
        double dd[ 2 * nn ];
        var_used[ vv ].value = i_left - number( 1, piuw );
        for( unsigned ii = 0; ii < nn; ++ii ){
          if( eval_tv( dom_expr ) == tv_T ){
            dd[ ii + nn ] = to_double( eval_expr( test_e1 ) );
            dd[ ii ] = to_double( eval_expr( test_e2 ) );
          }else{ dd[ ii ] = dd[ ii + nn ] = 0/0.; }
          var_used[ vv ].value += number( 1, piuw );
        }

        /* Compute limits preventing going too far from error values and 0. */
        double lo_lim = 0., hi_lim = 0., yy = to_double( test_left );
        if( yy < lo_lim ){ lo_lim = yy; }
        else if( yy > hi_lim ){ hi_lim = yy; }
        yy = to_double( test_right );
        if( yy < lo_lim ){ lo_lim = yy; }
        else if( yy > hi_lim ){ hi_lim = yy; }
        yy = hi_lim - lo_lim; hi_lim += 2. * yy; lo_lim -= 2. * yy;
        if( lo_lim == 0. ){ lo_lim = -2.; hi_lim = 2.; }

        /* Draw the curves and tell the scales. */
        draw_reset_colours(); draw_colour( draw::black );
        draw_colour( draw::red ); draw_colour( draw::dgreen );
        draw_img_curve( 2, nn, dd, x_zero, 120, true, lo_lim, hi_lim );
        if( draw_ok() ){
          out_html( "<br>" ); out_am( i_left ); out_am( " <= " );
          out_idx_name( vv ); out_am( " <= " ); out_print( i_left + 20 );
          out_html( " and " ); out_double( draw::min_dy );
          out_am( " <= uarr <= " ); out_double( draw::max_dy );
        }

      }
      break;
    case err_equ1:
      out_html( "A teacher-given root was lost here" );
      break;
    case err_equ2:
      out_html( "The equation does not hold" );
      if( gs_only_no_yes ){ break; }
      print_err_var_vals();
      if( err_expr ){
        out_html( "\n<br>The first expression with this root may be " );
        err_expr->print_AM(); err_expr = 0;
      }
      break;
    case err_equ3:
      out_html( "Your root" ); print_err_var_vals( "" );
      out_html( " was not given by the teacher" );
      break;
    case err_equ4:
      out_html( "Implication was used without returning to the original" );
      break;
    case err_equ5:
      out_html( "Your root" ); print_err_var_vals( "" );
      out_html( " does not satisfy the assumption" );
      break;
    case err_qua:
      out_html( "The range of quantified variable " );
      out_var_name( err_uns1 );
      //??? out_html( " (or some earlier quantified variable)" );
      out_html( " has not been made small enough" );
      print_err_var_vals();
      break;
    case err_tree:
      out_html( "Your last tree is not the same as the teacher's tree" );
      break;
    default: mc_err_msg = "Unknown ordinary error code";
  }
  err_end();
  if(
    err_inp <= err_mode && err_mode <= err_combs &&
    tkn_now != tkn_eoi && tkn_now != tkn_eoa && inp_chr
  ){ html_err_buff( err_pos ); }
  if( mc_err_msg ){ mc_err_print( mc_err_msg ); }
}


/*** Input-reading-level checking features ***/


/* Returns true iff every variable in ee has smaller index than idx. */
bool no_late_vars( expression *ee, unsigned idx ){
  if( !ee ){ return true; }
  if( ee->opr() == op_var && ee->var_idx() >= idx ){ return false; }
  return no_late_vars( ee->left(), idx ) && no_late_vars( ee->right(), idx );
}


/* Returns true iff variable idx is present in ee. */
bool has_var( expression *ee, unsigned idx ){
  if( !ee ){ return false; }
  if( ee->opr() == op_var && ee->var_idx() == idx ){ return true; }
  return has_var( ee->left(), idx ) || has_var( ee->right(), idx );
}


/* Finds the smallest and greatest value that a comparison chain lets variable
  idx have. Assumes that variables with smaller indices do and others do not
  have values. */
void rel_range(
  expression *ee, unsigned idx,
  int & lo_idx, int & hi_idx, int & lo_chn, int & hi_chn
){

  /* Improve the bounds for variable idx, if possible. */
  if( ee->opr() == op_var && ee->var_idx() == idx ){
    if( lo_chn > lo_idx ){ lo_idx = lo_chn; }
    if( hi_chn < hi_idx ){ hi_idx = hi_chn; }
    return;
  }

  /* Process expressions of the form variable idx +- something. */
  if(
    ( ee->opr() == op_plus || ee->opr() == op_minus ) && ee->left() &&
    ee->left()->opr() == op_var && ee->left()->var_idx() == idx
  ){
    number nn = eval_expr( ee->right() );
    if( ee->opr() == op_minus ){ nn = -nn; }
    int new_chn = ( number( lo_chn ) - nn ).to_int();
    if( new_chn > lo_idx ){ lo_idx = new_chn; }
    new_chn = ( number( hi_chn ) - nn ).to_int();
    if( new_chn != INT_MIN && new_chn < hi_idx ){ hi_idx = new_chn; }
    return;
  }

  /* Process the remaining arithmetic expressions. */
  if( !is_rel( ee->opr() ) ){

    /* Improve the chain bounds and detect impossibilities, if possible. */
    if( no_late_vars( ee, idx ) ){
      int new_chn = eval_expr( ee ).to_int();
      if( new_chn == INT_MIN ){ return; }
      if( lo_chn > new_chn || hi_chn < new_chn ){ lo_idx = 1; hi_idx = 0; }
      lo_chn = hi_chn = new_chn;
    }

    return;
  }

  /* Process the local value or variable on the way left. */
  rel_range( ee->right(), idx, lo_idx, hi_idx, lo_chn, hi_chn );

  /* Move left over the current comparison operator. */
  if( ee->opr() == op_lt ){
    lo_chn = INT_MIN; if( hi_chn > INT_MIN ){ --hi_chn; }
  }else if( ee->opr() == op_lq ){ lo_chn = INT_MIN; }
  else if( ee->opr() == op_gt ){
    hi_chn = INT_MAX; if( lo_chn < INT_MAX ){ ++lo_chn; }
  }else if( ee->opr() == op_gq ){ hi_chn = INT_MAX; }
  else if( ee->opr() != op_eq ){ lo_chn = INT_MIN; hi_chn = INT_MAX; }

  /* Process the rest of the chain. */
  rel_range( ee->left(), idx, lo_idx, hi_idx, lo_chn, hi_chn );

  /* Move right over the current comparison operator. */
  if( ee->opr() == op_lt ){
    hi_chn = INT_MAX; if( lo_chn < INT_MAX ){ ++lo_chn; }
  }else if( ee->opr() == op_lq ){ hi_chn = INT_MAX; }
  else if( ee->opr() == op_gt ){
    lo_chn = INT_MIN; if( hi_chn > INT_MIN ){ --hi_chn; }
  }else if( ee->opr() == op_gq ){ lo_chn = INT_MIN; }
  else if( ee->opr() != op_eq ){ lo_chn = INT_MIN; hi_chn = INT_MAX; }

  /* Process the local value or variable on the way right. */
  rel_range( ee->right(), idx, lo_idx, hi_idx, lo_chn, hi_chn );

}


/* Finds values such that if the value of variable idx is < lo1, then the
  expression yields lv1, and similarly with > hi1 and hv1. */
void range(
  expression *ee, unsigned idx, int & lo1, bool & lv1, int & hi1, bool & hv1
){
  lo1 = INT_MIN; hi1 = INT_MAX; lv1 = hv1 = false;
  if( is_rel( ee->opr() ) ){
    int lo_chn = INT_MIN, hi_chn = INT_MAX;
    if( is_rel( ee->left()->opr() ) ){
      rel_range( ee, idx, lo1, hi1, lo_chn, hi_chn ); return;
    }
    op_type rel_op = ee->opr(); int nn = INT_MIN;
    if( ee->left()->opr() == op_var && ee->left()->var_idx() == idx ){
      if( no_late_vars( ee->right(), idx ) ){
        nn = eval_expr( ee->right() ).to_int();
      }
    }else if( ee->right()->opr() == op_var && ee->right()->var_idx() == idx ){
      rel_op = op_rel_swap( rel_op );
      if( no_late_vars( ee->left(), idx ) ){
        nn = eval_expr( ee->left() ).to_int();
      }
    }
    if( nn == INT_MIN ){ return; }
    lo1 = hi1 = nn;
    if( !( rel_op == op_lq || rel_op == op_gt ) ){ --lo1; }
    if( hi1 < INT_MAX && !( rel_op == op_lt || rel_op == op_gq ) ){ ++hi1; }
    lv1 = rel_op == op_lt || rel_op == op_lq || rel_op == op_nq;
    hv1 = rel_op == op_gt || rel_op == op_gq || rel_op == op_nq;
    return;
  }
  if( ee->opr() == op_not ){
    range( ee->right(), idx, lo1, lv1, hi1, hv1 );
    lv1 = !lv1; hv1 = !hv1; return;
  }
  if( !ee->left() && ( ee->opr() == op_forall || ee->opr() == op_exists ) ){
    range( ee->right(), idx, lo1, lv1, hi1, hv1 ); return;
  }
  int lo2 = INT_MIN, hi2 = INT_MAX; bool lv2 = false, hv2 = false;
  if( ee->opr() == op_and || ee->opr() == op_exists ){
    range( ee->left(), idx, lo1, lv1, hi1, hv1 );
    range( ee->right(), idx, lo2, lv2, hi2, hv2 );
    if( lv1 && lv2 ){ if( lo2 < lo1 ){ lo1 = lo2; } }
    else{ if( lv1 || ( !lv2 && lo2 > lo1 ) ){ lo1 = lo2; lv1 = false; } }
    if( hv1 && hv2 ){ if( hi2 > hi1 ){ hi1 = hi2; } }
    else{ if( hv1 || ( !hv2 && hi2 < hi1 ) ){ hi1 = hi2; hv1 = false; } }
    return;
  }
  if( ee->opr() == op_or ){
    range( ee->left(), idx, lo1, lv1, hi1, hv1 );
    range( ee->right(), idx, lo2, lv2, hi2, hv2 );
    if( !lv1 && !lv2 ){ if( lo2 < lo1 ){ lo1 = lo2; } }
    else{ if( !lv1 || ( lv2 && lo2 > lo1 ) ){ lo1 = lo2; lv1 = true; } }
    if( !hv1 && !hv2 ){ if( hi2 > hi1 ){ hi1 = hi2; } }
    else{ if( !hv1 || ( hv2 && hi2 < hi1 ) ){ hi1 = hi2; hv1 = true; } }
    return;
  }
  if( ee->opr() == op_larr ){
    range( ee->left(), idx, lo1, lv1, hi1, hv1 );
    range( ee->right(), idx, lo2, lv2, hi2, hv2 );
    if( !lv1 && lv2 ){ if( lo2 < lo1 ){ lo1 = lo2; } }
    else{ if( !lv1 || ( !lv2 && lo2 > lo1 ) ){ lo1 = lo2; lv1 = true; } }
    if( !hv1 && hv2 ){ if( hi2 > hi1 ){ hi1 = hi2; } }
    else{ if( !hv1 || ( !hv2 && hi2 < hi1 ) ){ hi1 = hi2; hv1 = true; } }
    return;
  }
  if( ee->opr() == op_rarr || ee->opr() == op_forall ){
    range( ee->right(), idx, lo1, lv1, hi1, hv1 );
    range( ee->left(), idx, lo2, lv2, hi2, hv2 );
    if( !lv1 && lv2 ){ if( lo2 < lo1 ){ lo1 = lo2; } }
    else{ if( !lv1 || ( !lv2 && lo2 > lo1 ) ){ lo1 = lo2; lv1 = true; } }
    if( !hv1 && hv2 ){ if( hi2 > hi1 ){ hi1 = hi2; } }
    else{ if( !hv1 || ( !hv2 && hi2 < hi1 ) ){ hi1 = hi2; hv1 = true; } }
    return;
  }
  if( ee->opr() == op_harr ){
    range( ee->left(), idx, lo1, lv1, hi1, hv1 );
    range( ee->right(), idx, lo2, lv2, hi2, hv2 );
    if( lo2 < lo1 ){ lo1 = lo2; }
    if( hi2 > lo1 ){ hi1 = hi2; }
    lv1 = lv1 == lv2; hv1 = hv1 == hv2;
    return;
  }
}


/* Variables for checking the solution chain of an equation */
bool roots_given = false;             // the teacher gave roots
std::vector< number > roots_val;      // teacher-given roots
std::vector< expression * > sol_step; // solution steps

/* Finds explicit roots from within an expression and checks that they satisfy
  the original equation. */
bool check_expl_root( expression *ee ){

  /* Navigate between alternative answers. */
  if( ee->opr() == op_or ){
    return check_expl_root( ee->left() ) && check_expl_root( ee->right() );
  }

  /* Recognize an explicitly given value. */
  if(
    ee->opr() == op_eq && ee->left()->opr() == op_var &&
    known_const( ee->right() )
  ){

    /* Check that the explicit value is in the domain and satisfies the
      equation. */
    var_used[ 0 ].value = eval_expr( ee->right() );
    if( eval_tv( dom_expr ) != tv_T ){ test_fail( err_equ5 ); return false; }
    if( tv_no_T( eval_tv( sol_step[ 0 ] ) ) ){
      for( unsigned ii = 1; ii < sol_step.size(); ++ii ){
        if( tv_may_T( eval_tv( sol_step[ ii ] ) ) ){
          err_expr = sol_step[ ii ]; break;
        }
      }
      test_fail( err_equ2 ); return false;
    }

    /* If there are teacher-given values, check that the explicit value is
      among them. */
    if( !roots_given ){ return true; }
    for( unsigned ii = 0; ii < roots_val.size(); ++ii ){
      if( tv_may_T( num_eq( var_used[ 0 ].value, roots_val[ ii ] ) ) ){
        return true;
      }
    }
    test_fail( err_equ3 );
    return false;

  }

  return true;
}


/* Checks that the roots are explicit, without checking anything else. */
bool return_is_explicit( expression *ee ){
  if( ee->opr() == op_or ){
    return
      return_is_explicit( ee->left() ) && return_is_explicit( ee->right() );
  }
  return ee->opr() == op_eq && ee->left()->opr() == op_var &&
    known_const( ee->right() );
}


/* By trying the teacher-given and student-found values, check that the
  equation is correctly solved. */
bool root_is_explicit = true;   // the checked expr is an explicit root
void check_equation( expression *ee, bool been_up ){
  for( unsigned ii = 0; ii < roots_val.size(); ++ii ){
    var_used[ 0 ].value = roots_val[ ii ];
    if( tv_no_T( eval_tv( ee ) ) ){ test_fail( err_equ1 ); return; }
  }
  sol_step.push_back( ee );
  if( !been_up ){
    if( ee == expr_F ){ root_is_explicit = true; }
    else{ root_is_explicit = check_expl_root( ee ); }
  }
}


/* Print debug information. */
void print_debug_expr( expression *ee, expression *ed = 0 ){
  out_html( "<span class=debug>" ); print_type( ee ); out_am( '=' );
  ee = simplify( ee ); ee->print_AM(); print_type( ee );
  if( ed ){ out_html( " domain " ); ed->print_AM(); }
  out_html( "</span><br>\n" );
}


/* If told to skip mild errors, do so. */
bool skip_mild_errors(){
  bool result = false;
  if( tkn_now == tkn_skip_errs ){
    if( !gs_exam && err_is_mild() ){
      err_report(); err_reset();
      pgh_msg( "Error skipped" ); --err_cnt; result = true;
    }
    get_token();
  }
  return result;
}


/* Parse and record domain restriction. */
void parse_assumption(){
  if( inp_ff_exam || !parse_is( tkn_assume ) ){ return; }
  get_token();

  /* Parse domain restriction. */
  pm_type pm_ass = pm_no_qua;
  if( pb_mode == pb_equation ){ pm_ass |= pm_no_new_free; }
  else if( pb_mode == pb_prop_logic ){ pm_ass |= pm_tv; }
  else if( pb_mode == pb_real_logic ){ test_combs = 0; }
  else if( pb_mode == pb_modulo ){ pm_ass |= pm_mod; }
  dom_expr = parse_predicate( pm_ass );
  out_html( "\n<p>Assume " ); dom_expr->print_AM(); out_html( '\n' );
  parse_settings();
  parse_pass( tkn_semic, tkn_enda );

  /* If the domain cannot be seen empty, do the test. */
  dom_expr = simplify( dom_expr );
  bool dom_empty = dom_expr == expr_F;
  if(
    pb_mode == pb_real_logic && !real_logic_root( dom_expr )
  ){ dom_empty = true; }
  if( !dom_empty && ( pb_mode == pb_prop_logic || pb_mode == pb_modulo ) ){
    first_test_combination(); dom_empty = true;
    do{
      truth_val t1 = eval_tv( dom_expr );
      if( tv_may_T( t1 ) ){ dom_empty = false; break; }
      // if( !gs_u_impl_f && tv_may_U( t1 ) ){ dom_empty = false; break; }
    }while( next_test_combination() );
  }

  if( dom_empty && !err_mode ){
    out_html(
      "<p class=warn>Warning: the assumption makes the domain empty\n"
    );
  }

  parse_settings( true );

}


/* Checking of an arithmetic relation chain */
void parse_arithm_relation_chain(){
  parse_assumption();

  now_expr = 0; expression *d2 = 0;
  op_type rel_op = op_type( 0 );
  bool rel_up = true, rel_dn = true;
  out_html( "\n<p>" ); pgh_broken = false;
  do{

    /* Get the next expression. */
    qua_swap(); qua_reset();  // are these needed? For summation in future?
    expression *prev_expr = now_expr, *d1 = d2;
    now_expr = parse_arithmetic( 0 );
    if(
      !( parse_is( tkn_eq ) || (
        !ls_ban_comp && ( !inp_ff_exam || ls_allow_comp ) && (
          ( rel_up && ( parse_is( tkn_lq, tkn_Ule ) || parse_is( tkn_lt ) ) )
          ||
          ( rel_dn && ( parse_is( tkn_gq, tkn_Uge ) || parse_is( tkn_gt ) ) )
        )
      ) || is_setting_tkn() || ( rel_op && parse_is_top_tkn() ) )
    ){ parse_error(); }
    if( !err_mode ){ d2 = l_and( dom_expr, domain( now_expr ) ); }

    /* Check the previous relation, if it exists. */
    if( !gs_exam && !err_mode && prev_expr ){
      if(
        gs_prove && d1 == d2 && l_or(
          new_logic( simplify( prev_expr ), rel_op, simplify( now_expr ) ),
          l_not( d1 )
        ) == expr_T
      ){ err_mode = err_proven; }
      else{ check_relation( prev_expr, now_expr, rel_op ); }
    }

    /* Print the previous relation symbol, if it exists. */
    pgh_ensure();
    if( err_mode && err_mode < err_time ){
      out_html( "<span" ); err_class(); out_print( '>' );
    }
    if( rel_op ){
      out_am( op_AM[ rel_op ] ); err_set_warning( check_level );
      if( err_proven <= err_mode && err_mode <= err_plausible ){
        out_html( "</span>" ); err_reset();
      }
      out_html( '\n' );
    }

    /* Print the expression. */
    if( ls_hide_expr ){
      out_html( "model-answer\n" ); ls_hide_expr = false;
    }else{ now_expr->print_AM(); out_html( '\n' ); }
    if( err_mode && err_mode < err_time ){ out_print( "</span>\n" ); }
    if( gs_debug ){ print_debug_expr( now_expr, d2 ); }

    /* If told to skip mild errors, do so */
    if( skip_mild_errors() ){ now_expr = prev_expr; d2 = d1; }

    /* Check the current token. */
    parse_settings();
    bool got_rel = false;
    if( !err_mode ){
      if(
        parse_is( tkn_eq ) ||
        ( !ls_ban_comp && ( !inp_ff_exam || ls_allow_comp ) && (
          ( rel_up && ( parse_is( tkn_lq, tkn_Ule ) || parse_is( tkn_lt ) ) )
          ||
          ( rel_dn && ( parse_is( tkn_gq, tkn_Uge ) || parse_is( tkn_gt ) ) )
        ) )
      ){
        if( tkn_now == tkn_lq || tkn_now == tkn_Ule || tkn_now == tkn_lt ){
          rel_dn = false;
        }
        if( tkn_now == tkn_gq || tkn_now == tkn_Uge || tkn_now == tkn_gt ){
          rel_up = false;
        }
        got_rel = true;
      }
      else if( !( rel_op && parse_is_top_tkn() ) ){ parse_error(); }
    }

    /* If the relation chain continues, remember the relation operator. */
    if( !err_mode && got_rel ){
      if( err_pos < 2 ){ pgh_broken = true; }
      rel_op = op_tkn( tkn_now ); get_token(); parse_settings();
    }

    /* Otherwise terminate. */
    else{ rel_op = op_type( 0 ); }
  }while( rel_op );
}


/* Copy the nonempty tree to a tree drawing data structure. */
const char *too_big_tree = "Sorry, cannot draw, some node is too big";
draw_tree_node *copy_tree( expression *ee ){

  /* Construct name. */
  op_type opr = ee->opr();
  unsigned nb = 0;
  const char *name = 0; bool low_draw = false;
  if( opr == op_var ){
    unsigned nm = ee->var().name;
    draw::name_buff[ nb++ ] = nm <= 'z' ? nm : nm - 'z' - op_alpha + 128;
  }else if( opr == op_const ){
    switch( ee->val().type ){
    case number::und: name = "undefined"; break;
    case number::zer: name = "0"; break;
    case number::pos: case number::neg: {
      unsigned nn = ee->val().r_nu, digits = 0;
      if( ee->val().is_neg() ){ draw::name_buff[0] = '-'; ++digits; }
      do{ nn /= 10; ++digits; }while( nn );
      nn = ee->val().r_de;
      if( nn != 1 ){
        ++digits;
        do{ nn /= 10; ++digits; }while( nn );
      }
      if( digits > draw::name_max ){
        draw::name_buff[ nb++ ] = op_font[ op_const ];
      }else{
        nb = digits; nn = ee->val().r_de;
        if( nn != 1 ){
          do{ draw::name_buff[ --digits ] = nn % 10 + '0'; nn /= 10; }
          while( nn );
          draw::name_buff[ --digits ] = '/';
        }
        nn = ee->val().r_nu;
        do{ draw::name_buff[ --digits ] = nn % 10 + '0'; nn /= 10; }
        while( nn );
      }
      break;
    }
    case number::dbl: name = "dbl"; break;
    case number::dbu: name = "dbu"; break;
    case number::trv: {
      truth_val tv = ee->val().to_tv();
      if( tv_may_F( tv ) ){ draw::name_buff[ nb++ ] = 15; }
      if( tv_may_U( tv ) ){ draw::name_buff[ nb++ ] = 16; }
      if( tv_may_T( tv ) ){ draw::name_buff[ nb++ ] = 17; }
      break;
    }
    default: draw::name_buff[ nb++ ] = op_font[ op_const ];
    }
  }
  else if( is_sin_like( opr ) ){ name = op_AM[ opr ]; }
  else if( opr == op_ddn ){ name = "ddn"; }
  else if( opr == op_dup ){ name = "dup"; }
  else if( opr == op_idiv ){ name = "div"; }
  else if( opr == op_imod ){ name = "mod"; }
  else if( is_rel( opr ) ){
    unsigned ii = 1; expression *e2 = ee->left(); low_draw = true;
    while( is_rel( e2->opr() ) && ii < draw::name_max ){
      e2 = e2->left(); ++ii;
    }
    if( is_rel( e2->opr() ) ){ err_set_inp( too_big_tree ); }
    nb = ii;
    for( e2 = ee; ii; e2 = e2->left() ){
      draw::name_buff[ --ii ] = op_font[ e2->opr() ];
    }
  }
  else if( is_lrel( opr ) ){
    unsigned ii = 1; expression *e2 = ee->left(); low_draw = true;
    while( is_lrel( e2->opr() ) && ii < draw::name_max ){
      e2 = e2->left(); ++ii;
    }
    if( is_lrel( e2->opr() ) ){ err_set_inp( too_big_tree ); }
    nb = ii;
    for( e2 = ee; ii; e2 = e2->left() ){
      draw::name_buff[ --ii ] = op_font[ e2->opr() ];
    }
  }
  else{ draw::name_buff[ nb++ ] = op_font[ opr ]; }
  draw::name_buff[ nb ] = '\0';

  /* Copy the node. */
  draw_tree_node
    *nd = new draw_tree_node(
      name ? name : draw::name_buff, ee->extra_uns(), ee->extra_uns() >> 8,
      low_draw
    );

  /* Copy the children. */
  if( ee->right() ){ nd->add_child( copy_tree( ee->right() ) ); }
  if( is_rel( opr ) ){
    expression *e2 = ee->left();
    for( ; is_rel( e2->opr() ); e2 = e2->left() ){
      nd->add_child( copy_tree( e2->right() ) );
    }
    nd->add_child( copy_tree( e2 ) );
  }else if( is_lrel( opr ) ){
    expression *e2 = ee->left();
    for( ; is_lrel( e2->opr() ); e2 = e2->left() ){
      nd->add_child( copy_tree( e2->right() ) );
    }
    nd->add_child( copy_tree( e2 ) );
  }else if( ee->left() && opr != op_deriv ){
    nd->add_child( copy_tree( ee->left() ) );
  }
  if(
    opr == op_deriv || opr == op_forall || opr == op_exists || opr == op_sum
  ){ nd->add_child( copy_tree( new_expr( op_var, ee->val() ) ) ); }

  return nd;
}


/* Parsing an arithmetic expression, logical expression, or logical chain */
void parse_generalized_expression(){
  par_expr = 0;
  now_expr = parse_pred_or_expr( 0 );
  if( par_expr ){ now_expr = par_expr; par_expr = 0; }
  else{
    while(
      parse_is( tkn_lArr, tkn_UlArr ) || parse_is( tkn_hArr, tkn_UhArr ) ||
      parse_is( tkn_rArr, tkn_UrArr ) || parse_is( tkn_iden, tkn_Uiden )
    ){
      op_type rel_op = op_tkn( tkn_now ); get_token();
      now_expr = new_expr( now_expr, rel_op, parse_predicate( 0 ) );
    }
  }
}


/* Draw expression trees of a chain of expressions. */
void parse_tree_chain(){

  /* Scan the input until an expression or exit. */
  test_combs = 0;
  parse_settings();
  while( !parse_is_top_tkn() ){
    if( parse_is( tkn_semic ) ){ get_token(); parse_settings(); continue; }

    /* Get the next expression. */
    parse_generalized_expression();

    /* Print the expression. */
    out_html( "\n<p>" ); pgh_broken = false;
    if( ls_hide_expr ){
      out_print( "model-answer\n" ); ls_hide_expr = false;
    }else{ now_expr->print_AM(); out_html( '\n' ); }
    if( gs_debug ){ print_debug_expr( now_expr, expr_T ); }

    /* Copy the tree to a tree drawing data structure and draw it. */
    if( !gs_exam && !err_mode ){
      draw_reset_colours(); draw_colour( draw::white );
      draw_colour( draw::maroon );
      now_expr->recursive_extra( 2 | (1<<8) );
      draw_tree_node *nd = copy_tree( now_expr );
      if( !err_mode ){ nd->draw( "An expression tree" ); draw_ok(); }
    }

    /* Continue with the input. */
    skip_mild_errors();
    parse_settings();
    if( err_mode ){ return; }

  }
}


/* Compare and draw expression trees of a chain of expressions. */
void parse_tree_compare(){
  expression *hidden_expr = 0;  // teacher's expression
  unsigned round = 0;   // must be at least two rounds

  /* Scan the input until an expression or exit. */
  parse_settings();
  while( round < 2 || !parse_is_top_tkn() ){
    if( parse_is( tkn_semic ) ){ get_token(); parse_settings(); continue; }

    /* Get the next expression. */
    qua_reset();
    parse_generalized_expression();

    /* Copy the tree to a tree drawing data structure and draw it. */
    out_html( "\n<p>" ); pgh_broken = false;
    if( !gs_exam && !gs_only_no_yes && !err_mode ){
      draw_reset_colours(); draw_colour( draw::white );
      if( !round ){
        draw_colour( draw::maroon );
        now_expr->recursive_extra( 2 | (1<<8) );
        hidden_expr = now_expr;
      }else{
        draw_colour( draw::dgreen ); draw_colour( draw::red );
        now_expr->recursive_extra( 3 );
        hidden_expr->recursive_extra( 2 | (1<<8) );
      }
      draw_tree_node *nd = copy_tree( now_expr );
      if( !err_mode ){ nd->draw( "An expression tree" ); draw_ok(); }
    }

    /* Print the expression. */
    if( !round || ls_hide_expr ){ out_html( "model-answer\n" ); }
    else{ now_expr->print_AM(); out_html( '\n' ); }
    if( gs_debug ){ print_debug_expr( now_expr, expr_T ); }

    /* Continue with the input. */
    skip_mild_errors();
    parse_settings();
    if( err_mode ){ return; }

    ++round;
  }
  if( !gs_exam && now_expr != hidden_expr ){ test_fail( err_tree ); }
}


/* Get a possibly negated integer or decimal value. */
double parse_d_const(){
  double dd = 0/0.;
  bool is_neg = parse_is( tkn_Uminus, tkn_minus );
  if( is_neg ){ get_token(); }
  if( parse_is( tkn_number ) || parse_is( tkn_decimal ) ){
    dd = to_double( tkn_val ); get_token();
    if( is_neg ){ dd = -dd; }
  }else if( is_neg ){ parse_error(); }
  return dd;
}


/* Drawing at most two functions */
void parse_draw_function_chain(){
   parse_assumption();

  /* Read the optional scaling. */
  double x_left, x_right, y_down, y_up;
  x_left = parse_d_const();
  x_right = parse_d_const();
  if( !( x_left == x_left ) ){ x_left = -10; }
  if( !( x_right == x_right ) ){
    x_right = x_left < 0 ? -x_left : x_left + 20;
  }
  if( x_left >= x_right ){
    err_set_inp( "Right end must be bigger than left end" ); return;
  }
  y_down = parse_d_const();
  y_up = parse_d_const();
  if( !( y_up == y_up ) ){ y_up = y_down < 0 ? -y_down : y_down + 10; }
  if( y_down >= y_up ){
    err_set_inp( "Top end must be bigger than bottom end" ); return;
  }

  /* Pass the separator and optional settings. */
  parse_pass( tkn_semic );
  parse_settings();

  /* Initialize the drawing colours. */
  draw_reset_colours(); draw_colour( draw::black );

  /* Read the functions. */
  expression *ee[ draw_max_curves ] = {};
  unsigned nr_funcs = 0;
  for( ; nr_funcs < draw_max_curves; ++nr_funcs ){

    /* Get the next expression. */
    now_expr = parse_arithmetic( 0 );
    if(
      !( parse_is( tkn_semic ) || is_setting_tkn() || parse_is_top_tkn() )
    ){ parse_error(); }
    expression *dd = l_and( dom_expr, domain( now_expr ) );

    /* Choose colour, then print and store the expression. */
    unsigned cc =
      nr_funcs == 0 ? draw::dgreen :
      nr_funcs == 1 ? draw::red :
      nr_funcs == 2 ? draw::blue :
      nr_funcs == 3 ? draw::fuchsia :
      nr_funcs == 4 ? draw::orange :
      nr_funcs == 5 ? draw::maroon : 0;
    draw_colour( cc );
    out_html( "\n<p style=\"color: #" );
    for( unsigned ii = 24; ii; ){
      ii -= 4;
      char ch = ( cc >> ii ) & '\xF';
      if( ch <= '\x9' ){ out_print( char( ch + '0' ) ); }
      else{ out_print( char( ch + 'W' ) ); }
    }
    out_print( ";\">" );
    pgh_broken = false;
    if( ls_hide_expr ){
      out_print( "model-answer\n" ); ls_hide_expr = false;
    }else{ now_expr->print_AM(); out_html( '\n' ); }
    if( gs_debug ){ print_debug_expr( now_expr, dd ); }
    ee[ nr_funcs ] = now_expr;

    /* Process optional continuation command. */
    parse_settings();
    if( parse_is_top_tkn() ){ ++nr_funcs; break; }
    else if( parse_is( tkn_semic ) ){
      get_token(); parse_settings();
      if( parse_is_top_tkn() ){ ++nr_funcs; break; }
    }else{ parse_error(); }

    if( err_mode ){ return; }
  }

  /* Choose the variable used on the horizontal axis. */
  unsigned vv = var_c_first;
  if( vv >= var_f_cnt ){ test_fail( err_no_var ); return; }
  parse_ensure_real( vv );
  if( err_mode ){ return; }
  // if( var_used[ vv ].type != vtp_R ){
  //   err_set_inp( "The variable must be of real number type" ); return;
  // }

  if( !gs_exam ){
    /* See above the first draw_img_curve call for explanation of the
      horizontal axis. */
    const unsigned nn = 1603;   // 1 + 2 * width of the image in pixels
    double wos = ( x_right - x_left ) / ( nn - 3 );   // width of sample

    /* Compute the location of the y-axis. */
    unsigned x_zero = ~0u;
    if( x_left <= 0. && 0. <= x_right ){ x_zero = -x_left / wos + 1; }

    /* Compute the y-values. */
    double dd[ draw_max_curves * nn ], xx = x_left - wos;
    for( unsigned ii = 0; ii < nn; ++ii ){
      var_used[ vv ].value = number( number::dbl, xx, xx );
      if( eval_tv( dom_expr ) == tv_T ){
        for( unsigned jj = 0; jj < nr_funcs; ++jj ){
          dd[ nn * jj + ii ] = to_double( eval_expr( ee[ jj ] ) );
        }
      }else{
        for( unsigned jj = 0; jj < nr_funcs; ++jj ){
          dd[ nn * jj + ii ] = 0/0.;
        }
      }
      xx += wos; // = x_left + ii * wos;
    }

    /* Draw the curves and tell the scales. */
    out_html( "\n<p>" ); pgh_broken = false;
    draw_img_curve(
      nr_funcs, nn, dd, x_zero, 250, !( y_down == y_down ), y_down, y_up
    );
    if( draw_ok() ){
      out_print( "<br>" ); out_double( x_left ); out_am( " <= " );
      out_idx_name( vv ); out_am( " <= " ); out_double( x_right );
      out_html( " and " ); out_double( draw::min_dy );
      out_am( " <= uarr <= " ); out_double( draw::max_dy );
    }
  }

}


/* Checking of a logic chain of <=>, ==>, <==, and/or === */
void parse_logic_chain( pm_type pm_now, expression *parent = 0 ){

  /* Process optional domain restriction. */
  parse_assumption();
  if( err_mode ){ return; }

  /* Prepare for processing expressions and relations. Because a lot of things
    is done on each expression, every one of them is processed in the loop
    although the first loop lacks a relation operator. Because a relation is
    processed only after its next expression has been parsed, information is
    preserved between rounds and processed fairly late. */
  now_expr = 0; qua_set = false;
  expression *original = 0;
  op_type rel_op = op_type( 0 );
  bool rel_up = true, rel_dn = pb_mode != pb_equation, been_up = false;
  if( pb_mode == pb_real_logic ){ test_combs = 0; }
  out_html( "\n<p>" ); pgh_broken = false;
  do{

    /* Get the next logical claim. */
    qua_swap(); qua_reset();
    expression *prev_expr = now_expr;
    bool is_parent = !rel_op && parent && parse_is( tkn_original );
    if( is_parent ){
      now_expr = parent; qua_set = false; qua_reset(); get_token();
    }else{ now_expr = parse_predicate( pm_now ); }
    if( !rel_op ){ original = now_expr; }

    /* Check that the claim ends appropriately (no syntax error). */
    if(
      !(
        parse_is( tkn_hArr, tkn_UhArr ) ||
        ( pb_mode != pb_equation && parse_is( tkn_iden, tkn_Uiden ) ) ||
        ( rel_up && !ls_ban_comp && ( !inp_ff_exam || ls_allow_comp ) &&
          parse_is( tkn_rArr, tkn_UrArr ) ) ||
        ( rel_dn && !ls_ban_comp && ( !inp_ff_exam || ls_allow_comp ) &&
          parse_is( tkn_lArr, tkn_UlArr ) ) ||
        parse_is( tkn_original ) || parse_is( tkn_subproof ) ||
        is_setting_tkn() ||
        ( rel_op && !parent && parse_is_top_tkn() ) ||
        ( rel_op && parent && parse_is( tkn_subend ) )
      )
    ){
      if(
        ( !parent && parse_is_top_tkn() ) ||
        ( parent && parse_is( tkn_subend ) )
      ){
        now_expr->print_AM(); out_html( '\n' );
        prev_expr = now_expr; now_expr = expr_T; rel_op = op_iden;
      }else{ parse_error(); }
    }

    /* Check a step. */
    if( !err_mode ){

      /* Check an equation and record whether it explicitly shows roots. */
      if( pb_mode == pb_equation ){
        if( !gs_exam ){ check_equation( now_expr, been_up ); }
        if( !been_up ){
          if( now_expr == expr_F ){ root_is_explicit = true; }
          else{ root_is_explicit = return_is_explicit( now_expr ); }
        }
      }

      /* Check the previous relation, if it exists. */
      else if( !gs_exam && prev_expr ){
        if( pb_mode == pb_real_logic ){
          real_logic_check( prev_expr, now_expr, rel_op );
        }else{ check_logic( prev_expr, now_expr, rel_op ); }
      }

    }

    /* Print the previous relation symbol, if it exists. */
    pgh_ensure();
    if( err_mode && err_mode < err_time ){
      out_html( "<span" ); err_class(); out_print( '>' );
    }
    if( rel_op ){
      out_am( op_AM[ rel_op ] ); err_set_warning( check_level );
      if( err_proven <= err_mode && err_mode <= err_plausible ){
        out_html( "</span>" ); err_reset();
      }
      out_html( '\n' );
    }

    /* Print the expression. */
    if( is_parent ){ out_html( "Parent's original\n" ); is_parent = false; }
    else if( ls_hide_expr ){
      out_html( "model-answer\n" ); ls_hide_expr = false;
    }else{ now_expr->print_AM(); out_html( '\n' ); }
    if( err_mode && err_mode < err_time ){ out_print( "</span>\n" ); }

    /* If debug mode, print the simplified expression. */
    if( gs_debug ){ print_debug_expr( now_expr ); }

    /* If told to skip mild errors, do so */
    if( skip_mild_errors() ){ now_expr = prev_expr; qua_swap(); }

    /* Process optional return to original, and its preceding subproofs. */
    parse_settings();
    if( pb_mode != pb_equation && parse_is( tkn_subproof ) ){
      do{
        out_html( "\n<p style=color:navy>Subproof" );
        get_token(); parse_settings();
        parse_logic_chain( pm_now, original ); parse_pass( tkn_subend );
        out_html( "<p style=color:navy>Subend\n" ); parse_settings();
      }while( parse_is( tkn_subproof ) );
      if( !parse_is( tkn_original ) ){ parse_error(); }
    }
    if( parse_is( tkn_original ) ){
      now_expr = pb_mode == pb_equation ? 0 : original; rel_op = op_type( 0 );
      rel_up = true; rel_dn = pb_mode != pb_equation; been_up = false;
      out_html( "\n<p>Original " ); pgh_broken = false;
      get_token(); parse_settings();
    }

    /* Check the current token. */
    parse_settings();
    bool got_rel = false;
    if( !err_mode ){
      if(
        parse_is( tkn_hArr, tkn_UhArr ) ||
        ( pb_mode != pb_equation && parse_is( tkn_iden, tkn_Uiden ) ) ||
        ( rel_up && !ls_ban_comp && ( !inp_ff_exam || ls_allow_comp ) &&
          parse_is( tkn_rArr, tkn_UrArr ) ) ||
        ( rel_dn && !ls_ban_comp && ( !inp_ff_exam || ls_allow_comp ) &&
          parse_is( tkn_lArr, tkn_UlArr ) )
      ){
        if( tkn_now == tkn_rArr || tkn_now == tkn_UrArr ){
          rel_dn = false; been_up = true;
        }
        if( tkn_now == tkn_lArr || tkn_now == tkn_UlArr ){ rel_up = false; }
        got_rel = true;
      }else if(
        !(
          ( rel_op && !parent && parse_is_top_tkn() ) ||
          ( rel_op && parent && parse_is( tkn_subend ) )
        )
      ){ parse_error(); }
    }

    /* If the relation chain continues, remember the relation operator. */
    if( !err_mode && got_rel ){
      if( err_pos < 2 ){ pgh_broken = true; }
      rel_op = op_tkn( tkn_now ); get_token(); parse_settings();
    }

    /* Otherwise terminate. */
    else{ rel_op = op_type( 0 ); }
  }while( rel_op );

  /* Report one-sided equation solutions. */
  if( !err_mode && been_up && pb_mode == pb_equation ){ err_mode = err_equ4; }

}


/* Comparing two predicates on an array. */
void check_array_claim( expression *e1, expression *e2, bool U_is_F ){
  if( err_mode ){ return; }

  /* Start with the empty array. */
  unsigned sz = 0;
  arr_hi = arr_lo - 1; var_used[ arr_sz_var ].value = arr_hi - arr_ofst;
  while( true ){

    /* Try the selected combinations of values of free variables. */
    first_test_combination();
    if( U_is_F ){
      do{
        truth_val t1 = eval_tv( e1 ); if( err_mode ){ return; }
        truth_val t2 = eval_tv( e2 ); if( err_mode ){ return; }
        if( U_to_F( t1 ) != U_to_F( t2 ) ){ test_fail( t1, t2 ); return; }
      }while( next_test_combination() );
    }else if( e1 ){
      do{
        truth_val t1 = eval_tv( e1 ); if( err_mode ){ return; }
        truth_val t2 = eval_tv( e2 ); if( err_mode ){ return; }
        if( t1 != t2 ){ test_fail( t1, t2 ); return; }
      }while( next_test_combination() );
    }else{
      do{
        truth_val t2 = eval_tv( e2 ); if( err_mode ){ return; }
        if( t2 == tv_U ){ test_fail( err_plausible ); return; }
      }while( next_test_combination() );
    }

    /* Make the array have its next contents. */
    unsigned ii = 0;
    for( ; ii < sz; ++ii ){
      int jj = var_array[ ii ].to_int();
      if( jj >= int( arr_max_size ) + arr_test_min - 1 ){
        var_array[ ii ] = arr_test_min;
      }else{ var_array[ ii ] = jj + 1; break; }
    }
    if( ii >= sz ){
      if( sz >= arr_max_size ){ break; }
      var_array[ sz ] = arr_test_min; ++sz;
      var_used[ arr_sz_var ].value = ++arr_hi - arr_ofst;
    }

  }

}


/* Parses and checks a declaration of an array. */
void parse_array_start(){
  ls_f_ban_U = true;

  /* Pick the array and its lowest index. */
  if( !parse_is_var( vtp_none, 0 ) ){ parse_error(); return; }
  unsigned v_arr = to_var_idx( tkn_val );
  get_token(); parse_pass( tkn_lB );
  expression *e1 = parse_arithmetic( pm_int | pm_no_var );
  if( err_mode ){ return; }
  arr_lo = eval_expr( e1 ).to_int();
  if( arr_lo == INT_MIN ){
    err_set_inp( "An integer value not too far from 0 is needed here" );
    return;
  }

  /* Pick the size variable and its offset. */
  parse_pass( tkn_3dot );
  if( !parse_is_var( vtp_idx, 0 ) ){ parse_error(); return; }
  arr_sz_var = to_var_idx( tkn_val );
  get_token(); arr_ofst = 0;
  if( parse_is( tkn_plus ) || parse_is( tkn_Uminus, tkn_minus ) ){
    e1 = parse_arithmetic( pm_int | pm_no_var );
    if( err_mode ){ return; }
    arr_ofst = eval_expr( e1 ).to_int();
    if( arr_ofst == INT_MIN ){
      err_set_inp( "An integer value not too far from 0 is needed here" );
      return;
    }
  }
  parse_pass( tkn_rB );
  if( err_mode ){ return; }

  /* Print the array definition. */
  if( !gs_verbose ){ out_html( "\n<p>" ); }else{ out_html( ' ' ); }
  out_idx_name( v_arr ); out_am( '[' );
  out_print( arr_lo ); out_html( "&hellip;" ); out_idx_name( arr_sz_var );
  if( arr_ofst > 0 ){ out_am( '+' ); }
  if( arr_ofst ){ out_am( arr_ofst ); }
  out_am( ']' );

  /* Pick the smallest tested element value, if given. */
  arr_test_min = 0;
  if( parse_is( tkn_arr_test ) ){
    get_token();
    bool is_neg = false;
    if( parse_is( tkn_Uminus, tkn_minus ) ){ is_neg = true; get_token(); }
    if( parse_is( tkn_number ) ){ arr_test_min = tkn_val.to_int(); }
    if( is_neg ){ arr_test_min = -arr_test_min; }
    if( gs_verbose ){
      pgh_msg( "Minimal test value = " ); out_print( arr_test_min );
    }
    parse_pass( tkn_number );
  }

  /* Update the array variable data. The value of the array variable itself
    should not be used. A weird value is given to reveal unintended use. */
  var_used[ v_arr ].dimension = 1; var_used[ v_arr ].value = -98765;

  /* Check that the right number of free variables was created. */
  if( var_f_cnt != 2 ){ mc_err_print( "Wrong number of array variables" ); }

}


/* Parses and checks a sequence of predicates on a previously declared array.
  The latter predicates must match the first. The first predicate may and the
  latter may not introduce new free variables. */
void parse_array_chain(){

  /* Check that the array variables are ok and update variable count info. */
  if( arr_sz_var != 1 ){
    mc_err_print( "Array variable index" ); return;
  }
  /*???var_f_cnt = */var_c_first = 2; test_combs = 0;

  /* Read the reference predicate and prepare for reading the rest. */
  qua_reset();
  expression *e1 = parse_predicate( pm_int );
  if( err_mode ){ return; }
  now_expr = 0;
  out_html( "\n<p>model-answer\n" ); pgh_broken = false;
  parse_settings();
  if(
    !( parse_is( tkn_hArr, tkn_UhArr ) || parse_is( tkn_iden, tkn_Uiden ) )
  ){ parse_error(); }
  if( err_mode ){ return; }
  op_type rsn_op = op_tkn( tkn_now );
  get_token(); parse_settings();
  do{

    /* Get the next logical claim. */
    qua_swap(); qua_reset();
    now_expr = parse_predicate( pm_no_new_free | pm_int );
    if(
      !(
        parse_is( tkn_hArr, tkn_UhArr ) || parse_is( tkn_iden, tkn_Uiden ) ||
        is_setting_tkn() || parse_is_top_tkn()
      )
    ){ parse_error(); }

    /* Check the previous logical equivalence. */
    if( !err_mode && e1 ){
      check_array_claim( e1, now_expr, rsn_op == op_leq );
      if( gs_exam && err_mode && err_mode != err_qua ){ err_reset(); }
    }

    /* Print the previous logical equivalence symbol. */
    pgh_ensure();
    if( err_mode && err_mode < err_time ){
      out_html( "<span" ); err_class(); out_print( '>' );
    }
    out_am( op_AM[ rsn_op ] ); err_set_warning( check_level );
    if( err_proven <= err_mode && err_mode <= err_plausible ){
      out_html( "</span>" ); err_reset();
    }
    out_html( '\n' );

    /* Print the expression. */
    now_expr->print_AM(); out_html( '\n' );
    if( err_mode && err_mode < err_time ){ out_print( "</span>\n" ); }

    /* If debug mode, print the simplified expression. * /???
    if( gs_debug ){
      out_html( "<span class=debug>" ); print_type( now_expr );
      out_am( '=' );
      expression *ee = simplify( now_expr );
      ee->print_AM(); print_type( ee );
      out_html( "</span><br>\n" );
    }*/

    /* If told to skip mild errors, do so */
    if( skip_mild_errors() ){}

    /* Check the current token. */
    parse_settings();
    rsn_op = op_type( 0 );
    if( !err_mode ){
      if(
        parse_is( tkn_hArr, tkn_UhArr ) || parse_is( tkn_iden, tkn_Uiden )
      ){ rsn_op = op_tkn( tkn_now ); e1 = now_expr; }
      else if( !parse_is_top_tkn() ){ parse_error(); }
    }

    /* If the equivalence chain continues, continue its processing. */
    if( !err_mode && rsn_op ){
      if( err_pos < 2 ){ pgh_broken = true; }
      get_token(); parse_settings();
    }

    /* Otherwise terminate. */
  }while( rsn_op );

}


/* Parse teacher-given roots. */
void parse_given_roots(){
  roots_val.clear(); sol_step.clear(); roots_given = false;

  /* Read the variable that is solved. */
  if( parse_is_var( vtp_none, 0 ) ){
    ls_solve = var_used[ to_var_idx( tkn_val ) ].name;
    if( gs_verbose ){ out_var_name( ls_solve ); }
  }
  else{ ls_solve = 0; err_set_inp( "A variable is needed here" ); }
  if( err_mode ){ return; }
  get_token();

  /* If no roots were given, just pass the end marker. */
  if( parse_is( tkn_semic, tkn_ends ) ){ get_token(); return; }
  roots_given = true;

  /* Process explicit indication of no roots. */
  if( parse_is( tkn_FF ) ){
    get_token(); parse_pass( tkn_semic, tkn_ends ); return;
  }

  /* Read the roots. */
  parse_pass( tkn_eq );
  if( err_mode ){ return; }
  roots_val.push_back( eval_expr( parse_arithmetic( pm_no_var ) ) );
  while( parse_is( tkn_vv, tkn_du, tkn_2vbar, tkn_or, tkn_Uor ) ){
    get_token();
    if( !parse_is_var( vtp_none, pm_no_new_free ) ){
      err_set_inp( "An old variable is needed here" );
    }
    if( err_mode ){ return; }
    get_token(); parse_pass( tkn_eq );
    if( err_mode ){ return; }
    roots_val.push_back( eval_expr( parse_arithmetic( pm_no_var ) ) );
  }

  /* Pass the end marker. */
  parse_pass( tkn_semic, tkn_ends );

}


#include "instructions.cc"


/*** Main program and its small helpers ***/


/* Recognize problem mode token. If match, the current problem mode becomes
  pbt, msg is printed, and tkn_2 causes the global settings be reset. */
bool is_problem_mode(
  tkn_type tkn_1, tkn_type tkn_2, pb_type pbt, const char *msg
){
  if(
    !inp_ff_exam &&
    ( parse_is( tkn_1 ) || ( tkn_2 != tkn_err && parse_is( tkn_2 ) ) )
  ){
    bool was_gs_verbose = gs_verbose;
    if( was_gs_verbose ){
      out_html( "\n<p><hr>\n" );
      if( msg ){ pgh_msg( msg ); }
    }
    if( tkn_now == tkn_2 ){ reset_global_settings(); }
    pb_mode = pbt; get_token(); parse_settings( true ); return true;
  }else{ return false; }
}


int main(){

  /* Switch the debug mode on, if the compilation command requested so. */
#ifdef Debug
  gs_debug = true;
#endif

  /* Initializations */
  time_reset();                     // start run time measurement
  reset_global_settings();
#ifdef subhtml
  html_ptr = &std::cout;
#else
  html_begin( std::cout, "MathCheck Feedback" );  // start writing the output
#endif
  inp_deconfuse_on(); inp_start(); get_token();   // start reading the input
  if( tkn_now == tkn_verbose_on ){
    gs_verbose = true;
    out_html( "<h1>MathCheck Feedback</h1>\n\n" );
    out_print( "<p class=unimp>MathCheck version " );
    html_date( copyright::date ); out_print( '\n' );
  }

  /* If debug mode is on, do the internal checkings. */
  if( gs_debug || tkn_now == tkn_debug_on ){
    tv_check();
    num_check();
    tkn_check();
    op_check();
  }

  /* Check a solution sequence. */
  unsigned points = 0;  // total points
  do{

    /* Reset relevant variables. */
    expression::h_reset(); qua_reset();
    var_f_max = var_max; var_f_cnt = var_c_first = 0; test_combs = 1;
    mod_rel = false; check_level = 0;
    //??? Loytyyko parempi ratkaisu kuin toisto?
    expr_F = new_expr( tv_F );
    expr_U = new_expr( tv_U );
    expr_T = new_expr( tv_T );
    expr_FU = new_expr( tv_FU );
    expr_FT = new_expr( tv_FT );
    expr_UT = new_expr( tv_UT );
    expr_FUT = new_expr( tv_FUT );
    expr_0 = new_expr( 0 );
    expr_1 = new_expr( 1 );
    expr_m1 = new_expr( -1 );
    expr_2 = new_expr( 2 );
    expr_10 = new_expr( 10 );
    expr_m2 = new_expr( -2 );
    expr_numu = new_expr( numu );
    expr_e = new_expr( op_e2718, nume );
    expr_pi = new_expr( op_pi, numpi );
    expr_dummy = new_expr( op_err, numu );
    dom_expr = expr_T;

    /* Process explicit settings. */
    parse_settings( true );

    /* Recognize the problem mode and reset global settings if needed. */
    if( is_problem_mode( tkn_arithm, tkn_Arithm, pb_arithm, "Arithmetic" ) ){}
    else if( is_problem_mode( tkn_brief_help, tkn_Brief_help, pb_brief_help,
      0 ) ){}
    else if( is_problem_mode( tkn_CFG_cmp, tkn_err, pb_CFG_compare,
      "CFG comparison" ) ){ unget_token(); }
    else if( is_problem_mode( tkn_CFG_in, tkn_err, pb_CFG_in,
      "CFG membership test" ) ){ unget_token(); }
    else if( is_problem_mode( tkn_CFG_long, tkn_err, pb_CFG_long,
      "CFG string length upper bound" ) ){}
    else if( is_problem_mode( tkn_CFG_not_in, tkn_err, pb_CFG_not_in,
      "CFG nonmembership test" ) ){ unget_token(); }
    else if( is_problem_mode( tkn_CFG_set, tkn_err, pb_CFG_set,
      "CFG set grammar" ) ){ unget_token(); }
    else if( is_problem_mode( tkn_CFG_set2, tkn_err, pb_CFG_set2,
      "CFG student grammar" ) ){ unget_token(); }
    else if( is_problem_mode( tkn_CFG_short, tkn_err, pb_CFG_short,
      "CFG string length lower bound" ) ){}
    else if( is_problem_mode( tkn_CFG_start, tkn_err, pb_CFG_start,
      "CFG start symbol" ) ){ unget_token(); }
    else if( is_problem_mode( tkn_CFG_start2, tkn_err,
      pb_CFG_start2, "CFG student start symbol" ) ){ unget_token(); }
    else if( is_problem_mode( tkn_CFG_tree, tkn_err, pb_CFG_tree,
      "CFG parse tree" ) ){ unget_token(); }
    else if( is_problem_mode( tkn_draw, tkn_Draw, pb_draw_function,
      "Draw function" ) ){}
    else if( is_problem_mode( tkn_help, tkn_Help, pb_help, 0 ) ){}
    else if( is_problem_mode( tkn_mathcheck, tkn_MathCheck, pb_MathCheck,
      0 ) ){}
    else if( is_problem_mode( tkn_expr_tree, tkn_Expr_tree,
      pb_expression_tree, "Expression tree" ) ){}
    else if( is_problem_mode( tkn_parse, tkn_Parse, pb_expression_tree,
      "Expression tree" ) ){
      out_html( "\n<p class=err>The command <kbd>parse_tree</kbd> has been "
        "replaced by <kbd>expression_tree</kbd>. Please from now on, use the "
        "latter." ); parse_settings( true ); }
    else if( is_problem_mode( tkn_tree_cmp, tkn_Tree_cmp, pb_tree_compare,
      "Tree compare" ) ){}
    else if( is_problem_mode( tkn_equation, tkn_Equation, pb_equation,
      "Solve " ) ){}
    else if( is_problem_mode( tkn_prop3_logic, tkn_Prop3_logic, pb_prop_logic,
      "Ternary propositional logic" ) ){
      gs_prop3 = true; parse_settings( true ); }
    else if( is_problem_mode( tkn_prop_logic, tkn_Prop_logic, pb_prop_logic,
      "Binary propositional logic" ) ){}
    else if( is_problem_mode( tkn_real_logic, tkn_Real_logic, pb_real_logic,
      "First-degree integer arithmetic" ) ){}
    else if( is_problem_mode( tkn_mod, tkn_Mod, pb_modulo, "Modulo " ) ){}
    else if( is_problem_mode( tkn_array, tkn_Array, pb_array_claim,
      "Array claim" ) ){}

    /* If no error has been detected, check a solution chain. */
    if( !err_mode ){

      /* Do the actual checking. */
      if( pb_mode == pb_arithm ){
        parse_arithm_relation_chain();
      }else if( pb_mode == pb_array_claim ){
        parse_array_start(); parse_settings( true ); parse_array_chain();
      }else if( pb_mode == pb_brief_help || pb_mode == pb_help ){
        instructions( pb_mode == pb_help );
        if( !parse_is_top_tkn() ){ parse_error(); }
      }else if( pb_mode == pb_CFG_compare ){
        parse_CFG_compare();
        if( !err_mode ){ get_token(); }
        parse_settings();
        if( !parse_is_top_tkn() ){ parse_error(); }
      }else if( pb_mode == pb_CFG_in ){
        parse_CFG_in( true );
      }else if( pb_mode == pb_CFG_long ){
        if( parse_is( tkn_semic ) ){ CFG_long = ~0u; }
        else if( parse_is( tkn_number ) ){
          CFG_long = tkn_val.to_unsigned();
        }else{ parse_error(); }
        if( !err_mode ){ get_token(); }
        parse_settings();
        if( !parse_is_top_tkn() ){ parse_error(); }
      }else if( pb_mode == pb_CFG_not_in ){
        parse_CFG_in( false );
        parse_settings();
      }else if( pb_mode == pb_CFG_set ){
        CFG_read();
        if( !err_mode ){ get_token(); }
        parse_settings();
        if( !parse_is_top_tkn() ){ parse_error(); }
      }else if( pb_mode == pb_CFG_set2 ){
        CFG_read(1);
        if( !err_mode ){ get_token(); }
        parse_settings();
        if( !parse_is_top_tkn() ){ parse_error(); }
      }else if( pb_mode == pb_CFG_short ){
        CFG_short = tkn_val.to_unsigned();
        parse_pass( tkn_number );
        parse_settings();
        if( !parse_is_top_tkn() ){ parse_error(); }
      }else if( pb_mode == pb_CFG_start ){
        CFG_start_symbol();
        if( !err_mode ){ get_token(); }
        parse_settings();
        if( !parse_is_top_tkn() ){ parse_error(); }
      }else if( pb_mode == pb_CFG_start2 ){
        CFG_start_symbol(1);
        if( !err_mode ){ get_token(); }
        parse_settings();
        if( !parse_is_top_tkn() ){ parse_error(); }
      }else if( pb_mode == pb_CFG_tree ){
        parse_CFG_tree();
        if( !err_mode ){ get_token(); }
        parse_settings();
        if( !parse_is_top_tkn() ){ parse_error(); }
      }else if( pb_mode == pb_draw_function ){
        var_f_max = 1; parse_draw_function_chain();
      }else if( pb_mode == pb_MathCheck ){
        html_copyright( "Feedback Tool" );
        if( !parse_is_top_tkn() ){ parse_error(); }
      }else if( pb_mode == pb_equation ){
        parse_given_roots(); parse_settings( true );
        for( unsigned ii = 0; ii < roots_val.size(); ++ii ){
          var_used[ 0 ].value = roots_val[ ii ];
          if( eval_tv( dom_expr ) != tv_T ){ test_fail( err_equ5 ); }
        }
        if( !err_mode ){ parse_logic_chain( pm_no_qua | pm_no_new_free ); }
      }else if( pb_mode == pb_modulo ){
        unsigned mod_new = 0;
        mod_rel = true;
        if( parse_is( tkn_number ) ){ mod_new = tkn_val.to_unsigned(); }
        if( 2 <= mod_new && mod_new <= mod_b_max ){
          if( mod_base != mod_new ){ mod_base = mod_new; mod_initialize(); }
          if( gs_verbose ){ out_html( mod_base ); }
          get_token();
        }else{ err_set_inp( "An integer between 2 and 25 is needed here" ); }
        parse_settings( true );
        parse_logic_chain( pm_mod );
      }else if( pb_mode == pb_expression_tree ){
        parse_tree_chain();
      }else if( pb_mode == pb_tree_compare ){
        parse_tree_compare();
      }else if( pb_mode == pb_prop_logic ){
        parse_logic_chain( pm_tv | pm_no_qua );
      }else if( pb_mode == pb_real_logic ){
        parse_logic_chain( 0 );
      }else{ mc_err_print( "This feature has not yet been implemented" ); }

    }

    /* If an error was found, then print the error message and terminate or
      scan to the next problem. Since parse_-functions return false when
      confused, repeated err_reset is needed. */
    if( err_mode ){
      err_report();
      if( pb_mode < pb_MathCheck && !gs_fail_text.empty() ){
        out_html( "\n<p class=comment>" );
        out_print( gs_fail_text.c_str() ); out_print( '\n' );
      }
      if( err_mode >= err_time ){ tkn_now = tkn_eoi; }
      else{
        err_reset();
        while( !parse_is_top_tkn() ){ get_token(); err_reset(); }
        parse_settings();
      }
    }

    /* Otherwise do the additional checks on the final expression. */
    else{
      unsigned old_cnt = err_cnt;

      /* Finally banned operators */
      if( now_expr ){
        for( unsigned ii = 0; ii < ls_f_ban_cnt; ++ii ){
          if( now_expr->has_opr( ls_f_ban_ops[ ii ] ) ){
            out_html( "\n<p class=warn>The final expression must not contain "
              );
            out_op( ls_f_ban_ops[ ii ] ); out_html( ".\n" ); ++err_cnt;
          }
        }
      }

      /* Finally required top operator */
      if( now_expr && ls_f_top_opr != op_err ){
        op_type opr = now_expr->opr();
        if( is_sin_like( opr ) && now_expr->left() ){ opr = op_pow; }
        if(
          opr != ls_f_top_opr ||
          ( ls_f_top_var && now_expr->var().name != ls_f_top_var )
        ){
          out_html( "\n<p class=warn>The top operator of the final expression"
            " must be " );
          out_op( ls_f_top_opr, ls_f_top_var ); out_html( ".\n" );
          ++err_cnt;
        }else if(
          ( now_expr->left() && now_expr->left()->has_opr( ls_f_top_opr ) ) ||
          ( now_expr->right() && now_expr->right()->has_opr( ls_f_top_opr ) )
        ){
          out_html( "\n<p class=warn>The final expression must not contain "
            );
          out_op( ls_f_top_opr ); out_html( " except on top.\n" );
          ++err_cnt;
        }
      }

      /* Solving a variable explicitly */
      if( pb_mode == pb_equation ){
        if( !root_is_explicit ){
          out_html( "\n<p class=warn>The final answer is not explicit.\n" );
          ++err_cnt;
        }
      }else if( ls_solve && now_expr ){
        for( unsigned ii = 0; ii < var_f_cnt; ++ii ){
          if( var_used[ ii ].name == ls_solve ){
            if( !now_expr->solved( ii, op_err ) ){
              out_html( "\n<p class=warn>" ); out_var_name( ls_solve );
              out_html( " has not been solved.\n" );
              ++err_cnt;
            }
            break;
          }
        }
      }

      /* Required final form */
      if( now_expr ){
        if( ls_f_CNF && !now_expr->is_CNF() ){
          out_html( "\n<p class=warn>The final expression must be " );
          print_CNF(); out_html( ".\n" ); ++err_cnt;
        }
        if( ls_f_DNF && !now_expr->is_DNF() ){
          out_html( "\n<p class=warn>The final expression must be " );
          print_DNF(); out_html( ".\n" ); ++err_cnt;
        }
        if( ls_f_ban_U && !gs_exam ){
          check_array_claim( 0, now_expr, 0 );
          if( err_mode && err_mode < err_time ){
            out_html( "\n<p class=warn>The final expression yields "
              "<b>U</b>" );
            print_err_var_vals(); out_html( '\n' ); err_mode = err_none;
            ++err_cnt;
          }
        }
        if( ls_f_polynomial && !now_expr->is_polynomial() ){
          out_html( "\n<p class=warn>The final expression must be in "
            "polynomial form.\n" );
          ++err_cnt;
        }
        if(
          ls_f_simplify &&
          now_expr->node_cnt() > simplify( now_expr )->node_cnt()
        ){
          out_html( "\n<p class=warn>The final expression must be in a more "
            "simplified form.\n" );
          ++err_cnt;
        }
        if( ls_f_range && !now_expr->is_range() ){
          out_html( "\n<p class=warn>The final expression must not contain "
            "constants outside 0, &hellip;, " );
          out_print( mod_base-1 ); out_html( ".\n" ); ++err_cnt;
        }
      }

      /* Ambiguous grammar */
      if( CFG_ambiguous && CFG::duplicate ){
        out_html( "\n<p class=warn>The grammar generates the following string"
          " twice: \"" );
        out_print( CFG::duplicate ); out_print( "\".\n" ); ++err_cnt;
      }

      /* Complexity */
      if( err_cnt <= old_cnt && ( ls_f_nodes || ls_b_nodes ) && now_expr ){
        unsigned fc = now_expr->node_cnt();
        if( ls_f_nodes && fc > ls_f_nodes ){
          out_html( "\n<p class=warn>The complexity of the final expression"
            " is " );
          out_print( fc ); out_print( ", while it must be at most " );
          out_print( ls_f_nodes ); out_print( ".\n" );
          ++err_cnt;
        }else if( !gs_exam && ls_b_nodes && fc <= ls_b_nodes ){
          out_html( "\n<p class=assess>The complexity of the final expression"
            " is " );
          out_print( fc ); out_print( ", which is so small that you get"
            " additional praise!\n" );
          ++points;
        }else if( ls_b_nodes ){
          out_html( "\n<p>The complexity of the final expression is " );
          out_print( fc );
          if( gs_exam ){ out_print( ".\n" ); }
          else{
            out_print( ", which is accepted.\n" );
            out_print( "However, if you make it at most " );
            out_print( ls_b_nodes ); out_print( ", you will get additional"
              " praise!\n" );
          }
        }
      }

      /* Write the disclaimer. */
      if(
        err_cnt > old_cnt && pb_mode < pb_MathCheck && !gs_fail_text.empty()
      ){
        out_html( "\n<p class=comment>" );
        out_print( gs_fail_text.c_str() ); out_print( '\n' );
      }
      if( err_cnt <= old_cnt && pb_mode < pb_MathCheck ){
        if( gs_exam ){ out_html( "\n<p>No syntax errors found.\n" ); }
        else{
          ++points;
          if( !gs_ok_text.empty() ){
            out_html( "\n<p class=assess>" );
            out_print( gs_ok_text.c_str() ); out_print( '\n' );
          }else{
            out_html( "\n<p>No errors found. " );
            if( !check_level ){
              out_print( "MathCheck is convinced that there are no"
                " errors.\n" );
            }else if( check_level == 1 ){
              out_print( "Please notice that the check was not complete.\n" );
            }else{
              out_print( "<span class=warn>Warning!" );
              if( check_level & 4 ){
                out_print( " Some step looked bad, but MathCheck was unable"
                  " to check it." );
              }
              if( check_level & 2 ){
                out_print( " Some step was weakly checked." );
              }
              out_print( "</span>\n" );
            }
          }
        }
      }
      if( pb_MathCheck <= pb_mode && pb_mode <= pb_help ){
        pb_mode = pb_arithm;
      }

    }

    /* Reset chain-local settings. */
    ls_allow_comp = ls_ban_comp = ls_hide_expr = ls_var4 = ls_f_CNF = ls_f_DNF
      = ls_f_ban_U = ls_f_polynomial = ls_f_simplify = ls_f_range = false;
    ls_b_nodes = ls_f_nodes = ls_solve = ls_f_top_var = ls_f_ban_cnt = 0;
    ls_f_top_opr = op_err;

    /* Pass the optional end_of_answer token. */
    if( parse_is( tkn_eoa ) ){ get_token(); }

  }while( tkn_now != tkn_eoi );

  /* If the next URL has been set, print it or go back instruction. */
  if( next_URL[ 0 ] ){
    if( gs_verbose ){ out_html( "\n<p><hr>\n" ); }
    if( err_cnt ){
      out_html( "<p class=warn>Please go back and try to fix the errors." );
    }else if( next_URL[ 0 ] != ' ' ){
      out_html( "<p><a href= \"" ); out_print( next_URL );
      out_print( "\" >Click here to go to the next page</a>" );
    }else{
      out_html( "<p class=hides>This was the last problem of the series!" );
    }
  }

  /* Print the end-of-feedback line */
  if( gs_verbose ){ out_html( "\n<p><hr>" ); }

  /* If debug mode, print statistics. */
  if( gs_debug ){
    out_html( "\n<p><span class=debug>\n" );
    out_print( "Expressions: " ); out_print( expression::h_cnt );
    out_print( "\n<br>Max hash list length: " );
    out_print( expression::h_max_len );
    out_print( "\n<br>Max hash burden: " );
    out_print( expression::h_max_brdn );
    out_print( "</span>\n" );
  }

  /* Print the run time and complete the output HTML page. */
  if( gs_verbose ){
    time_now(); long us = time_usec;
    out_html( "\n<p class=unimp>MathCheck run time " );
    out_print( int( time_sec ) ); out_print( '.' );
    out_print( char( us / 100000 + '0' ) ); us %= 100000;
    out_print( char( us / 10000 + '0' ) ); us %= 10000;
    out_print( char( us / 1000 + '0' ) );
    out_print( " seconds\n" );
  }
  if( !gs_exam){ html_points( points ); }
#ifdef subhtml
  out_flush();
#else
  html_record();
  html_end();
#endif

}
