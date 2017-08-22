/* MathCheck input/output and some other utilities
  Copyright Antti Valmari. */ const unsigned date = 20170817;
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


/*** A copyright utility ***/


class copyright{
public:
  static unsigned date;           // date of the program as a whole
  static copyright *first;        // list of component copyrights
  const char *filename, *author;  // name and author of a single component
  unsigned filedate;              // date of a single component
  copyright *next;

  /* Link a new copyright object to the list and maintain the maximum date. */
  copyright(
    const char *filename, const char *author = 0, unsigned filedate = 0
  ):
    filename( filename ), author( author ), filedate( filedate )
  {
    if( filedate > date ){ date = filedate; }
    next = first; first = this;
  }

};
copyright *copyright::first = 0;
unsigned copyright::date = date;


/* Copyright of this file */
copyright input_output_cc( "input_output.cc", "Antti Valmari", date );


/*** Some small utilities ***/


/* True, iff ch is a digit */
inline bool is_digit( char ch ){ return '0' <= ch && ch <= '9'; }

/* True, iff ch is a letter */
inline bool is_ltr( char ch ){
  if( ch > 'Z' ){ return 'a' <= ch && ch <= 'z'; }
  return 'A' <= ch;
}

/* True, iff ch is a letter or digit */
inline bool is_ltr_dgt( char ch ){
  if( ch > 'Z' ){ return 'a' <= ch && ch <= 'z'; }
  if( ch > '9' ){ return 'A' <= ch; }
  return ch >= '0';
}

/* Char string comparison */
bool str_lt( const char *s1, const char *s2 ){
  while( *s2 == *s1 && *s1 ){ ++s1; ++s2; }
  return *s1 < *s2;
}

/* From hexadecimal character to a value */
inline char from_hex( char ch ){
  if( 'a' <= ch && ch <= 'f' ){ return ch - 'W'; }
  if( 'A' <= ch && ch <= 'F' ){ return ch - '7'; }
  if( '0' <= ch && ch <= '9' ){ return ch - '0'; }
  return '\x10';
}

/* Return the nr of bytes in a UTF-8-char that starts with the given byte. */
inline unsigned UTF8_size( char ch ){
  if( !( ch & '\x80' ) ){ return 1; }   // ASCII character
  if( !( ch & '\x40' ) ){ return 0; }   // error: extension byte
  if( !( ch & '\x20' ) ){ return 2; }
  if( !( ch & '\x10' ) ){ return 3; }
  if( !( ch & '\x8' ) ){ return 4; }
  return 5;   // error: illegal UTF-8 start byte
}

/* An array for b64-encoding six bits. */
const char b64chars[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/* Return six b64-encoded bits or an error code. */
inline char b64bits( char ch ){
  if( 'a' <= ch && ch <= 'z' ){ return ch - 'G'; }
  if( 'A' <= ch && ch <= 'Z' ){ return ch - 'A'; }
  if( '0' <= ch && ch <= '9' ){ return ch + '\4'; }
  if( ch == '/' ){ return '?'; }
  if( ch == '+' ){ return '>'; }
  return '@';
}


/*** Input features ***/


#include <iostream>
#include <fstream>


/* Input buffer and related data */
/* The buffer holds inp_line_max / 4 most recent UTF-8-chars of the current
  line (or the whole line, if it is shorter). It is a segment of a ring. */
const unsigned inp_line_max = 400;      // maximum stored bytes
char inp_buffer[ inp_line_max ] = {};   // stored part of the input line
unsigned
  inp_line_nr = 1,    // current input line nr
  inp_byte_end = 0,   // number of read bytes on the input line
  inp_byte_now = 0,   // current byte nr on the input line
  inp_byte_beg = 0,   // number of discarded bytes on the input line
  inp_UTF8_end = 0,   // number of read UTF-8-chars on the input line
  inp_UTF8_beg = 0;   // number of discarded UTF-8-chars on the input line
char inp_chr = '\0';  // most recently delivered byte
#ifdef record
std::string inp_record;   // all input (cgi-bin-decoded but not deconfused)
#endif


#ifdef confuse
/* 0: input is not being deconfused, 1: input is being deconfused, 2 3:
  deconfusion start sequence, >= 4: recovery from failed start (These are in
  roughly decreasing order of probability.) */
unsigned inp_confuse = 0;       // see above
bool inp_allow_dcnfs = false;   // <=> deconfusion may be switched on
#endif

inline void inp_deconfuse_on(){
#ifdef confuse
  inp_allow_dcnfs = true;
#endif
}

inline void inp_deconfuse_off(){
#ifdef confuse
  inp_allow_dcnfs = false; inp_confuse = 0;
#endif
}


/* Get (and deconfuse) an input byte or detect the end of input. */
void inp_raw(){

#ifdef confuse
  static char inp_pend = '\0';  // pending char due to failed deconfuse start
  if( inp_confuse == 4 ){ inp_chr = inp_pend; inp_confuse = 0; return; }
  if( inp_confuse > 4 ){ inp_chr = '\xAE'; inp_confuse = 4; return; }
#endif

  /* Read, cgi-bin-decode, and deconfuse input until something can be
    delivered or the input ends. */
  while( true ){

    /* Detect the end of input. */
    inp_chr = std::cin.get();
    if( !std::cin ){ inp_chr = '\0'; }

#ifdef cgibin

    /* Decode +. */
    else if( inp_chr == '+' ){ inp_chr = ' '; }

    /* If a form field name arrives, scan until the contents of the field. To
      ensure white space between successive fields, return a newline and reset
      the line count so that the first real line will be line nr 1. */
    else if( inp_chr == '&' ){
      while( std::cin.get( inp_chr ) && inp_chr != '=' );
      inp_line_nr = 0; inp_chr = '\n';
    }

    /* Decode %-codes. */
    else if( inp_chr == '%' ){
      char ch = from_hex( std::cin.get() );
      inp_chr = from_hex( std::cin.get() );
      if( !std::cin || ch == '\x10' || inp_chr == '\x10' ){ inp_chr = '\0'; }
      else{ inp_chr |= ch << 4; }
    }

#endif

    /* Skip <CR> to avoid spurious double newlines. */
    if( inp_chr == '\x0d' ){ continue; }

#ifdef record
    inp_record += inp_chr;
#endif

#ifdef confuse

    static unsigned
      key = 0,      // deconfusion key
      b64sz = 0;    // number of leftover bits from previous b64-char
    static char b64old = '\0';  // shifted leftover bits

    switch( inp_confuse ){

    /* If not currently deconfusing, may switch on start sequence */
    case 0:
      if( inp_chr == '\xC2' && inp_allow_dcnfs ){ inp_confuse = 2; continue; }
      return;

    /* If deconfusing ... */
    case 1: {

      /* Skip newlines in confused data. Stop upon '.', rejecting it. Stop
        upon other non-b64-chars, accepting them. */
      if( inp_chr == '\n' ){ continue; }
      if( inp_chr == '.' ){ inp_confuse = 0; continue; }
      unsigned char new_bits = b64bits( inp_chr );
      if( new_bits == '@' ){ inp_confuse = 0; return; }

      /* B64-decode the char, and fetch another if necessary. */
      if( !b64sz ){ b64old = new_bits << 2; b64sz = 6; continue; }
      b64sz -= 2; inp_chr = new_bits >> b64sz | b64old;
      b64old = new_bits << ( 8 - b64sz );

      /* Deconfuse and return the char. */
      new_bits = inp_chr;
      key *= 33333333; inp_chr ^= char( key >> 24 ); key += new_bits;
      return;

    }

    /* Deconfusion start sequence step 1 */
    case 2:
      if( inp_chr == '\xAE' ){ inp_confuse = 3; continue; }
      else if( inp_chr == '\xC2' ){ inp_confuse = 2; continue; }
      else{ inp_pend = inp_chr; inp_chr = '\xC2'; inp_confuse = 4; return; }

    /* Deconfusion start sequence step 2 */
    default: {
      if( inp_chr == '\xC2' ){ inp_confuse = 2; continue; }
      key = b64bits( inp_chr );
      if( key == '@' ){
        inp_pend = inp_chr; inp_chr = '\xC2'; inp_confuse = 5; return;
      }
      for( unsigned ii = 0; ii < 5; ++ii ){
        inp_chr = std::cin.get();
#ifdef cgibin
        if( inp_chr == '%' ){
          char ch = from_hex( std::cin.get() );
          inp_chr = from_hex( std::cin.get() );
          if( !std::cin || ch == '\x10' || inp_chr == '\x10' ){
            inp_chr = '\0';
          }else{ inp_chr |= ch << 4; }
        }
#endif
        char new_bits = b64bits( inp_chr );
        if( !std::cin || new_bits == '@' ){
          inp_chr = '\0'; inp_confuse = 0; return;
        }
#ifdef record
        inp_record += inp_chr;
#endif
        key <<= 6; key |= new_bits;
      }
      inp_confuse = 1; b64sz = 0; b64old = '\0'; continue;
    }

    }

#endif

    return;
  }
}


/* Read a UTF-8-char to the buffer. */
inline void inp_get_UTF8(){

  /* If necessary, make room in the buffer. */
  if( inp_UTF8_end - inp_UTF8_beg >= inp_line_max / 4 ){
    inp_byte_beg += UTF8_size( inp_buffer[ inp_byte_beg % inp_line_max ] );
    ++inp_UTF8_beg;
  }

  /* Read the character. */
  inp_raw();
  unsigned nn = UTF8_size( inp_chr );
  inp_buffer[ inp_byte_end++ % inp_line_max ] = nn ? inp_chr : '\0';
  for( unsigned ii = 1; ii < nn; ++ii ){
    inp_raw(); inp_buffer[ inp_byte_end++ % inp_line_max ] = inp_chr;
  }
  ++inp_UTF8_end;

}


/* Get an input character via the input buffer. End-of-input yields '\0'. */
void inp_get_chr(){

  /* If the input buffer is exhausted, read something. */
  if( inp_byte_now >= inp_byte_end ){

    /* Upon earlier newline, reset the buffer and increment line count. */
    if(
      inp_byte_now &&
      inp_buffer[ ( inp_byte_now - 1 ) % inp_line_max ] == '\n'
    ){
      inp_byte_end = inp_byte_now = inp_byte_beg = 0;
      inp_UTF8_end = inp_UTF8_beg = 0; ++inp_line_nr;
    }

    /* If the input buffer is not empty, the line is overlong and not fully in
      the buffer, so extend the buffer by reading one UTF-8-char. Otherwise a
      new line starts, so read until end of line, input, or the capacity of
      the buffer. */
    if( inp_UTF8_end ){ inp_get_UTF8(); }
    else{
      while( inp_UTF8_end < inp_line_max / 4 ){
        inp_get_UTF8();
#ifdef confuse
        if( !inp_chr || inp_chr == '\n' || inp_confuse == 1 ){ break; }
#else
        if( !inp_chr || inp_chr == '\n' ){ break; }
#endif
      }
    }

  }

  /* Give a byte from the buffer. */
  inp_chr = inp_buffer[ inp_byte_now++ % inp_line_max ];

}


void inp_start(){
#ifdef cgibin
  while( std::cin.get( inp_chr ) && inp_chr != '=' );
#endif
  inp_get_chr();
}


/* Unread nn bytes on the current line, if possible. */
inline void inp_unread( unsigned nn ){
  if( inp_byte_now > inp_byte_beg + nn ){
    inp_byte_now -= nn;
    inp_chr = inp_buffer[ ( inp_byte_now - 1 ) % inp_line_max ];
  }
}


/* Reverse to location nn on the current line, if possible. */
inline void inp_revert_to( unsigned nn ){
  if( inp_byte_beg < nn && nn <= inp_byte_now ){
    inp_byte_now = nn;
    inp_chr = inp_buffer[ ( inp_byte_now - 1 ) % inp_line_max ];
  }
}


/* Match the input to an element of strings[0...max-1] or a single letter. For
  a single-letter match, return 0 and set inp_ltr according to the letter.
  Return 1...max for a string match and max+1 for no match. May change inp_ltr
  and read input even if there is no match. */
char inp_ltr = '\0';
unsigned inp_match( const char *strings[], unsigned max ){

  /* If a letter was obtained, remember it. */
  inp_ltr = is_ltr( inp_chr ) ? inp_chr : '\0';

  /* Read input and make low...hig narrower as long as possible. */
  unsigned low = 0, hig = max - 1, best_match = max, best_loc = 0, ii = 0;
  while( true ){

    /* Remember the best word match. Reject incomplete letter-digit words. */
    if(
      !strings[ low ][ ii ] &&
      !( is_ltr_dgt( inp_chr ) && is_ltr_dgt( strings[ low ][ ii - 1 ] ) )
    ){ best_match = low; best_loc = inp_byte_now; }

    /* Find the first and last match of the current character. */
    unsigned tmp = hig;
    while( low < tmp ){
      unsigned mid = ( low + tmp ) / 2;
      if( strings[ mid ][ ii ] < inp_chr ){ low = mid + 1; }
      else{ tmp = mid; }
    }
    while( tmp < hig ){
      unsigned mid = ( tmp + hig ) / 2;
      if( strings[ mid + 1 ][ ii ] <= inp_chr ){ tmp = mid + 1; }
      else{ hig = mid; }
    }

    /* Proceed in the input and string iff the match continues. */
    if( strings[ low ][ ii ] == inp_chr && inp_chr ){ inp_get_chr(); ++ii; }
    else{ break; }
  }

  /* If word matches were obtained, report the best. */
  if( best_match != max ){ inp_revert_to( best_loc ); return best_match + 1; }

  /* Recognize and report a single-letter match. */
  if( inp_ltr && ii <= 1 ){
    if( !ii ){ inp_get_chr(); }
    if( !is_ltr_dgt( inp_chr ) ){ return 0; }
  }

  /* Report failure. */
  return max + 1;

}


/*** Output features ***/


/* HTML-file-related info of global nature */
#include "links.cc"   // (partial) locations of HTML, executable, etc. files
const char *html_lang = "en";   // language of produced HTML pages


std::ostream *html_ptr = 0;   // points to where the HTML is to be printed


/*** Line-dividing printing ***/


/* Output buffer and related data */
const unsigned
  out_UTF8_max = 79,                // maximum recommended line length
  out_byte_max = 4 * out_UTF8_max;  // enough space for UTF-8-chars
char out_buffer[ out_byte_max ];    // bytes waiting for printing
unsigned
  out_UTF8_line = 0,    // nr of UTF-8-chars already on the output line
  out_UTF8_buf = 0,     // nr of UTF-8-chars in the buffer
  out_byte_buf = 0,     // nr of bytes in the buffer
  out_vis_cnt = 0,      // total nr of received non-white-space UTF-8-chars
  out_init_sp = 0;      // nr of pending spaces in the beginning of the line
char out_wait = '\0';   // a byte perhaps to be printed before the buffer
bool out_soft_sp = true;  // true <=> spaces may be replaced with newlines


/* Flush the output buffer. */
inline void out_flush(){
  if( !out_byte_buf ){ return; }
  if(
    ( out_wait == ' ' && out_UTF8_line + out_UTF8_buf >= out_UTF8_max ) ||
    ( out_wait == '\t' && out_UTF8_line + out_UTF8_buf > out_UTF8_max )
  ){ *html_ptr << '\n'; out_UTF8_line = 0; }
  else if( out_wait == ' ' ){ *html_ptr << ' '; ++out_UTF8_line; }
  for( ; out_init_sp; --out_init_sp ){ *html_ptr << ' '; ++out_UTF8_line; }
  for( unsigned ii = 0; ii < out_byte_buf; ++ii ){
    *html_ptr << out_buffer[ ii ];
  }
  out_UTF8_line += out_UTF8_buf; out_UTF8_buf = out_byte_buf = 0;
  out_wait = '\0';
}


/* Print to the output via the buffer, removing unnecessary spaces and
  possibly changing spaces to newlines in an attempt to avoid overlong lines.
  Furthermore, each tabulator is changed to either newline or nothing. */
#ifdef confuse
void out_raw( char ch ){
#else
void out_print( char ch ){
#endif

  /* Store the byte into the buffer, if it is not a break point and there is
    room. */
  if(
    !( ch == ' ' && out_soft_sp ) && ch != '\n' && ch != '\t' &&
    out_byte_buf < out_byte_max
  ){
    out_buffer[ out_byte_buf ] = ch; ++out_byte_buf;
    if( ( ch & '\xC0' ) != '\x80' ){ ++out_UTF8_buf; ++out_vis_cnt; }
    return;
  }

  /* Otherwise flush the buffer, if there is anything. */
  out_flush();

  /* Process the new character. */
  if( ch == '\n' ){
    *html_ptr << '\n'; out_UTF8_line = out_init_sp = 0; out_wait = '\0';
  }else if( ch == ' ' && out_soft_sp ){
    if( out_UTF8_line ){ out_wait = ' '; }else{ ++out_init_sp; }
  }else if( ch == '\t' ){
    if( out_UTF8_line && out_wait == '\0' ){ out_wait = '\t'; }
  }else{
    *html_ptr << ch;
    if( ( ch & '\xC0' ) != '\x80' ){ ++out_UTF8_line; ++out_vis_cnt; }
  }

}


#ifdef confuse

bool out_confuse = false;   // confuse, iff true
unsigned
  out_key = 0,              // confusion key for output
  out_b64_m4 = 0;           // nr of outputted b64-chars modulo 4
char out_b64_old = '\0';    // leftover bits from previous write


/* Confuse and write a character. */
void out_print( char ch ){
  if( !out_confuse ){ out_raw( ch ); return; }

  /* Confuse the char. */
  unsigned char cu = ch;
  out_key *= 33333333; cu ^= out_key >> 24; out_key += cu;

  /* B64-encode the char. */
  if( !out_b64_m4 ){
    out_raw( b64chars[ cu >> 2 ] ); out_b64_old = cu & '\3'; ++out_b64_m4;
  }else if( out_b64_m4 == 1 ){
    out_raw( b64chars[ out_b64_old << 4 | cu >> 4 ] );
    out_b64_old = cu & '\xF'; ++out_b64_m4;
  }else{
    out_raw( b64chars[ out_b64_old << 2 | cu >> 6 ] ); out_raw( '\t' );
    out_raw( b64chars[ cu & '\x3F' ] ); out_b64_m4 = 0;
  }
  out_raw( '\t' );

}

#endif


/* Start confusing the output. */
#ifdef confuse
inline void out_confuse_on( unsigned key ){
  out_confuse = true; out_key = key; out_b64_m4 = 0; out_b64_old = 0;
  out_raw( '\xC2' ); out_raw( '\xAE' );
  for( int ii = 30; ii >= 0; ii -= 6 ){
    out_raw( b64chars[ out_key >> ii & 0x3F ] );
  }
}
#else
inline void out_confuse_on( unsigned ){}
#endif


/* Stop confusing the output. */
inline void out_confuse_off(){
#ifdef confuse
  if( !out_confuse ){ return; }
  if( out_b64_m4 == 1 ){ out_raw( b64chars[ out_b64_old << 4 ] ); }
  else if( out_b64_m4 ){ out_raw( b64chars[ out_b64_old << 2 ] ); }
  out_confuse = false; out_raw( '\t' ); out_raw( '.' );
#endif
}


/* Printing of some other data types than char to the output. */

inline void out_print( const char *s1 ){
  for( ; *s1; ++s1 ){ out_print( *s1 ); }
}

inline void out_print( const std::string &s1 ){
  for( unsigned ii = 0; ii < s1.size(); ++ii ){ out_print( s1[ ii ] ); }
}

inline void out_print( unsigned nn ){
  if( nn >= 10 ){ out_print( nn/10 ); }
  out_print( char( nn % 10 + '0' ) );
}

inline void out_print( int nn ){
  if( nn < 0 ){ out_print( '-' ); nn = -nn; }
  out_print( unsigned( nn ) );  // works also with 0x80000000
}


/* Output a number with at least two digits, perhaps with a leading zero.
  Does not affect the printing mode. */
inline void out_2digit( unsigned ii ){
  out_print( ii / 10 ); out_print( ii % 10 );
}


/*** HTML and HTML-AsciiMath printing features ***/


/* These print characters such that they appear on the web page as such.
  That is, if the character has a special HTML meaning, it is escaped. */

void out_esc( char ch ){
  if( ch == '&' ){ out_print( "&amp;" ); }
  else if( ch == '<' ){ out_print( "&lt;" ); }
  else if( ch == '>' ){ out_print( "&gt;" ); }
  else if( ch == '"' ){ out_print( "&quot;" ); }
  else{ out_print( ch ); }
}

void out_esc( const char *s1 ){ for( ; *s1; ++s1 ){ out_esc( *s1 ); } }


/* These features facilitate printing in HTML mode and AsciiMath mode. */

const bool om_html = false, om_am = true;   // printing modes
bool om_curr = om_html;   // keeps track of the printing mode

inline void out_mode( bool om ){
  if( om != om_curr ){ out_print( '`' ); om_curr = om; }
}

inline void out_html( char ch ){ out_mode( om_html ); out_print( ch ); }
inline void out_am( char ch ){ out_mode( om_am ); out_esc( ch ); }

inline void out_html( const char *s1 ){ out_mode( om_html ); out_print( s1 );}
inline void out_am( const char *s1 ){ out_mode( om_am ); out_esc( s1 ); }

inline void out_html( unsigned nn ){ out_mode( om_html ); out_print( nn ); }
inline void out_am( unsigned nn ){ out_mode( om_am ); out_print( nn ); }

inline void out_html( int nn ){ out_mode( om_html ); out_print( nn ); }
inline void out_am( int nn ){ out_mode( om_am ); out_print( nn ); }


/* These print certain parts of a HTML pge. html_begin also receives the file
  pointer. */

void html_set_lang( const char *lang ){
  html_lang = lang;
}

void html_begin_begin(
  std::ostream &file, const std::string &title, unsigned number = 0,
  bool use_MathJax = true
){
  html_ptr = &file;
#ifdef cgibin
  out_print( "Content-type: text/html\n\n" );
#endif
  out_print( "<!DOCTYPE html>\n<html lang=" ); out_print( html_lang );
  out_print( ">\n<head>\n" );
  out_print( "<meta charset=UTF-8>\n<title>\t" ); out_print( title );
  if( number ){ out_print( ' ' ); out_print( number ); }
  out_print( "\t</title>\n<link rel=stylesheet type=\"text/css\" href=\t\"" );
  out_print( URL_css ); out_print( "\"\t>\n" );
  if( use_MathJax ){
    out_print( "<script type=\"text/javascript\" src=\t\"" );
    out_print( URL_mathjax ); out_print( "\"\t>\t</script>\n" );
  }
}

void html_begin_end(){
  out_print( "</head>\n\n<body>\n" );
}

void html_begin(
  std::ostream &file, const std::string &title, unsigned number = 0,
  bool use_MathJax = true
){
  html_begin_begin( file, title, number, use_MathJax );
  html_begin_end();
}

void html_end(){
  out_html( "\n</body>\n</html>\n" ); out_flush();
}

void html_h1( const std::string &title, const char *cls = 0 ){
  out_html( "<h1" );
  if( cls ){
    out_print( " class=\t\"" ); out_print( cls ); out_print( "\"" );
  }
  out_print( ">\t" ); out_print( title );
  out_print( "\t</h1>\n" );
}

void html_h1(
  const std::string &title, unsigned number, unsigned last = 0
){
  out_html( "<h1>\t" ); out_print( title );
  if( number ){
    out_print( ' ' ); out_print( number );
    if( last ){ out_print( " / " ); out_print( last ); }
  }
  out_print( "\t</h1>\n" );
}


/* This prints the input buffer to the HTML output. Space sequences contain
  hard spaces to esacpe the fusion of successive spaces. */
void html_err_buff( unsigned err_pos ){
  unsigned err_UTF8 = 0;
  out_mode( om_html );

  /* If the line where the error was detected is not empty ... */
  if( inp_byte_end ){

    /* Skip the "end-of-" character at the end of the buffer, if exists. */
    unsigned end = inp_byte_end - 1;
    char ch = inp_buffer[ end % inp_line_max ];
    if( ch && ch != '\n' ){ ++end; }

    /* Print the stored part of the line. */
    out_print( "\n<p class=errbox>\n<kbd>" );
    bool hard_sp = true;  // tells to print space as &nbsp;
    for( unsigned ii = inp_byte_beg; ii < end; ++ii ){
      ch = inp_buffer[ ii % inp_line_max ];
      if( ch == ' ' ){
        if( hard_sp ){ out_print( "&nbsp;" ); hard_sp = false; }
        else{ out_print( ' ' ); hard_sp = true; }
      }else{ out_esc( ch == '`' ? '\'' : ch ); hard_sp = false; }
      if( ii < err_pos && ( ch & '\xC0' ) != '\x80' ){ ++err_UTF8; }
    }
    out_print( "</kbd>\n" );

    /* If the error point was printed, print a pointer below it. */
    if( err_pos >= inp_byte_beg && err_UTF8 ){
      out_print( "<br><kbd>" ); --err_UTF8;
      for( unsigned ii = 0; ii < err_UTF8/2; ++ii ){ out_print( "&nbsp; " ); }
      if( err_UTF8 % 2 ){ out_print( "&nbsp;" ); }
      out_print( "!</kbd>\n" ); err_UTF8 += inp_UTF8_beg + 1;
    }

  }

  /* Print error location in numbers */
  out_print( "<p class=err>Line " ); out_print( inp_line_nr );
  if( err_UTF8 >= inp_UTF8_beg ){
    out_print( " column " ); out_print( err_UTF8 );
  }
  out_print( '\n' );

}


/* Print the date in international standard form. */
void html_date( unsigned date ){
  out_html( date / 10000 ); date %= 10000;
  out_print( '-' ); out_2digit( date / 100 );
  out_print( '-' ); out_2digit( date % 100 );
}


/* Print the copyright information. */
void html_copyright( const char *tool_name ){

  /* Print the header and date. */
  out_html( "\n<h2>MathCheck " ); out_print( tool_name ); out_print( ' ' );
  html_date( copyright::date ); out_print( "</h2>\n" );

  /* Print licence information. */
  out_print( "\n<p>This program is licensed under the <a"
    " href=\"https://www.gnu.org/licenses/gpl-3.0.en.html\"\t>Version 3 of"
    " the GPL</a> or any later version.\n" );
  out_print( "This program comes with ABSOLUTELY NO WARRANTY.\n" );
  out_print( "This is free software, and you are welcome to redistribute it"
    " under certain conditions.\n" );
  out_print( "See the licence for details.\n" );
  out_print( "Please contact antti.valmari&#64;tut.fi for the source"
    " code (and a copy of the licence).\n\n<p>" );

  /* Sort the file list. */
  if( copyright::first && copyright::first->next ){
    bool swapped = true;
    do{
      copyright *cp1 = copyright::first, *cp2 = cp1->next, *cp3 = cp2->next;
      swapped = false;
      if( str_lt( cp2->filename, cp1->filename ) ){
        cp1->next = cp3; cp2->next = cp1; copyright::first = cp2;
        cp2 = cp1; cp1 = copyright::first; swapped = true;
      }
      while( cp3 ){
        if( str_lt( cp3->filename, cp2->filename ) ){
          cp2->next = cp3->next; cp3->next = cp2; cp1->next = cp3;
          cp1 = cp3; cp3 = cp2->next; swapped = true;
        }else{ cp1 = cp2; cp2 = cp3; cp3 = cp3->next; }
      }
    }while( swapped );
  }

  /* Print the source file list. */
  out_print( "<p>This program consists of the following source files:\n" );
  for( copyright *cp = copyright::first; cp; cp = cp->next ){
    out_print( "<br><kbd>\t" ); out_print( cp->filename );
    out_print( "\t</kbd>" );
    if( cp->author ){
      out_print( " &copy; " ); out_print( cp->author );
      out_print( ' ' ); html_date( cp->filedate );
    }
    out_print( '\n' );
  }

  /* Print MathJax information. */
  out_print( "\n<p>The program uses <a"
    " href=\"http://www.mathjax.org\">MathJax &copy; The MathJax"
    " Consortium</a> for presenting mathematical formulae nicely.\n" );

}


/* Print the recorded input. */
void html_record(){
#ifdef record
  bool bb = out_soft_sp; out_soft_sp = false;
  out_html( "\n<!--" );
  if( !inp_record.empty() && inp_record[0] == '>' ){ out_print( ' ' ); }
  for( unsigned ii = 0; ii < inp_record.size(); ++ii ){
    char ch = inp_record[ ii ];
    if( ch ){ out_print( ch ); }
    if( ch == '-' ){ out_print( '_' ); }
  }
  out_print( "-->\n" ); out_soft_sp = bb;
#endif
}
