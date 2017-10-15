/* This program extracts the contents of <textarea>s and (slightly encoded)
  comments from a HTML page. Some effort has been made to avoid being fooled
  by such cases as <textarea attribute = "...>">, but not very much. The
  program can be compiled as a command-line program or (#define cgibin)
  cgi-program.
*/
/* Copyright Antti Valmari 2017-08-25 */
#include <iostream>
#include "links.cc"

/* From hexadecimal character to a value */
inline char from_hex( char ch ){
  if( 'a' <= ch && ch <= 'f' ){ return ch - 'W'; }
  if( 'A' <= ch && ch <= 'F' ){ return ch - '7'; }
  if( '0' <= ch && ch <= '9' ){ return ch - '0'; }
  return '\x10';
}


/* Get (and cgi-bin-decode) an input char (or a byte of an UTF-8 char). Reject
  <CR>s. Use '\0' at end-of-input. */
char inp_do_read(){
  char inp_byte = '\0';

  /* Read (and cgi-bin-decode) input until something else than <CR> is
    obtained. Skipping <CR> avoids spurious double newlines. */
  while( true ){

    /* Read the char or detect the end of input. */
    if( !std::cin.get( inp_byte ) ){ inp_byte = '\0'; }

#ifdef cgibin

    /* Decode +. */
    else if( inp_byte == '+' ){ inp_byte = ' '; }

    /* If a form field name arrives, scan until the contents of the field. To
      ensure white space between successive fields, return a newline. */
    else if( inp_byte == '&' ){
      while( std::cin.get( inp_byte ) && inp_byte != '=' );
      inp_byte = '\n';
    }

    /* Decode %-codes. */
    else if( inp_byte == '%' ){
      char ch = from_hex( std::cin.get() );
      inp_byte = from_hex( std::cin.get() );
      if( !std::cin || ch == '\x10' || inp_byte == '\x10' ){
        inp_byte = '\0';
      }else{ inp_byte |= ch << 4; }
    }

#endif

    if( inp_byte != '\x0d' ){ return inp_byte; }
  }

}


/* Current input char (or a byte of an UTF-8-char) */
char inp_chr = '\0';

/* A ring buffer of input bytes that have been read in advance. */
const unsigned inp_buf_size = 100;  // capacity is one less than this
char inp_buff[ inp_buf_size ];
unsigned
  inp_head = 0,   // points to the first free location
  inp_tail = 0;   // if != inp_head, points to the oldest byte in the buffer


/* Update the current input char (or UTF-8 byte). */
void inp_get_chr(){
  if( inp_head == inp_tail ){ inp_chr = inp_do_read(); }
  else{ inp_chr = inp_buff[ inp_tail++ ]; inp_tail %= inp_buf_size; }
}


/* Return the ii'th input byte in advance to the current byte. To do so, load
  bytes until the input buffer contains at least ii bytes. */
char inp_advance( unsigned ii ){
  if( !ii ){ return inp_chr; }
  if( ii >= inp_buf_size ){ ii = inp_buf_size - 1; }
  for(
    unsigned nn = ( inp_buf_size + inp_head - inp_tail ) % inp_buf_size;
    nn < ii; ++nn
  ){ inp_buff[ inp_head++ ] = inp_do_read(); inp_head %= inp_buf_size; }
  return inp_buff[ ( inp_tail + ii - 1 ) % inp_buf_size ];
}


/* Remove ii oldest bytes in inp_chr and the input buffer. */
void inp_consume( unsigned ii ){
  unsigned
    nn = ( inp_buf_size + inp_head - inp_tail ) % inp_buf_size,
    jj = ii <= nn ? ii : nn + 1;
  if( jj > 1 ){ --jj; inp_tail += jj; inp_tail %= inp_buf_size; ii -= jj; }
  for( ; ii; --ii ){ inp_get_chr(); }
}


/* Start reading the input, that is, read until the first char intended for
  this program is in inp_chr. */
void inp_start(){
#ifdef cgibin
  while( std::cin.get( inp_chr ) && inp_chr != '=' );
#endif
  inp_get_chr();
}


/* Output a char. Four characters do not mean themselves in HTML. Replace them
  by character entities in the cgi-bin mode. */
void out_esc( char ch ){
#ifdef cgibin
  if( ch == '&' ){ std::cout << "&amp;"; }
  else if( ch == '<' ){ std::cout << "&lt;"; }
  else if( ch == '>' ){ std::cout << "&gt;"; }
  else if( ch == '"' ){ std::cout << "&quot;"; }
  else{ std::cout.put( ch ); }
#else
  std::cout.put( ch );
#endif
}


/* Try a match against cmp. If cis, try a case-insensitive match. Advance in
  the input if matched. */
bool match( const char *cmp, bool cis = false ){
  unsigned ii = 0;
  char inp_byte = inp_chr;
  while(
    cmp[ ii ] &&
    cmp[ ii ] == (
      cis && 'A' <= inp_byte && inp_byte <= 'Z' ? inp_byte + 32 : inp_byte
    )
  ){
    if( ii >= inp_buf_size - 1 ){ return false; }
    inp_byte = inp_advance( ++ii );
  }
  if( cmp[ ii ] ){ return false; }
  inp_consume( ii ); return true;
}


/* Read characters until ch or end-of-input is obtained, and read one more. */
void scan_past( char ch ){
  while( inp_chr && inp_chr != ch ){ inp_get_chr(); }
  inp_get_chr();
}


/* Find, decode, and print the contents of the textareas and comments. */
int main(){
  inp_start();

#ifdef cgibin
  /* Print the beginning of the HTML page, form, and textarea. */
  std::cout << "Content-type: text/html\n\n";
  std::cout << "<!DOCTYPE html>\n<html lang=en>\n<head>\n";
  std::cout << "<meta charset=UTF-8>\n";
  std::cout << "<title>MathCheck Extracted Textareas and Comments</title>\n";
  std::cout << "<link rel=stylesheet type=\"text/css\" href=\n\"" << URL_css;
  std::cout << "\">\n</head>\n\n<body>\n";
  std::cout << "\n<h1 class=teacher>MathCheck Extracted Textareas and";
  std::cout << " Comments</h1>\n";
  std::cout << "\n<form action=\n\"" << URL_cgi_base
     << "mathcheck.out\"\nmethod=post target=_blank>\n";
  std::cout << "\n<p>Please press\n"
    "<input type=submit value=\"submit same tab\" formtarget=_self> or\n"
    "<input type=submit value=\"submit new tab\">.\n";
  std::cout << "\n<textarea rows=24 cols=100 name=\"formula\">\n";
#endif

  while( inp_chr ){

    /* Quickly skip an obvious non-match. */
    if( inp_chr != '<' ){ inp_get_chr(); }

    /* If <textarea...> but not <textarea name="extra"...>, copy its
      contents. */
    else if( match( "<textarea", true ) ){

      /* Find the first attribute and check it. */
      while( inp_chr == ' ' || inp_chr == '\n' ){ inp_get_chr(); }
      bool do_copy = !match( "name=\"extra\"" );

      /* Skip up to and including the > closing the <textarea ...>. */
      while( inp_chr && inp_chr != '>' ){
        if( inp_chr == '\"' ){ inp_get_chr(); scan_past( '\"' ); }
        else if( inp_chr == '\'' ){ inp_get_chr(); scan_past( '\'' ); }
        else{ inp_get_chr(); }
      }
      inp_get_chr();

      /* Copy or bypass the contents of the textarea. */
      while( inp_chr ){

        /* If </textarea...>, return to non-copy mode. */
        if( inp_chr == '<' && match( "</textarea", true ) ){
          if( do_copy ){ out_esc( '\n' ); }
          scan_past( '>' ); break;
        }

        /* If not in copy mode, skip the character. */
        if( !do_copy ){ inp_get_chr(); continue; }

        /* Recognize one of the four character entities. */
        if( match( "&amp;" ) ){ out_esc( '&' ); }
        else if( match( "&lt;" ) ){ out_esc( '<' ); }
        else if( match( "&gt;" ) ){ out_esc( '>' ); }
        else if( match( "&quot;" ) ){ out_esc( '\"' ); }

        /* Nothing special, so just copy the char to output. */
        else{ out_esc( inp_chr ); inp_get_chr(); }

      }

    }

    /* If <!--!record!, copy and decode from a comment. */
    else if( match( "<!--!record!" ) ){
      bool was_hyphen = false;
      for( ; inp_chr; inp_get_chr() ){
        if( inp_chr == '-' ){
          if( was_hyphen ){ was_hyphen = false; break; }
          was_hyphen = true; continue;
        }
        if( was_hyphen ){
          out_esc( '-' ); was_hyphen = false;
          if( inp_chr == '_' ){ continue; }
        }
        out_esc( inp_chr );
      }
      if( was_hyphen ){ out_esc( '-' ); }
    }

    else{ inp_get_chr(); }
  }

#ifdef cgibin
  std::cout << "</textarea>\n\n</form>\n\n</body>\n</html>\n";
#endif

}
