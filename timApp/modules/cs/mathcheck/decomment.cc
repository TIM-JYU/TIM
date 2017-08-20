/* This program extracts the contents of <textarea>s and (slightly encoded)
  comments from a HTML page. Some effort has been made to avoid being fooled
  by such cases as <textarea attribute = "...>">, but not very much. The
  program can be compiled as a command-line program or (#define cgibin)
  cgi-program.
*/
/* Copyright Antti Valmari 2017-08-07 */
#include <iostream>

const char
  *URL_css = "http://math.tut.fi/mathcheck/mathcheck.css",
  *URL_cgi_base = "http://math.tut.fi/mathcheck/cgi-bin/";

/* From hexadecimal character to a value */
inline char from_hex( char ch ){
  if( 'a' <= ch && ch <= 'f' ){ return ch - 'W'; }
  if( 'A' <= ch && ch <= 'F' ){ return ch - '7'; }
  if( '0' <= ch && ch <= '9' ){ return ch - '0'; }
  return '\x10';
}

char inp_chr = '\0';  // most recently delivered char (or byte of UTF-8-char)


/* Get (and cgi-bin-decode) an input char and assign it to inp_chr.
  Reject <CR>s. Return '\0' for end-of-input. */
void inp_get_chr(){

  /* Read (and cgi-bin-decode) input until something can be delivered or the
    input ends. */
  while( true ){

    /* Read the char or detect the end of input. */
    if( !std::cin.get( inp_chr ) ){ inp_chr = '\0'; }

#ifdef cgibin

    /* Decode +. */
    else if( inp_chr == '+' ){ inp_chr = ' '; }

    /* If a form field name arrives, scan until the contents of the field. To
      ensure white space between successive fields, return a newline. */
    else if( inp_chr == '&' ){
      while( std::cin.get( inp_chr ) && inp_chr != '=' );
      inp_chr = '\n';
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
    if( inp_chr != '\x0d' ){ break; }

  }

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


/* Features for testing against a string. */
unsigned match_ii = 0;            // how far matched
const unsigned match_size = 10;   // make big enough!!!
char match_buff[ match_size ];    // so far matched chars

/* Try a match against cmp. If cis, try a case-insensitive match. Record the
  matched chars using match_buff and match_ii. */
bool match( const char *cmp, bool cis = false ){
  match_ii = 0;
  while(
    cmp[ match_ii ] &&
    cmp[ match_ii ] == (
      cis && 'A' <= inp_chr && inp_chr <= 'Z' ? inp_chr + 32 : inp_chr
    )
  ){
    if( match_ii >= match_size ){ return false; }
    match_buff[ match_ii ] = inp_chr; inp_get_chr(); ++match_ii;
  }
  return !cmp[ match_ii ];
}

/* Output the matched chars. */
void match_out(){
  for( unsigned ii = 0; ii < match_ii; ++ii ){ out_esc( match_buff[ ii ] ); }
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

  bool copy_mode = false;
  while( inp_chr ){

    /* Not copying */
    if( !copy_mode ){

      /* If <textarea...> or <!--, switch to copy (and decode) mode. */
      if( inp_chr != '<' ){ inp_get_chr(); }
      else{
        inp_get_chr();

        /* If <textarea...>, switch to copy mode. */
        if( inp_chr == 't' && match( "textarea", true ) ){
          while( inp_chr && inp_chr != '>' ){
            if( inp_chr == '\"' ){ inp_get_chr(); scan_past( '\"' ); }
            else if( inp_chr == '\'' ){ inp_get_chr(); scan_past( '\'' ); }
            else{ inp_get_chr(); }
          }
          inp_get_chr(); copy_mode = true;
        }

        /* If <!--, copy and decode from a comment. */
        else if( inp_chr == '!' && match( "!--" ) ){
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

      }

    }

    /* Copying from textarea */
    else if( copy_mode ){

      /* If </textarea...>, return to non-copy mode. */
      if( inp_chr == '<' ){
        if( !match( "</textarea", true ) ){ match_out(); }
        else{ scan_past( '>' ); out_esc( '\n' ); copy_mode = false; }
      }

      /* Recognize one of the four character entities. */
      else if( inp_chr == '&' ){
        inp_get_chr(); match_ii = 0;
        if( inp_chr == 'a' && match( "amp;" ) ){ out_esc( '&' ); }
        else if( inp_chr == 'l' && match( "lt;" ) ){ out_esc( '<' ); }
        else if( inp_chr == 'g' && match( "gt;" ) ){ out_esc( '>' ); }
        else if( inp_chr == 'q' && match( "quot;" ) ){ out_esc( '\"' ); }
        else{ out_esc( '&' ); match_out(); }
      }

      /* Nothing special, so just copy the char to output. */
      else{ out_esc( inp_chr ); inp_get_chr(); }

    }

  }

#ifdef cgibin
  std::cout << "</textarea>\n\n</form>\n\n</body>\n</html>\n";
#endif

}
