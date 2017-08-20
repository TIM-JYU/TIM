#define confuse
#include "input_output.cc"
copyright make_problem_cc( "make_problem.cc", "Antti Valmari", 20170817 );
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


/* ??? saisiko kuvien tapauksessa esc-koodauksen pois paalta? */


#include <ctime>
#ifdef cgibin
#define one_page
#endif


/*** (Pseudo)random number generator ***/


unsigned random_seed = 0;

void random_init(){
  random_seed = time(0);
  random_seed *= 2718281; random_seed += 314159265;
  random_seed *= 2718281; random_seed += 314159265;
}


/* Random number from 0 to range-1, or to 2^32 - 1 if range = 0 */
/* Range is assumed to be < 10^6 or a power of 2, so modulo bias is negligible
  and thus has not been fixed. */
unsigned random( unsigned range ){
  random_seed *= 2718281; random_seed += 314159265;
  unsigned result = ( random_seed >> 16 ) ^ ( random_seed << 16 );
  if( range ){ result %= range; }
  return result;
}


/*** Error reporting, and the output features needed for it ***/


/* The output files and their name buffer */
/* std::string is not used because file.open( std::string ) did not compile */
#ifdef one_page
bool q_file_is_open = false;    // mimics q_file.is_open()
bool html_started = false;      // html_begin() has been called
#else
std::ofstream q_file, a_file;   // question and answer file
#endif
const unsigned buf_size = 101;
char buffer[ buf_size ] = {};   // name of question and answer file; it is of
unsigned                        //   the form <name base><number>.html or .mc
  buf_mid = 0,                  // the number in the file name starts here
  buf_end = 0;                  // the end-of-string '\0' is here

/* Error-related global variables */
bool err_ok = true;     // no error has been detected
unsigned err_pos = 0;   // (potential) error location on the current line


/* The error reporting function */
void error( const char *msg, const char *tkns[] = 0, bool is_hash = false ){
  if( !err_ok ){ return; }
  err_ok = false;

#ifdef one_page

  /* Open the HTML page, if necessary. */
  if( !html_started ){
    const char *title = "MathCheck Authoring Tool Error Message";
    html_begin( std::cout, title ); html_h1( title, "err" );
    html_started = true;
  }

  /* Print the error message with parameters. */
  out_print( "\n\n<p class=err>??? Error: " ); out_print( msg );
  if( tkns ){
    if( is_hash ){
      out_print( " <kbd>#</kbd><em>&lt;newline&gt;</em>" );
      out_print( " <kbd>#</kbd><em>&lt;space&gt;</em>" );
      for( unsigned ii = 2; tkns[ ii ]; ++ii ){
        out_print( " <kbd>#" ); out_print( tkns[ ii ] );
        out_print( "</kbd>" );
      }
    }else{
      for( unsigned ii = 0; tkns[ ii ]; ++ii ){
        out_print( " <kbd>" ); out_print( tkns[ ii ] ); out_print( "</kbd>" );
      }
    }
  }
  out_print( '\n' );

  /* Print the line where the error occurred, and the pointer to the error. */
  html_err_buff( err_pos );

#else

  /* Print the error message (without the parameters). */
  std::cout << "??? Error on line " << inp_line_nr << " byte " << err_pos <<
    ":\n" << msg << '\n';
  if( tkns && is_hash ){}   // this prevents a compiler warning

  /* Print the current file name buffer, if not empty. */
  buffer[ buf_size - 1 ] = '\0';
  for( unsigned ii = 0; buffer[ ii ]; ++ii ){
    if( buffer[ ii ] < ' ' || buffer[ ii ] >= 127 ){
      buffer[ ii ] = '\0'; break;
    }
  }
  if( buffer[0] ){
    std::cout << "Current file name buffer: " << buffer << '\n';
  }

#endif
}

void error_( const char *msg ){ err_pos = inp_byte_now; error( msg ); }


/*** Token separation and recognition (directly from input) ***/


/* Skip spaces and at most one newline among them. */
unsigned inp_indent = 0;  // nr of spaces in the beginning of the current line
inline void inp_skip_white_space(){
  while( inp_chr == ' ' ){ inp_get_chr(); }
  if( inp_chr == '\n' ){
    inp_indent = 0; inp_get_chr();
    while( inp_chr == ' ' ){ inp_get_chr(); ++inp_indent; }
  }
  err_pos = inp_byte_now;
}


/* Get a non-negative integer. */
unsigned inp_get_nr(){
  inp_skip_white_space();
  if( inp_chr < '0' || inp_chr > '9' ){ error_( "Expected a number" ); }
  unsigned result = 0;
  while( inp_chr >= '0' && inp_chr <= '9' ){
    result *= 10; result += inp_chr - '0'; inp_get_chr();
  }
  return result;
}


/*** Main program tokens and their recognition ***/


enum tkn_type {
  tkn_prm, tkn_nl, tkn_space, tkn_quot, tkn_hash, tkn_percent, tkn_amp,
  tkn_lt, tkn_gt, tkn_Instructions, tkn_MathCheck, tkn_Question, tkn_SUbmit,
  tkn_Submit, tkn_Text,
  tkn_grave,
  tkn_answer, tkn_box_size, tkn_confuse_off, tkn_confuse_on,
  tkn_feedback_height, tkn_filename, tkn_hidden, tkn_html, tkn_initial,
  tkn_instructions, tkn_last_number, tkn_let, tkn_new_file, tkn_no_chain,
  tkn_no_focus, tkn_question, tkn_submit, tkn_suomi, tkn_text, tkn_title,
  tkn_err, tkn_none,
  tkn_eof
} inp_tkn = tkn_err;

/* These must be in ASCII order with the end of string considered bigger than
  any char. */
const char *tkn_names[] = {
  "\n", " ", "\"", "#", "%", "&", "<", ">",
  "Instructions", "MathCheck", "Question", "SUbmit", "Submit", "Text",
  "`",
  "answer", "box_size", "confuse_off", "confuse_on", "feedback_height",
  "filename", "hidden", "html", "initial", "instructions", "last_number",
  "let", "new_file", "no_chain", "no_focus", "question", "submit", "suomi",
  "text", "title", 0
};


/* Get and recognize the next #-token. It may be preceded with white space. It
  may also be #x for any letter x, yielding tkn_prm. */
bool inp_prgh = true;   // there was an empty line
void inp_get_tkn(){

  /* Skip white space including empty lines. */
  inp_skip_white_space();
  while( inp_chr == '\n' ){ inp_skip_white_space(); inp_prgh = true; }

  /* Skip the # or detect its absence. */
  if( !inp_chr ){ inp_tkn = tkn_eof; return; }
  if( inp_chr != '#' ){ inp_tkn = tkn_none; return; }
  inp_get_chr();

  /* Find the token match, if exists. */
  inp_tkn = tkn_type( inp_match( tkn_names, tkn_err - 1 ) );

}


/*** Problem parameters and copying strings from the input ***/


/* Problem parameters */
const unsigned prm_cnt = 26;      // number of parameters; A...Z or a...z
int prm_int[ prm_cnt ] = {};      // values of integer parameters (a...z)
std::string prm_str[ prm_cnt ];   // values of string parameters (A...Z)


/* Copying input to the target obeys escaping rules and string-level
  #-commands (which includes replacing parameters by their contents). It obeys
  indentation but removes all other leading, trailing, end-of-line, and
  duplicate spaces and leading and trailing newlines. It ends when the next
  unrelated #-command, empty line, or end of input is found.
inp_copy_status 0 means always escape ", &, <, and >. With inp_copy_status 1,
  2, or 3, they are not escaped outside `- or #`-delimited segments. 1 means
  being inside such a segment and escaping them, 2 means being inside and not
  escaping them, and 3 means being outside. When entering a segment while
  inp_copy_status is 3, ` does and #` does not switch escaping on. Both ` and
  #` terminate the segment independently of which one started it.
  inp_copy_status 4 means never escape.
make_answer causes the string be copied into the answer file, without escaping
  but otherwise processing the string similarly.
*/
unsigned inp_copy_status = 0;
std::string *inp_copy_ptr = 0;  // the target: 0 = output, others = the string
bool inp_copy_make_answer = false;


/* Copy the char to the target (and answer file). */
inline void inp_copy_item( char ch ){
  if( inp_copy_ptr ){ *inp_copy_ptr += ch; }else{ out_print( ch ); }
#ifndef one_page
  if( inp_copy_make_answer ){ a_file << ch; }
#endif
}


/* Copy the string to the target (and the char to the answer file). */
#ifdef one_page
inline void inp_copy_item( const char *str, char ){
  if( inp_copy_ptr ){ *inp_copy_ptr += str; }else{ out_print( str ); }
}
#else
inline void inp_copy_item( const char *str, char ch ){
  if( inp_copy_ptr ){ *inp_copy_ptr += str; }else{ out_print( str ); }
  if( inp_copy_make_answer ){ a_file << ch; }
}
#endif


/* Copy the number to the target (and answer file). */
inline void inp_copy_item( unsigned ii ){
  if( ii >= 10 ){ inp_copy_item( ii / 10 ); }
  inp_copy_item( char( ii % 10 + '0' ) );
}


/* This reads, processes, and copies the input. This is called via wrapper
  functions that set inp_copy_ptr and inp_copy_make_answer. */
void inp_copy_raw(){
  unsigned
    escaped = 0,      // 0 = print inp_chr as such, 1...13 special cases
    space_level = 0;  // 0 = nothing, 1 = space, 2 = line feed is pending

  /* Skip leading white space. */
  inp_skip_white_space();
  if( inp_chr == '\n' ){ inp_get_tkn(); inp_copy_status = 0; return; }

  /* Scan the input up to the end or a non-escape-code token. */
  while( true ){
    if( !inp_chr ){ inp_tkn = tkn_eof; break; }   // end of input, so exit

    /* Keep track of white space instead of copying it. */
    if( inp_chr == ' ' ){
      if( !space_level ){ space_level = 1; }
      inp_get_chr(); continue;
    }
    else if( inp_chr == '\n' ){
      space_level = 2; inp_skip_white_space();
      if( inp_chr == '\n' ){  // empty line, so exit
        inp_get_tkn(); inp_prgh = true; break;
      }
      continue;
    }

    /* Remember characters that must be escaped. */
    else if( inp_chr == '\"' && inp_copy_status < 2 ){ escaped = 1; }
    else if( inp_chr == '&' && inp_copy_status < 2 ){ escaped = 2; }
    else if( inp_chr == '<' && inp_copy_status < 2 ){ escaped = 3; }
    else if( inp_chr == '>' && inp_copy_status < 2 ){ escaped = 4; }

    /* Process the effect of ` to the escaping status. */
    else if( inp_chr == '`' && inp_copy_status && inp_copy_status < 4 ){
      inp_copy_status = inp_copy_status == 3 ? 1 : 3;
    }

    /* Remember escape-code tokens and detect non-escape-code tokens. */
    else if( inp_chr == '#' ){
      inp_get_tkn();
      /**/ if( inp_tkn == tkn_nl ){ escaped = 5; }
      else if( inp_tkn == tkn_space ){ escaped = 6; }
      else if( inp_tkn == tkn_quot ){ escaped = 7; }
      else if( inp_tkn == tkn_hash ){ escaped = 8; }
      else if( inp_tkn == tkn_amp ){ escaped = 9; }
      else if( inp_tkn == tkn_lt ){ escaped = 10; }
      else if( inp_tkn == tkn_gt ){ escaped = 11; }
      else if( inp_tkn == tkn_grave ){
        escaped = 12;
        if( inp_copy_status && inp_copy_status < 4 ){
          inp_copy_status = inp_copy_status == 3 ? 2 : 3;
        }
      }
      else if( inp_tkn == tkn_prm ){
        escaped = 13;
        if( !space_level ){ space_level = 1; }  // ensure space before the prm
      }
      else{ break; }  // irrelevant #-command, so exit
    }

    /* Print the pending white space, leaving out unnecessary space chars. */
    if( space_level == 2 ){ inp_copy_item( '\n' ); }
    else if( space_level == 1 && !inp_indent ){ inp_indent = 1; }
    space_level = 0;
    for( ; inp_indent; --inp_indent ){ inp_copy_item( ' ' ); }

    /* Print escape code or inp_chr. */
    if( !escaped ){ inp_copy_item( inp_chr ); }
    else if( escaped == 1 ){ inp_copy_item( "&quot;", '\"' ); }
    else if( escaped == 2 ){ inp_copy_item( "&amp;", '&' ); }
    else if( escaped == 3 ){ inp_copy_item( "&lt;", '<' ); }
    else if( escaped == 4 ){ inp_copy_item( "&gt;", '>' ); }
    else if( escaped == 5 ){ inp_copy_item( '\n' ); }
    else if( escaped == 6 ){ inp_copy_item( ' ' ); }
    else if( escaped == 7 ){ inp_copy_item( '\"' ); }
    else if( escaped == 8 ){ inp_copy_item( '#' ); }
    else if( escaped == 9 ){ inp_copy_item( '&' ); }
    else if( escaped == 10 ){ inp_copy_item( '<' ); }
    else if( escaped == 11 ){ inp_copy_item( '>' ); }
    else if( escaped == 12 ){ inp_copy_item( '`' ); }
    else if( escaped == 13 ){
      if( 'A' <= inp_ltr && inp_ltr <= 'Z' ){
        unsigned ii = inp_ltr - 'A';
        for( unsigned jj = 0; jj < prm_str[ ii ].size(); ++jj ){
          char ch = prm_str[ ii ][ jj ];
          /**/ if( ch == '\"' ){ inp_copy_item( "&quot;", '\"' ); }
          else if( ch == '&' ){ inp_copy_item( "&amp;", '&' ); }
          else if( ch == '<' ){ inp_copy_item( "&lt;", '<' ); }
          else if( ch == '>' ){ inp_copy_item( "&gt;", '>' ); }
          else if( ch == '#' ){
            if( ++jj < prm_str[ ii ].size() ){
              inp_copy_item( prm_str[ ii ][ jj ] );
            }
          }
          else{ inp_copy_item( ch ); }
        }
      }else if( 'a' <= inp_ltr && inp_ltr <= 'z' ){
        int ii = prm_int[ inp_ltr - 'a' ];
        inp_copy_item( '(' );
        if( ii < 0 ){ inp_copy_item( '-' ); ii = -ii; }
        inp_copy_item( unsigned( ii ) );
        inp_copy_item( ')' );
      }
      space_level = 1;  // ensure space after the parameter
    }

    /* Read the next char if not already done, and reset escaped. */
    if( escaped < 5 ){ inp_get_chr(); }
    escaped = 0;

  }

  /* Terminate the answer in the answer file; switch escaping on. */
#ifndef one_page
  if( inp_copy_make_answer ){ a_file << '\n'; }
#endif
  inp_copy_status = 0;

}


/* Copy to a string. */
inline void inp_copy_to_str( std::string *sp ){
  inp_copy_ptr = sp; inp_copy_make_answer = false; inp_copy_raw();
}


/* Copy to a HTML page. */
const unsigned
  inp_copy_nobreak = 1,   // do not prevent overlong lines by space -> newline
  inp_copy_answer = 2;    // copy also to the answer file
inline void inp_copy_to_html( unsigned mode = 0 ){
  inp_copy_ptr = 0;
  if( mode ){
    out_soft_sp = false;
    inp_copy_make_answer = mode == inp_copy_answer; inp_copy_raw();
    out_soft_sp = true;
  }else{ inp_copy_make_answer = false; inp_copy_raw(); }
}


/*** Features for producing parts of the HTML and answer files ***/


/* Some parameters of the problem pages and answering textareas */
unsigned                              // size of answering textarea
  rows_g = 15, cols_g = 50,           // each #filename starts with these
  rows_l = rows_g, cols_l = cols_g;   // valid to next #box_size or #filename
unsigned
  fb_height = 0,  // if > 0, a feedback iframe is created
  fb_cnt = 0;     // a running number of feedback iframes on the same page 
bool
  make_file_chain = true,   // ok answer page links to the next problem page
  confuse_hidden = true;    // confuse hidden textareas
unsigned
  file_number = 0,    // running number of problem pages in a chain
  last_number = 0;    // user-given last file number (0 = not given)
std::string
  title         // name (without number) of the problem page
#ifdef one_page
    = "<span class=err>Please set a title like this:</span>"
      "<br>#title An Example Title"
#endif
  , t_answer;   // teacher's answer (supposed to be a correct answer)


/* Add the number and ".html" to the file name. */
void set_number_and_html( unsigned nr ){
  unsigned pow10 = 1, ii = buf_mid + 1;
  while( nr / pow10 >= 10 ){ pow10 *= 10; ++ii; }
  buf_end = ii;
  if( buf_end + 6 > buf_size ){
    error( "Too long filename and number" ); return;
  }
  while( ii > buf_mid ){ buffer[ --ii ] = nr % 10 + '0'; nr /= 10; }
  buffer[ buf_end++ ] = '.';
  buffer[ buf_end++ ] = 'h';
  buffer[ buf_end++ ] = 't';
  buffer[ buf_end++ ] = 'm';
  buffer[ buf_end++ ] = 'l';
  buffer[ buf_end ] = '\0';
}


/* Write the answer, if there is a file for it. Then reset the answer. */
void write_answer(){
#ifndef one_page
  if( !t_answer.empty() && a_file.is_open() ){
    a_file << '\n' << t_answer << '\n';
  }
#endif
  t_answer.clear();
}


/* If an answer box is pending, make it and update the answer file. */
bool
  answer_box_pending = false,   // answer box should be printed
  answer_box_prgh = false,      // print the answer box in a new paragraph
  answer_box_printed = false,   // the most recent textarea is an answer box
  autofocus = true;
void make_pending_answer_box( bool initialize = false ){
  if( !answer_box_pending ){ return; }
  out_html( '\n' );
  if( answer_box_prgh ){ out_print( "<p>" ); }
  out_print( "<textarea rows=" ); out_print( rows_l );
  out_print( " cols=" ); out_print( cols_l );
  if( autofocus ){ out_print( " autofocus" ); autofocus = false; }
  out_print( " name=\"formula\">\t" );
  if( initialize ){ inp_copy_to_html( inp_copy_nobreak ); }
  out_html( "</textarea>\n" );
  out_html( "<textarea name=\"hidden\""
    " style=display:none>end_of_answer</textarea>\n" );
  write_answer();
  answer_box_pending = false; answer_box_printed = true;
}


/* Create the first part of a hidden text area. */
void hidden_begin(){
  out_html( "\n<textarea name=\"hidden\" style=display:none>\n" );
  if( confuse_hidden ){ out_confuse_on( random(0) ); inp_copy_status = 4; }
  if( fb_height && !answer_box_printed ){ out_html( "verbose_off\n" ); }
  answer_box_printed = false;
}


/* Create the last part of a hidden text area. */
void hidden_end(){
  out_confuse_off();
  out_html( "</textarea>\n" );
}


/* If a form is open, close it. */
bool form_is_open = false;
void close_form( bool main_buttons, bool
#ifndef one_page
  chain_continues
#endif
){
  if( !form_is_open ){ return; }

  make_pending_answer_box();

  /* Print link to next file if appropriate. */
#ifndef one_page
  if( main_buttons && make_file_chain && !fb_height ){
    hidden_begin();   // (ensures HTML mode)
    if( chain_continues ){
      buffer[ buf_mid ] = '\0';
      out_print( "next_URL " ); out_print( URL_html_base );
      out_print( buffer ); out_print( file_number + 1 );
      out_print( ".html\n" );
    }else{ out_print( "no_next_URL\n" ); }
    hidden_end();
  }
#endif

  /* Make the submit buttons. */
  out_html( '\n' );
  if( fb_height ){
    if( main_buttons ){
      out_print( "<p><input type=submit value=\"submit, new tab\">\nor " );
    }
    out_print( "<input type=submit formtarget=feedback" );
    out_print( ++fb_cnt );
    out_print( " value=\"submit, to the right\">\n" );
    out_print( "</td><td class=ifrr>\n<iframe name=feedback" );
    out_print( fb_cnt ); out_print( " height=" ); out_print( fb_height );
    out_print( "></iframe></td></tr>\n</table>\n</form>\n" );
  }else{
    if( main_buttons ){
      out_print( "<p><input type=submit value=\"submit, same tab\""
        " formtarget=_self>\nor " );
    }
    out_print( "<input type=submit value=\"submit, new tab\">\n</form>\n" );
  }

  form_is_open = answer_box_printed = false;
}


/* A file cannot be closed until it is known whether next_URL or no_next_URL
  should be printed to it. This bool marks the file to be closed then. */
bool close_previous_file = false;


/* If a file is open, close it. */
void close_file( bool chain_continues ){
  close_previous_file = false;

/* If the HTML file is not open, ensure that also the answer file is closed
  and terminate. */
#ifdef one_page
  if( !q_file_is_open ){ return; }
#else
  if( !q_file.is_open() ){
    if( a_file.is_open() ){ a_file.close(); }
    return;
  }
#endif

  /* Close any pending form. If !fb_height, may also print chaining info. */
  close_form( true, chain_continues );

  /* Print chaining information, if appropriate. */
#ifndef one_page
  if( make_file_chain && fb_height ){
    if( chain_continues ){
      buffer[ buf_mid ] = '\0';
      out_html( "\n<p><a href= \"" ); out_print( URL_html_base );
      out_print( buffer ); out_print( file_number + 1 );
      out_print( ".html\"\t>Click here to go to the next page</a>" );
    }else{
      out_html( "\n<p class=hides>This was the last page of the series!" );
    }
  }
#endif

  /* Print the file construction time. */
  time_t time_now = time(0);
  struct tm *time_fields = gmtime( &time_now );
  if( time_fields ){
    out_html( "\n<hr>\n<p class=unimp>This file was generated " );
    out_print( time_fields->tm_year + 1900 ); out_print( '-' );
    out_2digit( time_fields->tm_mon + 1 ); out_print( '-' );
    out_2digit( time_fields->tm_mday ); out_print( ' ' );
    out_2digit( time_fields->tm_hour ); out_print( ':' );
    out_2digit( time_fields->tm_min ); out_print( ':' );
    out_2digit( time_fields->tm_sec ); out_print( " UTC.\n" );
  }

  /* Close the files. */
#ifdef one_page
  q_file_is_open = false;
#else
  html_end();
  q_file.close();
  if( a_file.is_open() ){ a_file.close(); }
#endif

}


/* Open a file, unless it is open already and not marked for closing. */
void open_file(){
  make_pending_answer_box();
  if( close_previous_file ){ close_file( true ); }
#ifdef one_page
  if( q_file_is_open ){ return; }
  if( title.empty() ){ error( "No title given" ); return; }
  ++file_number; set_number_and_html( file_number );
  if( !err_ok ){ return; }
  q_file_is_open = true;
  if( !html_started ){
    html_begin( std::cout, title ); html_started = true;
    if( last_number ){
      html_h1( title, make_file_chain * file_number, last_number );
    }else{ html_h1( title ); }
  }else{
    out_html( "\n\n\n<p><hr>\n\n" );
    html_h1( title, make_file_chain * file_number, last_number );
  }
#else
  if( q_file.is_open() ){ return; }
  if( !buf_mid ){ error( "No file name given" ); return; }
  if( title.empty() ){ error( "No title given" ); return; }
  ++file_number; set_number_and_html( file_number );
  if( !err_ok ){ return; }
  std::ifstream exists( buffer );
  if( exists ){ error( ".html file exists" ); return; }
  q_file.open( buffer );
  html_begin( q_file, title, make_file_chain * file_number );
  html_h1( title, make_file_chain * file_number, last_number );
  autofocus = true; fb_cnt = 0;
#endif
  inp_prgh = true;
}


/* Open a form, unless it is open already. */
void open_form(){
  make_pending_answer_box();
  if( close_previous_file ){ close_file( true ); }
  if( form_is_open ){ return; }
  open_file();
  if( !err_ok ){ return; }
  out_html( "\n<form action=\t\"" ); out_print( URL_cgi_base );
  out_print( "mathcheck.out\" method=post target=_blank>\n" );
  if( fb_height ){ out_print( "<table class=ifr>\n<tr><td class=ifrl>\n" ); }
  form_is_open = true;
}


/*** Parameter statement and expression parsing and evaluation ***/


/* Some small helper functions */

inline unsigned abs_u( int ii ){ return ii < 0 ? -ii : ii; }

void append_digits( std::string &str, unsigned ii ){
  if( ii >= 10 ){ append_digits( str, ii / 10 ); }
  str += char( ii % 10 ) + '0';
}

void append_int( std::string &str, int ii ){
  if( ii < 0 ){ str += '-'; ii = -ii; }
  append_digits( str, ii );
}


/* Report that an expected char (or either of two) was not got. */
bool no_match_lp(){
  if( inp_chr == '(' ){ inp_get_chr(); inp_skip_white_space(); return false; }
  else{ error_( "Expected <kbd>(</kbd>" ); return true; }
}
char err_char[] = "Expected (operator or) <kbd> </kbd> or <kbd> </kbd>";
bool no_match( char ch ){
  if( inp_chr == ch ){ inp_get_chr(); inp_skip_white_space(); return false; }
  else{
    err_char[28] = ch; err_char[35] = '\0'; error_( err_char ); return true;
  }
}
bool no_match( char ch1, char ch2 ){
  if( inp_chr == ch1 || inp_chr == ch2 ){
    inp_get_chr(); inp_skip_white_space(); return false;
  }else{
    err_char[28] = ch1; err_char[44] = ch2; err_char[35] = ' ';
    error_( err_char ); return true;
  }
}


/* Integer expressions */

const char *prm_i_names[] = { "(", "gcd", "random", "select", 0 };
const unsigned prm_i_cnt = 4;   // nr of explicitly given items

int prm_i_expr();

int prm_i_atom(){
  int result = 0;

  /* Number */
  if( is_digit( inp_chr ) ){
    result = inp_get_nr(); inp_skip_white_space(); return result;
  }

  switch( inp_match( prm_i_names, prm_i_cnt ) ){

  case 0:   // variable
    if( 'a' <= inp_ltr && inp_ltr <= 'z' ){
      result = prm_int[ inp_ltr - 'a' ];
    }else{ error( "Expected an integer expression, got string" ); return 0; }
    break;

  case 1:   // "("
    result = prm_i_expr();
    if( !err_ok || no_match( ')' ) ){ return 0; }
    break;

  case 2: { // gcd
    inp_skip_white_space();
    if( no_match_lp() ){ return 0; }
    unsigned n1 = abs_u( prm_i_expr() );
    if( !err_ok || no_match( ',' ) ){ return 0; }
    unsigned n2 = abs_u( prm_i_expr() );
    if( !err_ok || no_match( ')' ) ){ return 0; }
    while( true ){
      if( !n2 ){ result = n1; break; }
      n1 %= n2;
      if( !n1 ){ result = n2; break; }
      n2 %= n1;
    }
    break; }

  case 3:   //  random
    inp_skip_white_space();
    if( no_match_lp() ){ return 0; }
    result = random( prm_i_expr() );
    if( !err_ok || no_match( ')' ) ){ return 0; }
    break;

  case 4: { // select
    inp_skip_white_space();
    if( no_match_lp() ){ return 0; }
    int ii = prm_i_expr();
    if( ii < 0 ){ ii = 0; }
    if( !err_ok || ( inp_chr != ',' && no_match( ',' ) ) ){ return 0; }
    int jj = 0, vv = 0;
    while( inp_chr == ',' ){
      inp_get_chr();
      vv = prm_i_expr();
      if( !err_ok ){ return 0; }
      if( ii == jj ){ result = vv; }
      ++jj;
    }
    if( ii >= jj ){ result = vv; }
    if( no_match( ',', ')' ) ){ return 0; }
    break; }

  default:
    error( "Unknown integer operator, the known ones are", prm_i_names );
    return 0;
  }

  inp_skip_white_space(); return result;
}


int prm_sign(){
  bool negate = false;
  while( inp_chr == '+' || inp_chr == '-' ){
    if( inp_chr == '-' ){ negate = !negate; }
    inp_get_chr(); inp_skip_white_space();
  }
  if( negate ){ return -prm_i_atom(); }
  else{ return prm_i_atom(); }
}


int prm_prod(){
  int result = prm_sign();
  while( err_ok && ( inp_chr == '*' || inp_chr == '/' || inp_chr == '%' ) ){
    char opr = inp_chr;
    inp_get_chr(); inp_skip_white_space();
    if( opr == '*' ){ result *= prm_sign(); }
    else if( opr == '/' ){ result /= prm_sign(); }
    else{ result %= prm_sign(); }
  }
  return result;
}


int prm_sum(){
  int result = prm_prod();
  while( err_ok && ( inp_chr == '+' || inp_chr == '-' ) ){
    char opr = inp_chr;
    inp_get_chr(); inp_skip_white_space();
    if( opr == '+' ){ result += prm_prod(); }
    else{ result -= prm_prod(); }
  }
  return result;
}


int prm_i_expr(){
  inp_skip_white_space();
  return prm_sum();
}


/* String expressions */

const char *prm_s_names[] = { "Pterm", "Pz", "pZ", "pterm", "select", 0 };
const unsigned prm_s_cnt = 5;   // nr of explicitly given items

void prm_s_expr( std::string & );

void prm_s_atom( std::string &result ){
  result = "";
  inp_skip_white_space();

  /* String literal */
  if( inp_chr == '\"' ){
    inp_get_chr();

    /* Scan until unescaped ", end of input, or empty line (or bad #) */
    unsigned status = 0;
    while( inp_chr != '\"' && inp_chr && status < 2 ){
      err_pos = inp_byte_now;

      /* Recognize empty lines */
      if( inp_chr == '\n' ){ ++status; }
      else if( inp_chr != ' ' ){ status = 0; }

      if( inp_chr == '`' ){
        error( "Not allowed in string literal" ); return;
      }

      /* Legal #-commands: #\n, # , #", ##, #%, #&, #<, #> */
      /* #" is kept as such like #& and so on, " is obtained with #% */
      else if( inp_chr == '#' ){
        inp_get_chr();
        if( inp_chr == '%' ){ inp_chr = '\"'; }
        else if( inp_chr == '\n' || inp_chr == ' ' ){}
        else if(
          inp_chr == '\n' || inp_chr == ' ' || inp_chr == '\"' ||
          inp_chr == '#' || inp_chr == '&' || inp_chr == '<' ||
          inp_chr == '>'
        ){ result += '#'; }
        else{ error( "Bad #-command in string literal" ); return; }
      }

      /* Append the char to the string and read the next. */
      result += inp_chr;
      inp_get_chr();

    }
    if( inp_chr != '\"' ){
      error_( "<kbd>\"</kbd> is missing somewhere" ); return;
    }

    inp_get_chr(); inp_skip_white_space(); return;
  }

  unsigned tkn = inp_match( prm_s_names, prm_s_cnt );
  switch( tkn ){

  case 0:   // variable
    if( 'A' <= inp_ltr && inp_ltr <= 'Z' ){
      result = prm_str[ inp_ltr - 'A' ];
    }else{
      result = '(';
      append_int( result, prm_int[ inp_ltr - 'a' ] );
      result += ')';
    }
    break;

  case 1: case 4: { // "Pterm", "pterm"
    inp_skip_white_space();
    if( no_match_lp() ){ return; }
    int vv = prm_i_expr();
    if( !err_ok || no_match( ',' ) ){ return; }
    std::string str; prm_s_expr( str );
    if( vv < 0 ){ vv = -vv; result += '-'; }
    else if( vv > 0 && tkn == 1 ){ result += '+'; }
    if( vv > 1 ){ append_digits( result, vv ); result += ' '; }
    if( vv ){ result += str; }
    if( !err_ok || no_match( ')' ) ){ return; }
    break; }

  case 2: { // "Pz"
    inp_skip_white_space();
    if( no_match_lp() ){ return; }
    int vv = prm_i_expr();
    if( vv < 0 ){ result += '-'; append_digits( result, -vv ); }
    else if( vv > 0 ){ result += '+'; append_digits( result, vv ); }
    if( !err_ok || no_match( ')' ) ){ return; }
    break; }

  case 3:   // "pZ"
    inp_skip_white_space();
    if( no_match_lp() ){ return; }
    append_int( result, prm_i_expr() );
    if( !err_ok || no_match( ')' ) ){ return; }
    break;

  case 5: { // "select"
    inp_skip_white_space();
    if( no_match_lp() ){ return; }
    int ii = prm_i_expr();
    if( ii < 0 ){ ii = 0; }
    if( !err_ok || ( inp_chr != ',' && no_match( ',' ) ) ){ return; }
    int jj = 0; std::string vv;
    while( inp_chr == ',' ){
      inp_get_chr();
      prm_s_expr( vv );
      if( !err_ok ){ return; }
      if( ii == jj ){ result = vv; }
      ++jj;
    }
    if( ii >= jj ){ result = vv; }
    if( no_match( ',', ')' ) ){ return; }
    break; }

  default:
    error( "Unknown string operator, the known ones are", prm_s_names );
    return;
  }

  inp_skip_white_space(); return;
}


void prm_s_expr( std::string &result ){
  prm_s_atom( result );
  while( err_ok && inp_chr == '+' ){
    inp_get_chr();
    std::string second;
    prm_s_atom( second );
    result += second;
  }
}


/* Set the value of zero or more parameters. */

enum { prm_var, prm_branch, prm_end, prm_last, prm_next };
const char *prm_p_names[] = { "branch", "end", "last", "next", 0 };
const unsigned prm_p_cnt = 4;   // nr of explicitly given items
unsigned prm_tkn = prm_p_cnt + 1;


void prm_statements( bool do_assign ){
  while( err_ok ){
    inp_skip_white_space();
    prm_tkn = inp_match( prm_p_names, prm_p_cnt );

    /* Variable */
    if( !prm_tkn ){
      char prm = inp_ltr;
      inp_skip_white_space();
      if( inp_chr != ':' ){ error( "Expected <kbd>:=</kbd>" ); return; }
      inp_get_chr();
      if( inp_chr != '=' ){ error( "Expected <kbd>:=</kbd>" ); return; }
      inp_get_chr();
      if( 'a' <= prm ){
        int ii = prm_i_expr();
        if( !err_ok ){ return; }
        if( do_assign ){ prm_int[ prm - 'a' ] = ii; }
      }else{
        std::string str; prm_s_expr( str );
        if( !err_ok ){ return; }
        if( do_assign ){ prm_str[ prm - 'A' ] = str; }
      }
    }

    /* Branch */
    else if( prm_tkn == 1 ){

      /* Read and non-negate the selector value. */
      int bb = prm_i_expr();
      if( !err_ok ){ return; }
      if( bb < 0 ){ bb = 0; }

      /* Process other branches than the optional "last"-branch. */
      int ii = 0;
      do{
        prm_statements( do_assign && ii == bb ); ++ii;
        if( !err_ok ){ return; }
      }while( prm_tkn == prm_next );
      if( prm_tkn != prm_last && prm_tkn != prm_end ){
        error( "Expected statement, <kbd>next</kbd>, <kbd>last</kbd>, or"
          " <kbd>end</kbd>" );
        return;
      }

      /* Process the optional "last"-branch. */
      if( prm_tkn == prm_last ){
        prm_statements( do_assign && ii <= bb );
        if( !err_ok ){ return; }
        if( prm_tkn != prm_end ){
          error( "Expected statement or <kbd>end</kbd>" ); return;
        }
      }

    }

    else{ break; }
  }
}


void set_parameters(){
  prm_statements( true );
  if( 2 <= prm_tkn && prm_tkn <= 4 ){
    error( "<kbd>branch</kbd> missing" ); return;
  }
  if( inp_chr && inp_chr != '\n' && inp_chr != '#' ){
    error_( "Expected an assignment, branch-statement, empty line, #-command,"
      " or end of input" );
  }
}


/*** User instructions ***/


/* These use ` on the web page. So the MathJax script should not be called.
Therefore, these must create a file of their own. */

void instructions(){

  const char *title = "MathCheck Authoring Tool Instructions";
  html_begin_begin( std::cout, title, 0, false );
  out_print( "<style type=\"text/css\">\np { text-align: justify }\n"
    "</style>\n" );
  html_begin_end();
  html_h1( title, "help" );

  out_print( "\n<p class=unimp>Version " ); html_date( copyright::date );
  out_print( '\n' );

  out_print( "\n<p class=centre><a href=\"#In\">Introduction</a> &bull;"
    " <a href=\"#WM\">Web Mode</a> &bull;"
    " <a href=\"#BM\">Batch Mode</a> &bull;"
    " <a href=\"#Co\">Commands</a> &bull;"
    " <a href=\"#SP\">String Parameters</a> &bull;"
    " <a href=\"#MS\">MathCheck Strings</a> &bull;"
    " <a href=\"#Va\">Variables</a>\n" );

  out_print( "\n\n<h2 id=In>Introduction</h2>\n" );

  out_print( "\n<p>This program inputs MathCheck problem descriptions and"
    " produces the corresponding HTML problem pages.\n" );
  out_print( "It has two modes, a <a href=\"#WM\">web mode</a> and <a"
    " href=\"#BM\">batch mode</a>.\n" );
  out_print( "If you do not know which mode you are using, then you are using"
    " the web mode and can ignore the instructions on the batch mode.\n" );

  out_print( "\n<p>A problem description consists of a sequence of"
    " <a href=\"#Co\">commands</a> of the form"
    " <kbd>#</kbd><em>&lt;command_name&gt;</em> <em>&lt;parameters&gt;</em>"
    "&nbsp;.\n" );
  out_print( "The most important commands are"
    " <a href=\"#title\"><kbd>#title</kbd></a>,"
    " <a href=\"#question\"><kbd>#question</kbd></a>, and"
    " <a href=\"#hidden\"><kbd>#hidden</kbd></a>.\n" );
  out_print( "They specify the title of the problem page, the question(s) on"
    " the page, and the hidden information that MathCheck uses when checking"
    " the solution.\n" );
  out_print( "In the batch mode, also"
    " <a href=\"#filename\"><kbd>#filename</kbd></a> is important.\n" );

  out_print( "\n<p><a href=\"#SP\">String Parameters</a> contain text that"
    " will be shown to the student (perhaps after some processing), are"
    " intended for MathCheck, or both.\n" );
  out_print( "<a href=\"#MS\">MathCheck strings</a> are string parameters"
    " that are intended for MathCheck.\n" );
  out_print( "Although a lot of effort was made to make their use as simple"
    " and safe as possible, some tricky issues cannot be avoided.\n" );
  out_print( "Therefore, <span class=yellow>please at least skim through"
    " these two sections</span>.\n" );
  out_print( "<a href=\"#Va\">Variables</a> allow (perhaps randomized)"
    " parameters in the problem pages.\n" );
  out_print( "You can safely ignore that section, if you want.\n" );

  out_print( "\n<p>These instructions are obtained by submitting an empty"
    " input in the web mode.\n" );
  out_print( "For convenience, there may also be copies of the instructions"
    " as static web pages.\n" );
  out_print( "They may be out of date.\n" );
  out_print( "Therefore, if what you read does not seem to match with the"
    " reality, please switch to the automatic version.\n" );
  out_print( "(Unfortunately, it, too, may be out of date, because after"
    " implementing a new feature or a modification to an old one, it has to"
    " be tested before it is reasonable to update the instructions.)\n" );

  out_print( "\n\n<h2 id=WM>Web Mode</h2>\n" );

  out_print( "\n<p>In the web mode, the program produces a single web"
    " page.\n" );
  out_print( "The generated page can be immediately used as such, or it can"
    " be saved with the save command of the web browser for (editing and)"
    " uploading to the teacher's site for future use.\n" );

  out_print( "\n<p>The program makes some sanity checks on the input.\n" );
  out_print( "The first error causes an error message on the produced web"
    " page, after which (in most cases) the program stops.\n" );

  out_print( "\n<p>If the input specifies more than one MathCheck problem"
    " pages, the web mode presents them as a sequence on the single produced"
    " page.\n" );
  out_print( "The batch mode splits them to many files.\n" );

  out_print( "\n\n<h2 id=BM>Batch Mode</h2>\n" );

  out_print( "\n<p>The batch mode reads the standard input and produces zero"
    " or more HTML files that contain the problem pages.\n" );
  out_print( "The names of the files are of the form"
    " <em>&lt;name base&gt;</em><em>&lt;number&gt;</em><kbd>.html</kbd>"
    " (please see <a href=\"#filename\"><kbd>#filename</kbd></a>).\n" );
  out_print( "By default, the problems with the same"
     " <em>&lt;name base&gt;</em> are <em>chained</em>, that is, the feedback"
     " that MathCheck provides on a correct answer contains a link to the"
     " next problem.\n" );

  out_print( "\n<p>The error messages from sanity checks go to the standard"
    " output.\n" );

  out_print( "\n<p>To the extent that the input contains the answers (please"
    " see <a href=\"#answer\"><kbd>#answer</kbd></a>), the batch mode also"
    " produces <kbd>.mc</kbd> files containing the hidden information and the"
    " answers.\n" );
  out_print( "They can be used for testing both the problem and"
    " MathCheck.\n" );

  out_print( "\n\n<h2 id=Co>Commands</h2>\n" );

  out_print( "\n<dl>\n" );

  out_print( "\n<dt id=answer><kbd>#answer</kbd> <em>&lt;MathCheck"
    " string&gt;</em>\n" );
  out_print( "<dd>If this command is given, it must be between a question and"
    " the generation of its answer box, and the string should be a correct"
    " answer.\n" );
  out_print( "In the batch mode, it causes a <kbd>.mc</kbd> file be created"
    " that can be used for testing both the problem and MathCheck.\n" );
  out_print( "In the web mode, <kbd>#answer</kbd> has no effect.\n" );

  out_print( "\n<dt><kbd>#box_size</kbd> <em>&lt;rows&gt;</em>"
    " <em>&lt;cols&gt;</em>\n" );
  out_print( "<dd>The parameters are positive integers specifying the height"
    " and width of the answer box.\n" );
  out_print( "There are two levels of box sizes.\n" );
  out_print( "Each <a href=\"#filename\"><kbd>#filename</kbd></a> starts with"
    " a default box size.\n" );
  out_print( "If <kbd>#box_size</kbd> is given soon enough after a"
    " <kbd>#filename</kbd>, it will be used as the global default.\n" );
  out_print( "If given later, it remains valid until the next"
    " <kbd>#box_size</kbd> or <kbd>#filename</kbd>.\n" );

  out_print( "\n<dt><kbd>#confuse_off</kbd>" );
  out_print( "\n<dt><kbd>#confuse_on</kbd>\n" );
  out_print( "<dd>If confusion is on, the contents of the text areas"
    " generated by <a href=\"#hidden\"><kbd>#hidden</kbd></a> are not"
    " human-readable.\n" );
  out_print( "However, the confusion is easily broken with a computer (and is"
    " thus not called encryption).\n" );

  out_print( "\n<dt id=filename><kbd>#filename</kbd>"
    " <em>&lt;name base&gt;</em>\n" );
  out_print( "<dd>The <em>&lt;name base&gt;</em> consists of a non-empty"
    " sequence of the following characters: <kbd>a</kbd> &hellip;"
    " <kbd>z</kbd>, <kbd>A</kbd> &hellip; <kbd>Z</kbd>, <kbd>_</kbd>.\n" );
  out_print( "The generated files will have filenames of the form"
    " <em>&lt;name base&gt;</em><em>&lt;number&gt;</em><kbd>.html</kbd>&nbsp;"
    ", where the number grows starting from 1 until the next"
    " <kbd>#filename</kbd>.\n" );
  out_print( "In the batch mode, this command must occur at least once"
    " (before anything is written on the file).\n" );
  out_print( "In the web mode, it is optional and has no effect.\n" );

  out_print( "\n<dt id=hidden><kbd>#hidden</kbd>"
    " <em>&lt;MathCheck string&gt;</em>\n" );
  out_print( "<dd>This command generates a hidden text area with the string"
    " as its contents.\n" );
  out_print( "This is where the teacher specifies the problem class and gives"
    " the information that MathCheck uses for checking the student's"
    " answer.\n" );
  out_print( "If used, this command must be between a question and the"
    " generation of its answer box.\n" );

  out_print( "\n<dt id=html><kbd>#html</kbd>\n" );
  out_print( "<dd>After this command, the next string will be used as HTML"
    " markup instead of as text (except in the case of <a "
    "href=\"#answer\"><kbd>#answer</kbd></a> and within"
    " <kbd>`</kbd>-delimited segments).\n" );
  out_print( "Please see <a href=\"#SP\">String Parameters</a> for more"
    " information.\n" );
  out_print( "<span class=yellow>If you use this command, please run a HTML"
    " validator on the result.</span>\n" );

  out_print( "\n<dt id=initial><kbd>#initial</kbd> <em>&lt;MathCheck"
    " string&gt;</em>\n" );
  out_print( "<dd>This command causes the generation of a pending answer box"
    " and puts the string as its initial content.\n" );

  out_print( "\n<dt><kbd>#last_number</kbd> <em>&lt;number&gt;</em>\n" );
  out_print( "<dd>The number of files in a chain can be given with this"
    " command.\n" );
  out_print( "Doing so makes the page headers show it.\n" );
  out_print( "After generating the files, the tool checks that the number is"
    " correct.\n" );
  out_print( "The tool also uses this information to reason whether to print"
    " a chain continuation or chain ends command.\n" );

  out_print( "\n<dt id=let><kbd>#let</kbd> <em>&lt;statements&gt;</em>\n" );
  out_print( "<dd>The parameter is a (possibly empty) sequence of assignments"
    " and <kbd>branch</kbd>-statements.\n" );
  out_print( "An assignment is of the form <em>&lt;variable&gt;</em>"
    " <kbd>:=</kbd> <em>&lt;expression&gt;</em>.\n" );
  out_print( "Please do not use <kbd>branch</kbd>-statements, because the"
    " design proved clumsy, so they will perhaps be moved to the level of"
    " <kbd>#</kbd>-commands.\n" );
  out_print( "A <kbd>branch</kbd>-statement is of the form <kbd>branch</kbd>"
    " <em>&lt;integer&nbsp;expression&gt;</em> <em>&lt;statements&gt;</em>"
    " <kbd>next</kbd> <em>&lt;statements&gt;</em> <kbd>last</kbd>"
    " <em>&lt;statements&gt;</em> <kbd>end</kbd>, where the"
    " <kbd>last</kbd>-branch is optional and there may be zero or more"
    " <kbd>next</kbd>-branches.\n" );
  out_print( "The value of the integer expression picks the branch that is"
    " executed.\n" );
  out_print( "Negative values and 0 pick the first branch.\n" );
  out_print( "If the value is bigger than the number of"
    " <kbd>next</kbd>-branches, the <kbd>last</kbd>-branch is chosen, if it"
    " is present.\n" );
  out_print( "Please see <a href=\"#Va\">Variables</a>.\n" );

  out_print( "\n<dt><kbd>#MathCheck</kbd>\n" );
  out_print( "<dd>This command generates a text paragraph with copyright"
    " information on this tool.\n" );

  out_print( "\n<dt><kbd>#no_chain</kbd>\n" );
  out_print( "<dd>In batch mode, this command suppresses the chaining of"
    " problem pages and the showing of the file number in the title and"
    " header of the generated HTML files.\n" );
  out_print( "The effect continues until next <a "
    "href=\"#filename\"><kbd>#filename</kbd></a>.\n" );
  out_print( "In the web mode, the chaining is not applied in any case, and"
    " <kbd>#no_chain</kbd> has no effect.\n" );

  out_print( "\n<dt><kbd>#no_focus</kbd>\n" );
  out_print( "<dd>By default, when a problem page is opened, the focus is in"
    " the first answer box.\n" );
  out_print( "If this command is written before the construction of the box,"
    " it switches this behaviour off for the current problem page.\n" );

  out_print( "\n<dt><kbd>#Question</kbd> <em>&lt;string&gt;</em>" );
  out_print( "\n<dt id=question><kbd>#question</kbd>"
    " <em>&lt;string&gt;</em>\n" );
  out_print( "<dd>The string will appear as the question text.\n" );
  out_print( "Each <kbd>#question</kbd> generates a new answer box to the"
    " problem web page.\n" );
  out_print( "Before generating it, the program continues reading the input"
    " for commands such as <a href=\"#hidden\"><kbd>#hidden</kbd></a> that"
    " are related to the question or the answer box.\n" );
  out_print( "The answer box is automatically generated upon a command such"
    " as <a href=\"#text\"><kbd>#text</kbd></a> that relates to something"
    " else, or when the input ends.\n" );
  out_print( "In particular, the next <kbd>#question</kbd> may be such a"
    " command.\n" );
  out_print( "<kbd>#Question</kbd> differs in one respect: in the batch mode,"
    " if a previous file is still open, <kbd>#Question</kbd> does and"
    " <kbd>#question</kbd> does not first close it.\n" );
  out_print( "These commands are not obligatory, but without them there will"
    " be no answer boxes.\n" );

  out_print( "\n<dt><kbd>#SUbmit</kbd>" );
  out_print( "\n<dt><kbd>#Submit</kbd>" );
  out_print( "\n<dt><kbd>#submit</kbd>\n" );
  out_print( "<dd>Submit buttons are usually generated automatically to the"
    " right place.\n" );
  out_print( "With these commands, it is possible to force the generation at"
    " a certain place.\n" );
  out_print( "<kbd>#SUbmit</kbd> generates both the submit same tab and"
    " submit new tab buttons.\n" );
  out_print( "If the chaining of problem pages is on, it also generates a"
    " chaining command.\n" );
  out_print( "If <kbd>last_number</kbd> has been given, it chooses between"
    " continuing and terminating the chain accordingly, and otherwise"
    " continues the chain.\n" );
  out_print( "<kbd>#Submit</kbd> is otherwise similar, but terminates the"
    " chain if <kbd>last_number</kbd> has not been given.\n" );
  out_print( "The choice between continuing and terminating the chain depends"
    " on user-given information, because the program cannot check the"
    " existence of a next file at this point of the input.\n" );
  out_print( "<kbd>#submit</kbd> generates a submit new tab button without"
    " any chaining information, to facilitate submit buttons that only use a"
    " subset of the answer boxes on a problem page.\n" );

  out_print( "\n<dt><kbd>#suomi</kbd>\n" );
  out_print( "<dd>Sets the &ldquo;lang&rdquo; attribute of the HTML pages to"
    " &ldquo;fi&rdquo;, for Finnish.\n" );
  out_print( "By default, the attribute is for English.\n" );
  out_print( "The HTML validator gives an error message if the attribute is"
    " too much in contradiction with the actual content of the HTML"
    " page.\n" );

  out_print( "\n<dt><kbd>#Text</kbd> <em>&lt;string&gt;</em>" );
  out_print( "\n<dt id=text><kbd>#text</kbd> <em>&lt;string&gt;</em>" );
  out_print( "\n<dt><em>&lt;string&gt;</em>\n" );
  out_print( "<dd>These commands copy the string as a text paragraph.\n" );
  out_print( "In batch mode, if a previous file is still open,"
    " <kbd>#Text</kbd> does and the other two do not first close it.\n" );
  out_print( "When text goes to the end of the previous file although it was"
    " meant to go to the beginning of the next file, <kbd>#Text</kbd> can be"
    " used to fix the problem.\n" );

  out_print( "\n<dt id=title><kbd>#title</kbd> <em>&lt;string&gt;</em>\n" );
  out_print( "<dd>The string (perhaps extended with a number) will appear as"
    " the title and header of the produced web page(s).\n" );
  out_print( "This command must occur at least once in the web mode, and at"
    " least once for each <a href=\"#filename\"><kbd>#filename</kbd></a> in"
    " the batch mode.\n" );
  out_print( "It must occur (after the <kbd>#filename</kbd> and) before any"
    " command that generates something on the problem page (such as <a"
    " href=\"#question\"><kbd>#question</kbd></a>).\n" );

  out_print( "\n<dt><kbd>#%</kbd> <em>&lt;string&gt;</em>\n" );
  out_print( "<dd>This command has no effect.\n" );
  out_print( "It facilitates writing comments in the input file.\n" );

  out_print( "\n</dl>\n" );

  out_print( "\n\n<h2 id=SP>String Parameters</h2>\n" );

  out_print( "\n<p>A string may consist of any UTF-8 characters.\n" );
  out_print( "(UTF-8 is a very comprehensive set of characters.)\n" );
  out_print( "<span class=yellow>Please be careful with <kbd>#</kbd> and"
    " <kbd>`</kbd></span>, because, as is explained below, they do not mean"
    " themselves.\n" );

  out_print( "\n<p>A string may contain spaces and newlines.\n" );
  out_print( "It terminates at an empty line, an instance of <kbd>#</kbd>"
    " other than those mentioned below, or the end of the input.\n" );
  out_print( "(An empty line contains nothing or only spaces.)\n" );
  out_print( "Redundant spaces at the beginning, end, and inside the string"
    " are removed, with the exception of spaces used for indenting"
    " lines.\n" );
  out_print( "Also newlines at the beginning and end (but not inside) of the"
    " string are removed.\n" );

  out_print( "\n<p>In the informal text, mathematical notation can be"
    " presented in <a href=\"http://asciimath.org/\">AsciiMath</a> that is"
    " preceded and succeeded by <kbd>`</kbd>.\n" );
  out_print( "Also allowing MathCheck notation is in future plans.\n" );
  out_print( "A further future plan is to facilitate the use of MathCheck"
    " drawing commands.\n" );
  out_print( "In the formal parts that go to MathCheck (that is, <a"
    " href=\"#MS\">MathCheck strings</a>), mathematical notation is expressed"
    " in MathCheck notation.\n" );

  out_print( "\n<p>In web pages, <kbd>&quot;</kbd>, <kbd>&amp;</kbd>,"
    " <kbd>&lt;</kbd>, and <kbd>&gt;</kbd> have a special meaning.\n" );
  out_print( "As a consequence, if they are written on a web page as such,"
    " they do not necessarily appear as themselves.\n" );
  out_print( "By default, in <kbd>.html</kbd> (but not in <kbd>.mc</kbd>)"
    " files, this program replaces them by so-called character entities"
    " <kbd>&amp;quot;</kbd>, <kbd>&amp;amp;</kbd>, <kbd>&amp;lt;</kbd>, and"
    " <kbd>&amp;gt;</kbd>, which appear as <kbd>&quot;</kbd>,"
    " <kbd>&amp;</kbd>, <kbd>&lt;</kbd>, and <kbd>&gt;</kbd>.\n" );
  out_print( "This means that as long as you do not want to write HTML"
    " markup, you need not worry about this issue.\n" );

  out_print( "\n<p>To facilitate the use of HTML markup in problem pages,"
    " encoding as character entities is switched off by <a"
    " href=\"#html\"><kbd>#html</kbd></a>.\n" );
  out_print( "\nHowever, <kbd>#html</kbd> affects neither"
    " <kbd>`</kbd>-delimited segments nor the parameter of <a"
    " href=\"#answer\"><kbd>#answer</kbd></a>, because they are not used as"
    " HTML markup.\n" );
  out_print( "If the segment is started with <kbd>#`</kbd> instead of"
    " <kbd>`</kbd>, then encoding as character entities remains switched"
    " off.\n" );
  out_print( "(This feature is probably unnecessary, but has been implemented"
    " to be on the safe side.)\n" );
  out_print( "The segment may be ended with <kbd>`</kbd> or <kbd>#`</kbd>"
    " independently of which one started it.\n" );
  out_print( "Also the contents of string variables are immune to"
    " <kbd>#html</kbd>, to make their use safer.\n" );
  out_print( "(With integer variables, the issue does not arise.)\n" );
  out_print( "The effect of <kbd>#html</kbd> switches off automatically after"
    " copying one string.\n" );

  out_print( "\n<p>The sequences <kbd>#</kbd><em>&lt;newline&gt;</em>,"
    " <kbd>#</kbd><em>&lt;space&gt;</em>, <kbd>#&quot;</kbd>, <kbd>##</kbd>,"
    " <kbd>#&amp;</kbd>, <kbd>#&lt;</kbd>, <kbd>#&gt;</kbd>, and"
    " <kbd>#`</kbd> are converted to <em>&lt;newline&gt;</em>,"
    " <em>&lt;space&gt;</em>, <kbd>&quot;</kbd>, <kbd>#</kbd>,"
    " <kbd>&amp;</kbd>, <kbd>&lt;</kbd>, <kbd>&gt;</kbd>, and"
    " <kbd>`</kbd>.\n" );
  out_print( "This facilitates, for instance, forcing a space at the end of"
    " the parameter of <a href=\"#initial\"><kbd>#initial</kbd></a>.\n" );
  out_print( "One can also have <kbd>#</kbd> on the produced web"
    " page(s).\n" );
  out_print( "The conversion causes <kbd>#`</kbd> to start an AsciiMath"
    " segment as described above, and gives an alternative to <a"
    " href=\"#html\"><kbd>#html</kbd></a> that is handy for small amounts of"
    " markup among big amounts of mathematics.\n" );
  out_print( "It also makes it possible to have HTML markup in string"
    " variables.\n" );
  out_print( "<span class=yellow>One should not use <kbd>#&quot;</kbd>,"
    " <kbd>#&amp;</kbd>, <kbd>#&lt;</kbd>, and <kbd>#&gt;</kbd> in the"
    " parameter of <a href=\"#hidden\"><kbd>#hidden</kbd></a></span>, because"
    " if something is forced correct with them in the <kbd>.html</kbd> file,"
    " then it becomes wrong in the <kbd>.mc</kbd> file.\n" );

  out_print( "\n<p>For <kbd>#</kbd><em>&lt;letter&gt;</em>, please see"
    " <a href=\"#Va\">Variables</a>.\n" );

  out_print( "\n\n<h2 id=MS>MathCheck Strings</h2>\n" );

  out_print( "\n<p>Some commands take a string parameter that consists of"
    " MathCheck notation.\n" );
  out_print( "Please see <a href=\t\"" ); out_print( URL_html_base );
  out_print( "symbols.html\">here for MathCheck notation</a>, and please read"
    " <a href=\"#SP\">String Parameters</a>.\n" );
  out_print( "<span class=yellow>One should not use <kbd>`</kbd> and"
    " <kbd>#`</kbd> in MathCheck notation</span>, to avoid confusion with"
    " AsciiMath.\n" );

  out_print( "\n<p>To get <kbd>#</kbd>, please write <kbd>##</kbd>.\n" );
  out_print( "If the program removes a space or newline that you want to"
    " stay, please try putting <kbd>#</kbd> to its front.\n" );
  out_print( "The reasons for these are explained in <a href=\"#SP\">String"
    " Parameters</a>.\n" );

  out_print( "\n\n<h2 id=Va>Variables</h2>\n" );

  out_print( "\n<p>Variables facilitate random values on problem pages and"
    " simplify writing identically repeating material.\n" );
  out_print( "The names of integer variables are <kbd>a</kbd>, &hellip;,"
    " <kbd>z</kbd> and the names of string variables are <kbd>A</kbd>,"
    " &hellip;, <kbd>Z</kbd>.\n" );
  out_print( "Their values are assigned with the <a"
    " href=\"#let\"><kbd>#let</kbd> command</a>.\n" );
  out_print( "A variable retains its value until assigned a new value, even"
    " if the problem page or file name base is changed in between.\n" );

  out_print( "\n<p>In <a href=\"#SP\">string parameters</a>, each"
    " <kbd>#</kbd><em>&lt;variable&gt;</em> is replaced by the value of the"
    " variable.\n" );
  out_print( "The value is surrounded by white space, to avoid accidental"
    " fusion of two tokens into one like in <kbd>sin h</kbd> &rarr;"
    " <kbd>sinh</kbd>.\n" );
  out_print( "The value of an integer variable is also surrounded by"
    " <kbd>(</kbd> and <kbd>)</kbd>, to ensure that the mathematical meaning"
    " is not affected by the environment.\n" );
  out_print( "Other appearance may be obtained by first copying the value to"
    " a string variable.\n" );

  out_print( "\n<p>Integer expressions may contain literals (non-empty"
    " sequences of digits), integer variables (without the <kbd>#</kbd> in"
    " front), parentheses <kbd>(</kbd> and <kbd>)</kbd>, unary <kbd>+</kbd>"
    " and <kbd>&dash;</kbd>, and binary <kbd>+</kbd>, <kbd>&dash;</kbd>,"
    " <kbd>*</kbd>, <kbd>/</kbd>, and <kbd>%</kbd>.\n" );
  out_print( "They have their familiar meanings (<kbd>%</kbd> is"
    " modulo).\n" );
  out_print( "Unary operators have the highest precedence, then <kbd>*</kbd>,"
    " <kbd>/</kbd>, and <kbd>%</kbd>, and finally binary <kbd>+</kbd> and"
    " <kbd>&dash;</kbd>.\n" );
  out_print( "The binary operators associate to the left.\n" );

  out_print( "\n<p>Integer expressions may also contain the following"
    " functions:\n" );

  out_print( "\n<dl>\n" );

  out_print(
    "\n<dt><kbd>gcd(</kbd><em>m</em><kbd>,</kbd><em>n</em><kbd>)</kbd>\n" );
  out_print( "<dd>Returns the greatest common divisor of <em>m</em> and"
    " <em>n</em>.\n" );

  out_print( "\n<dt><kbd>random(</kbd><em>n</em><kbd>)</kbd>\n" );
  out_print( "<dd>Returns a random value between 0 and"
    " <em>n</em>&ndash;1.\n" );

  out_print( "\n<dt><kbd>select(</kbd><em>i</em><kbd>,</kbd>"
    " <em>v</em><sub>0</sub><kbd>,</kbd> &hellip;<kbd>,</kbd>"
    " <em>v</em><sub><em>n</em></sub><kbd>)</kbd>\n" );
  out_print( "<dd>If <em>i</em> is between 0 and <em>n</em> inclusive,"
    " <kbd>select</kbd> returns <em>v</em><sub><em>i</em></sub>.\n" );
  out_print( "If <em>i</em> &lt; 0, <kbd>select</kbd> returns"
    " <em>v</em><sub>0</sub>.\n" );
  out_print( "If <em>i</em> &gt; <em>n</em>, <kbd>select</kbd> returns"
    " <em>v</em><sub><em>n</em></sub>.\n" );

  out_print( "\n</dl>\n" );

  out_print( "\n<p>The implementation is not protected against arithmetic"
    " overflows.\n" );
  out_print( "This means that arithmetic is unsafe above roughly 2 &times;"
    " 10<sup>9</sup>.\n" );

  out_print( "\n<p>A string expression consists of one or more string atoms"
    " separated by <kbd>+</kbd>.\n" );
  out_print( "A string atom is either a string literal, an integer or string"
    " variable (without the # in front), or any of the following"
    " functions.\n" );
  out_print( "The value of an integer variable is surrounded by <kbd>(</kbd>"
    " and <kbd>)</kbd>.\n" );

  out_print( "\n<p>A string literal begins and ends with"
    " <kbd>&quot;</kbd>.\n" );
  out_print( "It can contain any characters, with the following"
    " exceptions.\n" );
  out_print( "It cannot contain <kbd>`</kbd>.\n" );
  out_print( "It cannot contain <kbd>&quot;</kbd> as such (but the same"
    " effect is obtained with <kbd>#%</kbd>).\n" );
  out_print( "It cannot contain an empty line or a line that only contains"
    " spaces.\n" );
  out_print( "It can contain no other instances of <kbd>#</kbd> than"
    " <kbd>#</kbd><em>&lt;newline&gt;</em>,"
    " <kbd>#</kbd><em>&lt;space&gt;</em>, <kbd>#&quot;</kbd>, <kbd>##</kbd>,"
    " <kbd>#%</kbd>, <kbd>#&amp;</kbd>, <kbd>#&lt;</kbd>, and"
    " <kbd>#&gt;</kbd>.\n" );
  out_print( "Here <kbd>#%</kbd> denotes <kbd>&quot;</kbd>, and the rest have"
    " the same meaning as in <a href=\"#SP\">string parameters</a>.\n" );

  out_print( "\n<dl>\n" );

  out_print(
    "\n<dt><kbd>Pterm(</kbd><em>n</em><kbd>,</kbd><em>T</em><kbd>)</kbd>\n" );
  out_print(
    "<dt><kbd>pterm(</kbd><em>n</em><kbd>,</kbd><em>T</em><kbd>)</kbd>\n" );
  out_print( "<dd>These return <em>nT</em> formatted as a summand.\n" );
  out_print( "If <em>n</em> = 0, these return the empty string.\n" );
  out_print( "If <em>n</em> = 1, <kbd>Pterm</kbd> returns"
    " <kbd>+</kbd><em>T</em> and <kbd>pterm</kbd> returns <em>T</em>.\n" );
  out_print( "If <em>n</em> = &ndash;1, these return"
    " <kbd>&ndash;</kbd><em>T</em>.\n" );
  out_print( "If <em>n</em> &gt; 1, <kbd>Pterm</kbd> returns"
    " <kbd>+</kbd><em>n</em>&nbsp;<em>T</em> and <kbd>pterm</kbd> returns"
    " <em>n</em>&nbsp;<em>T</em>.\n" );
  out_print( "If <em>n</em> &lt; &ndash;1, these return"
    " <em>n</em>&nbsp;<em>T</em> (where <em>n</em> yields the minus"
    " sign).\n" );

  out_print(
    "\n<dt><kbd>Pz(</kbd><em>n</em><kbd>)</kbd>\n" );
  out_print( "<dd>Returns <em>n</em> formatted as a summand.\n" );
  out_print( "If <em>n</em> = 0, returns the empty string.\n" );
  out_print( "If <em>n</em> &gt; 0, returns <kbd>+</kbd><em>n</em>.\n" );
  out_print( "If <em>n</em> &lt; 0, returns <em>n</em> (including the minus"
    " sign).\n" );

  out_print( "\n<dt><kbd>pZ(</kbd><em>n</em><kbd>)</kbd>\n" );
  out_print( "<dd>Returns <em>n</em> in the usual format.\n" );
  out_print( "That is, the sign is printed only if it is <kbd>&ndash;</kbd>,"
    " and the value is printed even if it is 0.\n" );

  out_print( "\n<dt><kbd>select(</kbd><em>i</em><kbd>,</kbd>"
    " <em>V</em><sub>0</sub><kbd>,</kbd> &hellip;<kbd>,</kbd>"
    " <em>V</em><sub><em>n</em></sub><kbd>)</kbd>\n" );
  out_print( "<dd>This function is similar to the integer function with the"
    " same name.\n" );
  out_print( "Here the <em>V</em><sub><em>i</em></sub> and the return value"
    " are strings.\n" );

  out_print( "\n</dl>\n" );

  out_print( "\n<p><hr>\n" );

  html_end();

}


/*** Main program ***/


/* Main program */
int main(){
  random_init();

  /* Read the first character and find the first #-token. */
  inp_start(); inp_get_tkn();

#ifdef one_page
  /* If no input was available, print help and terminate. */
  if( inp_tkn == tkn_eof ){ instructions(); return 0; }
#endif

  /* Main loop */
  while( inp_tkn != tkn_eof && err_ok ){
    switch( inp_tkn ){

    /* In-string #-commands: unread the command, interpret as #text */
    case tkn_prm: case tkn_nl: case tkn_space: case tkn_quot: case tkn_hash:
    case tkn_amp: case tkn_lt: case tkn_gt: case tkn_grave:
      inp_tkn = tkn_text; inp_unread( 2 ); continue;

    /* Teacher's answer */
    case tkn_answer:
      if( !answer_box_pending ){ error( "No pending answer box" ); continue; }
      answer_box_prgh = inp_prgh;
      t_answer.clear(); inp_copy_status = 4; inp_copy_to_str( &t_answer );
#ifndef one_page
      if( !a_file.is_open() ){
        buf_end -= 4;
        buffer[ buf_end++ ] = 'm';
        buffer[ buf_end++ ] = 'c';
        buffer[ buf_end ] = '\0';
        std::ifstream exists( buffer );
        if( exists ){ error( ".mc file exists" ); continue; }
        a_file.open( buffer );
      }
#endif
      continue;

    /* Set answer textarea size. If given before the first #question after a
      #filename, affects the default that is adopted upon #filename. */
    case tkn_box_size:
      answer_box_prgh = inp_prgh;
      rows_l = inp_get_nr(); cols_l = inp_get_nr();
      if( !file_number ){ rows_g = rows_l; cols_g = cols_l; }
      break;

    /* Switch the confusion of the hidden fields off and on. */
    case tkn_confuse_off:
      answer_box_prgh = inp_prgh;
      confuse_hidden = false; break;
    case tkn_confuse_on:
      answer_box_prgh = inp_prgh;
      confuse_hidden = true; break;

    /* Set the height of the feedback area. */
    case tkn_feedback_height:
      close_form( true, false );
      answer_box_prgh = inp_prgh;
      fb_height = inp_get_nr();
      break;

    /* Get the filename and reset the title. */
    case tkn_filename:
      close_file( false );
      if( last_number && file_number != last_number ){
        error( "The last file number does not match" ); continue;
      }
      file_number = buf_mid = last_number = 0;
      rows_l = rows_g; cols_l = cols_g;
      title.clear(); make_file_chain = true;
      inp_skip_white_space();
      if( !is_ltr( inp_chr ) ){
        error( "File name must start with a letter" ); continue;
      }
      while(
        ( is_ltr_dgt( inp_chr ) || inp_chr == '_' ) && buf_mid < buf_size - 7
      ){ buffer[ buf_mid++ ] = inp_chr; inp_get_chr(); }
      if( buf_mid >= buf_size - 7 ){ error( "Too long filename" ); continue; }
      if( inp_chr && inp_chr != ' ' && inp_chr != '\n' ){
        error_( "Wrong character in filename" ); continue;
      }
      buf_end = buf_mid;    // safety precaution in case of accidental use
      buffer[ buf_end ] = '\0';
      break;

    /* Create a hidden textarea. */
    case tkn_hidden:
      if( !answer_box_pending ){ error( "No pending answer box" ); continue; }
      answer_box_prgh = inp_prgh;
      hidden_begin();
#ifdef one_page
      inp_copy_to_html( inp_copy_nobreak );
#else
      inp_copy_to_html(
        !t_answer.empty() && a_file.is_open() ? inp_copy_answer
          : inp_copy_nobreak
      );
#endif
      out_html( '\n' );
      hidden_end();
      continue;

    /* Switch on HTML copying mode (then " and so on will not be escaped). */
    case tkn_html:
      inp_copy_status = 3; break;

    /* Create a textarea with initial content, and update the answer file. */
    case tkn_initial:
      if( !answer_box_pending ){ error( "No pending answer box" ); continue; }
      answer_box_prgh = inp_prgh;
      make_pending_answer_box( true );
      continue;

    /* Create a button for printing brief or long instructions. */
    case tkn_Instructions:
    case tkn_instructions: {
      bool is_brief = inp_tkn == tkn_instructions;
      if( form_is_open ){
        error( "This command cannot be used inside a form." ); continue;
      }
      answer_box_prgh = inp_prgh;
      if( close_previous_file ){ close_file( true ); }
      open_file();
      if( !err_ok ){ continue; }
      out_html( "\n<form action=\t\"" ); out_print( URL_cgi_base );
      out_print( "mathcheck.out\" method=post target=_blank>" );
      hidden_begin();
      out_print( inp_tkn == tkn_Instructions ? "help" : "brief_help" );
      out_print( " end_of_answer" );
      hidden_end();
      inp_copy_to_html(); out_print( '\n' );
      out_html( "<input type=submit value=\"" );
      if( is_brief ){ out_html( "brief " ); }
      out_html( "typing instructions, new tab\">\n</form>\n" );
      continue;
    }

    /* Set the number of the last file in a chain. */
    case tkn_last_number:
      last_number = inp_get_nr(); break;

    /* Set the value of zero or more parameters. */
    case tkn_let:
      set_parameters(); break;

    /* Print some MathCheck information. */
    case tkn_MathCheck:
      open_file();
      if( !err_ok ){ continue; }
      out_print( "\n<p><hr>\n" );
      html_copyright( "Authoring Tool" );
      out_print( "\n<p><hr>\n" ); inp_prgh = true;
      break;

    /* Start a new file. */
    case tkn_new_file:
      close_file( true ); break;

    /* Switch off the construction of next URL information. */
    case tkn_no_chain:
      make_file_chain = false; break;

    /* Do not automatically put cursor to the first textarea. */
    case tkn_no_focus:
      autofocus = false; break;

    /* Comment */
    case tkn_percent:
      answer_box_prgh = inp_prgh;
      { std::string str; inp_copy_to_str( &str ); }
      continue;

    /* Print a question, starting or not starting a new form or file. */
    case tkn_Question:
      if( fb_height ){ close_form( true, false ); }
      else{ close_previous_file = true; }
    case tkn_question:
      open_form();
      if( !err_ok ){ continue; }
      out_html( '\n' );
      if( inp_prgh ){ out_html( "<p>\t" ); inp_prgh = false; }
      inp_copy_to_html();
      out_html( '\n' );
      answer_box_pending = true; answer_box_prgh = false;
      continue;

    /* Make submit buttons (if a form is open).
      tkn_submit makes only the "new tab" button, others make both.
      If the making of next_URL and no_next_URL is on, the result is
      determined by file_number if it is non-zero, and otherwise by the token:
      tkn_Submit makes no_next_URL and tkn_SUbmit makes next_URL. */
    case tkn_SUbmit: case tkn_Submit: case tkn_submit:
      if( !form_is_open ){ error( "No question given" ); continue; }
      answer_box_prgh = inp_prgh;
      close_form(
        inp_tkn != tkn_submit,
           ( !file_number && inp_tkn == tkn_SUbmit )
        || ( file_number && file_number < last_number )
      );
      break;

    case tkn_suomi:
      html_set_lang( "fi" ); break;

    /* Print a text paragraph, starting or not starting a new form or file. */
    case tkn_Text:
      if( fb_height ){ close_form( true, false ); open_form(); }
      else{ close_previous_file = true; }
    case tkn_text: case tkn_none:
      open_file();
      if( !err_ok ){ continue; }
      out_html( '\n' );
      if( inp_prgh ){ out_html( "<p>\t" ); inp_prgh = false; }
      inp_copy_to_html();
      out_html( '\n' );
      continue;

    /* Store the title. */
    case tkn_title:
      close_file( false );
      title.clear();
      inp_skip_white_space();
      if( !inp_chr || inp_chr == '\n' ){
        error( "The title string is missing" ); continue;
      }
      inp_copy_to_str( &title );
      continue;

    default:
      error( "Unknown #-command, the known ones are", tkn_names, true );
      continue;

    }

    inp_get_tkn();
  }

  open_file();
  if( last_number && file_number != last_number ){
    error( "The last file number does not match" );
  }
  close_file( false );
#ifdef one_page
  if( html_started ){ html_end(); }
#endif

}
