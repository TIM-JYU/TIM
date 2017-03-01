#include <iostream>

const unsigned width = 8, height = 12;

void send( bool bit ){
  static unsigned nn = 0;
  static char ch = 1;
  ch <<= 1;
  if( bit ){ ch |= 1; }
  if( nn < 5 ){ ++nn; }
  else{
    if( ch == 127 ){ std::cout.put( char( 63 ) ); }
    else if( ch == '\\' ){ std::cout << "\\\\"; }
    else{ std::cout.put( ch ); }
    ch = 1; nn = 0;
  }
}

unsigned line_nr = 1;
char next_ch = 0;

void get_char(){
  next_ch = 0; std::cin.get( next_ch );
  if( next_ch == '\n' ){ ++line_nr; }
}

void to_line_end(){
  while( next_ch && next_ch != '\n' ){ get_char(); }
}

int main(){

  std::cout << "// ";
  for( char ch = 32; ch < 127; ++ch ){
    std::cout << ch;
    if( ch % 32 == 31 ){ std::cout << "\n// "; }
  }
  std::cout << "\nconst char draw_font[] =\n";

  unsigned nn = 0;
  get_char();
  while( next_ch == '!' ){ to_line_end(); get_char(); }
  while( next_ch ){
    if( nn && nn % 4 == 0 ){ std::cout << "\n"; }
    if( nn % 4 == 0 ){ std::cout << "  \""; }
    for( unsigned jj = 0; jj < height; ++jj ){
      for( unsigned ii = 0; ii < width; ++ii ){
        if( next_ch == '#' ){ send( 1 ); get_char(); }
        else if( next_ch == ' ' ){ send( 0 ); get_char(); }
        else{
          if( next_ch != '\n' ){
            std::cerr << "Wrong character on line " << line_nr << " pos " <<
              ii+1 << '\n';
            to_line_end();
          }
          for( ; ii < width; ++ii ){ send( 0 ); }
        }
      }
      if( next_ch && next_ch != '\n' ){
        std::cerr << "Too long line " << line_nr << '\n'; to_line_end();
      }
      get_char();
      while( next_ch == '!' ){ to_line_end(); get_char(); }
    }
    if( ++nn % 4 == 0 ){ std::cout << "\""; }
  }
  if( nn % 4 ){ std::cout << "\""; }
  std::cout << ";\nunsigned draw_font_size = " << nn << ";\n";
  std::cout << "const unsigned draw_font_width = " << width <<
    ", draw_font_height = " << height << ";\n";

}
