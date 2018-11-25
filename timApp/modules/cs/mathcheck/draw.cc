copyright draw_cc( "draw.cc", "Antti Valmari", 20181004 );
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
/* This file contains features for drawing in-html-file images.
Antti Valmari 2016-12-11 ...,
excluding the GIF package (see below) */


#include <iostream>
typedef unsigned char byte;


/* Helper functions needed when this file is used outside MathCheck */
#ifndef mathcheck

const char b64chars[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

#endif


/* Base64 encoding of byte strings */

unsigned b64linelen = 0;
char b64old = 0;

void b64send( byte chr ){
  if( b64linelen % 4 == 0 ){
    std::cout.put( b64chars[ chr >> 2 ] ); b64old = chr & 0x3; ++b64linelen;
  }else if( b64linelen % 4 == 1 ){
    std::cout.put( b64chars[ ( b64old << 4 ) | ( chr >> 4 ) ] );
    b64old = chr & 0xF; ++b64linelen;
  }else{
    std::cout.put( b64chars[ ( b64old << 2 ) | ( chr >> 6 ) ] );
    std::cout.put( b64chars[ chr & 0x3F ] );
    if( b64linelen < 74 ){ b64linelen += 2; }
    else{ /*???std::cout << '\n';*/ b64linelen = 0; }
  }
}

void b64stop(){
  if( b64linelen % 4 == 1 ){
    std::cout.put( b64chars[ b64old << 4 ] );
    std::cout.put( '=' ); std::cout.put( '=' );
  }else if( b64linelen % 4 == 2 ){
    std::cout.put( b64chars[ b64old << 2 ] );
    std::cout.put( '=' );
  }
  b64linelen = 0;
}

void b64send_str( const char *str ){
  for( ; *str; ++str ){ b64send( *str ); }
}


/*** Drawing buffer ***/
const char *draw_err = 0;
namespace draw{

  const unsigned x_max_size = 1024, y_max_size = 512;   // maximum size of ...
  int x_size = 0, y_size = 0;               // actual size of the drawing area
  byte buffer[ y_max_size * x_max_size ];

  /* Colour map information */
  const byte colour_max = 16;   // max nr of different colours in an image
  byte colour_cnt = 0;          // number of different colours in the image
  byte colour_map[ 3 * colour_max ] = {};   // colour map for GIF
  const unsigned              // names of common and two not so common colours
    white = 0xFFffFF, silver = 0xC0C0C0, gray = 0x808080, black = 0,
    red = 0xFF0000, maroon = 0x800000, yellow = 0xFFff00, olive = 0x808000,
    lime = 0x00FF00, green = 0x008000, aqua = 0x00FFFF, teal = 0x008080,
    blue = 0x0000FF, navy = 0x000080, fuchsia = 0xFF00FF, purple = 800080,
    orange = 0xFFA500, dgreen = 0x009F00, grey = 0xDFDFDF;

  /* Registering a colour. */
  byte draw_colour( unsigned clr ){
    if( colour_cnt >= colour_max ){ draw_err = "Too many colours"; return 0; }
    unsigned ii = 3 * colour_cnt;
    colour_map[ ii ] = ( clr >> 16 ) & 0xFF;
    colour_map[ ++ii ] = ( clr >> 8 ) & 0xFF;
    colour_map[ ++ii ] = clr & 0xFF;
    return colour_cnt++;
  }

  /* Resetting the colours and registering the background colour. */
  void draw_reset_colours( unsigned background = grey ){
    colour_cnt = 0; draw_colour( background );
  }

  /* Reset the drawing buffer. */
  void draw_reset_buffer( unsigned x_sz, unsigned y_sz ){
    if( x_sz > x_max_size ){ draw_err = "The image is too wide"; return; }
    if( y_sz > y_max_size ){ draw_err = "The image is too tall"; return; }
    x_size = x_sz; y_size = y_sz;
    for( int yy = 0; yy < y_size; ++yy ){
      int ii = yy * x_max_size, kk = ii + x_size;
      for( ; ii < kk; ++ii ){ buffer[ ii ] = 0; }
    }
  }

  /* Get the colour of the pixel at (xx, yy). */
  inline byte draw_get( unsigned xx, unsigned yy ){
    return buffer[ yy * x_max_size + xx ];
  }

  /* Draw the pixel at (xx, yy) with colour cc. */
  inline void draw_pixel( unsigned xx, unsigned yy, byte cc ){
    if( xx < unsigned( x_size ) && yy < unsigned( y_size ) ){
      buffer[ yy * x_max_size + xx ] = cc;
    }
  }

}
using draw::draw_colour;
using draw::draw_reset_colours;
using draw::draw_reset_buffer;
using draw::draw_get;
using draw::draw_pixel;


/*
  GIF-KUVIEN TUOTTO PIKSELIKARTASTA

  Tämä aliohjelmien kokoelma vastaanottaa pikselikarttamuotoisia kuvia
  ja tulostaa ne GIF-kuvajonoina. GIF-kuvajono koostuu alueesta, jolle voi
  piirtää useita kuvia siten, että kuvien piirron välillä voi olla ajallista
  viivettä. Yksittäisen kuvan ei ole pakko kattaa koko aluetta. Koordinaatit
  ovat ei-negatiivisia, ja (0,0) on vasen alanurkka. (Giffissä (0,0) on vasen
  ylänurkka. Kuva käännetään piirrettäessä.)

  Tämä paketti ei sisällä kaikkia GIFin mahdollisuuksia.

  Tätä pakettia käytetään kutsumalla aliohjelmia seuraavassa järjestyksessä:

      GIF_start  GIF_frame^*  GIF_stop

  Paketti tekee seuraavia kutsuja ulos:

    byte draw_get( unsigned, unsigned )  tuo pikselin kartan paikasta (x,y)

  Aliohjelmat rajaavat parametrinsa järkevälle alueelle. Jos se ei ole
  mahdollista, ne palaavat välittömästi.

  Tätä pakettia ei ole (vielä) katselmoitu eikä perusteellisesti testattu. ???

  Antti Valmari 22.5.2006--17.8.2006.

  Modified 2016-12-08 to yield Base64-encoded output. At the same time, some
  names were translated to English.

*/


// Ennakkoviittauksia; ks. vastaava nimiavaruus jäljempää
namespace LZW_pakkaaja{ unsigned koodin_leveys; }



/**************************************************************************
*
*  BITTIKOODIEN PAKKAUS TAVUIKSI
*
**************************************************************************/


namespace GIF_puskuri{


  // Bittien tavuksi pakkauksen puskuri
  byte tavut[ 255 ];    // pakatut, lähettämättömät tavut
  byte vajaa_tavu;      // keskeneräinen tavu
  unsigned rivi,        // puskurissa olevien valmiiden tavujen määrä
    sarake;             // keskeneräisen tavun bittien määrä


  // Puskurin sisällön lähetys, ei muuta sisältöä
  void laheta(){
    b64send( rivi );    // sends the size of the buffer
    for( unsigned i1 = 0; i1 < rivi; ++i1 ){ b64send( tavut[ i1 ] ); }
  }


  // Puskurin käytön aloitus: puskuri alustetaan tyhjäksi
  inline void aloita(){ rivi = 0; sarake = 0; vajaa_tavu = 0; }


  // Puskurin käytön lopetus: jos puskurissa on vielä jotain, se matkaan
  inline void lopeta(){
    if( sarake ){ tavut[ rivi ] = vajaa_tavu; ++rivi; }
    if( rivi ){ laheta(); }
  }


  // Bittikoodin lisäys puskuriin, tarvittaessa puskuri tyhjennetään
  void lisaa( unsigned arvo ){

    // Yhdistetään uusi arvo viimeisimpään vajaaseen tavuun.
    // Saadaan <= 19 bittiä leveä koodi.
    arvo <<= sarake; arvo |= vajaa_tavu & 0xFF;
    sarake += LZW_pakkaaja::koodin_leveys;

    // Talletetaan valmiit tavut, tarvittaessa puskuri lähettäen.
    while( sarake >= 8 ){
      tavut[ rivi ] = arvo & 0xFF; arvo >>= 8; sarake -= 8;
      ++rivi;
      if( rivi >= 255 ){ laheta(); rivi = 0; }
    }

    // Talletetaan uusi vajaa tavu
    vajaa_tavu = arvo & 0xFF;

  }


}



/**************************************************************************
*
*  PIKSELEIDEN LZW-PAKKAUS
*
**************************************************************************/


/*
  LZW-muunnos perustuu taulukkoon, jota indeksoidaan vähintään 2- ja
  korkeintaan 12-bittisellä indeksillä. Taulukon ensimmäiset "nollauskoodi"
  alkiota vastaavat yksittäisiä merkkejä, kaksi seuravaa ei ole käytössä,
  ja siitä eteenpäin kohtaan "vapaa_koodi - 1" asti kukin alkio vastaa
  merkkijonoa, jonka pituus on >= 2. Taulukon alkiot ovat 32-bittisiä.
  Kohdan "nollauskoodi" jälkeisen alkion alimmat 8 bittiä sisältävät
  alkiota vastaavan merkkijonon viimeisen merkin. Kunkin alkion ylimmät
  12 bittiä aloittavat linkitetyn listan, johon on koottu ne alkiot, joiden
  merkkijono on alkiota vastaava merkkijono jatkettuna yhdellä merkillä.
  Seuraavat 12 bittiä ovat muualta alkaneen listan linkki eteenpäin. Listan
  loppumerkkinä toimii "nollauskoodi".

  Muunnoksen tuloksessa "nollauskoodi" saa vastaanottajan nollaamaan
  muunnostaulukkonsa, ja "nollauskoodi + 1" toimii loppumerkkinä.
*/


namespace LZW_pakkaaja{


  // LZW-muunnoksen tilatiedot
  // unsigned koodin_leveys;   koodisanojen senhetkinen leveys bitteinä
  unsigned pikselin_koko;   // pikselin bittien määrä, kuitenkin vähintään 2
  unsigned nollauskoodi,    // 2^pikselin_koko, nollauttaa LZW-taulukon
    vapaa_koodi,            // ensimmäinen käyttämätön koodi
    nyt_koodi;              // käsiteltävänä olevan merkkijonon koodi
  unsigned taulu[ 4096 ];   // LZW-muunnostaulu


  // LZW-muunnostaulun tyhjennys
  void nollaa_taulu(){
    for( unsigned i1 = 0; i1 < nollauskoodi; ++i1 ){
      taulu[ i1 ] = nollauskoodi << 20;     // merkitsee listat tyhjiksi
    }
    koodin_leveys = pikselin_koko + 1;
    vapaa_koodi = nollauskoodi + 2;
  }


  // Paria (solmu_os:n merkkijono, mrk) vastaavan solmun etsintä. Tyhjä
  // merkkijono annetaan parametriksi antamalla solmu_os == nollauskoodi.
  inline unsigned etsi( unsigned solmu_os, byte mrk ){
    if( solmu_os == nollauskoodi ){ return mrk; }
    solmu_os = taulu[ solmu_os ] >> 20;
    while(
      solmu_os != nollauskoodi &&
      ( taulu[ solmu_os ] & 0xFF ) != ( mrk & 0xFFU )
    ){ solmu_os = taulu[ solmu_os ] >> 8 & 0xFFF; }
    return solmu_os;
  }


  // Parin (solmu_os:n merkkijono, mrk) lisäys
  inline void lisaa( unsigned solmu_os, byte mrk ){
    taulu[ vapaa_koodi ] =
        nollauskoodi << 20  // Tämän lasten lista on tyhjä.
      | ( taulu[ solmu_os ] & 0xFFF00000 ) >> 12 // isän lasten listan alkuos.
      | ( mrk & 0xFF );
    taulu[ solmu_os ] &= 0x000FFFFF;
    taulu[ solmu_os ] |= vapaa_koodi << 20;// lisäys isän lasten listan alkuun
    ++vapaa_koodi;
  }


  // LZW-tietojen nollaus merkin leveyden mukaisesti ja LZW:n alun tulostus
  void aloita( unsigned uusi_pikselin_koko ){

    // Aseta muunnosaikaiset vakiot.
    pikselin_koko = uusi_pikselin_koko;
    if( pikselin_koko < 2 ){ pikselin_koko = 2; }
    nollauskoodi = 1 << pikselin_koko;

    // Alusta LZW-muunnos.
    nollaa_taulu();
    nyt_koodi = nollauskoodi;   // ilmaisee, että vanha merkkijono on tyhjä

    // Tulosta LZW-osuuden alku.
    b64send( pikselin_koko );
    GIF_puskuri::aloita();
    GIF_puskuri::lisaa( nollauskoodi );

  }


  // LZW-osuuden lopetus: viimeinen koodi ulos jos tarpeen, loppumerkit
  void lopeta(){
    if( nyt_koodi != nollauskoodi ){ GIF_puskuri::lisaa( nyt_koodi ); }
    GIF_puskuri::lisaa( nollauskoodi + 1 );     // = LZW-lopetuskoodi
    GIF_puskuri::lopeta();
    b64send( '\0' );
  }


  // LZW-pakattavan merkin käsittely
  void ota_merkki( byte mrk ){

    // Varmistetaan käsiteltävän merkin laillisuus.
    mrk &= nollauskoodi - 1;

    unsigned solmu_os = etsi( nyt_koodi, mrk );

    // Koodi oli --- muistetaan se ja jäädään odottamaan seuraavaa merkkiä.
    if( solmu_os != nollauskoodi ){ nyt_koodi = solmu_os; }

    // Koodia ei ollut --- tulosta alkuosa ja luo uusi koodi.
    else{
      GIF_puskuri::lisaa( nyt_koodi );

      // Tarvittaessa kasvata koodin leveyttä.
      if( vapaa_koodi == 1U << koodin_leveys ){
        ++koodin_leveys;

        // Jos kooditaulu on täysi, nollaa se.
        if( koodin_leveys > 12 ){
          --koodin_leveys;
          GIF_puskuri::lisaa( nollauskoodi );
          nollaa_taulu();
          nyt_koodi = mrk & 0xFF;
          return;
        }

      }

      // Lisää uusi koodi kooditauluun.
      lisaa( nyt_koodi, mrk );
      nyt_koodi = mrk & 0xFF;

    }

  }


}



/**************************************************************************
*
*  GIF-KUVAN TUOTTO
*
**************************************************************************/


namespace GIF_muunnin{


  // Kuvavirran yleiset tilatiedot
  unsigned bitteja_pikselissa,  // bittejä pikselissä
    vareja;                     // värien määrä == 2^( bittejä pikselissä )
  unsigned leveys, korkeus;     // koko kuva-alueen leveys ja korkeus


  // GIF-kuvan alun ja yleistietojen tulostus
  void GIF_start(
    unsigned lev, unsigned kork,    // pikselikartan (koko alueen) koko
    unsigned bitteja_piks,          // eri värejä on 2^bitteja_pikselissa
    const byte *varikartta = 0,     // 3 tavua / väri
    unsigned taustavari = 0,        // kuvien ulkopuolisten pisteiden väri
    bool kierrata = false,
    unsigned aspektisuhde = 0       // pikselin ( leveys/korkeus + 15 ) / 64
                                    // "0" = ei aspektisuhdetta.
  ){

    // Vastaanota parametrit ja rajoita järkeviksi.
    leveys = lev & 0xFFFF; korkeus = kork & 0xFFFF;
    if( bitteja_piks < 1 ){ bitteja_pikselissa = 1; }
    else if( bitteja_piks <= 8 ){ bitteja_pikselissa = bitteja_piks; }
    else{ bitteja_pikselissa = 8; }
    vareja = 1 << bitteja_pikselissa;
    if( taustavari >= vareja ){ taustavari = 0; }
    aspektisuhde &= 0xFF;

    // Tulosta "header".
    b64send_str( "GIF89a" );

    // Tulosta "logical screen descriptor" alku.
    b64send( leveys & 0xFF );
    b64send( leveys >> 8 );
    b64send( korkeus & 0xFF );
    b64send( korkeus >> 8 );

    // Tulosta "neljän kentän tavu".
    if( !varikartta ){
      b64send( 0x70 | ( bitteja_pikselissa - 1 ) );
      taustavari = 0;
    }else{
      b64send( 0xF0 | ( bitteja_pikselissa - 1 ) );
    }

    // Tulosta loput "logical screen descriptor" tiedot.
    b64send( taustavari );
    b64send( aspektisuhde );

    // Tulosta värikartta, jos sellainen on.
    if( varikartta ){
      unsigned raja = 3 * vareja;
      for( unsigned i1 = 0; i1 < raja; ++i1 ){ b64send( varikartta[ i1 ] ); }
    }

    // Tulosta kuvan moneen kertaan näytön käsky, jos sellainen on.
    if( kierrata ){
      b64send_str( "\x21\xFF\x0B""NETSCAPE2.0\x03\x01\x0A" );
      b64send( '\0' ); b64send( '\0' );
    }

  }


  // GIF-kuvan loppumerkin "trailer" tulostus.
  void GIF_stop(){
    b64send( '\x3B' ); b64stop();
  }


  // Yhden GIF-kuvan tulostus
  void GIF_frame(
    unsigned lapinakyva_vari = 256,   // 256 = läpinäkyvää väriä ei ole
    unsigned viive = 0,               // 0 = ikuinen, muut = näyttöaika s/100
    unsigned nyt_leveys = leveys,     // kuvan leveys
    unsigned nyt_korkeus = korkeus,   // kuvan korkeus
    unsigned x_offset = 0, // kuvan ja alueen vasempien reunojen et. piksel.
    unsigned y_offset = 0  // kuvan ja alueen yläreunojen etäisyys pikseleinä
  ){

    // Vastaanota parametrit ja rajoita järkeviksi.
    if( lapinakyva_vari >= vareja ){ lapinakyva_vari = 256; }
    viive &= 0xFFFF;
    if( nyt_leveys > leveys ){ return; }
    if( nyt_korkeus > korkeus ){ return; }
    if( x_offset + nyt_leveys > leveys ){ x_offset = leveys - nyt_leveys; }
    if( y_offset + nyt_korkeus > korkeus ){
      y_offset = korkeus - nyt_korkeus;
    }

    // Tulosta tarvittaessa "graphic control extension".
    if( lapinakyva_vari != 256 || viive ){
      b64send_str( "\x21\xF9\x04" );
      byte pakatut_bitit = 0;
      if( lapinakyva_vari == 256 ){ lapinakyva_vari = 0; }
      else{ pakatut_bitit |= 1; }
      b64send( pakatut_bitit );
      b64send( viive & 0xFF );
      b64send( viive >> 8 );
      b64send( lapinakyva_vari );
      b64send( '\0' );
    }

    // Tulosta lohkon alkumerkki.
    b64send( '\x2C' );

    // Tulosta koordinaattitiedot.
    b64send( x_offset & 0xFF );
    b64send( x_offset >> 8 );
    b64send( y_offset & 0xFF );
    b64send( y_offset >> 8 );
    b64send( nyt_leveys & 0xFF );
    b64send( nyt_leveys >> 8 );
    b64send( nyt_korkeus & 0xFF );
    b64send( nyt_korkeus >> 8 );

    // Tulosta "pakattu tietokenttä".
    b64send( '\0' );

    // Tulosta pikselikartta LZW-pakattuna.
    LZW_pakkaaja::aloita( bitteja_pikselissa );
    for( unsigned yy = nyt_korkeus; yy--; ){
      for( unsigned xx = 0; xx < nyt_leveys; ++xx ){
        LZW_pakkaaja::ota_merkki( draw_get( xx, yy ) );
      }
    }
    LZW_pakkaaja::lopeta();

  }


}
using GIF_muunnin::GIF_start;
using GIF_muunnin::GIF_stop;
using GIF_muunnin::GIF_frame;


/* Rounding to two significant digits down and up */
void rough_down_up( double &xx, double &yy ){

  /* Eliminate not-a-numbers. */
  if( !( xx == xx ) ){ if( !( yy == yy ) ){ return; }else{ xx = yy; } }
  if( !( yy == yy ) ){ yy = xx; }

  /* Let aa := max{ |xx|, |yy| }. */
  double aa = xx < 0. ? -xx : xx;
  if( yy > aa ){ aa = yy; }else if( -yy > aa ){ aa = -yy; }

  /* Process infinite scale. */
  if( aa == 1/0. ){ xx = -1/0.; yy = 1/0.; return; }

  /* Process the case where both xx and yy are very near zero. */
  double p10 = 1., q10 = 10.;
  while( p10 > aa ){ q10 = p10; p10 /= 10.; }
  if( p10 == 0. ){
    xx = xx < 0. ? -q10 : 0.; yy = yy > 0. ? q10 : 0.; return;
  }

  /* Let p10 := biggest power of 10 that is at most aa. Let q10 := p10/10. */
  while( 10. * p10 <= aa ){ p10 *= 10.; }
  q10 = p10 / 10.;

  /* Round xx down. */
  aa = 0.;
  if( 0. <= xx ){
    while( aa + p10 <= xx ){ aa += p10; }
    if( q10 > 0. ){ while( aa + q10 <= xx ){ aa += q10; } }
  }else{
    while( aa - p10 >= xx ){ aa -= p10; }
    if( q10 > 0. ){ while( aa > xx ){ aa -= q10; } }
    else if( aa > xx ){ aa -= p10; }
  }
  xx = aa;

  /* Round yy up. */
  aa = 0.;
  if( 0. <= yy ){
    while( aa + p10 <= yy ){ aa += p10; }
    if( q10 > 0. ){ while( aa < yy ){ aa += q10; } }
    else if( aa < yy ){ aa += p10; }
  }else{
    while( aa - p10 >= yy ){ aa -= p10; }
    if( q10 > 0. ){ while( aa - q10 >= yy ){ aa -= q10; } }
  }
  yy = aa;

}


/*
double rough_down( double xx, bool do_up = false ){
  if( xx == 0. || xx == 1/0. || !( xx == xx ) ){ return xx; }
  if( xx < 0. ){ return -rough_down( -xx, true ); }
  double p10 = 1., q10 = 10.;
  while( p10 > xx ){ q10 = p10; p10 /= 10.; }
  if( p10 == 0. ){ return do_up ? q10 : 0.; }
  while( 10. * p10 <= xx ){ p10 *= 10.; }
  double yy = p10;
  while( yy + p10 <= xx ){ yy += p10; }
  q10 = p10; p10 /= 10.;
  if( p10 == 0. ){ return do_up && yy < xx ? yy + q10 : yy; }
  while( yy + p10 <= xx ){ yy += p10; }
  return do_up && yy < xx ? yy + p10 : yy;
}

inline double rough_up( double xx ){ return rough_down( xx, true ); }
*/


/*** Higher-level drawing facilities ***/
copyright draw_font_def_cc( "draw_font_def.cc" );
#include "draw_font_def.cc"
namespace draw{


  /* A buffer for delivering string parameters. */
  const unsigned name_max = 12;
  char name_buff[ name_max + 1 ] = {};


  /* Produce the img element containing the image as gif. */
  void draw_make_img( const char *alt_name ){

    /* Produce the beginning of the image element, either for other
      applications or for MathCheck. */
    #ifdef mathcheck
      out_html( "<img alt=\"" ); out_html( alt_name );
      out_html( "\" height=" ); out_html( y_size );
      out_html( " width=" ); out_html( x_size ); out_html( " src=\n" );
    #else
      std::cout << "<img alt=\"" << alt_name << "\" height=" << y_size <<
        " width=" << x_size << " src=\n";
    #endif

    /* Print the gif image data string and terminate the img element. This is
      printed directly to std::cout because lines must not be divided. */
    std::cout << "\"data:image/gif;base64,";
    unsigned colour_bits = 0; --colour_cnt;
    while( colour_cnt ){ ++colour_bits; colour_cnt >>= 1; }
    GIF_start( x_size, y_size, colour_bits, colour_map );
    GIF_frame();
    GIF_stop();
    std::cout << "\">\n";
  }


  /* Draw a line to the right from (x1, yy) to (x2, yy) of colour cc,
    overflow-protected. */
  inline void draw_horizontal( int x1, int x2, int yy, byte cc = 1 ){
    if( yy < 0 || yy >= y_size ){ return; }
    if( x1 < 0 ){ x1 = 0; }
    if( x2 >= x_size ){ x2 = x_size - 1; }
    for( ; x1 <= x2; ++x1 ){ draw_pixel( x1, yy, cc ); }
  }


  /* Draw a line upwards from (xx, y1) to (xx, y2) of colour cc,
    overflow-protected. */
  inline void draw_vertical( int xx, int y1, int y2, byte cc = 1 ){
    if( xx < 0 || xx >= x_size ){ return; }
    if( y1 < 0 ){ y1 = 0; }
    if( y2 >= y_size ){ y2 = y_size - 1; }
    for( ; y1 <= y2; ++y1 ){ draw_pixel( xx, y1, cc ); }
  }


  /* Draw a straight line between (x1, y1) and (x2, y2) of colour cc,
    overflow-protected. */
  void draw_line( int x1, int y1, int x2, int y2, byte cc = 1 ){

    /* Special case: vertical line */
    if( x1 == x2 ){
      if( y1 > y2 ){ int tmp = y1; y1 = y2; y2 = tmp; }
      draw_vertical( x1, y1, y2, cc ); return;
    }

    /* Always draw from left to right. */
    if( x2 < x1 ){ int
      tmp = x1; x1 = x2; x2 = tmp;
      tmp = y1; y1 = y2; y2 = tmp;
    }

    /* Set the variables so that diff always guides to the adjacent pixel that
      best matches the line. */
    int xd = y2 - y1, yd = x2 - x1, diff = 0;

    /* Staircase-draw guided by diff. */
    if( x2 >= x_size ){ x2 = x_size - 1; }
    if( xd > 0 ){
      while( true ){
        if( x1 >= 0 ){ draw_pixel( x1, y1, cc ); }
        if( diff > 0 ){ ++x1; diff -= xd; if( x1 > x2 ){ break; } }
        else{ ++y1; diff += yd; if( y1 > y2 ){ break; } }
      }
    }else{
      while( true ){
        if( x1 >= 0 ){ draw_pixel( x1, y1, cc ); }
        if( diff >= 0 ){ ++x1; diff += xd; if( x1 > x2 ){ break; } }
        else{ --y1; diff += yd; if( y1 < y2 ){ break; } }
      }
    }

  }


  /* Draw an up arrowhead. */
  void draw_upa( int xx, int yy, byte cc = 1 ){
    --yy;
    for( unsigned ii = 0; ii < 2; ++ ii ){
      --yy; draw_pixel( xx - 1, yy, cc ); draw_pixel( xx + 1, yy, cc );
    }
    for( unsigned ii = 0; ii < 2; ++ ii ){
      --yy; draw_pixel( xx - 2, yy, cc ); draw_pixel( xx - 1, yy, cc );
      draw_pixel( xx + 1, yy, cc ); draw_pixel( xx + 2, yy, cc );
    }
  }


  /* Draw a right arrowhead. */
  void draw_ria( int xx, int yy, byte cc = 1 ){
    --xx;
    for( unsigned ii = 0; ii < 2; ++ ii ){
      --xx; draw_pixel( xx, yy - 1, cc ); draw_pixel( xx, yy + 1, cc );
    }
    for( unsigned ii = 0; ii < 2; ++ ii ){
      --xx; draw_pixel( xx, yy - 2, cc ); draw_pixel( xx, yy - 1, cc );
      draw_pixel( xx, yy + 1, cc ); draw_pixel( xx, yy + 2, cc );
    }
  }


  /* Draw a filled rectangle. */
  void draw_frect( int x1, int y1, int x2, int y2, byte cc = 1 ){
    for( int yy = y1; yy <= y2; ++yy ){ draw_horizontal( x1, x2, yy, cc ); }
  }


  /* Draw a circle centered at (xx, yy) with radius rr. Fill the interior with
    colour fc, if given. If hor_len > 0, draw a box of length 2*hor_len+1 and
    height 2*rr+1 with rounded ends. */
  void draw_circle(
    int xx, int yy, int rr, byte cc = 1, byte fc = 255, int hor_len = 0
  ){
    draw_pixel( xx, yy + rr, cc );
    draw_pixel( xx, yy - rr, cc );
    if( fc != 255 ){ draw_vertical( xx, yy - rr + 1, yy + rr - 1, fc ); }
    int jj = rr, diff = 0;
    for( int ii = 0; ii <= rr && jj; ){
      if( jj < diff + ii ){ diff += 1 - 2*jj; --jj; }
      else{ diff += 2*ii + 1; ++ii; }
      draw_pixel( xx - ii - hor_len, yy - jj, cc );
      draw_pixel( xx - ii - hor_len, yy + jj, cc );
      draw_pixel( xx + ii + hor_len, yy - jj, cc );
      draw_pixel( xx + ii + hor_len, yy + jj, cc );
      if( fc != 255 ){
        draw_vertical( xx - ii - hor_len, yy - jj + 1, yy + jj - 1, fc );
        draw_vertical( xx + ii + hor_len, yy - jj + 1, yy + jj - 1, fc );
      }
    }
    for( int ii = 1; ii <= hor_len; ++ii ){
      draw_pixel( xx - ii, yy + rr, cc );
      draw_pixel( xx - ii, yy - rr, cc );
      draw_pixel( xx + ii, yy + rr, cc );
      draw_pixel( xx + ii, yy - rr, cc );
      if( fc != 255 ){
        draw_vertical( xx - ii, yy - rr + 1, yy + rr - 1, fc );
        draw_vertical( xx + ii, yy - rr + 1, yy + rr - 1, fc );
      }
    }
  }


  /* Draw a character with left bottom corner at (xx, yy). Use background
    colour fc, if given. */
  void draw_char( int xx, int yy, byte ch, byte cc = 1, byte fc = 255 ){

    /* Draw the background, if colour given. */
    if( fc != 255 ){
      draw_frect(
        xx, yy, xx + draw_font_width - 1, yy + draw_font_height - 1, fc
      );
    }

    /* Draw the glyph. */
    if( ch >= draw_font_size ){ return; }
    const char
      *cp = &draw_font[ ( draw_font_width * draw_font_height + 5 ) / 6 * ch ];
    char code = *cp, mask = 32;
    for( unsigned jj = draw_font_height; jj--; ){
      for( unsigned ii = 0; ii < draw_font_width; ++ii ){
        if( code & mask ){ draw_pixel( xx + ii, yy + jj, cc ); }
        if( mask == 1 ){ mask = 32; ++cp; code = *cp; }else{ mask >>= 1; }
      }
    }

  }


  /* Draw a character string with left bottom corner at (xx, yy). Use
    background colour fc, if given. */
  void draw_string(
    int xx, int yy, const char *str, byte cc = 1, byte fc = 255
  ){
    while( *str ){
      draw_char( xx, yy, *str, cc, fc ); xx += draw_font_width; ++str;
    }
  }


  /*** Curve drawing ***/

  /* Convert a data value to a pixel y-coordinate, with special values for
    not-a-number, too small, and too big. */
  double
    min_dy = 0., max_dy = 0.,   // minimum and maximum y in the drawing area
    y_scale = 0.;               // scaling factor towards pixel-y-coordinate
  int y_to_int( double yy ){
    if( !( yy == yy ) || yy == -1/0. || yy == 1/0. ){ return -2; }
    if( yy < min_dy ){ return -1; }
    if( yy > max_dy ){ return y_size; }
    return y_scale * ( yy - min_dy );
  }


  /* Draw a curve using already computed scaling, etc., parameters. */
  void raw_curve( unsigned nn, double dd[], byte cc ){

    int y0 = -2;  // x-pixel left side curve height, -2 = does not exist
    for( unsigned ii = 0; ii < nn; ii += 2 ){
      int
        y1 = y_to_int( dd[ ii ] ),  // x-pixel middle curve height
        y2 = ii + 1 < nn ? y_to_int( dd[ ii + 1 ] ) : -2;   // ... right ...

      /* If middle value does not exist or none of the three is in the range,
        draw nothing. */
      if(
        y1 == -2 || !(
          ( 0 <= y0 && y0 < y_size ) ||
          ( 0 <= y1 && y1 < y_size ) ||
          ( 0 <= y2 && y2 < y_size )
        )
      ){ y0 = y2; continue; }

      /* Find the lowest and highest to be drawn. */
      int lo = y1, hi = y1;   // draw at least the middle value
      if( y0 != -2 && y0 < lo ){ lo = y0; }
      if( y2 != -2 && y2 < lo ){ lo = y2; }
      if( y0 > hi ){ hi = y0; }
      if( y2 > hi ){ hi = y2; }

      draw_vertical( ii/2, lo, hi, cc ); y0 = y2;
    }

  }


  /* Draw the curves whose values are given in dd[ 0...cn*nn-1 ]. Two values
    are used per x-pixel. If x_zero is given, draw the y-coordinate axis at
    it. The height is that of the image in pixels. If auto_scale is off, uses
    the last parameters as the y-values of the lowest and highest pixel, with
    reasonable default values. Otherwise computes such y-values from the data,
    not going beyond the given limits. Resets the drawing buffer but not the
    colours. */
  const unsigned draw_max_curves = 6;
  void draw_img_curve(
    unsigned cn, unsigned nn, double dd[], unsigned x_zero = ~0u,
    int height = 101, bool auto_scale = true,
    double min_lim = -1/0., double max_lim = 1/0.
  ){

    /* Ensure that cn is legal. */
    if( !cn ){ draw_err = "There are no curves"; return; }
    if( cn > draw_max_curves ){ cn = draw_max_curves; }

    /* Initialize the drawing pad. */
    draw_reset_buffer( nn / 2, height );
    if( draw_err ){ return; }

    /* If required, set the y-range on the basis of the data. */
    if( auto_scale ){

      /* Find the minimum and maximum in the data. */
      min_dy = 1/0., max_dy = -1/0.;    // code is slightly unusual because
      for( unsigned ii = 0; ii < cn * nn; ++ii ){       // of not-a-numbers
        if( dd[ ii ] < min_dy ){ min_dy = dd[ ii ]; }
        if( dd[ ii ] > max_dy ){ max_dy = dd[ ii ]; }   // no "else"!
      }

      /* If there was no data, draw nothing. */
      if( min_dy == 1/0. ){ draw_err = "There is no data to draw"; return; }

      /* Adjust the minimum and maximum for more informative pictures. */
      if( max_dy == min_dy ){
        if( max_dy > 0. ){ min_dy = 0.; max_dy = 2. * max_dy; }
        else if( max_dy < 0. ){ min_dy = 2. * min_dy; max_dy = 0.; }
      }
      else if( 0. < min_dy && max_dy >= 2. * min_dy ){ min_dy = 0.; }
      else if( max_dy < 0. && min_dy <= 2. * max_dy ){ max_dy = 0.; }
      if( min_dy < min_lim ){ min_dy = min_lim; }
      if( max_dy > max_lim ){ max_dy = max_lim; }
      double margin = ( max_dy - min_dy ) * 7. / ( y_size - 1 );  // 7 pixels
      if( margin > 0. ){ min_dy -= margin; max_dy += margin; }
      else{ min_dy = -1.; max_dy = 1.; }
      rough_down_up( min_dy, max_dy );

    /* Else set the given or default y-range. */
    }else{
      min_dy = min_lim == -1/0. ? -10. : min_lim;
      max_dy = max_lim == 1/0. ? 10. : max_lim;
    }

    /* Compute the scaling factor. */
    y_scale = ( y_size - 1 ) / ( max_dy - min_dy );

    /* Draw the coordinate axes. */
    unsigned uu = -y_scale * min_dy;
    draw_horizontal( 0, x_size - 1, uu, 1 ); draw_ria( x_size - 1, uu, 1 );
    uu = x_zero / 2;
    draw_vertical( uu, 0, y_size - 1, 1 ); draw_upa( uu, y_size - 1, 1 );

    /* Draw the curves. */
    for( unsigned ii = 0; ii < cn; ++ii ){
      raw_curve( nn, &dd[ ii * nn ], ii + 2 );
    }

    draw_make_img( "a curve" );
  }


  /*** Tree drawing ***/

  /* Tree nodes and arcs */
  struct draw_tree_node{

    struct tree_arc{
      draw_tree_node *head; int offset; tree_arc *next;
      tree_arc( draw_tree_node *head, tree_arc *next ):
        head( head ), offset(0), next( next ) {}
    };

    /* Data members */
    static const unsigned
      rr = draw_font_height / 2 + 3,  // radius of node circular parts
      x_sep = 10,     // minimum empty horizontal distance between nodes
      y_sep = 40;     // vertical distance between centres of nodes
    char name[ name_max ]; unsigned n_len;  // node contents text
    byte f_clr, b_clr;  // contents and background colour
    bool low_draw;    // true ==> relational operators' drawing mode
    tree_arc *children;   // list of arcs to subtrees
    draw_tree_node *l_next, *r_next;  // left and right contour
    int l_plus, r_plus;   // offsets along the contours

    /* Constructor */
    draw_tree_node(
      const char *nm, byte f_clr = 1, byte b_clr = 0, bool low_draw = false
    ):
      n_len(0), f_clr( f_clr ), b_clr( b_clr ), low_draw( low_draw ),
      children(0), l_next(0), r_next(0), l_plus(0), r_plus(0)
    {
      for( ; nm[ n_len ] && n_len < name_max; ++n_len ){
        name[ n_len ] = nm[ n_len ];
      }
    }

    /* Destructor */
    ~draw_tree_node(){
      for( tree_arc *ac1 = children; ac1; ){
        tree_arc *ac2 = ac1; ac1 = ac1->next; delete ac2->head; delete ac2;
      }
    }

    /* Half of width of straight part of node */
    inline int hw(){
      return n_len <= 0 ? 0 :
        ( ( n_len - 1 ) * ( draw_font_width + 3 * low_draw ) ) / 2;
    }

    /* Compute depth. */
    int depth(){
      if( !children ){ return 1; }
      int d1 = 0;
      for( tree_arc *ac = children; ac; ac = ac->next ){
        int d2 = ac->head->depth();
        if( d2 > d1 ){ d1 = d2; }
      }
      return d1 + 1;
    }


    /* Design the layout. */
    void layout(){
      if( !children ){ return; }

      /* Layout the leftmost subtree and make the contours of the current tree
        go via it. */
      tree_arc *lac = children;
      l_next = r_next = lac->head; l_next->layout();
      lac->offset = l_plus = r_plus = 0;

      /* Layout the remaining subtrees and join them to the current tree. */
      tree_arc *rac = lac->next;
      for( ; rac; rac = rac->next ){

        /* Traverse down left and right contours of subtrees lac and rac. */
        draw_tree_node
          *llp = l_next, *lrp = r_next, *rlp = rac->head, *rrp = rlp;
        int llx = 0, lrx = r_plus, rlx = 0, rrx = 0;
        r_next = rlp; r_next->layout();   // layout the subtree
        while( true ){

          /* Update the necessary separation of the subtrees. */
          int sep = lrx + lrp->hw() + rlp->hw() - rlx;
          if( sep > r_plus ){ r_plus = sep; }

          /* If either subtree ends, exit the loop. */
          if( !( llp->l_next && rrp->r_next ) ){ break; }

          /* Go down one step. */
          llx += llp->l_plus; llp = llp->l_next;
          lrx += lrp->r_plus; lrp = lrp->r_next;
          rlx += rlp->l_plus; rlp = rlp->l_next;
          rrx += rrp->r_plus; rrp = rrp->r_next;

        }

        /* Continue a contour of the shallower tree to the deeper tree. */
        r_plus += x_sep + 2 * rr + 1;
        if( llp->l_next ){
          rrp->r_next = lrp->r_next;
          rrp->r_plus = lrx - rrx - r_plus + lrp->r_plus;
        }
        else if( rrp->r_next ){
          llp->l_next = rlp->l_next;
          llp->l_plus = rlx - llx + r_plus + rlp->l_plus;
        }
        rac->offset = r_plus;

      }

      /* Center the root above the subtrees. */
      l_plus = -r_plus/2; r_plus += l_plus;
      for( rac = children; rac; rac = rac->next ){ rac->offset += l_plus; }

      return;
    }

    /* Space-wasting layout
    int layout_wide(){
      if( !children ){ return hw() + rr; }
      int sum = -x_sep;
      for( tree_arc *ac = children; ac; ac = ac->next ){
        ac->offset = ac->head->layout_wide(); sum += 2*ac->offset+1 + x_sep;
      }
      int mid = sum / 2; sum = 0;
      for( tree_arc *ac = children; ac; ac = ac->next ){
        int ao = ac->offset; ac->offset += sum - mid; sum += 2*ao + x_sep + 1;
      }
      return mid;
    }*/

    /* Make a tree node a child of the current node. */
    void add_child( draw_tree_node *child ){
      children = new tree_arc( child, children );
    }

    /* Reverse the list of the children of the current node. */
    void reverse_children(){
      tree_arc *cd1 = 0, *cd2 = 0;
      while( children ){
        cd1 = children; children = children->next; cd1->next = cd2; cd2 = cd1;
      }
      children = cd2;
    }

    /* Draw a subtree. */
    void draw_subtree( int xx, int yy ){
      if( low_draw && n_len >= 2 ){
        int x2 = xx - ( n_len - 2 ) * ( draw_font_width + 3 ) / 2;
        for( tree_arc *ac = children; ac; ac = ac->next ){
          if( ac == children || !ac->next ){
            if( !ac->next ){ x2 -= draw_font_width + 3; }
            draw_line( x2, yy, xx + ac->offset, yy - y_sep, f_clr );
          }else{
            draw_line( x2, yy - rr, xx + ac->offset, yy - y_sep, f_clr );
            x2 += draw_font_width + 3;
          }
          ac->head->draw_subtree( xx + ac->offset, yy - y_sep );
        }
      }else{
        for( tree_arc *ac = children; ac; ac = ac->next ){
          draw_line( xx, yy, xx + ac->offset, yy - y_sep, f_clr );
          ac->head->draw_subtree( xx + ac->offset, yy - y_sep );
        }
      }
      draw_circle( xx, yy, rr, f_clr, b_clr, hw() );
      unsigned xc = low_draw
        ? xx - n_len * ( draw_font_width + 3 ) / 2 + 2
        : xx - hw() - ( draw_font_width - 1 ) / 2;
      if( n_len ){ draw_char( xc, yy + 3 - rr, name[0], f_clr ); }
      for( unsigned ii = 1; ii < n_len; ++ii ){
        xc += draw_font_width;
        if( low_draw ){
          draw_vertical( xc + 1, yy - rr + 1, yy + rr - 1, f_clr ); xc += 3;
        }
        draw_char( xc, yy + 3 - rr, name[ ii ], f_clr );
      }
    }

    /* Draw the tree. */
    void draw( const char *alt_name ){

      #if 1 // Reingold--Tilford layout
      layout();

      /* Find the left- and rightmost used x-coordinates. */
      int x_now = 0, x_min = 0, x_max = 0;
      for( draw_tree_node *nd = this; nd; nd = nd->l_next ){
        int xx = x_now - nd->hw() - rr;
        if( xx < x_min ){ x_min = xx; }
        x_now += nd->l_plus;
      }
      x_now = 0;
      for( draw_tree_node *nd = this; nd; nd = nd->r_next ){
        int xx = x_now + nd->hw() + rr;
        if( xx > x_max ){ x_max = xx; }
        x_now += nd->r_plus;
      }

      #else // simple but wide layout
      int x_min = -layout_wide(), x_max = -x_min - 1;
      #endif

      /* Draw and delete the tree */
      int dh = ( depth() - 1 ) * y_sep;
      draw_reset_buffer( x_max - x_min + 1, dh + 2*rr + 1 );
      if( !draw_err ){
        draw_subtree( -x_min, dh + rr );
        draw_make_img( alt_name );
      }
      delete this;

    }

  };


}
using draw::draw_make_img;
using draw::draw_horizontal;
using draw::draw_vertical;
using draw::draw_line;
using draw::draw_upa;
using draw::draw_ria;
using draw::draw_frect;
using draw::draw_circle;
using draw::draw_char;
using draw::draw_string;
using draw::draw_max_curves;
using draw::draw_img_curve;
using draw::draw_tree_node;
