copyright CFG_file( "CFG.cc", "Antti Valmari", 20190207 );
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


/* String 3-way comparison in shortlex order */
int shortlex( const char *str1, const char *str2 ){
  while( *str1 && *str1 == *str2 ){ ++str1; ++str2; }
  int lex = *str1 - *str2;
  while( *str1 && *str2 ){ ++str1; ++str2; }
  if( *str1 ){ return 1; }
  if( *str2 ){ return -1; }
  return lex;
}


/* Make a copy of a C string, reserving memory. */
//??? This computes size unnecessarily for the only use
inline char *C_string_copy( const char *original ){
  unsigned sz = 1;
  for( const char *cp = original; *cp; ++cp ){ ++sz; }
  char *result = new char[ sz ], *cp = result;
  for( ; *original; ++original ){ *cp = *original; ++cp; }
  *cp = '\0';
  return result;
}


/* This class contains the features for processing a context-free grammar. Its
  object represents a nonterminal. The nonterminals are named with 'A',  ...,
  'Z', and the remaining chars are terminals. There are two sets of the
  nonterminals, so that the languages of two CFGs can be compared. */
class CFG{


/*** Main data ***/

  /* Objects for recording invocations of a nonterminal */
  struct nt_user{
    CFG *ntp;       // nonterminal whose rule invokes the current nonterminal
    unsigned rr, ll;  // rule number and location in the rule
    nt_user( CFG *ntp, unsigned rr, unsigned ll ):
      ntp( ntp ), rr( rr ), ll( ll ) {}
  };

  /* Contents of a nonterminal object = CFG object */
  std::vector< const char *> rules;   // array of xxx s.t. (this, xxx) in R
  std::vector< const char *> words;   // found words in shortlex order
  std::vector< nt_user > users;       // instances of this nt in rules
  unsigned min_length_;   // ~0u = not yet known, ~1u = undefined, ~2u = biiig
  unsigned offset;        // 'A' for the first 26 nts, 'A'-26 for the rest
  bool gw_started;        // has the generation of words started?

  /* All nonterminal objects */
  static CFG nonterminals[ 52 ];      // indexed by name-'A' or name-'A'+26

public:

  static char start0, start1;         // start symbols of both grammars
  static const char *duplicate;       // a word with two derivations in start1


/*** Some small functions ***/

private:

  void rules_clear(){
    for( unsigned rr = 0; rr < rules.size(); ++rr ){ delete rules[ rr ]; }
    rules.clear();
  }

public:

  CFG():
    min_length_( ~0u ), offset( this - nonterminals >= 26 ? 'A' - 26 : 'A' ),
    gw_started( false )
  {};

  /* Capital letters are nonterminals. Other characters are terminals. */
  static bool is_terminal( char ch ){ return ch < 'A' || ch > 'Z'; }

  /* Returns a pointer to the nonterminal named ch either from the first or
    the second set of 26 nonterminals. If ch is illegal, uses 'A' instead. */
  static CFG *nt_named( char ch, bool nt_set = false ){
    if( is_terminal( ch ) ){ ch = 'A'; }
    return &nonterminals[ ch - 'A' + ( nt_set ? 26 : 0 ) ];
  }

  /* A buffer for various purposes */
  static unsigned incomplete;   // 1 = buffer overflow, 2 = heap overflow
  static const unsigned buff_max = 50;
  static unsigned buff_sz;
  static char buff[ buff_max + 1 ];
  inline static bool buff_add( char ch ){
    if( buff_sz < buff_max ){ buff[ buff_sz ] = ch; ++buff_sz; return true; }
    else{ incomplete = 1; return false; }
  }

  /* Addition of a rule for the nonterminal */
  inline void add_rule( const char *rs ){
    rules.push_back( C_string_copy( rs ) );
  }
  inline void add_rule(){ buff[ buff_sz ] = '\0'; add_rule( buff ); }

  /* Tells if the nonterminal has no rules. */
  bool empty(){ return rules.empty(); }

  /* Minimum length of the words in the language of the nonterminal */
  unsigned min_length(){
    if( min_length_ == ~0u ){   // memoization
      min_length_ = ~1u;        // avoid infinite recursion

      /* Compute the minimum length for each rule. */
      for( unsigned rr = 0; rr < rules.size(); ++rr ){
        const char *rule = rules[ rr ];
        unsigned length = 0;
        for( unsigned jj = 0; rule[ jj ] && length < min_length_; ++jj ){
          if( is_terminal( rule[ jj ] ) ){ ++length; }
          else{
            unsigned kk = nonterminals[ rule[ jj ] - offset ].min_length();
            if( kk == ~1u ){ length = ~1u; break; }
            length += kk;
            if( length < kk || length > ~2u ){ length = ~2u; break; }
          }
        }

        /* Keep the minimum length over all rules. */
        if( length < min_length_ ){
          min_length_ = length;
          if( !min_length_ ){ break; }
        }

      }

      //if( min_length_ == ~1u ){ rules_clear(); }  // probably unsafe?
    }
    return min_length_;
  }

  /* Print the grammar. *//*
  static void print(){
    for( unsigned ii = 0; ii < 52; ++ii ){
      if( ii == 26 ){ std::cout << "----\n"; }
      CFG &nt = nonterminals[ ii ];
      if( nt.empty() ){ continue; }
      std::cout << char( 'A' + ii % 26 ) << " ::= ";
      for( unsigned rr = 0; rr < nt.rules.size(); ++rr ){
        if( rr ){ std::cout << " | "; }
        std::cout << '\"' << nt.rules[ rr ] << '\"';
      }
      std::cout << '\n';
    }
  }*/

  /* The name and bank of the nonterminal */
  char name(){ return ( this - nonterminals ) % 26 + 'A'; }
  bool nt_set(){ return ( this - nonterminals ) / 26; }


/*** Features for testing that a string belongs to the language ***/

private:

  static const char *tested_word;   // the string under membership test
  static unsigned rule_search;  // number of current search, starts from 2,
                                // not protected against overflow because
                                // number of searches believed small

  /* Hash table for membership test */
  struct hash_elem{
    unsigned ii, jj;    // rule points to a suffix of some rule, and
    const char *rule;   //   rule_word_ tells whether tested_word[ ii...jj-1 ]
    unsigned            //   matches the suffix: 0 = no, 1 = yes, others =
      rule_word_,       //   number of last search where found.
      cut_point,  // If rule starts with a nonterminal, match tells its rule
      match;      //   that matched and cut_point tells where tested_word cut.
    hash_elem *next;
  };
  static const unsigned hash_size = 100;  // 100???
  static hash_elem *hash_table[ hash_size ];

  /* Reset the hash table. */
  static void hash_clear(){
    for( unsigned ii = 0; ii < hash_size; ++ii ){
      while( hash_table[ ii ] ){
        hash_elem *he = hash_table[ ii ]; hash_table[ ii ] = he->next;
        delete he;
      }
    }
    rule_search = 1;
  }

  static hash_elem *hash_find( const char *rp, unsigned ii, unsigned jj ){
    unsigned hi = ( 31 * ii + jj ) % hash_size;
    hash_elem *he = hash_table[ hi ];
    while( he && ( he->ii != ii || he->jj != jj || he->rule != rp ) ){
      he = he->next;
    }
    return he;
  }

  /* Tests whether tested_word[ ii...jj-1 ] matches the rule suffix *rp. The
    result is memoized. Infinite recursion is avoided by using search numbers
    and not entering with the same rp, ii, and jj again during the same
    recursive search. A recursive call belongs to the same search as its
    parent if and only if ii and jj remain the same. The search number is
    incremented for each top level recursive call, and failure by running out
    of rules is treated as decisive only for top level calls.
  */
  bool rule_word(
    const char *rp, unsigned ii, unsigned jj, bool top_level
  ) const {

    /* Find memoized value from hash table, if exists. */
    unsigned hi = ( 31 * ii + jj ) % hash_size;
    hash_elem *he = hash_table[ hi ];
    while( he && ( he->ii != ii || he->jj != jj || he->rule != rp ) ){
      he = he->next;
    }

    /* Use old value or create a new hash table element. */
    if( he ){
      if( he->rule_word_ < 2 ){ return he->rule_word_; }    // final answer
      if( top_level ){ ++rule_search; }
      else if( he->rule_word_ >= rule_search ){ return false; }   // uncertain
    }else{
      if( top_level ){ ++rule_search; }
      he = new hash_elem;
      he->ii = ii; he->jj = jj; he->rule = rp;
      he->next = hash_table[ hi ]; hash_table[ hi ] = he;
    }
    he->rule_word_ = rule_search;

    /* Fill in the new hash table element and return the reply. */
    while( true ){

      /* The rule suffix has ended. */
      if( !*rp ){ he->rule_word_ = ii >= jj; return he->rule_word_; }

      /* The rule symbol is a terminal symbol. */
      if( is_terminal( *rp ) ){
        if( ii >= jj || *rp != tested_word[ ii ] ){
          he->rule_word_ = 0; return false;
        }
        ++rp; ++ii; continue;
      }

      /* The rule symbol is a nonterminal symbol. */
      CFG &nt = nonterminals[ *rp - offset ];
      for( unsigned kk = ii; kk <= jj; ++kk ){
        unsigned match = nt.has_word( ii, kk, kk < jj );
        if( match != ~0u && rule_word( rp+1, kk, jj, ii < kk ) ){
          if( he->rule_word_ != 1 ){
            he->rule_word_ = 1; he->cut_point = kk; he->match = match;
          }
          return true;
        }
      }
      if( top_level ){ he->rule_word_ = 0; }
      return false;

    }

  }

  /* If tested_word[ ii...jj-1 ] is in the language of this nonterminal,
    has_word returns the number of a rule that applied. Otherwise, it returns
    ~0u. top_level is just passed through. */
  unsigned has_word( unsigned ii, unsigned jj, bool top_level ) const {
    for( unsigned rr = 0; rr < rules.size(); ++rr ){
      if( rule_word( rules[ rr ], ii, jj, top_level ) ){ return rr; }
    }
    return ~0u;
  }

public:

  /* Tests whether wd is in the language of this nonterminal. The return value
    is like above. */
  unsigned has_word( const char *wd ) const {
    hash_clear();   //??? miksi tata kutsutaan?
    tested_word = wd;
    unsigned jj = 0;
    for( ; tested_word[ jj ]; ++jj );
    return has_word( 0, jj, true );
  }

  friend draw_tree_node *CFG_make_tree( char, unsigned, unsigned, unsigned );


/*** Features for generating strings of the language up to given length and
  comparing two languages ***/

private:

  /* Helper variables for the generation of the short words in the language */
  static unsigned gw_max, gw_size, gw_ll;
  static const char *gw_rp;

  /* The heap for generated but not yet further processed words */
  struct heap_elem{
    const char *wd;   // the generated word
    CFG *ntp;         // wd belongs to the language of *nt
  };
  static unsigned heap_cnt;                   // total number of pushed items
  static unsigned const heap_limit = 50000;   // when met, terminate wg
  static std::vector< heap_elem > word_heap;

  /* Push a copy of the buffer to the heap as a word of this nt. */
  void heap_push(){

    /* Give up after too much work. */
    if( heap_cnt < heap_limit ){ ++heap_cnt; }
    else{ incomplete = 2; return; }

    /* Copy buff to fresh memory. */
    char *new_wd = new char[ buff_sz + 1 ];
    for( unsigned jj = 0; jj < buff_sz; ++jj ){ new_wd[ jj ] = buff[ jj ]; }
    new_wd[ buff_sz ] = '\0';

    /* Push the copy to the heap. */
    unsigned ii = word_heap.size();
    word_heap.resize( ii + 1 );
    while( ii ){
      unsigned jj = ( ii - 1 ) / 2;
      if( shortlex( new_wd, word_heap[ jj ].wd ) >= 0 ){ break; }
      word_heap[ ii ] = word_heap[ jj ]; ii = jj;
    }
    word_heap[ ii ].wd = new_wd; word_heap[ ii ].ntp = this;

  }

  /* Remove the topmost element of the heap. Do not release its memory. */
  static void heap_pop(){
    unsigned sz = word_heap.size() - 1;
    heap_elem he = word_heap[ sz ];
    word_heap.resize( sz );
    unsigned ii = 0;
    while( true ){
      unsigned jj = 2*ii + 1;
      if(
        jj+1 < sz &&
        shortlex( word_heap[ jj ].wd, word_heap[ jj + 1 ].wd ) >= 0
      ){ ++jj; }
      if( jj >= sz || shortlex( word_heap[ jj ].wd, he.wd ) >= 0 ){ break; }
      word_heap[ ii ] = word_heap[ jj ]; ii = jj;
    }
    word_heap[ ii ] = he;
  }

  /* Generate the words that do not need nonterminal invocations, record the
    instances of nonterminal invocations, and proceed recursively to all other
    relevant nonterminals. */
  void gw_bottom(){
    if( incomplete > 1 ){ return; }

    /* Avoid infinite recursion. */
    if( gw_started ){ return; }
    gw_started = true;

    /* Try all rules of this nonterminal. */
    for( unsigned rr = 0; rr < rules.size(); ++rr ){
      bool success = true;
      buff_sz = 0;

      /* Go through the rule. */
      for( const char *rp = rules[ rr ]; *rp; ++rp ){
        if( is_terminal( *rp ) ){
          if( success && !buff_add( *rp ) ){ success = false; }
        }else{
          CFG &nt = nonterminals[ *rp - offset ];
          nt.users.push_back( nt_user( this, rr, rp - rules[ rr ] ) );
          success = false; nt.gw_bottom();
        }
      }
      if( success ){ heap_push(); }
    }

  }

  /* Generate the words that do need nonterminal invocations. */
  void gw_sub( unsigned jj ){
    if( incomplete > 1 ){ return; }

    while( gw_rp[ jj ] && is_terminal( gw_rp[ jj ] ) ){
      if( buff_add( gw_rp[ jj ] ) ){ ++jj; }else{ break; }
    }

    if( !gw_rp[ jj ] ){ heap_push(); return; }

    if( is_terminal( gw_rp[ jj ] ) ){ return; }

    unsigned buff_old = buff_sz;
    if( jj != gw_ll ){
      CFG &nt = nonterminals[ gw_rp[ jj ] - offset ];
      for( unsigned ww = 0; ww < nt.words.size(); ++ww ){
        unsigned kk = 0;
        for( ; nt.words[ ww ][ kk ]; ++kk ){
          if( !buff_add( nt.words[ ww ][ kk ] ) ){ break; }
        }
        if( !nt.words[ ww ][ kk ] ){ gw_sub( jj+1 ); }
        buff_sz = buff_old;
      }
    }
    if( jj == gw_ll || ( jj < gw_ll && gw_rp[ jj ] == gw_rp[ gw_ll ] ) ){
      const char *wd = word_heap[0].wd;
      unsigned kk = 0;
      for( ; wd[ kk ]; ++kk ){
        if( !buff_add( wd[ kk ] ) ){ break; }
      }
      if( !wd[ kk ] ){ gw_sub( jj+1 ); }
      buff_sz = buff_old;
    }
  }

  /* Free the dynamically allocated words. */
  static void words_clear(){
    buff_sz = 0; incomplete = 0; heap_cnt = 0;
    for( unsigned ii = 0; ii < word_heap.size(); ++ii ){
      delete word_heap[ ii ].wd;
    }
    word_heap.clear();
    for( unsigned ii = 0; ii < 52; ++ii ){
      CFG &nt = nonterminals[ ii ];
      for( unsigned jj = 0; jj < nt.words.size(); ++jj ){
        delete nt.words[ jj ];
      }
      nt.words.clear(); nt.gw_started = false;
      delete duplicate; duplicate = 0;
    }
  }

public:

  /* Generate all short words in the language of this nonterminal. */
  void generate_words(){

    /* Generate the relevant words that do not use nonterminal invocations. */
    words_clear();
    gw_bottom();

    /* Generate all words up to maximum length. */
    while( !word_heap.empty() ){

      /* Pop a word from the heap. If it is a duplicate, release its memory.
        Otherwise, add it to the word list after using it in all possible ways
        as a reply to nonterminal invocations, to generate new words. */
      CFG *ntp = word_heap[0].ntp;
      if(
        ntp->words.empty() || shortlex( ntp->words.back(), word_heap[0].wd )
      ){

        for( unsigned uu = 0; uu < ntp->users.size(); ++uu ){
          nt_user &us = ntp->users[ uu ];
          gw_rp = us.ntp->rules[ us.rr ]; gw_ll = us.ll;
          buff_sz = 0; us.ntp->gw_sub( 0 );
        }
        ntp->words.push_back( word_heap[0].wd );

      }else{ delete word_heap[0].wd; }

      heap_pop();
    }

  }

  /* Check that the languages of ch0,0 and ch1,1 agree up to words of given
    length. */
  static void compare_languages( char ch0, char ch1 ){
    words_clear();
    if( is_terminal( ch0 ) ){ ch0 = 'A'; }
    if( is_terminal( ch1 ) ){ ch1 = 'A'; }

    /* Generate the relevant words that do not use nonterminal invocations. */
    CFG *start_0 = nt_named( ch0, 0 ), *start_1 = nt_named( ch1, 1 );
    start_0->gw_bottom(); start_1->gw_bottom();

    /* Generate all words up to maximum length. */
    while( !word_heap.empty() ){

      /* Pop a word from the heap. If it is a duplicate, release its memory
        except that record it, if it is the first found duplicate of start1.
        Otherwise, add it to the word list after using it in all possible ways
        as a reply to nonterminal invocations, to generate new words. */
      CFG *ntp = word_heap[0].ntp;
      if(
        ntp->words.empty() || shortlex( ntp->words.back(), word_heap[0].wd )
      ){

        if(
          start_0->words.size() > start_1->words.size() &&
          shortlex( start_0->words.back(), word_heap[0].wd ) < 0
        ){ break; }
        if(
          start_1->words.size() > start_0->words.size() &&
          shortlex( start_1->words.back(), word_heap[0].wd ) < 0
        ){ break; }

        /* Use the word to generate new words. */
        for( unsigned uu = 0; uu < ntp->users.size(); ++uu ){
          nt_user &us = ntp->users[ uu ];
          gw_rp = us.ntp->rules[ us.rr ]; gw_ll = us.ll;
          buff_sz = 0; us.ntp->gw_sub( 0 );
        }

        ntp->words.push_back( word_heap[0].wd );

      }else if( !duplicate && ntp->offset == 'A'-26 ){
        duplicate = word_heap[0].wd;

      }else{ delete word_heap[0].wd; }

      heap_pop();
    }

    if( incomplete ){ err_set_warning( check_level ); }
    if( incomplete > 1 ){
    }else if( start_0->words.size() > start_1->words.size() ){
      err_msg = start_0->words.back(); err_mode = err_CFG_1;
    }else if( start_1->words.size() > start_0->words.size() ){
      err_msg = start_1->words.back(); err_mode = err_CFG_2;
    }

  }

  /* Free all dynamically reserved memory except perhaps one set of rules. */
  static void clear( unsigned nt_set = 2 ){
    hash_clear();
    words_clear();
    for( unsigned ii = 0; ii < 52; ++ii ){ nonterminals[ ii ].users.clear(); }
    unsigned lo = nt_set == 1 ? 26 : 0, hi = nt_set == 0 ? 26 : 52;
    for( unsigned ii = lo; ii < hi; ++ii ){
      nonterminals[ ii ].rules_clear();
    }
    if( nt_set != 1 ){ start0 = 'A'; }
    if( nt_set ){ start1 = 'A'; }
  }

};
CFG CFG::nonterminals[ 52 ];
char CFG::start0 = 'A', CFG::start1 = 'A';
const char *CFG::duplicate = 0;
unsigned CFG::incomplete = 0;
unsigned CFG::buff_sz = 0;
char CFG::buff[] = {};
const char *CFG::tested_word;
CFG::hash_elem *CFG::hash_table[] = {};
unsigned CFG::rule_search = 1;
unsigned CFG::gw_ll = 0;
const char *CFG::gw_rp = 0;
unsigned CFG::heap_cnt = 0;
std::vector< CFG::heap_elem > CFG::word_heap;


/* Read a string. */
bool CFG_ignore_CR = false;
void CFG_string( char banned = '\0' ){
  inp_skip_white_space(); err_pos = inp_byte_now; CFG::buff_sz = 0;
  if( !inp_chr || inp_chr == banned ){
    err_set_inp( "A string is needed here" );
    CFG::buff[ CFG::buff_sz ] = '\0'; return;
  }
  char stopper = ' ';
  while( stopper && inp_chr && lc() ){
    if( inp_chr == '"' || inp_chr == '\'' ){
      stopper = inp_chr; inp_get_chr();
    }
    while( inp_chr && inp_chr != stopper ){
      if( inp_chr == '\n' ){
        if( !CFG_ignore_CR ){
          if( stopper != ' ' ){ stopper = '\0'; }
          break;
        }
      }else if( !CFG::buff_add( inp_chr ) ){
        stopper = '\0'; err_set_inp( "The string is too long" ); break;
      }
      inp_get_chr();
    }
    if( stopper == ' ' ){ stopper = '\0'; }
    else{
      if( inp_chr != stopper ){
        err_set_inp( "The string was not terminated properly" ); break;
      }else{
        inp_get_chr(); inp_skip_white_space(); err_pos = inp_byte_now;
        if( inp_chr != '"' && inp_chr != '\'' ){ stopper = '\0'; }
      }
    }
  }
  CFG::buff[ CFG::buff_sz ] = '\0';
}


/* Read the start symbol. */
char CFG_start_symbol( bool nt_set = false ){
  inp_skip_white_space();
  if( CFG::is_terminal( inp_chr ) ){
    err_pos = inp_byte_now;
    err_set_inp( "Capital letter expected" ); return 'A';
  }
  char start_symbol = inp_chr;
  if( nt_set ){ CFG::start1 = start_symbol; }
  else{ CFG::start0 = start_symbol; }
  inp_get_chr(); return start_symbol;
}


/* Read the grammar. */
void CFG_read( unsigned nt_set = 2 ){
  CFG::clear( nt_set );
  if( nt_set > 1 ){ nt_set = 0; }

  CFG *ntp = 0;   // the current nonterminal

  /* Find and check the first token. */
  inp_skip_white_space();
  if( CFG::is_terminal( inp_chr ) && inp_chr != ':' ){
    err_pos = inp_byte_now;
    err_set_inp( "capital letter or \":\" expected" ); return;
  }

  /* Scan all rule sets. */
  while( !CFG::is_terminal( inp_chr ) ){

    /* Read the nonterminal symbol. */
    if( !ntp ){
      if( nt_set ){ CFG::start1 = inp_chr; }
      else{ CFG::start0 = inp_chr; }
    }
    ntp = CFG::nt_named( inp_chr, nt_set );
    if( !ntp->empty() ){
      err_pos = inp_byte_now;
      err_set_inp( "The nonterminal has already been given rules" ); return;
    }
    inp_get_chr();

    /* Bypass the ::= symbol. */
    inp_skip_white_space(); err_pos = inp_byte_now;
    bool inp_ok = true;
    if( inp_chr != ':' ){ inp_ok = false; }else{
      inp_get_chr();
      if( inp_chr != ':' ){ inp_ok = false; }else{
        inp_get_chr();
        if( inp_chr != '=' ){ inp_ok = false; }else{ inp_get_chr(); }
      }
    }
    if( !inp_ok ){
      inp_revert_to( err_pos ); err_set_inp( "::= expected" ); return;
    }

    /* Read the right hand sides of the rules of the nonterminal. */
    while( true ){

      /* Read the right hand side and record the rule. */
      CFG_string( ';' );
      if( err_mode ){ return; }
      ntp->add_rule();

      /* Switch to next right hand side or exit. */
      inp_skip_white_space();
      if( inp_chr != '|' ){ break; }
      inp_get_chr();

    }

    if( CFG::is_terminal( inp_chr ) && inp_chr != ':' && inp_chr != ';' ){
      err_pos = inp_byte_now;
      err_set_inp( "\"|\", capital letter, \":\", or \";\" expected" );
      return;
    }

  }

  /* Read the optional start symbol. */
  if( inp_chr == ':' ){
    inp_get_chr(); CFG_start_symbol( nt_set );
    inp_skip_white_space();
    if( inp_chr != ';' ){
      err_pos = inp_byte_now; err_set_inp( "\";\" expected" ); return;
    }
  }

  /* Bypass the end-of-CFG symbol ";". */
  inp_get_chr();

}


void parse_CFG_compare(){
  if( err_mode ){ return; }
  if( CFG::start0 != CFG::start1 ){ err_mode = err_CFG_start; return; }
  CFG::compare_languages( CFG::start0, CFG::start1 );
}


/* Create a drawable (sub)tree from the result of has_word. */
draw_tree_node *CFG_make_tree(
  char nt_name, unsigned ii, unsigned jj, unsigned match
){

  /* Find info about the matched rule. */
  const char *rp = CFG::nt_named( nt_name )->rules[ match ];
  CFG::hash_elem *he = CFG::hash_find( rp, ii, jj );
  if( !he ){ return 0; }

  /* Create the root node. */
  draw::name_buff[0] = nt_name; draw::name_buff[1] = '\0';
  draw_tree_node *nd = new draw_tree_node( draw::name_buff, 2, 1 );

  /* If the rule is empty, prepare to draw epsilon. */
  unsigned draw_buff_sz = 0;
  if( !*rp ){
    draw::name_buff[ 0 ] = char( 128 + op_epsilon - op_alpha );
    ++draw_buff_sz;
  }

  /* Go through the rule. */
  for( ; *rp; ++rp ){

    /* Copy terminal to the node name buffer. Consume it from the string. */
    if( CFG::is_terminal( *rp ) ){
      if( draw_buff_sz < CFG::buff_max ){
        draw::name_buff[ draw_buff_sz ] = *rp; ++draw_buff_sz;
      }//??? else{ err_set_inp( "Too long node content" ); break; }
      ++ii;

    }else{

      /* If there have been unprocessed terminals, create a node for them. */
      if( draw_buff_sz ){
        draw::name_buff[ draw_buff_sz ] = '\0';
        nd->add_child( new draw_tree_node( draw::name_buff, 2, 1 ) );
        draw_buff_sz = 0;
      }

      /* Create the subtree corresponding to the nonterminal. */
      unsigned kk = he->cut_point;
      draw_tree_node *nd2 = CFG_make_tree( *rp, ii, kk, he->match );
      if( !nd2 ){ delete nd; return 0; }
      nd->add_child( nd2 ); ii = kk;
      he = CFG::hash_find( rp+1, ii, jj );
      if( !he ){ delete nd; return 0; }

    }
  }

  /* If there have been unprocessed terminals or epsilon should be drawn,
    create a node for them. */
  if( draw_buff_sz ){
    draw::name_buff[ draw_buff_sz ] = '\0';
    nd->add_child( new draw_tree_node( draw::name_buff, 2, 1 ) );
  }

  /* Fix the order of the children and return the result. */
  nd->reverse_children();
  return nd;

}


/* Draw a parse tree or report that the string is not in the language. */
void parse_CFG_tree(){

  /* Ensure that the grammar exists. */
  if( CFG::is_terminal( CFG::start0 ) ){
    err_set_inp(
      "Cannot test membership, because no grammar has been given"
    );
    return;
  }

  /* Read the string to be parsed. */
  CFG_string();
  if( err_mode ){ return; }

  /* Test membership and return if not. */
  CFG *ntp = CFG::nt_named( CFG::start0 );
  unsigned match = ntp->has_word( CFG::buff );
  out_html( "\n<p>" ); pgh_broken = false;
  out_print( "The string \"" ); out_esc( CFG::buff );
  if( match == ~0u ){ out_print( "\" is <strong>not" ); }
  else{ out_print( "\" <strong>is" ); }
  out_print( "</strong> in the language <i>" ); out_print( CFG::start0 );
  out_print( "</i>.\n" );
  if( match == ~0u ){ return; }

  /* Draw the tree, if possible. */
  draw_tree_node *root = CFG_make_tree( CFG::start0, 0, CFG::buff_sz, match );
  if( root && !err_mode ){
    draw_reset_colours();
    draw_colour( draw::white ); draw_colour( draw::maroon );
    root->draw( "A CFG tree" ); draw_ok();
  }else{
    out_html( "\nUnfortunately I cannot draw its parse tree.\n" );
    delete root;
  }

}


unsigned CFG_short = 0, CFG_long = ~0u;


void parse_CFG_in( bool positive ){
  CFG_string();
  if( err_mode ){ return; }
  err_msg = CFG::buff;
  if( CFG::buff_sz < CFG_short ){ err_mode = err_CFG_short; return; }
  if( CFG::buff_sz > CFG_long ){ err_mode = err_CFG_long; return; }
  if( CFG::nt_named( CFG::start0 )->has_word( CFG::buff ) == ~0u ){
    if( positive ){ err_mode = err_CFG_not_in; return; }
  }else{
    if( !positive ){ err_mode = err_CFG_in; return; }
  }
  get_token();
  if( !parse_is_top_tkn() ){ parse_error(); }
}
