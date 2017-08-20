/* This file lists the (partial) locations of HTML, executable, etc. files of
  the ordinary and examination versions of MathCheck. */

const char
#ifdef exam
  *URL_html_base = "http://matriisi.ee.tut.fi:740/",
  *URL_cgi_base = "http://matriisi.ee.tut.fi:740/cgi-bin/",
  *URL_css = "http://matriisi.ee.tut.fi:740/mathcheck.css",
  *URL_mathjax =
    "http://matriisi.ee.tut.fi:740/MathJax/MathJax.js?config=AM_HTMLorMML";
#else
  *URL_html_base = "http://math.tut.fi/mathcheck/",
  *URL_cgi_base = "http://math.tut.fi/mathcheck/cgi-bin/",
  *URL_css = "http://math.tut.fi/mathcheck/mathcheck.css",
  *URL_mathjax =
    "http://math.tut.fi/mathcheck/MathJax/MathJax.js?config=AM_HTMLorMML";
#endif
