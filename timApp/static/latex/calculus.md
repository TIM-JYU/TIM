RAWTEXseuraus

Tämä luentomateriaali on tarkoitettu syksyn 2016 kurssille

$\phantom{sisennys-sisennys}$ **Calculus 2**

(matematiikan perusopinnot, 5 op). Materiaali on vielä keskeneräistä ja
täydentyy kurssin aikana. Ilmoitathan löytämistäsi virheistä
luennoitsijalle!

-   Merkintä \[A, *x*.*y*\] viittaa kirjan

        [A] Adams, Robert A. Calculus: a complete course, 8. painos, Pearson 2013

    luvun *x* kappaleeseen *y*.

+Katso kurssiin liittyvät käytännön asiat (suorittaminen, aikataulut)
kurssin kotisivulta. Linkki kotisivulle löytyy Korpista.

ENDRAWTEX

# Käänteisfunktio

-   Yleistä kurssista ja transkendenttifunktioista
-   Injektio
-   Käänteisfunktio
-   Määrittelyjoukon rajoittaminen
-   Käänteisfunktion derivointi

\[A, 3.1\]

## Transkendenttifunktioista ja kurssin sisällöstä hiukan

-   *Algebrallisia funktioita* ovat
    -   polynomit
    -   rationaalifunktiot
    -   juurifunktiot ($x^{\frac{1}{n}}$)
    -   sekä näiden yhdistelmät, esim.
        $(x^2+\frac{1}{x^2})^{\frac{3}{2}}- \frac{4}{x^5}$
-   Funktioita, jotka eivät ole algebrallisia, kutsutaan
    *transkendenttisiksi funktioiksi*, esim.
    -   trigonometriset funktiot
-   Tällä kurssilla tavataan lisää transkendenttisia funktioita, lähinnä
    -   (yleinen) eksponenttifunktio $a^x$ ja
    -   (yleinen) logaritmifunktio $\log_a x$
        -   näissä kantaluku $a>0$ on vakio (mikä vain; "funktioperhe")
        -   esim. $f(x) = 2^x$, $f(5)=2^5=32$
    -   sekä lisäksi erikoistapaukset
        -   eksponenttifunktio $e^x$ ja
        -   (luonnollinen) logaritmifunktio $\log x$ (joskus merkitään
            myös $\operatorname{ln} x$)
            -   näissä kantalukuna on *Neperin luku*
                $e= 2{,}718281828459\ldots$
        -   ynnä vielä näistä saatavia johdannaisia
            -   hyperboliset ja areahyperboliset funktiot (myöhemmin)
    -   ja vielä lisäksi trigonometrisiin funktioihin liittyvät
        -   arkusfunktiot (myöhemmin)
-   Kaikkia edellämainittuja kutsutaan *alkeisfunktioiksi*
    -   "niillä on lauseke"
-   +Kurssin lopulla opitaan integrointi, jonka avulla voidaan
    määritellä lisää funktioita
-   +Seuraavalla kurssilla (Calculus 3) opitaan laskemaan "äärettömiä
    summia", joiden avulla voidaan määritellä vielä lisää funktioita,
    jotka eivät ole alkeisfunktioita
-   +Differentiaaliyhtälöiden kurssilla...

## Injektio

### Lämmittelytehtävä

1.  Mihin potenssiin luku $2$ pitää korottaa, jotta saadaan luku $8$?
    -   Eli ratkaise yhtälö $$2^x = 8$$
2.  Ratkaise yhtälöt $$3^x = 81 \quad \text{ ja } \quad 4^x=2$$
3.  Ratkaise yhtälöt $$x^2=1 \quad \text{ ja } \quad \sqrt{x} = 5$$
4.  Ratkaise yhtälöt $$x^3=1 \quad \text{ ja } \quad \sqrt[3]{x} = 2$$
5.  Mieti: milloin sait yhden ratkaisun, milloin useamman?

### Injektio

"Jos funktio saa saman arvon vain kerran, sitä sanotaan injektioksi."

Tarkemmin:

Sanomme, että funktio $f\colon A \to B$ on injektio, jos
$$ f(x_1) \not = f(x_2) \text{ aina, kun } x_1 \not = x_2, \text{  } x_1, x_2 \in A$$
eli jos kaikilla $x_1, x_2 \in A$ on voimassa
$$f(x_1) = f(x_2) \implies x_1=x_2.$$

-   Kuvana:
    -   funktio on injektio, joss se ei leikkaa mitään vaakasuoraa
        suoraa kuin kerran.
-   Engl. "one-to-one", "injective"

-   Yhteys monotonisuuteen:
    -   jos $f$ on aidosti monotoninen, se on injektio.
    -   Huom: injektiivisyys ei takaa monotonisuutta, ellei $f$ ole
        jatkuva.
-   Yhteys derivaattaan: jos $f$ on derivoituva ja
    -   $f'(x)>0$ kaikkialla, niin $f$ on aidosti kasvava ja siten
        injektio
    -   $f'(x)<0$ kaikkialla, niin $f$ on aidosti vähenevä ja siten
        injektio
    -   Huom: $f(x)=0$ yksittäisissä pisteissä ei haittaa
-   Huom: injektiivisyys ei takaa jatkuvuutta eikä jatkuvuus
    injektiivisyyttä!

    (Jatkuvuus, monotonisuus ja derivaatta: ks. [Calculus
    1](https://tim.jyu.fi/view/kurssit/matematiikka/Calculus/Calculus1),
    luvut 7, 11 ja 9.)

**Esimerkkejä**

1.  $f \colon \mathbb{R} \to \mathbb{R}, \, f(x) = x^3\,$ on injektio
2.  $g \colon \mathbb{R} \to \mathbb{R}, \, g(x) = x^2 \,$ ei ole
    injektio

## Käänteisfunktio

"Jos tiedän funktion arvon, tiedänkö muuttujan arvon?"

-   Esimerkki:
    1.  Olkoon $\, f(x) = x^3\,$. Jos $f(x)=8$, mikä on $x$?
        -   käänteisfunktio on $\sqrt[3]{x}$ eli $x^{\frac13}$
        -   "toimii kaikilla $x \in \mathbb{R}$"

### Määritelmä

-   Jos $f$ on injektio, sillä on käänteisfunktio, jota merkitään
    $f^{-1}$.

-   Käänteisfunktio on määritelty kaikilla niillä arvoilla, joita $f$
    saa (eli funktion $f$ arvojoukossa)
    -   funktion $f^{-1}$ määrittelyjoukko on funktion $f$ arvojoukko
        ($A_f$)
    -   funktion $f^{-1}$ arvojoukko on funktion $f$ määrittelyjoukko
        ($M_f$)
-   Siis
    $$f(x) = y \iff x=f^{-1}(y), \quad \text{ kun } \, x \in M_f \text{ ja } y \in A_f$$
    -   esim. $\,x^3 = 8 \iff x=\sqrt[3]{8}$

### Ominaisuuksia

-   "Kumoutuminen": $$f^{-1}(f(x))=x, \quad \text{ kun } \, x \in M_f$$
    ja $$f(f^{-1}(y)) = y, \quad \text{ kun } \, y \in A_f$$

-   Lausekkeen etsiminen:
    -   ratkaise $x$ yhtälöstä $f(x) = y$, saat muuttujan $y$ lausekkeen
    -   vaihda muuttujan $y$ paikalle $x$
    -   esim. edellä $\,f(x) = x^3$, $f^{-1}(x)=\sqrt[3]{x}$
    -   esim. $\,g(x) = x-2$, $g^{-1}(x) = x+2$
-   Kuva:
    -   käänteisfunktion kuvaaja $y=f^{-1}(x)$ on kuvaajan $y=f(x)$
        peilaus suoran $y=x$ suhteen
-   Käänteisfunktion käänteisfunktio: $$
      (f^{-1})^{-1}(x) = f(x)
    $$

**Huom!**

-   Älä sekoita näitä kahta merkintää:
    -   käänteisfunktion arvo pisteessä $x$ on $f^{-1}(x)$
    -   funktion arvon käänteisluku on $\frac{1}{f(x)} = (f(x))^{-1}$
    -   esim. edellä, kun $f(x) = x^3$:
        -   $f^{-1}(x)= \sqrt[3]{x}=x^{\frac13} \quad (=2$, jos $x=8)$
        -   $\frac{1}{f(x)} = \frac{1}{x^3} = x^{-3} \quad (=\frac{1}{512},$
            jos $x=8)$

**Esimerkkejä**

1.  Osoita, että $f(x)=3x+1$ on injektio ja määrää sen käänteisfunktion
    lauseke.
    -   Funktio $\,f\,$ on määritelty kaikilla $\,x\in \mathbb{R}\,$ ja
        sen arvojoukko on $\mathbb{R}$
    -   Koska $\,f'(x)=3 > 0\,$ kaikilla $\,x\in \mathbb{R}$, on $f$
        aidosti kasvava ja siis injektio (koko reaalilukujen joukossa).
    -   Etsitään käänteisfunktion lauseke: $$
          3x+1 = y \iff 3x=y-1 \iff x=\frac{y-1}{3}
        $$
    -   Siis $f^{-1}(x)=\frac{x-1}{3} \quad$ (kaikilla
        $\,x\in \mathbb{R}\,$)

\phantom{v}

2.  Osoita, että $g(x)=\sqrt{2x+1}$ on injektio ja määrää sen
    käänteisfunktion lauseke.
    -   Vastaavasti kuin edellä tai ks. \[A,s.167\].
    -   Huomaa, että nyt funktion $g$
        -   määrittelyjoukko on $[-\frac12, \infty[$
        -   arvojoukko on $[0,\infty[$
    -   Siis $g^{-1}(x)=\frac{x^2-1}{2}$, kun $x \geq 0$.
    -   +Hahmottele lisäksi kuva ja etsi kuvasta kummankin funktion
        määrittely- ja arvojoukot.

### Määrittelyjoukon rajoittaminen

Entä, jos $f$ ei ole injektio, kuten esim. $\,f(x) = x^2-3\,$?

-   Etsitään määrittelyjoukosta osa, jossa $f$ on injektio, esim.
    $[0, \infty[$
-   Tässä osassa funktiolla on käänteisfunktio
    -   eli "funktion $f$ rajoittumalla joukkoon $[0, \infty[$ on
        käänteisfunktio"
-   Käänteisfunktion määrittelyjoukko = ne arvot, joita $f$ tässä osassa
    saa
    -   esim. edellä $[-3,\infty[$
-   Käänteisfunktion arvojoukko = tämä osa
    -   esim. edellä siis $[0,\infty[$
-   Lausekkeen laskeminen kuten aiemmin

### Käänteisfunktion derivointi

-   Joskus käänteisfunktion lausekkeen etsiminen on hankalaa.
-   Derivointi onnistuu silti alkuperäisen funktion derivaatan avulla:

$$
  (f^{-1})'(y) = \frac{1}{f'(x)}, \quad \text{kun } \, y=f(x)
$$

**Esimerkki**

1.  Osoita, että $\,f(x) = x^3+x\,$ on injektio koko joukossa
    $\mathbb{R}$ ja määritä $(f^{-1})'(10)$, kun tiedetään, että
    $f(2)=10$.
    -   Lasketaan derivaatta $f'(x)=3x^2+1$
    -   Koska $f'(x) > 0$ kaikilla $x$, funktio $f$ on aidosti kasvava
        koko joukossa $\mathbb{R}$ ja siis injektio koko joukossa
        $\mathbb{R}$.
    -   Käänteisfunktion derivaatta pisteessä $10$ on $$
          (f^{-1})'(10) = \frac{1}{f'(2)}= \frac{1}{3\cdot 2^2 + 1}=\frac{1}{13}
        $$
    -   Piirrä kuva: mitkä ovat toistensa käänteislukuja?

\[A, s. 169\]

# Eksponentti- ja logaritmifunktiot

-   yleinen ja luonnollinen logaritmi- ja eksponenttifunktio sekä näiden
    derivointi
-   laskusäännöt ja derivointi
-   vertailu polynomeihin
-   eksponentiaalinen kasvu

\[A, 3.2-3.4\]

## Yleinen eksponenttifunktio

**Lämmittelyksi:**

-   Laske funktion $f(x) = 2^x$ arvot, kun $x=0, 1, 2, 3$ ja piirrä
    pisteet koordinaatistoon.
-   Laske lisää arvoja, kun $x= -1, -2, -3$ ja piirrä.
-   Laske myös likiarvoja, kun $x=\frac{1}{2}, \frac{4}{5}, ...$
    (valitse itse murtolukuja) ja piirrä.

-   Miltä kuvaaja alkaa näyttää?

-   Hahmottele vastaavalla tavalla funktion $g(x) = 3^x$ kuvaajaa. Mikä
    muuttuu?

-   Entä funktion $h(x)= (\frac{1}{2})^x$ kuvaaja, miltä mahtaisi
    näyttää - hahmotatko laskematta?

### Määritelmä

Funktioita $$
  f(x) = a^x,
$$ missä

-   kantaluku $a$ on vakio, $\,a>0\,$ ja
-   muuttuja $\,x\,$ esiintyy vain eksponentissa

sanotaan *eksponenttifunktioiksi*.

**Huomautuksia**

-   Eri vakion $a$ arvoilla saadaan siis eri funktio, "*$a$-kantainen
    eksponenttifunktio*"
    -   esim. $\,f(x) = 2^x\,$ tai $\,g(x)= 3^x\,$ tai $\,k(x)= \pi^x$
-   Eri eksponenttifunktioilla on samoja ominaisuuksia, jotka eivät
    riipu vakion $a$ arvosta
    -   jatkuvuus, derivoituvuus, monotonisuus, positiivisuus,
        laskusäännöt, raja-arvot
-   Älä sekoita *potenssifunktioon* $\,x^b$, missä
    -   muuttuja on kantalukuna
    -   eksponentti on vakio ($b \in \mathbb{R}$)

Miten määritellään vaikkapa $2^x$?

-   Osataan jo, kun $\,x\,$ on rationaaliluku:
    -   $\, 2^0=1$
    -   $\, 2^n=2 \cdot \ldots \cdot 2\,$ ($n$ kpl), kun
        $n=1, 2, \ldots$
    -   $\,2^{-n} = \frac{1}{2^n}\,$, kun $n=1, 2, \ldots$
    -   $\,2^{\frac{1}{n}}= \sqrt[n]{2}\,$ "se luku $p\geq 0$, jolle
        $p^n = 2\,$"
        -   (eli funktion $\,y=z^n\,$ käänteisfunktion arvo pisteessä 2)
    -   $\,2^{\frac{m}{n}}= (2^m)^{\frac{1}{n}}= \sqrt[n]{2^m}$
-   Entä, kun $\,x\,$ on irrationaaliluku?
    -   Otetaan raja-arvo "rationaalilukuja pitkin",
        $$2^x = \underset{r \in \mathbb{Q}}{\lim_{r \to x}} 2^r$$
        -   eli "täytetään kuvaajaan jääneet rei'ät".
    -   Tällä kurssilla tämä riittää, myöhemmillä kursseilla tarkemmin.
        -   Pitäisi osoittaa jatkuvuus, derivoituvuus, ...
        -   Tällä kurssilla uskotaan ominaisuudet ja käytetään niitä.

Samalla tavalla määritellään $a^x$ mille tahansa $a>0$.

-   Mieti: mitä ongelmia aiheuttaisi negatiivinen kantaluku?

### Ominaisuuksia

#### Laskusäännöt

Kun $a>0$, niin

-   $\,a^0 = 1$
-   $\,a^{-x} = \frac{1}{a^x}$
-   $\, a^{x+y} = a^x a^y$
-   $\, a^{x-y} = \frac{a^x}{a^y}$
-   $\, (a^{x})^y = a^{xy}$
    -   Samat laskusäännöt kuin kokonaislukupotensseille!

Lisäksi, kun myös $\,b>0$, on

-   $\, (ab)^{x} = a^x b^x$
-   $\, (\frac{a}{b})^x = \frac{a^x}{b^x}$
    -   kuten kokonaislukupotensseille

Huomaa, että kaikki eksponenttifunktiot saavat nollassa arvon 1 (koska
$a^0=1$).

#### Monotonisuus

Miten vakio $\,a\,$ vaikuttaa?

-   Jos $\,a>1$, niin $\, a^x\,$ on aidosti kasvava
    -   esim. $2^x$
-   Jos $\,0<a<1$, niin $\, a^x$ on aidosti vähenevä
    -   esim. $(\frac{1}{2})^x$
    -   vrt. peilaus, $\, a^x= (\frac1a)^{-x}$
-   Jos $\,a=1$, niin $\,a^x = 1^x = 1\,$ kaikilla $x \in \mathbb{R}$.

#### Positiivisuus

Kun $a>0$, on $$
  a^x > 0 \quad \text{ kaikilla } x \in \mathbb{R} .
$$

#### Raja-arvot

Kun $a>1$, $$
  \lim_{x \to \infty} a^x = \infty \quad \text{ ja }  \quad
  \lim_{x \to -\infty} a^x = 0 .
$$

Kun $0<a<1$, $$
  \lim_{x \to \infty} a^x = 0 \quad \text{ ja }  \quad
  \lim_{x \to -\infty} a^x = \infty .
$$

-   Siis jos $\,a\not = 1$, kuvaajalla $\,y=a^x\,$ on vaakasuora
    asymptootti $y=0$.

#### Jatkuvuus ja derivoituvuus

Kun $a>0$, eksponenttifunktio $a^x$ on

-   kaikkialla jatkuva funktio ja
-   kaikkialla derivoituva funktio (derivointi myöhemmin).

### Yhteenveto

-   Emme todista yllä mainittuja ominaisuuksia tällä kurssilla, käytämme
    vain.

-   Eksponenttifunktio $a^x$ on siis
    -   määritelty kaikilla $x \in \mathbb R\,$ ja
    -   saa kaikki arvot väliltä $]0, \infty[$ eikä muita, eli sen
        -   määrittelyjoukko on $\mathbb{R}$ ja
        -   arvojoukko on $]0, \infty[$.

## Yleinen logaritmifunktio

Kun funktio $\,a^x\,$ on aidosti monotoninen, sillä on käänteisfunktio,
jolle annamme nimen *$a$-kantainen logaritmi(funktio)* ja merkinnän
$\log_a x$.

### Määritelmä

Kun $\,a>0\,$ ja $\,a \not = 1\,$, $$
  \log_a x = \text{ "se luku } y, \text{ jolle } a^y = x \text{ "}
$$ eli "se potenssi, johon kantaluku $a$ pitää korottaa, jotta saadaan
$x$".

-   Toisin sanoen $$
      y = \log_a x \iff x = a^y \quad (a>0, a \not = 1)
    $$

### Ominaisuuksia

Kun $\,a>0$, on funktion $\,a^x$

-   määrittelyjoukko $\mathbb{R}$ ja
-   arvojoukko $\, ]0, \infty[$.

Siksi funktion $\, \log_a x \,$

-   määrittelyjoukko on $\, ]0, \infty[$ ja
-   arvojoukko on koko $\mathbb{R}$.

Kumoutuminen: $$
  \log_a (a^x) = x \quad \text{ kaikilla } \, x \in \mathbb{R}
$$ ja $$
  a^{\log_a x} = x \quad \text{ kaikilla } x >0.
$$

#### Laskusäännöt

Kun $\, a> 0\,$ ja $\,a\not = 1\,$,

-   $\,\log_a 1 = 0$
-   $\,\log_a (\frac{1}{x}) = - \log_a x$
-   $\,\log_a (xy) = \log_a x + \log_a y$
-   $\,\log_a (\frac{x}{y}) = \log_a x - \log_a y$
-   $\,\log_a (x^y) = y \log_a x$

Nämä seuraavat suoraan potenssien laskusäännöistä:

-   merkitse $u=\log_a x$, jolloin $x=a^u$ ja esim.
    $\frac{1}{x} = x^{-1} = (a^u){-1} = a^{-u}$ eli
    $\,\log_a (\frac{1}{x}) = - u = - \log_a x$. Osoita itse loput (HT).

Lisäksi, kun myös $b>0$,

-   $\log_a x = \frac{\log_b x}{\log_b a} \quad$ (vastaavalla tavalla,
    HT).
    -   Tämän avulla esim. laskimesta
        $\log_2 5 = \frac{\log_{10} 5}{\log_{10} 2} = 2{,}3219\ldots$
    -   Huom: joissakin laskimissa käytetään $10$-kantaisesta
        logaritmista merkintää $\operatorname{lg}$ tai jopa $\log$;
        tällä kurssilla $\log$ on muussa käytössä.

#### Monotonisuus

Kuten eksponenttifunktio, myös logaritmi on aidosti monotoninen:

-   Jos $\,a>1$, niin $\, \log_a x\,$ on aidosti kasvava
-   Jos $\,0<a<1$, niin $\, \log_a x$ on aidosti vähenevä
    -   piirrä samaan kuvaan $y=2^x$ ja $y=\log_{2} x$ (tai vaikkapa
        $y=10^x$ ja $y=\log_{10} x$)

#### Raja-arvot

Kun $a>1$, $$
  \lim_{x \to \infty} \log_a x = \infty \quad \text{ ja }  \quad
  \lim_{x \to 0+} \log_a x = -\infty .
$$

Kun $0<a<1$, $$
  \lim_{x \to \infty} \log_a x = -\infty \quad \text{ ja }  \quad
  \lim_{x \to 0+} \log_a x = \infty .
$$

-   Siis kuvaajalla $\,y=\log_a x\,$ on pystysuora asymptootti $x=0$.

#### Jatkuvuus ja derivoituvuus

Kun $a>0$ ja $a\not = 1$, logaritmifunktio $\log_a x$ on koko
määrittelyjoukossaan $]0, \infty[$ jatkuva ja derivoituva funktio
(derivointi myöhemmin).

## Neperin luku ja eksponenttifunktio

### Neperin luku $e$

-   Määritelmiä useita, esim. $$
      \lim_{n \to \infty} (1+\tfrac{1}{n})^n
    $$
    -   Ei todisteta suppenemista tällä kurssilla.
-   Likiarvo $$
      e = 2{,}718281828459 \ldots
    $$
-   Käyttö *kantalukuna*: $$
      e^x = \text{ (luonnollinen) eksponenttifunktio}
    $$

### Eksponenttifunktio $e^x$

-   Tärkein eksponenttifunktio (ja eniten käytetty)
-   Kaikki edellä mainitut eksponenttifunktion ominaisuudet (kantaluku
    $e>1$)
-   Lisäksi $$
      D e^x = e^x
    $$
    -   Siksi tärkeä! Ainoa funktio, joka on itse itsensä derivaatta!
        -   Vakiokerrointa vaille ainoa... (eli toki esim. $D3e^x=3e^x$)

## Luonnollinen logaritmi

-   Eksponenttifunktion $e^x$ käänteisfunktio on $e$-kantainen
    logaritmi, jota kutsutaan myös *luonnolliseksi logaritmiksi* ja
    merkitään $$
      \log_e x = \operatorname{ln} x = \log x
    $$

**Huom:**

-   merkintä $\log x$ on siis varattu luonnolliselle logaritmille (tällä
    kurssilla ja usein matemaattisissa teksteissä).
    -   joissakin laskimissa samaa merkintä tarkoittaa jotakin muuta,
        usein 10-kantaista logaritmia

Kertauksena aiemmasta (nyt siis kantaluku $a = e$): $$
  y = e^x \iff x = \log y \quad \quad (y>0)
$$ sekä $$
  \log(e^x) = x \quad \text{ kaikilla } x \in \mathbb{R} \quad \quad
  \text{ ja } \quad \quad
  e^{\log x} = x \quad \text{ kaikilla } x > 0.
$$ Raja-arvot, laskusäännöt ym. ominaisuudet kuten edellä kantaluvulle
$a>1$.

## Eksponenttifunktioiden derivointi

-   $e$-kantaiselle siis $$
      D e^x = e^x
    $$
-   muille eksponenttifunktioille ($a>0$) $$
      D a^x = D (e^{\log a})^x = De^{x \log a} = e^{x \log a} \log a = a^x \log a
    $$

## Logaritmifunktioiden derivointi

-   Luonnolliselle logaritmille on $$
      D\log x = \frac{1}{x}
    $$ (Voidaan perustella esim. käänteisfunktion derivointikaavan
    avulla, kun tiedetään, että $D e^x=e^x$.)
-   Muille logaritmeille saadaan $$
      D\log_a x= D\left(\frac{\log x}{\log a} \right) = \frac{1}{\log a} D \log x =
      \frac{1}{x \log a}
    $$

Ohjeita:

-   Vain luonnollisen logaritmin ja eksponenttifunktion
    derivointisäännöt kannattaa muistaa, muut voi aina johtaa kuten
    yllä.
-   Harjoittele logaritmien laskusääntöjen käyttöä - ja mieti, miten
    voit johtaa/varmistaa ne tutuista potenssien laskusäännöistä.

## Eksponentiaalinen/logaritminen kasvu

-   $e^x$ kasvaa nopeasti, $\log x$ kasvaa hitaasti, kun $x \to \infty$
-   potenssifunktiot "siltä väliltä"

Tarkemmin:

kun $a>0$,

-   $$
      \lim_{x\to \infty} \frac{x^a}{e^x} = 0 \quad \text{ ja} \quad \lim_{x \to -\infty} |x|^a e^x = 0
    $$
    -   eli "eksponenttifunktio voittaa potenssifunktion";
-   $$
      \lim_{x\to \infty} \frac{\log x}{x^a} = 0 \quad \text{ ja} \quad \lim_{x \to 0+} x^a \log x = 0
    $$
    -   eli "potenssi voittaa logaritmin"

Eksponentiaalisen kasvun/vähenemisen malleja (eli $y = C e^{kx}$, $C$ ja
$k$ vakioita) käytetään paljon niin luonnontieteessä kuin
taloustieteessä ja teknisillä aloilla, esim.

-   radioaktiivinen hajoaminen
-   rajoittamaton populaation kasvu
-   jatkuva korko

-   Idea: "kasvu/väheneminen on verrannollinen kokoon"
    -   mitä enemmän jo on, sitä nopeammin määrä kasvaa (tai vähenee)
-   Differentiaaliyhtälönä:
    -   $y'=ky \quad$ ($k$ vakio)

### Jatkuva korko

Esimerkki:

-   Jos tilille maksetaan 5% vuotuista korkoa ja korko lisätään pääomaan
    vuosittain, kolmen vuoden jälkeen tilillä on $$
      (1+\tfrac{5}{100})^3 K= 1{,}157625 K ,
    $$ missä $K$ on alkupääoma.
-   Jos sama 5% vuotuinen korko lisätäänkin pääomaan kuukausittain (eli
    $\tfrac{5}{12}$% joka kuukausi), saadaan "korkoa korolle" ja kolmen
    vuoden jälkeen tilillä on $$
      (1+\tfrac{5}{12\cdot100})^{3\cdot12} K \approx 1{,}161472 K .
    $$
-   Teoreettinen jatkuvan koron malli: jos korko lisätään pääomaan
    "jatkuvasti", yhden vuoden jälkeen tilillä on $$
      e^{0{,}05}K
    $$ ja kolmen vuoden jälkeen siis $$
      (e^{0{,}05})^3 K = e^{0{,}15} K \approx 1{,}161834 K .
    $$

Yllä käytetään siis mallia, jossa $\, x\,$ on talletusaika (vuosina, ei
välttämättä kokonaisluku) ja $y=Ke^{0,05 x}$ on talletuksen määrä ajan
$\, x\,$ kuluttua, kun $K$ on talletuksen määrä hetkellä $x=0$.

# Arkusfunktiot

-   trigonometristen funktioiden käänteisfunktiot ja niiden derivointi

\[A, 3.5\]

### Lämmittelyksi

-   Kertaa trigonometriset funktiot (vaikkapa kurssilta [Calculus
    1](https://tim.jyu.fi/view/kurssit/matematiikka/Calculus/Calculus1#trigFunktiot)),
    ainakin
    -   määrittely yksikköympyrältä,
    -   kuvaajat ja
    -   derivointi.
    -   Tarvitsemme tänään myös
        -   muunnoskaavoja sekä
        -   muistikolmioita, joiden avulla saadaan joitakin tarkkoja
            arvoja (mitkä?).
-   Piirrä sinin, kosinin ja tangentin kuvaajat.
    -   Onko sini injektio? Entä kosini? Entä tangentti?
    -   Voidaanko määrittelyjoukkoa rajoittaa niin, että saadaan
        injektio? Mille välille rajoitutaan?
        -   sini
        -   kosini
        -   tangentti

## Sinin käänteisfunktio

-   Välillä $\,[-\frac{\pi}{2}, \frac{\pi}{2}]\,$ sini on aidosti
    kasvava ja siis injektio
    -   (tämä nähdään siitä, että derivaatta $\cos x > 0$ kaikilla
        $\, x \in ]-\frac{\pi}{2}, \frac{\pi}{2}[\,$ ja sini on jatkuva
        välin päätepisteisiin saakka).
-   Siten sillä on käänteisfunktio, jolle annamme nimen *arkussini*,
    merk. $arcsin$:

### Määritelmä

$$
  y = \arcsin x \iff x = \sin y , \quad \text{ kun } -1 \leq x \leq 1
  \text{ ja } 
  -\tfrac{\pi}{2} \leq y \leq  \tfrac{\pi}{2} 
$$

-   Huomaa, että sini saa välillä $\,[-\frac{\pi}{2}, \frac{\pi}{2}]\,$
    kaikki arvot väliltä $\,[-1, 1]\,$ eikä muita. Siis arkussinin
    -   määrittelyjoukko on $\,[-1, 1]\,$ ja
    -   arvojoukko on $\,[-\frac{\pi}{2}, \frac{\pi}{2}]\,$.
-   Sanallisesti $$
      \arcsin x = "\text{se kulma } \alpha, \text{ jolle } \sin \alpha = x"
    $$ ja kulma $\alpha$ on väliltä
    $\,[-\frac{\pi}{2}, \frac{\pi}{2}]\,$
    -   (sinin jaksollisuuden takia ratkaisuja löytyisi muitakin).
-   kulma = kaaren pituus, lat. "arcus" = kaari

-   Kumoutuminen: $$
      \arcsin(\sin x)) = x, \quad \text{ kun } -\tfrac{\pi}{2} \leq x \leq \tfrac{\pi}{2}
    $$ ja $$
      \sin (\arcsin x) = x, \quad \text{ kun } -1 \leq x \leq 1 .
    $$

**Esimerkkejä**

1.  $\,\arcsin(\frac{1}{2}) = \frac{\pi}{6}$, koska
    $\sin(\frac{\pi}{6})=\frac12$ ja
    $\frac{\pi}{6} \in [-\frac{\pi}{2}, \frac{\pi}{2}]$

2.  $\,\arcsin(-\frac{1}{2}) = -\frac{\pi}{6}$, koska
    $\sin(-\frac{\pi}{6})=\frac12$ ja
    $-\frac{\pi}{6} \in [-\frac{\pi}{2}, \frac{\pi}{2}]$

3.  $\,\arcsin(-1) = -\frac{\pi}{2}$, koska $\sin(-\frac{\pi}{2})=1$ ja
    $-\frac{\pi}{2} \in [-\frac{\pi}{2}, \frac{\pi}{2}]$
    -   (esim. $\frac{3\pi}{2}$ ei käy, vaikka
        $\sin(\frac{3\pi}{2})=-1$, koska se ei ole välillä
        $[-\frac{\pi}{2}, \frac{\pi}{2}]$ )
4.  $\,\arcsin(\pi)\,$ ei ole määritelty! ($\pi \not \in [-1,1]$)

5.  Sievennä
    -   $\, \arcsin(\sin 0{,}2)$
        -   koska $0{,}2 \in [-\frac{\pi}{2}, \frac{\pi}{2}]$, on
            $\, \arcsin(\sin 0{,}2)=0{,}2$
    -   $\, \arcsin(\sin \frac{33\pi}{40})$
        -   koska
            $\frac{33\pi}{40} \not \in [-\frac{\pi}{2}, \frac{\pi}{2}]$,
            ei saada suoraan $\frac{33\pi}{40}$
        -   muunnoskaavat: $\sin(x)=\sin(\pi -x)$, siis
            $\sin(\frac{33\pi}{40})=\sin(\frac{7\pi}{40})$ ja siten $$
              \arcsin(\sin \tfrac{33\pi}{40}) = \arcsin(\sin \tfrac{7\pi}{40}) = \tfrac{7\pi}{40},
            $$ koska
            $\frac{7\pi}{40} \in [-\frac{\pi}{2}, \frac{\pi}{2}]$
6.  Sievennä $\cos(\arcsin 0{,}6)$
    -   ei tunneta tarkkaa arvoa $\arcsin 0{,}6$; miten saadaan tarkka
        arvo $\cos(\arcsin 0{,}6)$? Onko mahdollista?
    -   Piirrä suorakulmainen kolmio, jossa $"\arcsin 0{,}6 \, "$ näkyy:
        hypotenuusaksi $1$, toiseksi kateetiksi $0{,}6$
        -   mikä on toinen kateetti?
        -   missä on kulma $\arcsin 0{,}6$ ?
    -   Lue kolmiosta tämän kulman kosini; saat
        $\cos(\arcsin 0{,}6) = 0{,}8$.
7.  Sievennä $\tan(\arcsin x)$, kun $0<x<1$
    -   Kuten edellä: piirrä kolmio, laske toinen kateetti ja lue
        tangentti.
        -   Onko sama tulos voimassa kaikilla $x \in [-1,1]$?
            -   (ei päätepisteissä, muualla kyllä)

### Derivointi

Arkussinin derivaatta on $$
  D \arcsin x = \frac{1}{\sqrt{1-x^2}}, \quad \text{ kun } x \in ]-1, 1[ .
$$

-   Perustelu implisiittisen derivoinnin (ks. Calculus 1) tai
    käänteisfunktion derivointikaavan avulla.

### Kumoutumisesta vielä

Sini on määritelty kaikilla $\, x \in \mathbb{R}\,$ ja sen arvo
$\,\sin x \in [-1,1]\,$ aina.

Siispä funktio $\arcsin(\sin x)$ on määritelty kaikilla $\,x$. Piirrä
sen kuvaaja.

-   Huomaa, että arkussinin arvot ovat aina välillä
    $[-\frac{\pi}{2}, \frac{\pi}{2}]$.
    -   Tulos ei siis voi olla $x$ kaikkialla.
    -   Mitä tapahtuu välin $[-\frac{\pi}{2}, \frac{\pi}{2}]$
        ulkopuolella?
    -   Voit käyttää derivaattaa apuna. Missä derivoituva? Onko jatkuva?

\[A, s. 193-194\]

## Kosinin käänteisfunktio

-   Kosini on "siirretty sini", $\cos x = \sin(x+\frac{\pi}{2})$;
    käänteisfunktio *arkuskosini* toimii likimain samalla tavalla kuin
    arkussini.
-   Erot:
    -   Arkuskosinilla ja arkussinillä on eri arvojoukot
        -   Missä kosini on injektio? Valitaan näistä väli $\,[0,\pi]\,$
            (sopimus).
    -   Arkussini on aidosti kasvava (tarkista itse), arkuskosini on
        aidosti vähenevä.

### Määritelmä

$$
  y = \arccos x \iff x = \cos y, \quad \text{ kun } \, 0 \leq x \leq \pi \, \text{ ja } \, -1 \leq y \leq 1
$$

-   $\arccos x =$ "se kulma/kaarenpituus $\alpha$, jolle
    $\cos \alpha = x$ ja $\alpha \in [0,\pi]$".

### Derivointi

Arkuskosinin derivaatta on $$
  D \arccos x = -\frac{1}{\sqrt{1-x^2}}, \quad \text{ kun } x \in ]-1, 1[
$$

-   Perustelu implisiittisen derivoinnin (ks. Calculus 1) tai
    käänteisfunktion derivointikaavan avulla kuten arkussinille.

-   Huomaa, että $D \arccos x = - D \arcsin x$. Miksi?
    -   Mieti kuvaajien avulla ensin.
    -   Sitten seuraava havainto auttaa:

### Havainto

Kun $-1 \leq x \leq 1$, on $$
  \arccos x = \frac{\pi}{2} - \arcsin x .
$$

-   Perustelu muunnoskaavan $\cos x = \sin (\frac{\pi}{2} - x)$ avulla.

## Tangentin käänteisfunktio

Tangenttifunktio $\tan x = \frac{\sin x}{\cos x}$

-   ei ole määritelty kaikilla $x \in \mathbb{R}$
    -   kosinin nollakohdat pois eli $x \not = \frac{\pi}{2} + n\pi$,
        $n \in \mathbb{Z}$
-   on aidosti kasvava välillä $]-\frac{\pi}{2}, \frac{\pi}{2}[$
    -   (tarkista derivaatan avulla)
-   saa välillä $]-\frac{\pi}{2}, \frac{\pi}{2}[$ kaikki reaalilukuarvot
    eli arvojoukko on $]-\infty, \infty[$
-   on välille $]-\frac{\pi}{2}, \frac{\pi}{2}[$ rajoitettuna injektio,
    joten sillä on tällä välillä käänteisfunktio, jolle annamme nimen
    *arkustangentti*, merk. $\arctan$:

### Määritelmä

$$
  \arctan x = y \iff x = \tan y, \quad \text{ kun } \, -\tfrac{\pi}{2} < y < \tfrac{\pi}{2}
$$

-   Arkustangentin
    -   määrittelyjoukko on koko $\mathbb R$
    -   arvojoukko on $]-\frac{\pi}{2}, \frac{\pi}{2}[$
    -   epäoleelliset raja-arvot ovat $$
          \lim_{x \to \infty} \arctan x = \frac{\pi}{2} \quad \quad \text{ja} \quad \quad
          \lim_{x \to -\infty} \arctan x = -\frac{\pi}{2} ;
        $$ vrt. $$
          \lim_{x \to \frac{\pi}{2}} \tan x = \infty
          \quad \quad \text{ja} \quad \quad
          \lim_{x \to -\frac{\pi}{2}} \tan x = -\infty .
        $$
-   Piirrä kuvaajat $y = \tan x$, kun
    $x \in ]-\frac{\pi}{2}, \frac{\pi}{2}[$ ja $y = \arctan x$.

-   Kumoutumiset: $$
      \arctan(\tan x) = x \quad \text{ kun } -\tfrac{\pi}{2} < x < \tfrac{\pi}{2}
    $$ ja $$
      \tan(\arctan x) = x \quad \text{ kaikilla } x \in \mathbb{R} .
    $$

**Esimerkkejä**

1.  $\arctan (-1) = -\frac{\pi}{4}$, koska $\tan(-\frac{\pi}{4}) = -1$
    ja $-\frac{\pi}{4} \in ]-\frac{\pi}{2},\frac{\pi}{2}[$

2.  $\tan(\arctan 3) = 3$

3.  $\arctan(\tan(\frac{3\pi}{4})) = \arctan(-1) = -\frac{\pi}{4}$

4.  $\cos(\arctan 2 ) = ?$
    -   Kuten aiemmin, piirrä kolmio, jossa näet $"\arctan 2 \,"$
        -   (eli suorakulmainen kolmio, jossa kateetit ovat $2$ ja $1$)
    -   ja laske hypotenuusa ja lue kosini

### Derivointi

Arkustangentin derivaatta on $$
  D \arctan x = \frac{1}{1+x^2} 
$$

-   Perustelu implisiittisen derivoinnin (ks. Calculus 1) tai
    käänteisfunktion derivointikaavan avulla kuten arkussinille.

**Kumoutumisesta vielä:**

-   Millaisen kuvaajan saat funktiolle $\arctan(\tan x)$ ? Onko jatkuva,
    onko derivoituva (missä)?

# Hyperboliset ja areahyperboliset funktiot

-   määritelmät, muunnos- ja summakaavat, derivointi

\[A, 3.6\]

### Lämmittelytehtävä:

Tarkastellaan funktioita

-   $\,f(x)=e^x\,$ ja
-   $\,g(x)=e^{-x}\,$.

1.  Piirrä samaan kuvaan kuvaajat $y=f(x)$ ja $y=g(x)$.

2.  Millaisen kuvaajan saisit näiden funktioiden summalle ja
    erotukselle? Hahmottele kuvan avulla
    -   $\,y=e^x+e^{-x}\,$ ja
    -   $\,y=e^x-e^{-x}\,$.
3.  Laske $$
      [f(x)+g(x)] + [f(x)-g(x)] .
    $$

## Hyperbelisini ja hyperbelikosini

### Määritelmät

-   *Hyperbelisini* eli hyperbolinen sini, merk. $\sinh$, on funktio $$
      \sinh x = \frac{e^x - e^{-x}}{2} ,
    $$
    -   määrittelyjoukko on koko $\, \mathbb{R}$.
-   *Hyperbelikosini* eli hyperbolinen kosini, merk. $\cosh$, on funktio
    $$
      \cosh x = \frac{e^x + e^{-x}}{2} ,
    $$
    -   määrittelyjoukko on koko $\, \mathbb{R}$.

### Ominaisuuksia

-   Hyperbolinen sini ja hyperbolinen kosini molemmat ovat
    -   jatkuvia ja
    -   derivoituvia funktioita.
        -   (Mistä tiedät nämä?)
-   Hyperbolinen kosini $\,\cosh x = \frac{e^x+ e^{-x}}{2}\,$ on
    -   parillinen funktio eli
        $$\cosh (-x) = \cosh x \quad \text{kaikilla } x \in \mathbb{R}.$$
-   Hyperbolinen sini $\,\sinh x = \frac{e^x - e^{-x}}{2}\,$ on
    -   pariton funktio eli
        $$\sinh (-x) = -\sinh x \quad \text{kaikilla } x \in \mathbb{R}.$$
        -   (Laske itse!)
-   Hyperbolista kosinia kutsutaan myös nimellä *ketjukäyrä*, koska
    päistään kiinnitetty vapaasti roikkuva ketju (tai naru) asettuu
    tähän muotoon.
    -   +parametreja...

#### Muunnoskaavoja ja yhteys hyperbeliin

-   Huomaa, että $$
      e^x = \sinh x + \cosh x \quad \text{ kaikilla } x \in \mathbb{R}.
    $$
    -   Laske!
        -   \[A propos, jokainen reaalifunktio voidaan kirjoittaa
            parillisen ja parittoman funktion summana - osaisitko
            todistaa? \[A, P.5, harj. 35\] Jopa yksikäsitteisesti (eli
            vain yhdellä tavalla). HT\*\*\]
-   Lisäksi $$
      \cosh^2 x - \sinh^2 x = 1 \quad \text{ kaikilla } x \in \mathbb{R}.
    $$
    -   Laske!
    -   Nimitys "hyperbolinen" tulee tästä:
        -   kaikilla $t \in \mathbb{R}$ tason piste $(\cosh t, \sinh t)$
            on hyperbelillä $x^2-y^2=1$
            -   (vrt. $(\cos t , \sin t)$ on ympyrällä $x^2+y^2 = 1$)
            -   luvulle $t$ ei ole tulkintaa kaarenpituutena, mutta
                $\frac{t}{2}$ on hyperbelisektorin ala \[A, s. 200\]
-   Huomaa myös (laske!), että $$
      \sinh 0 = 0 \quad \text{ ja } \quad \cosh 0 = 1 .
    $$

-   (Huom. sini ja kosini *määriteltiin* yksikköympyrän avulla;
    hyperbolisille funktioille meillä on määritelmät eksponenttifunktion
    lausekkeina.)

#### Summakaavat

$$
  \sinh(x+y) = \sinh x \cosh y + \cosh x \sinh y
$$ ja $$
  \cosh(x+y) = \cosh x \cosh y + \sinh x \sinh y
$$

-   Laske itse (HT)
-   vrt. trigonometristen funktioiden summakaavat
-   näistä esim. $$
      \sinh(x-y) = \sinh x \cosh y - \cosh x \sinh y
    $$ ja $$
      \sinh(2x) = 2 \sinh x \sinh y
    $$

#### Arvojoukot

Mitä arvoja hyperbelisini ja hyperbelikosini saavat?

Tämä nähdään epäoleellisten raja-arvojen avulla:

-   hyperbelisini: $$
      \lim_{x \to \infty} \sinh x = \infty 
      \quad \text{ ja } \quad
      \lim_{x \to -\infty} \sinh x = -\infty 
    $$ ja funktio on jatkuva, joten hyperbolisen sinin arvojoukko on
    $]-\infty, \infty[$.
-   hyperbelikosini: $$
      \lim_{x \to \infty} \cosh x = \infty 
      \quad \text{ ja } \quad
      \lim_{x \to -\infty} \cosh x = \infty ,
    $$ funktio on jatkuva + derivoituva, derivaatta
    $D \cosh x = 0 \iff x=0$ ja $\cosh 0 = 1$, joten hyperbolisen
    kosinin arvojoukko on $[1, \infty[$. (Voit tehdä kulkukaavion
    vahvistamaan uskoa.)
    -   Totea yllämainitut raja-arvot itse, esim.
        $$\lim_{x \to \infty} \sinh x = \lim_{x \to \infty}\frac{e^x-e^{-x}}{2} = \infty,$$
        koska $e^{-x} \overset{x \to \infty}{\longrightarrow} 0$.

### Derivointi

-   Hyperbolinen sini ja hyperbolinen kosini ovat kumpikin kaikkialla
    derivoituvia funktioita, koska eksponenttifunktio on kaikkialla
    derivoituva. Derivaatat ovat $$
      D \sinh x = \cosh x
    $$ ja $$
      D \cosh x = \sinh x
    $$
    -   Laske itse!
    -   Huomataan samalla, että koska $D \sinh x = \cosh x > 0$ kaikilla
        $x\in \mathbb{R}$,
        -   hyperbolinen sini on aidosti kasvava koko joukossa
            $\mathbb{R}$.

## Hyperbolinen tangentti

-   Määritelmä: $$
      \tanh x = \frac{\sinh x}{\cosh x}
    $$

-   Määrittelyjoukko on koko $\mathbb{R}$
    -   koska nimittäjä $\cosh x$ ei ole nolla missään (ja $\sinh$,
        $\cosh$ on määritelty kaikkialla).
-   Derivointi: $$
      D \tanh x = D \left(\frac{\sinh x}{\cosh x} \right)
      = \frac{\sinh ^2 x - \cosh ^2 x}{\cosh ^2 x}
      = \frac{1}{\cosh ^2 x}
    $$
    -   Huomataan, että $D \tanh x > 0$ kaikilla $x \in \mathbb{R}$,
        joten hyperbelitangentti on aidosti kasvava funktio.
-   Arvojoukko? $$
      \lim_{x\to \infty} \tanh x = 
      \lim_{x\to \infty} \frac{e^x-e^{-x}}{e^x+e^{-x}} = 
      \lim_{x\to \infty} \frac{1-e^{-2x}}{1+e^{-2x}} = 1
    $$ ja $$
      \lim_{x\to -\infty} \tanh x = 
      \lim_{x\to -\infty} \frac{e^x-e^{-x}}{e^x+e^{-x}} = 
      \lim_{x\to -\infty} \frac{e^{2x}-1}{e^{2x}+1} = -1 ,
    $$ ja $\tanh$ on jatkuva+derivoituva, derivaatta &gt;0 kaikkialla
    eli $\tanh$ on aidosti kasvava; siis
    -   arvojoukko on $]-1,1[$.

## Hyperbolisten funktioiden käänteisfunktiot

Koska hyperbolinen sini ja tangentti ovat aidosti kasvavia (ja siis
injektioita) koko joukossa $\mathbb{R}$, niillä on käänteisfunktiot,
joita kutsutaan nimellä *areahyperbelifunktiot*.

Hyperbolinen kosini ei ole injektio koko määrittelyjoukossaan, mutta
kylläkin välillä $[0, \infty[$. Rajoittumalla tälle välille saadaan
käänteisfunktio (areahyperbelikosini).

Kuten aiemmin, käänteisfunktion määrittelyjoukko on alkuperäisen
funktion arvojoukko.

### Määritelmät

-   Areahyperbelisini: $$
      y= \operatorname{arsinh} x \iff x = \sinh y
    $$
    -   määrittelyjoukko on koko $\mathbb{R}$.
-   Areahyperbelitangentti: $$
      y= \operatorname{artanh} x \iff x = \tanh y
    $$
    -   määrittelyjoukko on $]-1,1[$.
-   Areahyperbelikosini: $$
      y= \operatorname{arcosh} x \iff x = \cosh y
    $$
    -   määrittelyjoukko on $[1, \infty[$.

### Lausekkeet

-   Areahyperbelisini: $$
      \operatorname{arsinh} x = \log\left(x + \sqrt{x^2+1}\right) .
    $$
-   Areahyperbelikosini: $$
      \operatorname{arcosh} x = \log\left(x + \sqrt{x^2-1}\right), \quad x \geq 1.
    $$
-   Areahyperbelitangentti: $$
      \operatorname{artanh} x = \frac12 \log\left(\frac{1+x}{1-x}\right), \quad -1 < x < 1.
    $$
    -   Laske! (HT\*)

### Derivointi

-   Areahyperbelisini: $$
      D \operatorname{arsinh} x = \frac{1}{\sqrt{x^2+1}} \quad (\text{kaikilla } x \in \mathbb{R}).
    $$
-   Areahyperbelikosini: $$
      D \operatorname{arcosh} x = \frac{1}{\sqrt{x^2+1}}, \quad x > 1.
    $$
-   Areahyperbelitangentti: $$
      D \operatorname{artanh} x = \frac{1}{1-x^2}, \quad -1 < x < 1.
    $$
    -   Laske! (HT\*)

### Merkinnöistä vielä

-   Areahyperbelifunktioille käytetään joskus myös muita merkintöjä,
    esim.
    -   areahyperbelisinille merkinnän $\operatorname{arsinh} x$ sijasta
        $$
          \sinh^{-1} x \quad \text{ tai } \quad \operatorname{arcsinh} x 
          \quad \text{ tai } \quad 
          \operatorname{argsinh} x 
        $$ tai jopa $$
          \operatorname{asinh} x
        $$ (laskentaohjelmissa).

# Sovelluksia ja muuta jännää

-   lisää eksponentiaalisesta kasvusta, sovelluksia eri aloilta
-   hiukan DY:itä
-   toisistaan riippuvien suureiden muutosnopeudesta

\[A, 3.4, (3.7,) 4.1\] (valikoiden)

## Eksponentiaalinen kasvu

-   Ks. myös aiheen käsittely [aiemmin](#eksponentiaalinenKasvu).

-   "Suureen $y$ muutosnopeus on verrannollinen suureen $y$ kokoon
    (kullakin hetkellä)" $$
      y'=k y \quad \quad (k \text{ vakio })
    $$

-   Usein muuttujana käytetään symbolia $\,t\,$ ("aika"), siis
    $\, y=y(t), \, y'=y'(t) = \frac{d y}{d t}$.

-   Ratkaisu: $$
      y' = k y \iff y = C e^{kt} 
    $$ ($C \in \mathbb{R} \,$ voi olla mikä tahansa vakio)
    -   tarkista itse, että $y = C e^{kt}$ on yhtälön $y'=ky$ ratkaisu,
        olipa $C$ mikä tahansa vakio.

### Alkuarvotehtävä

$$
  \begin{cases} y' = ky \\ y(0) = y_0 \end{cases}
$$

-   $y(0) = y_0$ on *alkuehto*, $y_0 \in \mathbb{R}$

-   Ratkaisu: $$
      \begin{cases} y' = ky \\ y(0) = y_0 \end{cases}
      \iff y = y_0 e^{kt}
    $$
    -   tarkista itse laskemalla.
-   Huomaa vakion $k$ vaikutus: jos $y_0 > 0$,
    -   $k > 0$ "eksponentiaalinen kasvu"
    -   $k < 0$ "eksponentiaalinen väheneminen" ( / "hajoaminen",
        "sammuminen", ...)

**Esimerkki**

Eräs soluviljemä kasvaa nopeudella joka on suoraan verrannollinen
viljelmän kokoon. Jos soluja on alunperin 500 ja 24 tunnin kuluttua 800,
kuinka monta solua viljelmässä on seuraavien 12 tunnin kuluttua (eli 36
h alkuhetkestä)?

-   $y(t) =$ solujen määrä $t$ tunnin kuluttua (alusta eli hetkestä,
    jolloin soluja oli 500)
    -   siis $y(0) = 500$ ja $y(24) = 800$
    -   $y(36) = ?$
-   Malli: kasvunopeus on verrannollinen kokoon eli $$
      y'(t) = k y(t)
    $$ eli $$
      y(t) = C e^{kt}, 
    $$ alkuehdosta $y(0) = 500$ saadaan $C=500$; siis $$
      y(t) = 500 e^{kt} .
    $$

-   Ratkaistaan $k$ toisen annetun tiedon avulla: $y(24) = 800$, joten
    $$
      800 = y(24) = 500 e^{24k} \iff e^{24k} = \frac{800}{500}= 1{,}6
    $$ eli $$
      k = \frac{\log 1{,}6}{24}
    $$ ja siis $$
      y(t) = 500 e^{\frac{t}{24} \log 1{,}6} = 500 \cdot 1{,}6^{\frac{t}{24}}.
    $$

-   Tästä saadaan kysytty tieto $$
      y(36) = 500 \cdot 1{,}6^{\frac32} \approx 1011,9289 \ldots \approx 1012
    $$

-   Vastaus: Kun aikaa on kulunut vielä 12 tuntia, soluja on noin 1012
    kpl.

### Kaksinkertaistumis- / puoliintumisaika

-   Eksponentiaalinen kasvu $\iff$ kiinteä "tuplaantumisaika":

Jos $\,y(t) = y_0 e^{kt}$ ja $y(T)= 2y(0)=2y_0\,$ eli alkutilanteesta
$y(0) = y_0$ suure $y$ kasvaa kaksinkertaiseksi ajan $T$ kuluessa, niin
$$
  y_0 e^{kT} = 2 y_0
$$ eli $$
  e^{kT} = 2
$$ ja $$
  y(t+T) = y_0 e^{k(t+T)} = y_0 e^{kt+kT} = y_0 e^{kt}e^{kT} = 2y(t)
$$ eli milloin tahansa (millä hetkellä $t$ tahansa) $y$ kaksinkertaistuu
saman ajan $T$ kuluessa.

-   Vastaavasti jos $k<0$, puoliintumisaika on kiinteä: $$
      y(T) = \frac12 y(0) \implies e^{kT} = \frac12
    $$ ja $$
      y(t+T) = y_0 e^{kt}e^{kT} = \frac12 y(t) .
    $$

**Esimerkki**

Erään radioaktiivisen aineen puoliintumisaika on 1200 vuotta.

a)  Kuinka suuri osa radioaktiivista ainetta on jäljellä 10 vuoden
    kuluttua?
b)  Kuinka pitkän ajan kuluessa radioaktiivisuus vähenee 10 prosentilla?

-   Tiedot:
    -   $p(t) =$ kuinka monta prosenttia radioaktiivista ainetta on
        jäljellä $t$ vuoden kuluttua
    -   alkuehto: $p(0) = 100$
    -   puoliintumisaika 1200 vuotta $\implies p(1200) = 50$
-   Malli: radioaktiivinen aine vähenee suhteessa määrään ($\iff$
    puoliintumisaika on vakio), joten mallina on tässäkin
    eksponentiaalisen kasvun/vähenemisen malli, $$
      p'(t) = k p(t) \iff p(t) = C e^{kt}
    $$ ja alkuehdosta saadaan $C = 100$; siis $$
      p(t) = 100 e^{kt}.
    $$
-   Puoliintumisajan avulla ratkaistaan $k$: $$
      50 = p(1200) = 100 e^{1200k} \iff e^{1200k} = \tfrac12 \iff 1200k = \log \tfrac12 = -\log 2
    $$ eli $$
      k= -\frac{\log 2}{1200}
    $$
    -   huomaa, että $k<0$, radioaktiivisuus vähenee ajan kuluessa.

1.  Siis 10 vuoden jälkeen $$
      p(10) = 100 e^{-\frac{\log 2}{1200} \cdot 10} = 100 \cdot \left( \tfrac12 \right)^{\frac1{120}}
      \approx 99{,}42404 > 99{,}4
    $$
    -   10 vuoden jälkeen radioaktiivista ainetta on jäljellä vielä yli
        99{,}4 %.
2.  Ratkaistaan $t$ siten, että $p(t)=90$: $$
      p(t) = 90 \iff 100e^{kt} = 90 \iff e^{kt} = \tfrac9{10} \iff kt = \log 0{,}9 
    $$ eli $$
      t= \tfrac1k \log 0{,}9 = -\frac{1200}{\log 2} \log 0{,}9 
      = \frac{1200}{\log 2} \log \tfrac{10}9 \approx 182,404
    $$
    -   Radioaktiivinen aine on vähentynyt 10 % noin 182 vuoden
        kuluttua.

## Muunnelmia

### Newtonin jäähtymislaki (esim.)

"Muutosnopeus on suoraan verrannollinen erotukseen (jostain kiinteästä
arvosta $a$)"

$$
  y' = k(y-a)
$$

-   Ratkaiseminen:
    -   merkitään $u(t) = y(t) -a$, jolloin $u'(t) = y'(t)$ ja $$
          y' = k(y-a) \iff u' = k u
        $$
    -   ratkaistaan kuten aiemmin
    -   palataan takaisin funktioon $y= y(t) = u(t) +a$.

**Esimerkki**

-   Huoneen lämpötila on 20 astetta (ja pidetään vakiona). Jos
    kupillinen kahvia jäähtyy 80 asteesta 50 asteeseen viidessä
    minuutissa, kuinka kauan vielä pitää odottaa, että kahvi on 40
    asteista?
    -   $y(t) =$ kahvin lämpötila $t$ minuutin jälkeen
    -   $y(0) = 80$
    -   $y(5) = 50$
    -   $y'=k(y-20)$
    -   $u(t) = y(t) -20, u'=ku$
    -   Laske: $u(0) = 60, u(5) = 30$; ratkaise $k$ (saat
        $k = \frac15 \log \frac12$)
    -   Ratkaise $t$ yhtälöstä $y(t) = 40$ eli $u(t) = 20$
        -   vastaus: vielä noin $2{,}92$ minuuttia eli noin 2 min 55
            sek.

### Jatkuva korko

-   Jatkuvan koron malli (ks. myös käsittely [aiemmin](#jatkuvaKorko)):
    $$
      y = K e^{pt},
    $$
    -   $K$ on alkupääoma
    -   $p$ on nimellinen vuosikorko
    -   $t$ on talletusaika vuosina

### Logistinen kasvu

-   "Suureen $y$ eksponentiaalista kasvua rajoittaa järjestelmän
    kantokyky $L$" $$
      y' = ky\left( 1-\frac{y}{L} \right)
    $$
-   Ratkaisu on $$
      \begin{cases} y' = ky\left( 1-\frac{y}{L} \right) \\ y(0) = y_0 \end{cases}
      \iff
      y = \frac{L y_0}{y_0 + (L-y_0) e^{-kt}}
    $$
    -   totea itse, että annettu $y$ toteuttaa differentiaaliyhtälön ja
        alkuehdon (HT\*)

## Toisen kertaluvun differentiaaliyhtälöistä

**Esimerkki**

-   Toteuttaako $$y= Ae^x+Be^{2x}$$ differentiaaliyhtälön $$
      y''-3y'+2y = 0
    $$ olivatpa $A$ ja $B$ mitä tahansa vakioita?
    -   Laske derivaatat:
        -   $y' = Ae^{x}+2Be^{2x}$
        -   $y'' = Ae^x + 4Be^{2x}$
    -   Sijoita yhtälöön ja laske: $$
          y''-3y'+2y = Ae^x + 4Be^{2x} - 3(Ae^{x}+2Be^{2x}) + 2(Ae^x+Be^{2x})
        $$ $$
          = (A-3A+2A)e^x + (4B-6B+2B)e^{2x} = 0
        $$
    -   Johtopäätös: kyllä toteuttaa.

## Toisistaan riippuvat muutosnopeudet

-   engl. *related rates*

**Esimerkki**

Eräällä hetkellä suorakaiteen leveys on 10 cm ja pituus 8 cm. Tuolla
hetkellä leveys kasvaa nopeudella 2 cm/s ja pituus vähenee 3cm/s. Kuinka
nopeasti suorakaiteen pintala kasvaa/pienenee kyseisellä hetkellä?

-   Merkitään $x=10$ cm, $y = 8$ cm
    -   tällöin $\frac{dx}{dt} = 2$ cm/s ja $\frac{dy}{dt}=-3$ cm/s .
-   Suorakaiteen pinta-ala on $A=xy$ eli $A(t) = x(t) y(t)$.

-   Suorakaiteen pinta-ala muuttuu nopeudella $$
      \frac{dA}{dt} = \frac{dx}{dt} y + \frac{dy}{dt} x
    $$ eli $$
      \frac{dA(t)}{dt} = \frac{dx(t)}{dt} y(t) + \frac{dy(t)}{dt} x(t) .
    $$

-   Sijoitetaan tiedot: $$
      \frac{dA}{dt}\bigg|_{x=10, y=8} 
      = \left(\frac{dx}{dt} y + \frac{dy}{dt} x  \right)\bigg|_{x=10, y=8}
      = 2\cdot 8 + (-3) \cdot 10 = -14
    $$

-   Vastaus: Pinta-ala pienenee 14 cm$^2$/s.

### Ohjeita

1.  Mitä tunnetaan, mitä kysytään?
2.  Piirrä kuva, jos mahdollista.
3.  Merkitse puuttuvia suureita haluamillasi symboleilla.
4.  Mieti, millainen yhtälö sitoo suureet toisiinsa.
5.  Jos kaikki suureet ovat ajan funktioita, voit derivoida
    yhtälön/yhtälöt ajan suhteen.
6.  Sijoita arvot, jotka tunnet.
7.  Ratkaise tuntemattomat (arvot/derivaatat).
8.  Tulkitse saamasi tulos.
    -   Vastaako kysymykseen?
    -   Onko järkevä?

-   Lisää esimerkkejä \[A, 4.1\]

# Numeriikkaa

-   lisää yhtälöiden numeerisesta ratkaisemisesta; kiintopistelause,
    Newtonin menetelmä

\[A, 4.2\]

## Yhtälöiden ratkaisemisesta

-   Kuten [tuttua kurssilta Calculus
    1](https://tim.jyu.fi/view/kurssit/matematiikka/Calculus/Calculus1#yhtalonRatkaisujenEtsiminenJaLkm),
    yhtälön voi ratkaista
    -   *symbolisesti* eli *tarkasti* tai
    -   likimääräisesti joko
        -   *graafisesti* eli kuvan avulla tai
        -   *numeerisesti* eli laskemalla ratkaisu(i)lle likiarvoja.
-   Ratkaistava yhtälö kirjoitetaan usein muodossa $\,f(x) = 0\,$ tai
    $f(x) = g(x)$.

-   Minkä tyyppisille funktioille $\,f\,$ yhtälö $\,f(x) = 0\,$ osataan
    ratkaista symbolisesti?
    -   Yleisesti vain joitakin, kuten
        -   ensimmäisen asteen polynomiyhtälöt: $$
              ax+b = 0 \quad (\text{ja } a \not = 0) \quad \iff x = -\frac{b}{a}
            $$
        -   toisen asteen polynomiyhtälöt: $$
              ax^2 + bx + c = 0 \quad (\text{ja } a \not = 0) \quad 
              \iff x = \frac{-b \pm \sqrt{b^2-4ac}}{2a}
            $$
    -   Lisäksi osaamme ratkaista joitakin erikoistapauksia kuten
        -   $2\sin x - 1=0$
        -   $e^x=1$
            -   mutta emme osaa ratkaista symbolisesti vaikkapa yhtälöä
                $\,2\sin x = x$, emmekä varsinkaan osaa ratkaista
                yleisesti yhtälöä $\,ax+b\sin x = 0$.
    -   Myös kolmannen ja neljännen asteen polynomiyhtälöille on
        ratkaisukaavat, mutta ne ovat monimutkaisia; usein näitäkin
        ratkaistaan numeerisesti.
-   Numeerinen ratkaiseminen tehdään (nykyään) usein tietokoneen tai
    laskimen avulla.

-   Numeerisella menetelmällä löydetään likiarvo yhdelle ratkaisulle;
    -   ratkaisujen lukumäärä (onko yhtään? onko monta?) pitää selvittää
        muilla keinoin
        -   Calculus 1
    -   usein halutaan löytää useasta ratkaisusta jokin tietty (tietyltä
        väliltä oleva) ratkaisu.

## Yhtälöiden numeerisia ratkaisumenetelmiä

-   Yhtälön $$f(x) = 0$$ numeeriset ratkaisumenetelmät perustuvat
    funktion $f$ ominaisuuksiin.
    -   Mitä "mukavampi" $\,f$, sen "parempia" menetelmiä.
    -   Vähintään funktion $\,f\,$ on oltava jatkuva
        -   (jollakin välillä, jolta ratkaisuja etsitään).
-   Yleensä tarvitaan jonkinlainen karkea käsitys siitä, mistä päin
    ratkaisu voisi löytyä;
    -   väli, jolla ratkaisu on, tai
    -   "alkuarvaus" ratkaisusta.

### Haarukointi / puolitushaku

-   Bolzanon lause: "jos jatkuva funktio vaihtaa merkkiään, jossain
    tällä välillä se saa arvon nolla".
    -   Tarvitaan siis kaksi pistettä,
        -   joissa $f$ saa eri merkkiset arvot, ja
        -   joiden välissä $f$ on jatkuva.
    -   Menetelmä:
        -   Jos $f$ on jatkuva välillä $[a, b]$ ja saa eri merkkiset
            arvot pisteissä $a$ ja $b$, niin
            1.  Lasketaan $f(\frac{a+b}{2})$. Onko $>0$ vai $<0$?
                -   jos $f(\frac{a+b}{2})=0$, ratkaisu löytyi!
            2.  Valitaan joko väli $[a, \frac{a+b}{2}]$ tai
                $[\frac{a+b}{2},b]$ sen mukaan, kummalla välillä $f$
                vaihtaa merkkiään.
            3.  Jatketaan alusta.
-   Menetelmällä löydetään yhtälön $f(x) = 0$ ratkaisulle niin tarkka
    likiarvo kuin halutaan,
    -   mutta hitaasti (tarvitaan paljon laskentatyötä);
    -   olettaen vain funktion jatkuvuus (ja eri merkkiset arvot
        jossain).
-   Yllä kuvatusta menetelmästä käytetään myös nimitystä *puolitushaku*
    -   tutkittava väli jaetaan aina puoliksi
    -   (voitaisiin jakaa myös jotenkin muuten).

\[A, esimerkki 12 s. 86, "The Bisection Method"\]

### Kiintopistemenetelmä

-   Ratkaistaan yhtälöä $$f(x) = x.$$

-   Pistettä $a$, jossa $f(a) = a$, kutsutaan funktion $f$
    *kiintopisteeksi*
    -   etsitään siis funktion $f$ kiintopistettä.
-   Tarvitaan
    -   alkuarvaus $x_0$
    -   joitakin lisäominaisuuksia funktiolta $f$ (myöhemmin).
-   Menetelmä:
    -   Rakennetaan jono $x_0, x_1, x_2, \ldots$ seuraavasti: $$
          x_{n+1} = f(x_n), \quad n = 0, 1, 2 \ldots
        $$ eli "iteroidaan" funktiota $f$ aloittaen pisteestä $x_0$.
    -   ...ja toivotaan, että jono suppenee, eli että $$
          \lim_{n \to \infty} x_n = r
        $$
        -   jos jono suppenee, raja-arvo on haluttu kiintopiste
        -   likiarvoratkaisu saadaan, kun iterointi lopetetaan
            -   milloin lopetetaan? (tarkkuus...)
            -   mistä tiedetään, suppeneeko?
            -   jos ei suppene, ei hyötyä

**Esimerkki**

-   Ratkaise $\cos x = 5x$.
    -   Alkuarvaus $x_0=0{,}2$ (miksi?).
    -   \[A, s. 221\]

(Lukujonoja ja niiden suppenemista käsitellään kurssilla Calculus 3.)

#### Kiintopistelause

Jos $f$ on määritelty välillä $[a,b]$ ja lisäksi

1.  $\,f(x) \in [a,b]$ kaikilla $x \in [a,b]$ sekä
2.  jollakin luvulla $K \in ]0,1[$ on $$
      |f(x)-f(y)| < K|x-y|
    $$ kaikilla $x, y \in [a,b]$,

niin funktiolla $f$ on täsmälleen yksi kiintopiste $r \in [a,b]$ ja
kiintopistemenetelmän jono $$
  x_{n+1}=f(x_n)
$$ suppenee kohti tätä lukua $r$.

### Newtonin menetelmä

"Mennään tangentin suuntaan: uusi arvaus on tangenttisuoran ja
$x$-akselin leikkaus."

-   Ratkaistaan yhtälöä $$
      f(x) = 0.
    $$

-   Tarvitaan
    -   alkuarvaus $x_0$ ja
    -   funktion $f$ derivoituvuus.
-   Menetelmä:
    -   Lasketaan funktion arvo $f(x_0)$ ja derivaatta $f(x_0)$
    -   Lasketaan, missä käyrän $y=f(x)$ pisteeseen $x_0$ piirretty
        tangenttisuora leikkaa $x$-akselin:
        -   Tangenttisuora on $$
              y= f(x_0) + f'(x_0) (x-x_0).
            $$
        -   Tangenttisuoran leikkauspiste $x$-akselin kanssa, merk.
            $(x_1,0)$: $$
              0=f(x_0) + f'(x_0)(x_1-x_0) .
            $$
            -   Ratkaistaan $x_1$: $$
                  x_1 = x_0 - \frac{f(x_0)}{f'(x_0)}.
                $$
    -   Otetaan saatu $x_1$ uudeksi arvaukseksi ja jatketaan alusta.
-   Sama menetelmä kuin kiintopistemenetelmässä, mutta funktiolle
    $g(x) = x-\frac{f(x)}{f'(x)}.$
    -   Siis $$
          x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)}.
        $$
-   Derivaatta $f'$ ei saa olla nolla laskettavissa pisteissä...

-   Kuten kiintopistemenetelmä, ei aina suppene. (Mutta suppenee
    samoilla oletuksilla.)

-   Mihin ratkaisuun päätyy? Ei välttämättä alkuarvausta lähimpään!

**Esimerkki**

-   Yhtälöllä $$x^3-x-1$$ on vain yksi reaalijuuri (miksi?). Etsi
    Newtonin menetelmällä tämän reaalijuuren likiarvo haluamallasi
    tarkkuudella (vaikkapa 3 desimaalia).
    -   Alkuarvaus $x_0 = 1{,}5$

\[A, esimerkki 3, s. 224\]

## Likiarvoratkaisujen tarkkuudesta

Ratkaistaan numeerisesti yhtälöä $$f(x) = 0.$$

-   Mitä tarkoittaa "ratkaisun tarkkuus"?
    -   Ratkaisun likiarvo $\hat{x}$ on lähellä oikeaa ratkaisua $r$
        -   lähempänä kuin annettu tarkkuus $\varepsilon$, siis
            $|\hat{x}-r|<\varepsilon$, VAI
    -   funktion $f$ arvot ovat lähellä nollaa
        -   lähempänä nollaa kuin haluttu toleranssi $m$, siis
            $|f(\hat{x})| < m$.
-   Terminä "tarkkuus" tarkoittaa näistä edellistä; usein käytännössä
    tarvitaan jälkimmäistä.

-   Usein laskentaohjelmistoissa käyttäjä voi määrätä tarkkuuden ja/tai
    toleranssin.

-   Usein ohjelmisto osaa kertoa, että menetelmä ei näytä suppenevan
    -   "too many iterations", lopettaa tietyn määrän jälkeen ellei
        haluttua tarkkuutta saavuteta.
-   Eri ohjelmistoissa / laskimissa yksityiskohdiltaan erilaisia
    menetelmiä
    -   lue käyttöohjeet!

Numeerisista menetelmistä lisää samannimisellä kurssilla (TIEA381).

# Epämääräiset raja-arvotilanteet

-   l'Hospitalin sääntö x2

\[A, 4.3\]

### Lämmittelyksi

-   Laske raja-arvot $$
      \lim_{x \to 2} \frac{2x-4}{x-2}
    $$ $$
      \lim_{x \to 2} \frac{x^2-4x-4}{x-2}
    $$ $$
      \lim_{x \to 2} \frac{2x-4}{(x-2)^2}
    $$

-   Kaikki edellämainitut ovat "epämääräisiä tilanteita",
    -   ylläolevissa tapauksissa osoittajan ja nimittäjän kummankin
        raja-arvo on nolla
        -   (eli sijoittamalla saataisiin $\frac00$)
    -   tulos voi olla mitä vain, kuten edellä laskit!
-   Aiemmin ([Calculus
    1](https://tim.jyu.fi/view/kurssit/matematiikka/Calculus/Calculus1),
    ks. 6.5) olemme oppineet käsittelemään erilaisia epämääräisiä
    raja-arvotilanteita: $$
      \frac{0}{0}, \quad 
      \frac{\infty}{\infty}, 
      \quad 0\cdot \infty, 
      \quad \infty - \infty, 
      \quad (1^{\infty},
      \quad 0^0,
      \quad \infty^0 \quad \text{ei vielä})
    $$
    -   **muokkaamalla lauseketta**, esim.
        -   jaetaan tekijöihin, supistetaan nollaan menevä tekijä pois
            tai
        -   lavennetaan sopivalla lausekkeella ja sievennetään; sekä
            lisäksi
    -   suppiloperiaatteen avulla
        -   erityisesti $\frac{\sin x}{x} \to 1$, kun $x \to 0$.
-   Tällä kurssilla olemme nähneet lisää muokkauskeinoja, esim.
    -   kirjoita eksponenttifunktion avulla ($a=e^{\log a}$) ja käytä
        potenssien laskusääntöjä sekä eksponenttifunktion ominaisuuksia.
        -   (Tämä auttaa usein mm. tilanteessa $0^0$, harjoituksissa
            esimerkkejä.)
-   Seuraavaksi näemme, miten epämääräisiä raja-arvotilanteita voidaan
    käsitellä **derivaatan** avulla.
    -   Huom! Näitä keinoja **ei voida** käyttää kaikkiin tilanteisiin;
        selvitä aina ensin, mistä tilanteesta on kyse
        -   onko vaikkapa $"\frac{0}{0}"$ vai $"\infty-\infty"$.
    -   Usein "vääränmuotoiset" tilanteet voidaan muokata
        "oikeanmuotoisiksi".

## l'Hospitalin sääntö (1)

"Tilanteessa $"\frac00"$ lasketaan osamäärän raja-arvo derivaattojen
osamäärän raja-arvona."

-   Tarkemmin:
    -   Jos
        -   funktiot $f$ ja $g$ ovat molemmat derivoituvia välillä
            $]a,b[$ ja
        -   $g'(x) \not = 0$ kaikilla $x\in ]a,b[$ ja
        -   $f(x) \to 0$ ja $g(x) \to 0$, kun $x \to a{+}$ sekä $$
              \lim_{x\to a+} \frac{f'(x)}{g'(x)} = L,
            $$
    -   niin $$
          \lim_{x \to a+} \frac{f(x)}{g(x)} = L.
        $$
-   Huomaa, että yllä muotoilu on toispuoleiselle (oikeanpuoleiselle)
    raja-arvolle pisteessä $a$.
-   Toimii vastaavasti, kun
    -   $x \to b{-}$ (eli vasemmanpuoleiselle raja-arvolle) tai
    -   $x \to c$, $c \in ]a,b[$ (eli molemminpuoliselle kerralla) tai
    -   $a=-\infty$ ja/tai $b = \infty$ (epäoleelliset raja-arvot).
-   Yllä $L$ voi olla luku tai $\pm \infty$.

### Perustelu

Tarvitaan yleistetty DVAL (Calculus 1, ks. 11.4.2). Idea:

-   koska $\lim_{x \to a+}f(x) = 0$, merkitään $f(a)=0$ ("jatketaan
    jatkuvasti")
-   vastaavasti jatketaan $g(a) = 0$

-   YDVAL: väliltä $]a,x[$ löytyy piste $c$, jolle $$
      \frac{f(x)}{g(x)} = \frac{f(x)-f(a)}{g(x)-g(a} = \frac{f'(c)}{g'(c)}
    $$

-   kun $x \to a{+}$, myös $c \to a{+}$ ja siis $$
      \lim_{x\to a+} \frac{f(x)}{g(x)}
      =   \lim_{c\to a+} \frac{f'(c)}{g'(c)} = L.
    $$

-   Tapaus $x \to \infty$ (tai $x\to -\infty$):
    -   merkitään $x=\frac1u$, jolloin $x\to \infty \iff u \to 0{+}$ $$
          \lim_{x\to \infty} \frac{f(x)}{g(x)}
          =   \lim_{u\to 0+} \frac{f(\frac1u)}{g(\frac1u)}
        $$ ja l'Hospitalin säännöllä $$
           \lim_{u\to 0+} \frac{f(\frac1u)}{g(\frac1u)}
           = \lim_{u\to 0+} \frac{f'(\frac1u) \cdot \frac{-1}{u^2}}{g'(\frac1u) \cdot \frac{-1}{u^2}}
           =    \lim_{u\to 0+} \frac{f'(\frac1u)}{g'(\frac1u)}
           = \lim_{x\to \infty} \frac{f'(x)}{g'(x)}.
        $$

### Huomautuksia

-   Tarvitaan tilanne $"\frac00"$.

-   Lasketaan erikseen osoittajan $f(x)$ ja nimittäjän $g(x)$ derivaatat
    ja näiden osamäärä
    -   ei osamäärän $\frac{f(x)}{g(x)}$ derivaattaa!
-   Raja-arvo $L$ voi olla myös $\infty$ tai $-\infty$ (nk.
    epäoleellinen raja-arvo).

-   Samalla tavalla voidaan tutkia raja-arvoa äärettömyydessä (eli
    $x \to \infty$ tai $x \to -\infty$).

### Esimerkki (yksinkertainen tapaus)

-   Laske raja-arvo $$\lim_{x\to 1} \, \frac{\log x}{x^2-1}$$

-   Koska $\lim_{x \to 1} \log x = \log 1 = 0$ ja
    $\lim_{x \to 1} (x^2-1) = 1^2-1=0$, on tilanne epämääräinen
    $"\frac00"$. Osoittaja ja nimittäjä kumpikin ovat derivoituvia
    vaikkapa välillä $]0,2[$, joka sisältää pisteen 1, derivaatat
    $D(\log x)=\frac1x$ ja $D(x^2-1)=2x$. l'Hospitalin säännöllä siis $$
      \lim_{x\to 1} \, \frac{\log x}{x^2-1}
      = \lim_{x\to 1} \, \frac{D\log x}{D(x^2-1)} 
      = \lim_{x\to 1} \, \frac{\frac1x}{2x} = \frac12 .
    $$

### Esimerkki (säännön ketjuttaminen)

-   Laske $$
      \lim_{x\to 0} \, \frac{2 \sin x - \sin (2x)}{2 e^x-2-2x-x^2} .
    $$

-   Kun $x \to 0$, osoittaja $$
      2 \sin x - \sin (2x) \to 2 \sin 0 - \sin 0 = 0
    $$ ja nimittäjä $$
      2 e^x-2-2x-x^2 \to 2e^0 -2-0-0^2 = 0 
    $$ eli $$
      \lim_{x\to 0} \, \frac{2 \sin x - \sin (2x)}{2 e^x-2-2x-x^2} 
      \quad "=\tfrac00" .
    $$ Derivoidaan osoittaja ja nimittäjä (molemmat kaikkialla
    derivoituvia): $$
      D(2 \sin x - \sin (2x)) = 2 \cos x - 2 \cos 2x 
    $$ ja $$
      D(2 e^x-2-2x-x^2) = 2e^x -2-2x.
    $$ Nyt l'Hospitalin säännöllä $$
      \lim_{x\to 0} \, \frac{2 \sin x - \sin (2x)}{2 e^x-2-2x-x^2} 
      = \lim_{x\to 0} \, \frac{2 \cos x - 2 \cos 2x}{2 e^x-2-2x} 
      = \lim_{x\to 0} \, \frac{\cos x - \cos 2x}{ e^x-1-x} ,
    $$ jolloin edelleen ollaan tilanteessa $"\frac00"$; jatketaan
    l'Hospitalin säännöllä: $$
      \lim_{x\to 0} \, \frac{\cos x - \cos 2x}{ e^x-1-x} 
      = \lim_{x\to 0} \, \frac{-\sin x + 2\sin 2x}{ e^x-1} 
    $$ (sama juttu, vielä kerran l'H): $$
      \lim_{x\to 0} \, \frac{-\sin x + 2\sin 2x}{ e^x-1} 
      = \lim_{x\to 0} \, \frac{-\cos x + 4\cos 2x}{ e^x} = \frac{-1+4}{1} = 3.
    $$
    -   Kysymys: pitäisikö tarkistaa oletuksia?
        -   viimeinen "kolmansien derivaattojen osamäärän" raja-arvo on
            olemassa, joten viimeinen l'H ok; vastaavasti tästä saadaan
            toiseksi viimeinen l'H jne.
        -   siis se, että raja-arvo lopulta löytyy, riittää.

## l'Hospitalin sääntö (2)

"Tilanteessa $"\frac{\infty}{\infty}"$ lasketaan osamäärän raja-arvo
derivaattojen osamäärän raja-arvona."

-   Tarkemmin:
    -   Jos
        -   funktiot $f$ ja $g$ ovat molemmat derivoituvia välillä
            $]a,b[$ ja
        -   $g'(x) \not = 0$ kaikilla $x\in ]a,b[$ ja
        -   $f(x) \to \infty$ ja $g(x) \to \infty$, kun $x \to a+$ sekä
            $$
              \lim_{x\to a+} \frac{f'(x)}{g'(x)} = L,
            $$
    -   niin $$
          \lim_{x \to a+} \frac{f(x)}{g(x)} = L.
        $$
-   Kuten edelliselle säännölle:
    -   Yllä muotoilu on toispuoleiselle (oikeanpuoleiselle)
        raja-arvolle pisteessä $a$.
    -   Toimii vastaavasti, kun
        -   $x \to b{-}$ (eli vasemmanpuoleiselle raja-arvolle) tai
        -   $x \to c$, $c \in ]a,b[$ (eli molemminpuoliselle kerralla)
            tai
        -   $a=-\infty$ ja/tai $b = \infty$ (epäoleelliset raja-arvot).
    -   Yllä $L$ voi olla luku tai $\pm \infty$.
-   Perustelun idea hiukan monimutkaisempi kuin edelliselle säännölle
    -   \[A, s.233, tehtävä 35\]
    -   (menee jo yli tämän kurssin oppimistavoitteiden).
        -   Käytetään kyllä!

### Esimerkki

-   Laske $$
      \lim_{x \to \infty} \frac{x^2}{e^x} .
    $$
-   Tilanne on $"\frac{\infty}{\infty}"$, derivoituvat funktiot
    osoittajassa ja nimittäjässä, joten l'Hospital $\implies$ $$
      \lim_{x \to \infty} \frac{x^2}{e^x} 
      =   \lim_{x \to \infty} \frac{2x}{e^x} .
    $$ Edelleen $"\frac{\infty}{\infty}"$, joten l'Hospital $\implies$
    $$
      \lim_{x \to \infty} \frac{2x}{e^x} 
      = \lim_{x \to \infty} \frac{2}{e^x} =0.
    $$
    -   Samalla tavalla mille tahansa potenssifunktiolle $x^a$, kun
        $a>0$ (HT).

## Huomioita ja varoituksia

-   l'Hospitalin sääntöä ei voi käyttää, ellei raja-arvotilanne ole
    $"\frac00"$ tai $"\frac{\infty}{\infty}"$
    -   toki "käyttää voi" ja laskun saa loppuun, mutta tulos on
        väärä...
    -   Esim. $$\lim_{x \to 1+} \frac{x}{\log x} = \infty$$
        -   (osaathan laskea/perustella?)
        -   Mitä saisit "l'Hospitalin sääntöä käyttäen"?
-   Jos derivaattojen osamäärällä $\frac{f'(x)}{g'(x)}$ ei ole
    raja-arvoa (edes epäoleellista), ei l'Hospitalin säännöllä saada
    tietoa.
    -   Voi olla, että muilla keinoilla silti saadaan raja-arvo
        selvitettyä, esim.
        -   suppiloperiaatteella (kuten
            $\lim_{x \to 0} \frac{x^2 \sin(\frac1x)}{\sin x} = 0$)
        -   tai Taylorin polynomien avulla, joista lisää myöhemmin
            (luento 13 + Calculus 3).

## Entä muut epämääräiset muodot?

**Esimerkkejä**

1.  Laske $$
      \lim_{x\to 0+} \left(\frac{1}{x} - \frac{1}{\sin x}  \right) .
    $$
    -   tässä epämääräinen muoto $"\infty-\infty"$
    -   laventamalla samannimisiksi saadaan muotoon $"\frac00"$
    -   l'H kahdesti, tulos on 0; yksityiskohdat \[A, s.231\].
2.  Laske $$
      \lim_{x\to \infty} \left( 1+ \sin \tfrac3x \right)^x .
    $$
    -   tässä epämääräinen muoto $"1^{\infty}"$
    -   kirjoitetaan $y=e^{\log y}$ eli $$
          \left( 1+ \sin \tfrac3x \right)^x = e^{x \log(1+\sin \frac3x)}
        $$
    -   lasketaan raja-arvo $$
          x \log(1+\sin \tfrac3x) = \frac{\log (1+\sin \frac3x)}{\frac1x} \to 3
        $$ (l'Hospitalin säännön avulla)
    -   tulos on siis $e^3$.
        -   \[A, s.232\]

# Ääriarvot ja funktion kulku

-   Ääriarvot
-   kuperuus
-   käännepiste
-   toinen derivaatta ja ääriarvot
-   funktion kuvaajan hahmottelu
-   asymptootit

\[A, (4.4), 4.5, 4.6, 4.8\]

## Funktion ääriarvot

-   Kertaa funktion ääriarvojen etsiminen kurssilta [Calculus
    1](https://tim.jyu.fi/view/kurssit/matematiikka/Calculus/Calculus1)
    (luento 12 kokonaan; sisältö kuuluu myös tälle kurssille).

**Esimerkki**

1.  Selvitä, onko funktiolla $f(x)=x+\frac{4}{x}$ pienintä arvoa välillä
    $]0, \infty[$.
    -   Epäoleelliset raja-arvot $\lim_{x \to 0+} f(x) = \infty$ ja
        $\lim_{x \to \infty} f(x) = \infty$
    -   $f$ on jatkuva koko välillä $]0, \infty[$, joten minimi löytyy
        (mistä?)
    -   derivaatan nollakohdasta ($x=2$), koska $f$ on derivoituva koko
        välillä $]0, \infty[$
    -   pienin arvo on siis $f(2)=4$ (tee kulkukaavio "varmuuden
        vuoksi")
        -   tarkemmin \[A, s. 238-9\].
2.  Selvitä funktion $f(x) = xe^{-x^2}$ lokaalit ja globaalit ääriarvot
    sekä hahmottele kuvaaja. Mikä on funktion arvojoukko?
    -   $f$ on derivoituva kaikkialla, joten lokaalit ääriarvot löytyvät
        derivaatan nollakohdista ($x=\pm\frac{1}{\sqrt{2}}$)
    -   tee kulkukaavio nähdäksesi, missä $f$ on kasvava, missä vähenevä
    -   lokaali minimi $f(-\frac{1}{\sqrt{2}}) = -\frac{1}{\sqrt{2e}}$
    -   lokaali maksimi $f(\frac{1}{\sqrt{2}}) = \frac{1}{\sqrt{2e}}$
    -   epäoleelliset raja-arvot $\lim_{x\to \pm\infty} f(x) = 0$, joten
        ainoa lokaali maksimi
        $f(\frac{1}{\sqrt{2}}) = \frac{1}{\sqrt{2e}} >0$ on myös
        globaali maksimi; minimi vastaavasti.
    -   $f$ on jatkuva joten arvojoukko on väli
        $[-\frac{1}{\sqrt{2e}}, \frac{1}{\sqrt{2e}}]$
        -   tarkemmin \[A, s. 239\]

## Kuperuus (eli konveksisuus)

### Määritelmä ja vastaavia ehtoja

-   Derivoituva funktio $f$ on välillä $I$
    -   *alaspäin kupera* eli *konveksi*, jos $f'$ on aidosti kasvava
        välillä $I$
    -   *ylöspäin kupera* eli *konkaavi*, jos $f'$ on aidosti vähenevä
        välillä $I$

Mitä tämä tarkoittaa

-   kuvaajan $y=f(x)$ muodolle:
    -   missä $f$ on alaspäin kupera, kuvaajassa on "notko", "kaartuu
        ylöspäin"
    -   missä $f$ on ylöspäin kupera, kuvaajassa on "kumpu", "kaartuu
        alaspäin"
-   tangenttisuorien avulla:
    -   missä $f$ on alaspäin kupera, tangenttisuorat kulkevat kuvaajan
        alapuolella
    -   missä $f$ on ylöspäin kupera, tangenttisuorat kulkevat kuvaajan
        yläpuolella
-   toisen derivaatan avulla: jos $f$ on kahdesti derivoituva, niin
    -   missä $f''(x) >0$, siellä $f$ on alaspäin kupera
    -   missä $f''(x) <0$, siellä $f$ on ylöspäin kupera
        -   huom: $f''(x) >0$ välillä $I$ $\implies f'$ aidosti kasvava
            välillä $I$
            -   ei $"\Longleftarrow"$; voi olla $f''(x) =0$
                yksittäisessä pisteessä ja silti $f'$ aidosti kasvava
                koko välillä (ks. Calculus 1, 11.1.2).

### Huomautuksia ja esimerkkejä

-   Älä sekoita funktion kasvavuuteen/vähenevyyteen:
    -   esim. $f(x) = x^3$ on aidosti kasvava koko reaalilukujen
        joukossa, mutta
        -   konkaavi välillä $]-\infty, 0]$
        -   konveksi välillä $[0, -\infty[$
            -   laske itse + piirrä kuva!
    -   esim. $f(x) = \sin x$ on
        -   aidosti kasvava väleillä
            $[-\frac{\pi}{2} +n2\pi, \frac{\pi}{2}+n2\pi]$,
            $n \in \mathbb{Z}$
        -   aidosti vähenevä väleillä
            $[\frac{\pi}{2} +n2\pi, -\frac{\pi}{2}+n2\pi]$,
            $n \in \mathbb{Z}$
        -   konkaavi väleillä $[0+ n2\pi,\pi + n2\pi]$,
            $n \in \mathbb{Z}$
        -   konveksi väleillä $[\pi+ n2\pi,2\pi + n2\pi]$ eli väleillä
            $[-\pi+ n2\pi,n2\pi]$, $n \in \mathbb{Z}$
            -   piirrä kuva + laske myös itse.

**Huomautus(+)**

-   Edellä oletettiin, että funktio on derivoituva välillä $I$.
-   Yleisemmin konveksisuus voidaan määritellä sekantin avulla:
    -   funktio on alaspäin kupera eli konveksi, jos kuvaajan pisteitä
        yhdistävä jana kulkee kuvaajan yläpuolella.
        -   Piirrä kuva.
-   Vastaavasti tason osajoukko on *konveksi* joukko, jos sen kahta
    pistettä yhdistävä jana pysyy joukossa.
    -   Piirrä kuva.

## Käännepiste

"Suoristuspiste"; kohta, jossa kuperuus muuttuu.

### Määritelmä

-   Piste $(x_0, f(x_0)$ on kuvaajan $\,y=f(x)\,$ *käännepiste*, jos
    -   käyrällä $y=f(x)$ on tangenttisuora pisteessä $x=x_0$ ja
    -   funktion $f$ kuperuus vaihtuu pisteessä $x=x_0$
        -   eli $f$ on pisteen $x=x_0$ toisella puolella ylöspäin
            kupera, toisella alaspäin kupera.
-   Vastaavasti sanomme, että funktiolla $f$ on käännepiste kohdassa
    $x=x_0$, jos
    -   piste $(x_0, f(x_0))$ on kuvaajan $y=f(x)$ käännepiste.

### Huomautuksia

-   Käännepisteessä $(x_0, f(x_0))$ funktion kuvaajalla on
    tangenttisuora; siis
    -   joko $f$ on derivoituva pisteessä $x_0$
    -   tai kuvaajalla $y=f(x)$ on pystysuora tangentti pisteessä
        $(x_0, f(x_0))$.
-   Käännepisteen eri puolilla kuvaaja $y=f(x)$ kulkee eri puolilla em.
    tangenttisuoraa.

-   Myös muille käyrille kuin funktioiden kuvaajille määritellään
    käännepiste vastaavalla tavalla:
    -   piste $P$ on käyrän käännepiste, jos käyrällä on tangentti
        pisteessä $P$ ja kuljettaessa käyrää pitkin pisteestä $P$
        vastakkaisiin suuntiin käyrä kulkee tämän tangenttisuoran
        vastakkaisilla puolilla.

### Käännepisteiden löytäminen

-   Toisen derivaatan avulla:
    -   Jos $f$ on kahdesti jatkuvasti derivoituva, $f''$ vaihtaa
        merkkiään käännepisteissä; siis
        -   riittää etsiä toisen derivaatan nollakohdat ja
        -   tutkia, ovatko todella käännepisteitä (vaihtuuko $f''$:n
            merkki).
    -   Vaikka toista derivaattaa ei olisi (tutkittavassa pisteessä),
        piste on käännepiste, jos
        -   ensimmäinen derivaatta on olemassa ja
        -   toisen derivaatan merkki vaihtuu.

## Esimerkki (ääriarvot, kuperuus, käännepisteet, kuvaajan hahmottelu)

1.  Tutki funktion $f(x)= x^4 - 2x^3+1$ kulkua:
    -   selvitä, millä väleillä $f$ on kasvava, missä vähenevä;
    -   selvitä funktion $f$ lokaalit ääriarvot;
    -   selvitä, millä väleillä $f$ on konveksi, millä konkaavi; sekä
    -   hahmottele näiden tietojen perusteella funktion $f$ kuvaajaa.

-   Polynomina $f$ on kaikkialla kahdesti (tai vaikka kuinka monta
    kertaa) derivoituva;
    -   lokaalit ääriarvot löytyvät derivaatan nollakohdista, jos niitä
        on;
    -   käännepisteet löytyvät toisen derivaatan nollakohdista, jos
        niitä on;
    -   kasvavuus ja vähenevyys derivaatan merkin avulla
    -   kuperuus toisen derivaatan merkin avulla
        -   tee kulkukaavio!
    -   $f'(x) = 4x^3-6x^2= 2x^2(2x-3))$
        -   nollakohdat $x=0$ ja $x=\frac32$
    -   $f''(x) = 12x^2-12x=12x(x-1)$,
        -   nollakohdat $x=0$ ja $x=1$
    -   kulkukaavioon pisteet $0,1,\frac32$.
-   Ensimmäisen derivaatan merkki kertoo kasvavuuden:
    -   $f'(x) < 0$, kun $x <\frac32$, siis $f$ on aidosti vähenevä
        välillä $]-\infty, \frac32]$
    -   $f'(x) > 0$, kun $x >\frac32$, siis $f$ on aidosti kasvava
        välillä $[\frac32, \infty[$
        -   lokaali minimi kohdassa $x=\frac32$
        -   kriittinen piste $x=0$ ei ole lokaali ääriarvopiste.
-   Toisen derivaatan merkki kertoo kuperuuden:
    -   $f''(x) < 0$, kun $x \in ]0,1[$, siis $f$ on konkaavi välillä
        $x \in [0,1]$
    -   $f''(x) > 0$, kun $x \in ]-\infty, 0[$ tai $x \in ]1,\infty[$,
        siis $f$ on konveksi väleillä $x \in ]-\infty, 0]$ ja
        $[1, \infty[$
    -   käännepisteet $(0, f(0))=(0,1)$ ja $(1, f(1))=(1,0)$

## Toisen derivaatan testi

Palataan ääriarvojen selvittämiseen. Derivoituvan funktion $f$ lokaalit
ääriarvot löytyvät kriittisistä pisteistä eli derivaatan nollakohdista.

-   "Kuperuus kertoo, ollaanko maksimissa vai minimissä."

Tarkemmin:

Kriittisten pisteiden laatu voidaan selvittää toisen derivaatan avulla:

-   Jos $f'(x_0) =0$ ja
    -   $f''(x_0) < 0$, niin $x_0$ on lokaali maksimipiste;
    -   $f''(x_0) > 0$, niin $x_0$ on lokaali minimipiste.

**Huomautuksia**

-   Jos $f'(x_0) =0$ ja
    -   $f''(x_0) = 0$, niin testi ei anna tietoa kriittisen pisteen
        $x_0$ laadusta.
-   Jos $f'(x_0) \not=0$, ei kyse ole kriittisestä pisteestä eikä siis
    lokaalia ääriarvoa voi löytyä (sikäli kun $f$ on derivoituva).

## Kuvaajan hahmottelusta

-   Halutaan siis tutkia funktion kulkua:
    -   missä määritelty
    -   missä kasvava / vähenevä, lokaalit/globaalit ääriarvot
    -   asymptootit eli rajasuorat
    -   myös kuvaajan "muotoa" eli missä konveksi, missä konkaavi
        -   tiedot myös sitovat toisiaan (kaikki yhdistelmät eivät ole
            mahdollisia)
            -   $\implies$ virheentarkistuskeino

### Asymptootit

Funktion $f$ kuvaajalla $y=f(x)$ on

-   vaakasuora asymptootti $y=a$, jos $$
      \lim_{x \to \infty} f(x) = a \quad  \text{tai} \quad 
      \lim_{x \to -\infty} f(x) = a \quad
    $$
-   pystysuora asymptootti $x=b$, jos $$
      \lim_{x \to b-} f(x) = \pm\infty \quad \text{ tai } \quad
      \lim_{x \to b+} f(x) = \pm\infty \quad
    $$
-   vino asymptootti $y=cx+d$, jos $$
      \lim_{x\to \infty} [f(x)-(cx+d)] = 0 \quad \text{ tai } \quad
      \lim_{x\to -\infty} [f(x)-(cx+d)] = 0.
    $$
    -   Pystysuoria ja vaakasuoria asymptootteja käsiteltiin jo
        kurssilla Calculus 1 (kohta 6.3), kertaa tarvittaessa
        -   myös esimerkkejä löydät kurssilta Calculus 1.

**Esimerkkejä**

1.  Etsi funktion $f(x) = \frac{x^2+1}{x}$ kuvaajan asymptootit.
    -   Nimittäjän nollakohdassa $(x=0)$ on pystysuora asymptootti,
        sillä $$
          \lim_{x \to 0+} f(x) = \infty
        $$ ja $$
          \lim_{x \to 0-} f(x) = -\infty
        $$ (toinenkin riittäisi).
    -   Koska $$
          f(x) = \frac{x^2+1}{x}= x+\frac{1}{x},
        $$ on $$
          \lim_{x \to \pm\infty} \left(f(x) - x \right) 
          = \lim_{x \to \pm\infty} \frac{1}{x} = 0
        $$ ja siis $y=x$ on kuvaajan $y=f(x)$ (kaksisuuntainen) vino
        asymptootti.
        -   Vaakasuoria asymptootteja ei ole, koska $f(x) \to \infty$,
            kun $x \to \infty$ ja $f(x) \to -\infty$, kun
            $x \to -\infty$ (sama vino asymptootti, kun
            $x \to \pm\infty$)
2.  Etsi funktion $f(x) = \frac{x^3}{x^2+x+1}$ kuvaajan asymptootit.
    -   Jaa jakokulmassa tai päättele: $$
          \frac{x^3}{x^2+x+1} = x-1+\frac{1}{x^2+x+1} .
        $$
    -   Koska $\frac{1}{x^2+x+1} \to 0$, kun $x \to \pm \infty$,
        -   löytyy vino asymptootti $y=x-1$.
    -   Pystysuoria ei ole, koska nimittäjällä $x^2+x+1$ ei ole
        reaalisia nollakohtia (diskriminantti
        $(-1)^2-4\cdot 1 \cdot 1 = -4<0$).
    -   Vaakasuoria ei ole.

### "Muistilista"

Tehtävä: hahmottele (huolellisesti) funktion $f$ kuvaajaa.

-   Muistilista:
    -   selvitä funktion määrittelyjoukko (onko kaikkialla määritelty?)
    -   laske $f'$ ja $f''$ ja sievennä tarvitsemaasi muotoon (yleensä
        tulo)
    -   selvitä asymptootit
        -   pystysuorat (nimittäjän nollakohdissa, jos on)
        -   vaakasuorat/vinot (tutki $\lim_{x \to \pm \infty} f(x)$)
    -   onko ilmiselviä ominaisuuksia (esim. parillisuus/parittomuus,
        muut symmetriat)
    -   laske joitakin funktion arvoja, esim.
        -   missä kuvaaja leikkaa akselit (jos onnistuu helposti)
        -   lisää listaan kriittiset pisteet, singulaaripisteet,
            käännepisteet (jos löytyy)
        -   jos funktio on epäjatkuva tai määritelty paloissa, laske
            ainakin yksi arvo joka osasta
    -   tutki derivaattaa $f'$:
        -   onko kriittisiä pisteitä ($f'(x) = 0$)
        -   onko singulaaripisteitä (derivaattaa $f'$ ei ole)
        -   millä väleillä $f'$ on positiivinen, millä negatiivinen
            -   $\implies$ missä $f$ on kasvava, missä vähenevä
            -   kulkukaavio on hyvä apu
                -   laita kulkukaavioon kaikki tarpeelliset pisteet
                    (kriittiset pisteet, singulaaripisteet, välien
                    päätepisteet)
            -   lokaalit ääriarvot löytyvät jo näillä tiedoilla
    -   tutki toista derivaattaa $f''$:
        -   missä $f''(x) = 0$
        -   missä $f''$ ei ole olemassa
        -   millä väleillä $f''$ on positiivinen, millä negatiivinen
            -   kertoo funktion $f$ kuperuuden
                -   tässäkin kulkukaavio on hyvä apu; voit käyttää
                    erillistä kaaviota tai laittaa kaikki "kiinnostavat
                    pisteet" samaan kulkukaavioon
            -   käännepisteet löytyvät näillä tiedoilla
-   Hahmottele kuvaa:
    -   löytyykö ristiriitaisia tietoja (teitkö virheen jossain)
    -   mitkä piirteet ovat kiinnostavia - millainen kuva kannattaa
        piirtää (askelit, mittakaava)

### Miksi ihmeessä?

Miksi nähdä vaivaa funktion lausekkeen ja kuvaajan yhteyksien
ymmärtämiseksi, kun voin käyttää tietokonetta (tai graafista laskinta)
kuvaajan piirtämiseen?

-   Kokeile piirtää vaikkapa $e^x \log (1+e^{-x})$.
    -   Miltä kuvaaja näyttää vaikkapa välillä $[30,40]$ ?
    -   Miltä kuvaajan pitäisi näyttää?
-   Lisätietoja mm. \[A, 4.7\] sekä kurssit Symbolinen laskenta ja
    Tietokoneavusteinen matematiikka.

(Toki muitakin motiiveja löytyy...)

# Riemannin integraali

-   Pinta-ala
-   Summamerkintä ja summien käsittelystä
-   Pinta-alan laskeminen summien raja-arvona
-   Määrätty integraali eli Riemannin integraali

\[A, 5.1-5.3\]

### Lämmittelytehtävä

1.  Ota kaksi ruutupaperia.
    -   Suunnittele 1. paperille uusi piparkakkumuotti.
        -   (= piirrä tasokuvio)
    -   Arvioi kuviosi pinta-alaa (piparitaikinan menekin
        arvioimiseksi).
        -   Kirjoita arviosi 2. paperille.
2.  Anna 1. papereista toiselle opiskelijalle.

3.  Arvioi saamasi kuvion pinta-alaa (ruutuina):
    -   Kuinka monta ruutua on kokonaan kuvion sisällä? (S)
    -   Kuinka monta ruutua peittäisi koko kuvion? (P)
        -   Kirjoita arviosi kuvion viereen: $$
              S \leq \text{ pinta-ala } \leq P
            $$
4.  Palauta paperi lähettäjälle.
5.  Tutki piirtämäsi kuvion saamaa pinta-ala-arviota.
    -   Osuiko oma alkuperäinen arviosi lukujen $S$ ja $P$ väliin?
        -   Jos ei, kumpaan luotat enemmän?
        -   Mikä on kuviosi "oikea pinta-ala"?
        -   Onko kaikilla kuvioilla pinta-ala?

## Pinta-alasta

-   Halutaan että tasoalueen "pinta-ala" $A$ toteuttaa ainakin seuraavat
    ehdot:
    -   $A \geq 0$ "aina"
    -   suorakulmiolle, jonka pituus on $a$ ja leveys on $b$, pinta-ala
        on $A=ab$
    -   yhtenevillä tasokuvioilla on sama pinta-ala
    -   jos kahdelle tasoaluelle on $T_1 \subset T_2$, niin
        pinta-aloille $A_1\leq A_2$
    -   jos tasoalue on äärellisen monen sellaisen tasoalueen yhdiste,
        jotka eivät leikkaa toisiaan, sen pinta-ala saadaan laskemalla
        näiden osa-alueiden pinta-alat yhteen
-   Suorakulmion pinta-alasta saadaan näiden avulla
    -   suunnikkaan pinta-ala (kanta kertaa korkeus)
    -   kolmion pinta-ala (suunnikas muodostuu kahdesta yhtenevästä
        kolmiosta)
    -   minkä tahansa monikulmion pinta-ala (jakamalla kolmioihin)
-   Entä muiden tasokuvioiden pinta-alat?
    -   Tarvitaan raja-arvo!
    -   Raja-arvoina on saatu myös koulusta tunnetut
        -   ympyrän pinta-ala $A=\pi r^2$, kun säde on $\,r$, ja
        -   ellipsin pinta-ala $A=\pi ab$, kun puoliakselit ovat $a$ ja
            $b$
            -   käytämme näitä tässä vaiheessa perustelematta.
    -   Jaetaan tasokuvio osiin, jotka ovat muotoa
        -   "käyrien $y=f(x)$, $y=0$, $x=a$ ja $x=b$ väliin jäävä alue,
            $f(x)\geq 0$ kaikilla $x \in [a,b]$"
            -   ((Kaikki funktiot eivät tähän sovellu, mutta
                -   välillä $[a,b]$ jatkuvat funktiot kyllä
                -   samoin paloittain jatkuvat.))
    -   Lasketaan tällaisen osan pinta-ala "viipaloimalla":
        -   käytetään suorakaiteita, joiden
            -   korkeus on funktion arvo jossain pisteessä, $f(x_k)$
            -   leveys on näiden pisteiden etäisyys $|x_k - x_{k-1}|$
        -   lasketaan suorakaiteiden alat yhteen ja otetaan raja-arvo,
            kun suorakaiteiden leveys menee nollaan
            -   "virhe pienenee nollaan"
            -   jos onnistuu, ok; jos ei, "pinta-alaa ei ole"

## Summamerkintä

-   Pinta-alojen laskemisessa on siis tarpeen laskea yhteen "paljon"
    termejä (ja sen jälkeen ottaa raja-arvo).

-   Otetaan käyttöön lyhennysmerkintä
    -   (sama merkintä tulee eteen myöhemmin muissa yhteyksissä).

### Määritelmä, käsitteitä, eri muotoja

-   *Summamerkintä* on summan lyhennysmerkintä: $$
      \sum_{k=1}^n a_k = a_1+a_2+ \cdots + a_n
    $$
    -   lue: "summa $a_k$, kun $k$ kulkee yhdestä $n$:ään"
    -   $k$ on *summausindeksi*
        -   ei näy "tuloksessa"!
        -   voidaan vaihtaa, $\sum_{k=1}^n a_k = \sum_{j=1}^n a_j$
    -   $a_k$ on summan $k$:s *termi*
    -   $n$ on *summauksen yläraja*
        -   näkyy (yleensä) tuloksessa
    -   yllä $1$ on *summauksen alaraja*
-   Summa voidaan aloittaa myös muualta kuin ykkösestä: $$
      \sum_{k=m}^n a_k = a_m+a_{m+1}+ \cdots + a_n
    $$
    -   huom. yllä pitää olla $m \leq n$
-   Usein termit kirjoitetaan jonkin funktion $f$ avulla: $$
      \sum_{k=m}^n f(k) = f(m)+f(m+1)+ \cdots + f(n)
    $$

### Esimerkkejä

1.  $$
      \sum_{k=1}^5 k^2 = 1^2+2^2+3^2+4^2+5^2 = 55
    $$
2.  $$
      \sum_{j=1}^n 8 = 8+8+\cdots +8 = 8n \quad (n \text{ termiä})
    $$
3.  $$
      \sum_{k=-2}^3 \frac{1}{k+7} 
      = \frac{1}{5} + \frac{1}{6} +\frac{1}{7} +\frac{1}{8} +\frac{1}{9} +\frac{1}{10} 
    $$

### Ominaisuuksia

-   Lineaarisuus: $$
      \sum_{k=m}^n (Af(k)+Bg(k)) = A \sum_{k=m}^n f(k) + B \sum_{k=m}^n g(k)
    $$

-   Indeksin siirto: $$
      \sum_{k=m}^{m+n} f(k) = \sum_{k=0}^n f(k+m)
    $$
    -   usein on mukavampi vaihtaa samalla indeksinä käytettyä
        kirjainta, esim. $$
          \sum_{j=3}^{17} \sqrt{1+j^2} = \sum_{k=1}^{15} \sqrt{1+(k+2)^2}
        $$
        -   yllä $"k=j-2"$

### Eräitä summia

-   Joissakin erikoistapauksissa osaamme laskea summan arvon "suljetussa
    muodossa" eli saamme tulokseksi lausekkeen, jossa ei ole
    summamerkintää $\Sigma$ (tai $\cdots$): $$
      \begin{align*}
      & \sum_{k=1}^n 1 = n \\
      & \sum_{k=1}^n k = 1 +2+3+\cdots +n = \frac{n(n+1)}{2} \\
      & \sum_{k=1}^n k^2 = 1^2 +2^2+3^2+\cdots +n^2 = \frac{n(n+1)(2n+1)}{6} \\
      & \sum_{k=1}^n r^{k-1} = 1 + r + r^2+ \cdots r^{n-1} = \frac{r^{n}-1}{r-1}, 
      \quad \text{ kun }\, r \not = 1 .
      \end{align*}
    $$
    -   perustelut \[A, s. 291-292\]
        -   esim. $$
              2 \sum_{k=1}^n k = (1 +2+3+\cdots +n) + (n+\cdots +3+2+1) \\
              = (1+n) + (2+n-1) +\cdots +(n+1) \\
              = n(n+1)
            $$
-   Joissakin tilanteissa summa sievenee kauniisti: $$
      \sum_{k=m}^{n} (f(k+1)-f(k)) = f(n+1) - f(m)
    $$
    -   Tarkista itse! Kirjoita summa auki: mitkä termit kumoutuvat?
    -   Tällaista summaa kutsutaan "teleskooppisummaksi".

## Pinta-alan laskeminen summien raja-arvona

### Esimerkkejä

1.  Laske sen tasoalueen pinta-ala, jota rajoittavat käyrät $y=x^2$,
    $y=0$, $x=0$ ja $x=3$ *approksimoivien suorakaiteiden avulla*.
    -   Piirrä kuva!
    -   Jaetaan väli $[0,3]$ tasan $n$ osaväliin:
        -   jakopisteet
            $\{0, \frac{1}{n}\cdot 3, \frac{2}{n}\cdot 3, \frac{3}{n}\cdot 3, \ldots, \frac{n-1}{n}\cdot 3, 3\}$,
            \quad yhteensä ($n+1$) kpl
            -   merkitään $x_k=\frac{3k}{n}$, $k=0, 1, \ldots, n$
        -   jakovälin pituus $x_k-x_{k-1}=\frac{3}{n}$, $\,n$ väliä
    -   Lasketaan funktion arvot jakopisteissä:
        -   $f(x_k) = x_k^2 = \frac{9k^2}{n^2}$
    -   Peitetään haluttu alue suorakaiteilla, joita on $n$ kpl,
        jokaisen leveys $\frac{3}{n}$ ja $k$:nnen suorakaiteen korkeus
        $f(x_k)$
        -   Funktio on tutkittavalla välillä kasvava, joten jokaisella
            osavälillä se on suurimmillaan osavälin oikeanpuoleisessa
            päätepisteessä.
        -   Siis välillä $[0, \frac{3}{n}]$ käytetään suorakaidetta,
            jonka korkeus on $\frac{9}{n^2}$, ja välillä
            $[\frac{3}{n}, \frac{6}{n}]$ käytetään suorakaidetta, jonka
            korkeus on $\frac{36}{n^2}$ jne.
    -   Alueen peittävien suorakaiteiden yhteenlaskettu pinta-ala on $$
          \sum_{k=1}^n \frac{3}{n} f(x_k) = \sum_{k=1}^n \frac{3}{n} \frac{9k^2}{n^2}
          = \frac{27}{n^3}\sum_{k=1}^n k^2 = \frac{27}{n^3} \frac{n(n+1)(2n+1)}{6} 
        $$
        -   kun $n\to \infty$, tällä on raja-arvo $9$; siis kysytty
            pinta-ala on 9.
2.  Yleisemmin: sen tasoalueen pinta-ala, jota rajoittavat käyrät
    $y=x^2$, $y=0$, $x=a$ ja $x=b$, on $\frac{b^3}{3} - \frac{a^3}{3}$.
    -   (Laske itse; vastaava lasku x2 ja vähennys.)
3.  Mikä pinta-ala on raja-arvo $$
      \lim_{n \to \infty} \sum_{k=1}^n \frac{n-k}{n^2}
    $$ ja kuinka suuri se on?
    -   Muokataan: $$
          \lim_{n \to \infty} \sum_{k=1}^n \frac{n-k}{n^2} =
          \lim_{n \to \infty} \sum_{k=1}^n \left(1-\frac{k}{n}\right) \frac{1}{n}
        $$ eli summassa lasketaan yhteen $n$ kpl sellaisten
        suorakaiteiden pinta-aloja, joilla on leveys $\frac{1}{n}$ ja
        korkeus $1-x_k$, missä $x_k=\frac{k}{n}$, $k=1, 2, \ldots, n$.
        (Siis kaikilla sama leveys, jokaisella oma korkeus.)
        -   Siis raja-arvo on sen alueen pinta-ala, jota rajoittavat
            käyrät $y=1-x$, $y=0$, $x=0$ (sekä $x=1$). Piirrä kuva.
        -   Tämä alue on kolmio, jonka korkeus on 1 ja kanta 1; siis sen
            pinta-ala on $\frac12$.
        -   Raja-arvo on $$
              \lim_{n \to \infty} \sum_{k=1}^n \frac{n-k}{n^2} = \frac12 .
            $$

-   Lisää esimerkkejä \[A, luku 5.2\].

## Määrätty integraali eli Riemannin integraali

Mikä oikein on jatkuvan funktion $f$ *määrätty integraali yli välin
$[a,b]$*, jota merkitään $\int_a^b f(x) \, dx$ ?

Käytetään seuraavia merkintöjä:

-   välin $[a,b]$ jako $P=\{x_0, x_1, \ldots, x_n\}$
-   $k$:nnen osavälin pituus $\Delta x_k = x_k - x_{k-1}$ (ei
    välttämättä tasavälinen jako)
-   $k$:nnella osavälillä $f$ saavuttaa suurimman arvonsa pisteessä
    $u_k$ (ok, $f$ jatkuva)
-   $k$:nnella osavälillä $f$ saavuttaa pienimmän arvonsa pisteessä
    $l_k$ (ok, $f$ jatkuva)

### Määritelmiä

-   Funktion $f$ jakoon $P$ liittyvä *yläsumma* on $$
      U(f,P) = \sum_{k=1}^n f(u_k) \Delta x_k
    $$
-   Funktion $f$ jakoon $P$ liittyvä *alasumma* on $$
      L(f,P) = \sum_{k=1}^n f(l_k) \Delta x_k
    $$

-   Määrätty integraali: "jos yläsummilla ja alasummilla on yksi
    yhteinen raja-arvo, kun jakoa tihennetään, tämä luku on
    $\int_a^b f(x) \, dx$"
    -   tällöin sanomme, että $f$ on integroituva (välillä $[a,b]$)
        -   suljetulla välillä jatkuva funktio on integroituva (ei
            todisteta)
            -   (tällä kurssilla emme käsittele funktioita, jotka eivät
                ole integroituvia)
        -   jos $f$ on integroituva, saadaan $\int_a^b f(x) \, dx$
            raja-arvona vaikkapa tasavälisiä jakoja tihentäen; tällä
            kurssilla tämä riittää
            -   jos haluaisimme *todistaa*, että annettu funktio on
                integroituva, pitäisi olla huolellisempi.

### Pinta-alatulkinta

-   Määrätyn integraalin (Riemannin integraalin) määritelmässä ei
    oletettu, että $f\geq 0$.

-   Jos $f\geq 0$, niin alueelle, jota rajoittavat $y=f(x)$, $y=0$,
    $x=a$ ja $x=b$
    -   $U(f,P)$ on "peittävien suorakaiteiden yhteispinta-ala" ja
    -   $L(f,P)$ on "sisään jäävien suorakaiteiden yhteispinta-ala" ja
    -   näiden raja-arvona $\int_a^b f(x) \, dx$ on mainitun alueen
        pinta-ala (määritelmä!) $$
          A=\int_a^b f(x) \, dx.
        $$
-   Jos $f \leq 0$, niin (symmetrian nojalla) alueen, jota rajoittavat
    $y=f(x)$, $y=0$, $x=a$ ja $x=b$, pinta-ala on sama kuin sen alueen
    pinta-ala, jota rajoittavat $y=-f(x)$ ja muuten samat käyrät; $$
      A=\int_a^b -f(x) \, dx = -\int_a^b f(x) \, dx
    $$
    -   huom: pinta-ala $A\geq 0$ aina, mutta
        $\int_a^b f(x) \, dx \leq 0$, jos $f(x) \leq 0$ välillä $[a,b]$.
    -   Tästä ja muista määrätyn integraalin ominaisuuksista lisää
        myöhemmin.
-   Jos $f$ vaihtaa merkkiään, luvuilla $U(f,P)$ ja $L(f,P)$ saati
    $\int_a^b f(x) \, dx$ ei ole suoraa tulkintaa pinta-aloina.
    -   Jakamalla tarkasteltava väli osiin, joissa $f$ ei vaihda
        merkkiään, saadaan tulkinta vastaavien osa-alueiden pinta-alojen
        kautta
        -   Lasketaan osa-alueiden pinta-alat "yhteen" siten, että
            $x$-akselin alapuolella olevat pinta-alat otetaan huomioon
            negatiivisina.

**Esimerkki**

1.  $$
      \int_1^2 (-x) \, dx = -\frac32,
    $$ koska käyrät $y=-x$, $y=0$, $x=1$ ja $x=2$ rajaavat
    puolisuunnikkaan, jonka pinta-ala on $1+\frac12 =\frac32$ ja
    integroitava funktio $f(x) = -x$ on negatiivinen välillä $[1,2]$.
    (Piirrä kuva.)

2.  $$
      \int_{-2}^2 x^3 \, dx = 0, 
    $$ koska funktio $f(x)=x^3$ on pariton (eli $f(-x) = -f(x)$ kaikilla
    $x$) ja väli $[-2,2]$ on symmetrinen nollan suhteen; siis
    $x$-akselin ylä- ja alapuolelle jäävät alueet ovat yhtä suuret.

### Huomautuksia ja nimityksiä

-   Merkinnässä $\int_a^b f(x) \, dx$
    -   muuttujaa $x$ kutsutaan *integroimismuuttujaksi*
        -   integroimismuuttuja ei näy tuloksessa ja voidaan vaihtaa,
            $\int_a^b f(x) \, dx = \int_a^b f(t) \, dt \quad$ (kuten
            summausindeksi aiemmin).
    -   lukuja $a$ ja $b$ kutsutaan *integroimisrajoiksi* (alaraja ja
        yläraja)
    -   väli $[a,b]$ on *integroimisväli*
    -   $\int$ on *integraalimerkki*
    -   funktio $f$ on *integroitava*
    -   *differentiaali* $dx$ on pelkkä merkintä; se kertoo, minkä
        muuttujan suhteen integroidaan.

# Integraalin ominaisuuksia ja IVAL

-   Mitä "integraali" tarkoittaa?
-   Määrätyn integraalin ominaisuudet
-   Integraalilaskennan väliarvolause
-   Paloittain määritellyn funktion integraali

\[A, 5.4\]

## Integraali (mitä se on)

-   englanniksi:
    -   "integrate"
        -   (vb) to make or be made into a whole
        -   (adj.) made up of parts
    -   "integral"
        -   (adj.) intact, entire; formed of constituent parts; united
-   suomeksi:
    -   "integroida" = koostaa, yhdistää
-   funktion $f \colon I \to \mathbb{R}$
    -   *integraali yli välin $[a,b]$*, merk. $\int_a^b f(x) \, dx$
        -   määriteltiin [viime luennolla](#maarInt)
            -   suunnilleen "viipaloimalla" ja "koostamalla" funktion
                $f$ kuvaajan ja $x$-akselin väliin jäävä pinta-ala, jos
                $f\geq 0$
                -   vastaavasti pinta-alan vastaluku, jos $f<0$ (ylä- ja
                    alasumma $<0$ )
            -   tähän mennessä vain jatkuville funktioille
                -   voit käyttää "määritelmää" $$
                      \int_a^b f(x) dx = \lim_{n\to \infty} \sum_{k=1}^n f(c_k) \Delta x_k ,
                    $$ missä $\{x_0, x_1, \ldots, x_n\}$ on välin
                    $[a,b]$ jako ja pisteet $c_k \in [a,b]$ voit valita
                    miten haluat (vaikka osavälien päätepisteistä, esim.
                    $c_k = x_k$)
            -   muita nimiä: *määrätty integraali*
        -   on luku!
    -   *integraalifunktio*
        -   on sellainen funktio $F$, jolle $F'(x)=f(x)$ kaikilla
            $x \in I$
            -   muita nimiä: *antiderivaatta*, *primitiivi*,
                *kantafunktio*
            -   funktion $f$ kaikkien antiderivaattojen perhettä
                merkitään $\int f(x) \, dx$
                -   "funktion $f$ integraali"
            -   "integroiminen" tarkoittaa integraalifunktion eli
                antiderivaatan etsimistä
                -   tästä lisää ensi viikolla
        -   on funktio!
-   yllä $I$ on väli, jolla $f$ on määritelty (esim. $\mathbb{R}$ tai
    $]0,\infty[$)
    -   on oltava $[a,b] \subset I$, jotta voidaan kirjoittaa
        $\int_a^b f(x) dx$
        -   (kurssilla Calculus 3 myös "epäoleellinen integraali")
-   Kysymys:
    -   Miksi antiderivaattaa sanotaan integraalifunktioksi?
        -   myöhemmin: APL $\implies$ jos $f$ on jatkuva, sen määrätty
            integraali saadaan laskettua (minkä tahansa) antiderivaatan
            avulla:
            -   $\int_a^b f(x) dx = F(b) -F(a)$
                -   "viipalointia" helpompi tapa laskea tasoalueiden
                    pinta-aloja (ja myöhemmin myös tilavuuksia ym.)
            -   Tästä ensi viikolla! Tällä viikolla **ei vielä
                käytössä**.

## Määrätyn integraalin ominaisuuksia

-   Alla oletetaan, että funktiot $f$ ja $g$ ovat integroituvia jollakin
    suljetulla välillä $I$ ja $a, b \in I$
    -   (huom. jatkuvuus riittää integroituvuuteen).
-   Emme tällä kurssilla todista alla mainittuja ominaisuuksia
    -   piirrä kuvia ja mieti, mitä ominaisuudet tarkoittavat
        pinta-alatulkinnan ja/tai "määritelmän" $$
          \int_a^b f(x) \, dx = \lim_{n\to \infty} \sum_{k=1}^n f(x_k) \Delta x_k, 
          \quad \Delta x_k = x_k - x_{k-1}
        $$ avulla.

### Integroimisrajoista

-   Laajennetaan määrätyn integraalin $$
      \int_a^b f(x) \, dx
    $$ määritelmää hiukan:
    -   tilanne $a=b$ (integroimisvälin pituus on 0)
    -   tilanne $a>b$ (integroimisväli on "väärin päin")
-   Pidetään mielessä sama "määritelmä" kuin aiemmin, $$
      \int_a^b f(x) \, dx = \lim_{n\to \infty} \sum_{k=1}^n f(x_k) \Delta x_k, 
      \quad \Delta x_k = x_k - x_{k-1}
    $$
    -   jos $a=b$, jokaisen osavälin pituus on nolla eli
        $\Delta x_k = 0$ kaikilla $k$, joten $$
          \int_a^a f(x) \, dx = 0
        $$
    -   jos $b<a$, osavälit ovat "väärin päin" eli $x_k < x_{k-1}$,
        joten $\Delta x_k < 0$ ja siksi $$
          \int_a^b f(x) \, dx = - \int_b^a f(x) \, dx.
        $$

### Integraalin lineaarisuus

-   Kun $A$ ja $B$ ovat vakioita, on $$ 
      \int_a^b (Af(x) + Bg(x)) dx = A\int_a^b f(x) \, dx + B\int_a^b g(x) \, dx
    $$ koska sama toimii summalle ([summa on
    lineaarinen](#summanOminaisuuksia)) ja raja-arvolle (ks. Calculus 1,
    kohta 5.1.2 Raja-arvon laskusäännöt).

### Integroimisväleistä

-   Jos aiempien oletusten lisäksi myös $c \in I$, niin $$
      \int_a^b f(x) \, dx +
      \int_b^c f(x) \, dx =
      \int_a^c f(x) \, dx .
    $$
    -   Huom: ei tarvitse olla $a<b<c$ !

### Järjestyksen säilyminen

-   Jos $a<b$ ja $f(x) \leq g(x)$ kaikilla $x \in [a,b]$, niin $$
      \int_a^b f(x) \, dx \leq \int_a^b g(x) \, dx .
    $$

### Itseisarvoista

-   Jos $a\leq b$, niin $$
      \left| \int_a^b f(x) \, dx \right| \leq \int_a^b |f(x)| \, dx
    $$
    -   vrt. kolmioepäyhtälö: $|x+y|\leq |x|+|y|$

### Symmetriasta

-   Jos $f$ on pariton funktio eli $f(-x) = -f(x)$, niin $$
      \int_{-a}^a f(x) \, dx = 0
    $$

-   Jos $f$ on parillinen funktio eli $f(-x) = f(x)$, niin $$
      \int_{-a}^a f(x) \, dx = 2\int_0^a f(x) \, dx
    $$

### Esimerkkejä

1.  Laske määrätty integraali pinta-alatulkinnan ja määrätyn integraalin
    ominaisuuksien avulla: $$
      \int_{-2}^{2} (2+5x) \, dx
    $$
    -   lineaarisuuden avulla $$
          \int_{-2}^{2} (2+5x) \, dx = \int_{-2}^{2} 2 \, dx + 5\int_{-2}^{2} x \, dx
        $$
    -   ensimmäinen osa on suorakaiteen pinta-ala (leveys $=2-(-2) = 4$
        ja korkeus $2$)
    -   toinen osa on nolla, koska integroitava $(x)$ on pariton ja
        integroimisväli on symmetrinen nollan suhteen.
    -   Siis $$
          \int_{-2}^{2} (2+5x) \, dx = 8+0.
        $$
2.  Laske (kuten edellä) $$
      \int_{-3}^3 \sqrt{9-x^2} \, dx .
    $$
    -   Piirrä kuva!
    -   Kyseessä on 3-säteisen origokeskisen ympyrän ylemmän puolikkaan
        pinta-ala; siis $$
          \int_{-3}^3 \sqrt{9-x^2} \, dx = \frac12 \cdot\pi \cdot 3^2 = \frac{9\pi}{2} .
        $$

## Integraalilaskennan väliarvolause (IVAL)

"Jatkuva funktio saavuttaa jossain keskiarvonsa."

Tarkemmin:

-   Jos $f$ on jatkuva välillä $[a,b]$, niin $$
      \int_a^b f(x) \, dx = (b-a)f(c)
    $$ jollakin $c \in [a,b]$.

### Funktion keskiarvo

-   Lukua $$
      \frac{1}{b-a}\int_a^b f(x) \, dx
    $$ sanotaan funktion $f$ *keskiarvoksi* välillä $[a,b]$.
    -   "Jos suorakaiteen leveys on $b-a$, kuinka korkea se on, jotta
        pinta-ala on sama kuin $\int_a^b f(x) \, dx$ ?"

### IVAL, perustelun idea

-   Jatkuvana funktiona $f$ saavuttaa välillä $[a,b]$ suurimman ja
    pienimmän arvonsa joissakin pisteissä $s$ ja $p$ (Calculus 1,
    Weierstrassin lause)
    -   merkitään näitä kirjaimilla $M=f(s)$ (suurin arvo) ja $m= f(p)$
        (pienin arvo).
-   Koska siis $m \leq f(x) \leq M$ kaikilla $x \in [a,b]$, on $$
      m (b-a)=\int_a^b m \, dx \leq \int_a^b f(x) \, dx \leq \int_a^b M \, dx = M(b-a)
    $$ eli $$
      f(p)=m \leq \frac{1}{b-a} \int_a^b f(x) \, dx \leq M = f(s)
    $$
-   Jatkuvien funktioiden väliarvolause (Calculus 1) $\implies$ $f$
    saavuttaa myös kaikki arvot arvojen $f(p)$ ja $f(s)$ väliltä, siis
    jollakin $c$ on $$
      f(c) = \frac{1}{b-a} \int_a^b f(x) \, dx .
    $$

### Esimerkki

1.  Laske funktion $f(x) = 2x$ keskiarvo välillä $[1,5]$.
    -   Lasketaan ensin integraali $$\int_1^5 2x \, dx$$
        -   Piirrä kuva!
        -   Kysytty integraali on puolisuunnikkaan pinta-ala eli
            -   suorakaiteen (leveys $5-1=4$, korkeus $f(1)=2$) ja
            -   kolmion (kanta $4$, korkeus $f(5) - f(1) = 10-2=8$)
                pinta-alojen summa $$
                  4\cdot 2 + \frac12 \cdot 4\cdot 8 = 8+16 = 24.
                $$
    -   Kysytty keskiarvo on $$
          \frac{1}{5-1} \int_1^5 2x \, dx = \frac14 \cdot 24 =  6.
        $$
    -   Tässä tapauksessa tulos olisi ollut helppo päätelläkin...

## Paloittain jatkuvan funktion määrätty integraali

-   Tähän asti olemme käsitelleet määrättyä integraalia
    $$\int_a^b f(x) \, dx$$ vain, kun $f$ on jatkuva välillä $[a,b]$.
-   Käsite yleistyy luonnollisella tavalla "paloittain jatkuville"
    funktioille $f$.

### Paloittain jatkuva funktio

Jos funktio $f$ on määritelty välillä $[a, b]=[c_0, c_n]$ ja

-   jatkuva muualla paitsi äärellisen monessa pisteessä
    $\{c_1, \ldots, c_{n-1}\}$, ja lisäksi
-   jokaisella osavälillä $[c_{k-1}, c_{k}]$ voidaan jatkaa jatkuvaksi
    päätepisteisiin saakka, niin

sanomme, että $f$ on *paloittain jatkuva* välillä $[a,b]$.

-   Huom:
    -   funktion arvoilla pisteissä $\{c_0, c_1, \ldots, c_n\}$ ei ole
        merkitystä
        -   (itse asiassa funktion ei ole tarpeen olla edes määritelty
            näissä pisteissä)
    -   funktio ei voi karata äärettömiin missään välin $[a,b]$
        pisteessä
        -   tällöin jatkaminen jatkuvaksi ko. pisteeseen ei olisi
            mahdollista
            -   "jatkuvaksi jatkaminen": ks. Calculus 1, kohta 7.6
    -   yllä oleva paloittain jatkuvuuden määritelmä on sama kuin
        seuraava:
        -   kullekin välille $[c_{k-1}, c_{k}]$ löydetään jatkuva
            funktio $\tilde{f}_k$, joka avoimella välillä
            $]c_{k-1}, c_{k}[$ saa samat arvot kuin $f$ eli $$
              \tilde{f}_k (x) = f(x) \quad \text{ kaikilla } x \in ]c_{k-1}, c_{k}[ .
            $$

### Paloittain jatkuvan funktion määrätty integraali

"Lasketaan jatkuvien palojen integraalit yhteen."

-   Edellisen kohdan viimeisen huomautuksen merkinnöin $$
      \int_{c_0}^{c_n} f(x) \, dx = \sum_{k=1}^n \int_{c_{k-1}}^{c_k} \tilde{f}_k (x) \, dx .
    $$

**Esimerkki**

1.  Laske $\int_0^3 f(x) \, dx$, kun $$
      f(x) = \begin{cases}
    \sqrt{1-x^2}, \quad \text { kun } 0 \leq x \leq 1 \\
    2, \quad \text { kun } 1< x \leq 2 \\
    x-2, \quad \text { kun } 2< x \leq 3.
      \end{cases}
    $$
    -   Piirrä kuva!
    -   Integraalin arvo on seuraavien pinta-alojen summa:
        -   yksikkökiekon neljännes
        -   suorakulmio, jonka kanta on 1 ja korkeus 2
        -   kolmio, jonka kanta on 1 ja korkeus 1.
    -   Tulos on $\frac{\pi+10}{4}$.

# Antiderivaatta, APL ja integrointitekniikoita

-   Antiderivaatta
-   Analyysin peruslause
-   Derivointisäännöistä saatavia integrointikaavoja
-   Sijoitusmenetelmä
-   Trigonometrisistä funktioista

\[A, 2.10, 5.5, 5.6\]

### Lämmittelytehtävä

1.  Ota kaksi paperia.
    -   Kirjoita 1. paperille jokin funktio (keksi itse).
    -   Kirjoita 2. paperille äsken keksimäsi funktion derivaatta.
2.  Anna 2. paperi toiselle opiskelijalle.
3.  Tutki saamaasi lauseketta:
    -   minkä funktion derivaatta tämä voisi olla?
        -   Kirjoita arvauksesi alkuperäisestä funktiosta saamallesi
            paperille.
4.  Anna paperi takaisin lähettäjälle.
5.  Tutki takaisin saamaasi paperia:
    -   Menikö arvaus oikein?
        -   Mikä meni pieleen?
6.  Mieti, mitä keinoja käytit "arvaamiseen".

## Antiderivaatta

### Määritelmä

Funktion $f:I\to \mathbb{R}$ *antiderivaatta* välillä $I$ on mikä
tahansa sellainen funktio $F:I\to \mathbb{R}$, jolle $$
      F'(x) = f(x) \quad \text{ kaikilla } x \in I .
    $$

-   Huomautuksia:
    -   Antiderivaatta on siis välttämättä derivoituva (välillä $I$ ) -
        ja siten myös jatkuva.
    -   Muita nimityksiä:
        -   integraalifunktio
            -   joissakin lähteissä tämä nimitys on muussa käytössä
        -   primitiivi(funktio)
        -   kantafunktio
    -   Kaikilla funktioilla ei ole antiderivaattaa!
        -   Joillakin on, ja löydetään helposti
            -   esim. polynomeille.
        -   Joillakin on, mutta sen kirjoittaminen "lausekkeena" ei
            onnistu (tästä lisää myöhemmin).
        -   Joillakin ei ole.
    -   Määritelmä ei anna keinoja löytää annetun funktion
        antiderivaattaa
        -   vaan ehdon, jonka täyttyessä annettu $F$ on annetun $f$
            antiderivaatta annetulla välillä $I$.
        -   Siis "ehdokas" antiderivaataksi pitää keksiä itse, jos vain
            $f$ on annettu.
            -   Tällä kurssilla joitakin "arvauskeinoja", kurssilla
                Calculus 3 lisää.
        -   Myös väli $I$ pitää miettiä itse:
            -   "millä välillä tämä tieto on voimassa".

**Esimerkkejä**

1.  Funktio $F(x) = x$ on funktion $f(x) = 1$ antiderivaatta millä
    tahansa välillä, koska $F'(x) = 1=f(x)$ kaikilla $x\in \mathbb{R}$.

2.  Funktio $F(x) = \frac{1}{3}x^3 + 7$ on funktion $f(x) = x^2$
    antiderivaatta millä tahansa välillä, koska
    $F'(x) = \frac13 \cdot 3x^2 +0 = x^2=f(x)$ kaikilla
    $x\in \mathbb{R}$.

3.  Funktio $F(x) = -\frac{1}{x}$ on funktion $f(x) = \frac{1}{x^2}$
    antiderivaatta millä tahansa välillä, joka ei sisällä nollaa,
    -   siis vaikkapa väleillä $]-100, 0[$ tai $]0, 2[$, mutta ei
        välillä $]-1,2[$,
    -   koska
        $$F'(x) = \frac{1}{x^2} = f(x) \quad \text{ kaikilla } x\not = 0 .$$
        -   Voidaan merkitä "$I=]-\infty, 0[$ tai $I=]0, \infty[$".

### Yksikäsitteisyys

-   Antiderivaatta ei ole yksikäsitteinen:
    -   Jos funktio $F$ on funktion $f$ antiderivaatta, myös $F+3$ on,
        ja mikä tahansa $F+C$, $C\in \mathbb{R}$ vakio.
-   Antiderivaatta on "vakiota vaille yksikäsitteinen":
    -   Jos $F$ ja $G$ ovat saman funktion antiderivaattoja, niin $F-G$
        on vakio.
        -   Perustelu: välillä $I$ on $$
              \frac{d}{dx}(F(x) - G(x))= F'(x) - G'(x) = f(x) - f(x) = 0,
            $$ joten $F(x)-G(x)=C$ jollain $C\in \mathbb{R}$ kaikilla
            $x\in I$
            -   (tämä seuraa differentiaalilaskennan väliarvolauseesta,
                ks. Calculus 1, 11.4.1.2).

**Huomautuksia**

1.  Kaikki edellä mainittu pätee vain kullakin välillä $I$ erikseen;
    esim. funktion
    $$ f(x) = \begin{cases} \,1, \quad x> 0 \\ -1, \quad x<0\end{cases}$$
    derivaatta on nolla kaikilla $x \not = 0$, mutta $f$ ei ole vakio.
    -   Siis eri väleillä $I$ vakio voi vaihtua.

### Määräämätön integraali

-   Funktion $f$ kaikkia antiderivaattoja merkitään $$
      \int f(x) \, dx
    $$ eli välillä $I$ on $$
      \int f(x) \, dx = F(x) + C, \quad C \in \mathbb{R},
    $$ jos $F'(x) = f(x)$ kaikilla $x \in I$.
    -   Vakiota $C$ kutsutaan *integroimisvakioksi*.

## Analyysin peruslause (APL)

Analyysin peruslauseessa oletetaan, että $f$ on jatkuva välillä $I$.

### Osa 1

-   Jos $f$ on jatkuva välillä $I$ ja $a\in I$, niin funktio $$
      F(x) = \int_a^x f(t) \, dt
    $$ on derivoituva välillä $I$ ja $F'(x) = f(x)$ kaikilla $x \in I$.
    -   Siis $$
          \frac{d}{dx} \int f(t) \, dt = f(x) .
        $$

### Osa 2

-   Jos $f$ on jatkuva välillä $I$ ja $G(x)$ on jokin funktion $f$
    antiderivaatta, niin kaikilla $a,b\in I$ on $$
      \int_a^b f(x) \, dx = G(b) -G(a).
    $$

### Perustelut

-   Osa 1: $$
      F'(x) = ... = \lim_{h \to 0} \frac{1}{h} \int_x^{x+h} f(t) \, dt
    $$ IVAL $\implies$ $$
      F'(x) = \lim_{h_to 0}f(c_h)
    $$ jollakin $c_h$ lukujen $x$ ja $x+h$ välissä;
    $h\to 0 \implies c_h \to x$, joten $$
      F'(x) = \lim_{c \to x}f(c) = f(x),
    $$ koska $f$ on jatkuva.

-   Osa 2:
    -   Merkitään $F(x) = \int_a^x f(t) \, dt$.
    -   Nyt $F(a)=0$ ja $\int_a^b f(t) \, dt = F(b) = F(b)-F(a)$.
    -   Jos $G'(x) = f(x)$, niin $G(x) = F(x) + C$ jollakin
        $C \in \mathbb{R}$ ja $$
          \int_a^b f(t) \, dt = F(b)-F(a) = (G(b)-C) -(G(a) -C) = G(b) - G(a).
        $$

### Sijoitusmerkintä

-   Joskus käytetään nk. sijoitusmerkintää: $$
      \bigg/^b_{\!\!\!\!\!\! a} F(t) = F(b) - F(a)
    $$
-   Joissakin lähteissä sijoitusmerkintä on eri muodossa: $$
       F(t)\bigg|_a^b = F(b) - F(a)
    $$

### Esimerkkejä

1.  $$
      \int_{-1}^2 (x^2-3x +2) dx = ... = \frac{9}{2} .
    $$

2.  Laske käyrän $y=3x-x^2$ ja $x$-akselin väliin jäävä pinta-ala.
    -   Leikkauspisteet: $3x-x^2 = 0 \iff x = 0$ tai $x = 3$
    -   Piirrä kuva.
    -   Pinta-ala on $$
          A = \int_0^3  (3x-x^2) \, dx = ... = \frac{9}{2} .
        $$
3.  Laske pinta-ala, joka jää käyrän $y=\sin x$ alapuolelle ja käyrän
    $y=0$ yläpuolelle suorien $x=0$ ja $x=\pi$ väliin.
    -   Piirrä kuva!
    -   Pinta-ala on $$
          A = \int_0^{\pi} \sin x \, dx = ... = 2 .
        $$

#### Integroimisrajana funktio

-   Derivoidaan $$\int_a^{g(x)} f(t) \, dt$$ yhdistetyn funktion
    derivoimissäännöllä, F(g(x)): $$
      \frac{d}{dx} \int_a^{g(x)} f(t) \, dt = f(g(x)) g'(x).
    $$

**Esimerkki**

1.  $$
      \frac{d}{dx} \int_2^{3x} \cos t \, dt = 3\cos(3x)
    $$

## Integrointikaavoja

-   Derivointikaavoista saadaan suoraan integrointikaavoja: $$
      \begin{array}{rl}
    \int x^r \, dx &= \frac{1}{r+1} x^{r+1} + C \quad (r \not = -1) \\
    \int x^{-1} \, dx &= \log |x| + C \\
    \int \sin x \, dx &= - \cos x + C \\
    \int \cos x \, dx &= \sin x + C \\
    \int \frac{1}{\sqrt{1-x^2}} \, dx &= \arcsin x + C \\
    \int \frac{1}{1+x^2} \, dx &= \arctan x + C \\
    \int e^x \, dx &= e^x+C
      \end{array}
    $$
    -   Perustele kaavat itsellesi derivointikaavojen avulla.
        -   Mieti myös, millä välillä / väleillä tulos on voimassa.

## Integroinnin lineaarisuus

-   Jos $A$ ja $B$ ovat vakioita, niin $$
      \int (Af(x) + B g(x)) \, dx = A\int f(x) \, dx + B \int g(x) \, dx .
    $$

### Sijoitusmenetelmä eli muuttujanvaihto

-   Yhdistetyn funktion derivointisäännöstä: $$
      \int f'(g(x))g'(x) \, dx = f(g(x)) + C
    $$

-   Apuna voi käyttää merkintöjä $$
      u= g(x), \quad  du=g'(x) dx
    $$
    -   tämä (merkintä differentiaalien $du$ ja $dx$ avulla) tulee
        helposti merkinnästä $\frac{du}{dx} = g'(x)$; siis $$
          \int f'(g(x))g'(x) \, dx = \int f'(u) du = f(u) + C
          = f(g(x)) + C
        $$
-   Aina ei onnistu! (Joihinkin integrointeihin ei löydy sopivaa
    sijoitusta.)
-   Sijoitusta $u=g(x)$ kannattaa yrittää erityisesti silloin, kun
    -   derivaatta $g'(x)$ on tekijänä integroitavassa funktiossa.
-   Esimerkkejä \[A, s. 319\]

## Trigonometristen funktioiden integroinnista

-   Sini ja kosini nähtiin jo; entä tangentti? $$
      \int \tan x \, dx = \log |\tfrac{1}{\cos x}| +C
    $$
    -   laske!
    -   millä välillä voimassa?
-   Funktioiden $f(x)=\sin^n \!\! x \, \cos ^m \!\! x$ integroinnissa
    muunnoskaavoista on apua:
    -   jos $m$ tai $n$ on positiivinen pariton kokonaisluku,
        Pythagoraan lause auttaa,
        -   esim.
            $\int \sin ^3 x \, \cos ^8 x \, dx = \int \sin x (1-\cos^2x) \cos^8 x = \ldots =\frac{1}{11} \cos^{11}x - \frac{1}{9} \sin^9x + C$.
    -   jos molemmat ovat parillisia, käytetään kosinin kaksinkertaisen
        kulman kaavaa $\cos 2x = \cos^2 x - \sin ^2 x$
        -   on helpompi integroida moninkertaisen kulman kosinia kuin
            sinin tai kosinin parillista potenssia
        -   esim.
            $\int \cos^2 x \, dx = \frac12 \int (1+\cos 2x)\, dx = \frac{x}{2} + \frac14 \sin 2x + C = \frac12 (x + \sin x \cos x) + C$
            -   viimeinen muoto saadaan edellisestä kaavan
                $\sin 2x = 2 \sin x \cos x$ avulla.

# Tasoalueen pinta-ala

-   käyrien väliin jäävän alueen pinta-ala

\[A, 5.7\]

### Johdannoksi

-   Mitä eroa on määrätyllä integraalilla ja pinta-alalla?

-   Esim.
    -   Laske määrätty integraali $$
          \int_{-2}^2 x \, dx.
        $$ (Tulos on 0.)
    -   Mikä on käyrien $y=x$, $y=0$, $x=-2$ ja $x=2$ rajoittaman alueen
        pinta-ala?
        -   Piirrä kuva.
        -   Tulos on 4 (eikä nolla).

## Pinta-alasta ja määrätystä integraalista

-   Pinta-ala
    -   on aina $\geq 0$
    -   voidaan laskea määrätyn integraalin avulla:
        -   käyrien $y=f(x)$, $y=0$, $x=a$ ja $x=b$ rajoittaman alueen
            pinta-ala on $$
              A = \int_a^b |f(x)| \, dx.
            $$
-   Määrätty integraali $\int_a^b f(x) \, dx$
    -   voi olla positiivinen, negatiivinen tai nolla
    -   voidaan laskea kuvasta pinta-alojen avulla, jos vastaavat
        pinta-alat tunnetaan
        -   esim. kolmiot, suorakaiteet, ympyrät
        -   $x$-akselin alapuolella olevat pinta-alat lasketaan mukaan
            negatiivisina.

**Esimerkki**

1.  Mikä on käyrien $y=\cos x$, $y=0$, $x=0$ ja $x=\frac{3\pi}{2}$
    rajaaman alueen pinta-ala?
    -   Piirrä kuva!
    -   Pinta-ala $A$ on $$
          A=\int_0^{\frac{3\pi}{2}} |\cos x | \, dx
        $$
    -   Ei tätä osata laskea suoraan minkään derivointikaavan avulla!
        -   Pitää "poistaa itseisarvot" (miten?)
    -   Etsitään pisteet, joissa integroitavan merkki välillä
        $[0, \frac{3\pi}{2}]$ voi vaihtua: $$
          x \in [0, \tfrac{3\pi}{2}] \, \text{ ja } \, \cos x = 0 \iff 
          x = \tfrac{\pi}{2} \, \text{ tai } \, x = \tfrac{3\pi}{2}
        $$
    -   Poistetaan itseisarvot: välillä $[0, \frac{3\pi}{2}]$ $$
          |\cos x | = \begin{cases} 
        \cos x, \text{ kun } x \in [0, \tfrac{\pi}{2}] \\
        - \cos x, \text{ kun } x \in ]\tfrac{\pi}{2}, \tfrac{3\pi}{2}[
        \end{cases}
        $$
    -   Lasketaan: $$
          A = \int_0^{\frac{3\pi}{2}} |\cos x | \, dx 
          = \int_0^{\frac{\pi}{2}} \cos x  \, dx + \int_{\frac{\pi}{2}}^{\frac{3\pi}{2}} -\cos \, dx \\
          = \bigg/_{\!\!\!\! 0}^{\frac{\pi}{2}} \sin x - \bigg/_{\!\!\!\! \frac{\pi}{2}}^{\frac{3\pi}{2}} \sin x
          = (1-0) - (-1-1) = 3
        $$

## Käyrien väliin jäävän alueen pinta-ala

Tarkastellaan vain jatkuvia käyriä.

-   Useimmiten tutkimamme käyrät ovat jatkuvien funktioiden kuvaajia.

-   Jos funktiosi eivät ole jatkuvia, piirrä kuva ja
    -   paloittele kysymyksesi tilanteisiin, joissa funktiot ovat
        jatkuvia (ja niiden kuvaajat siis jatkuvia käyriä).
-   Jos tutkit käyrää, joka ei ole funktion kuvaaja,
    -   mieti,
        -   onko käyrä yhdiste joidenkin funktioiden kuvaajista
            -   esim. $x^2+y^2=1 \iff y=\pm \sqrt{1-x^2}$
        -   tai onko helpompi integroida muuttujan $y$ suhteen
            -   esim. $x=y^2$ (piirrä kuva!)
    -   piirrä kuva ja ratkaise ongelma tarvittaessa paloissa, joissa
        voit käyttää em. funktioita.

### Jos käyrät eivät leikkaa

-   Jos funktiot $f$ ja $g$ ovat välillä $[a,b]$ jatkuvia ja
    $f(x) \leq g(x)$ kaikilla $x \in [a,b]$, niin käyrien $y=f(x)$,
    $y=g(x)$, $x=a$ ja $x=b$ rajaaman alueen pinta-ala on $$
      A = \int_a^b \left( g(x) - f(x) \right)  \, dx .
    $$
    -   Piirrä kuva...

**Huomautuksia**

-   "Käyrät eivät leikkaa" tarkoittaa, että
    -   toinen käyrä on aina toisen yläpuolella, koska funktiot ovat
        jatkuvia.
-   Funktioiden $f$ ja $g$ merkillä ei ole väliä
    -   (voivat olla toinen tai vaikka molemmat negatiivisia)
    -   oleellista on, että järjestys ei vaihdu
        -   eli $f(x) \leq g(x)$ kaikilla $x \in [a,b]$
        -   eli $g(x) - f(x) \geq 0$ kaikilla $x \in [a,b]$.

### Yleisesti (käyrät voivat leikata)

-   Jos funktiot $f$ ja $g$ ovat välillä $[a,b]$ jatkuvia, niin käyrien
    $y=f(x)$, $y=g(x)$, $x=a$ ja $x=b$ rajaaman alueen pinta-ala on $$
      A = \int_a^b |g(x)-f(x)| \, dx.
    $$
    -   Piirrä kuva...

**Huomautuksia**

-   Kuten aiemmin, integroitava väli on pilkottava osiin,
    -   joissa erotus $g(x) - f(x)$ ei vaihda merkkiään,
    -   jotta itseisarvot saadaan poistettua ja integraali laskettua.
-   Funktioiden $f$ ja $g$ merkillä ei ole väliä,
    -   vain erotuksen $g-f$ merkillä on
    -   eli sillä, kumpi käyrä on ylempänä eli kumpi funktioista saa
        suurempia arvoja.
-   Itseisarvojen sisällä kirjoitusjärjestyksellä ei ole väliä:
    -   $|g(x) - f(x)| = |f(x) -g(x)|$.

## Esimerkkejä

1.  Laske käyrien $y=x^2-2x$ ja $y=4-x^2$ väliin jäävän alueen
    pinta-ala.
    -   Piirrä kuva.
    -   Laske leikkauspisteet: $$
          x^2- 2x = 4-x^2 \iff x=2 \, \text{ tai } \, x = -1
        $$
    -   Katso kuvaa ja mieti, mitä olet tekemässä.
    -   Kun $x \in [-1, 2]$, on $4-x^2 \geq x^2-2x$, joten $$
          A = \int_{-1}^2 |(x^2-2x) - (4-x^2)| \, dx \\
          = \int_{-1}^2 ((4-x^2)- (x^2-2x)) \, dx \\
          = \int_{-1}^2 (4-3x^2+2x) \, dx = \ldots = 9
        $$
2.  Kuinka suuri pinta-ala jää käyrien $y=\sin x$ ja $y=\cos x$ väliin,
    kun $x \in [0, 2\pi]$?
    -   Piirrä kuva.
    -   Laske leikkauspisteet: välillä $[0,2\pi]$ on
        $$\sin x = \cos x \iff x= \frac{\pi}{4} \, \text{ tai } \, x = \frac{5\pi}{4}$$
    -   Selvitä, missä sini on suurempi, missä kosini.
    -   Laske ala: $$
          \begin{array}{rl}
          A &= \int_0^{2\pi} |\sin x - \cos x| \, dx \\
          &= \int_0^{\frac{\pi}{4}} (\cos x - \sin x) \, dx + \int_{\frac{\pi}{4}}^{\frac{5\pi}{4}} (\sin x - \cos x) \, dx + \int_{\frac{5\pi}{4}}^{2\pi} (\cos x - \sin x) \, dx \\
          &= \ldots = 4 \sqrt{2} .
          \end{array}
        $$
3.  Alue rajoittuu vasemmalla paraabeliin $x=y^2-12$ ja oikealla suoraan
    $y=x$. Mikä on alueen pinta-ala?
    -   Piirrä kuva.
    -   Laske leikkauspisteet: $y^2-12 = y \iff y=4$ tai $y=-3$
    -   Kun $-3\leq y \leq 4$, on $y^2 - 12 \leq y$, joten $$
          A = \int_{-3}^4 (y-(y^2-12)) \, dy = ... = \frac{343}{6} .
        $$
        -   Saman tuloksen saa integroimalla muuttujan $x$ suhteen,
            mutta lasku on hankalampi: $$
              \int_{-12}^{-3} (\sqrt{12+x}-(-\sqrt{12+x})) \, dx + \int_{-3}^4 (\sqrt{12+x}-x) \, dx
            $$
            -   huomaa, että tässä on laskettava erikseen osa, jossa
                alarajana on paraabelikäyrä, ja osa, jossa alarajana on
                suora.

# Paluu derivaattaan

-   Linearisointi
-   Approksimointi ja virhe
-   Taylorin polynomit
-   Landaun iso-O -merkintä
-   koneellisia ongelmia ja muuta jännää

\[A, 4.9-4.11 + muuta\]

## Lineaarinen approksimointi

-   Aiemmin: muutoksen approksimointi derivaatan avulla (Calculus 1,
    13.3)
-   Nyt: sama idea, kirjoitetaan funktioksi $L(x)$

### Määritelmä

-   Funktion $f$ *lineaarinen approksimaatio* pisteen $a$ lähistöllä on
    funktio $$
      L(x) = f(a) + f'(a) (x-a).
    $$

**Huomautuksia**

-   Funktion $f$ pitää siis olla derivoituva.
-   Funktiota $L$ kutsutaan myös funktion $f$ *linearisaatioksi*
    pisteessä $a$, ja sen arvoja funktion $f$ arvojen *lineaarisiksi
    approksimaatioiksi* (pisteen $a$ lähellä).
-   Merkintä $\approx$ tarkoittaa approksimaatiota ja luetaan
    "likimain", siis $$
      f(x) \approx L(x), \text{ kun } x \text{ on lähellä lukua } a
    $$
    -   huom: $f(a) = L(a)$
-   Funktion $L$ kuvaaja on funktion $f$ kuvaajalle kohtaan $x=a$
    piirretty tangenttisuora.

**Esimerkkejä**

1.  Funktion $f(x) = \sqrt{1+x}$ lineaarinen approksimaatio nollan
    lähellä:
    -   $f(0) = 1$
    -   $f'(x) = \frac{1}{2\sqrt{1+x}}$, joten $f'(0)=\frac12$
    -   Siis $$
          L(x) = 1+ \frac12 (x-0) = 1+\frac{x}{2} .
        $$
2.  Arvioi lukua $\sqrt{26}$ käyttäen funktion $f(x)=\sqrt{x}$
    linearisointia kohdassa $x=25$.
    -   \[A, s. 268-269\]

### Approksimaatiovirhe

Aina, kun suuretta approksimoidaan eli arvioidaan toisella, syntyy
virhettä: $$
  \text{virhe } = \text{ oikea arvo } - \text{ approksimaatio}
$$ eli ylläkäytetyin merkinnöin $$
  E(x) = f(x)-L(x) .
$$

-   Kuvassa $E(x)$ on kuvaajan pisteen ja vastaavan tangenttisuoran
    pisteen välinen pystysuora etäisyys
    -   pieni, kun $x$ on lähellä lukua $a$
    -   erityisesti pieni verrattuna lukujen $x$ ja $a$ väliseen
        (vaakasuoraan) etäisyyteen.

#### Lineaarisen approksimaation virhe

-   Lineaarisessa approksimoinnissa virhettä voidaan arvioida toisen
    derivaatan avulla: $$
      E(x) = \frac{f''(s)}{2} (x-a)^2
    $$ jollain $s \in [a,x]$ tai $s\in[x,a]$, jos toinen derivaatta
    olemassa välillä, joka sisältää luvut $a$ ja $x$.
    -   Perustelu yleistetyn differentiaalilaskennan väliarvolauseen
        avulla \[A, s. 270\].
-   Seurauksia:
    -   jos $f''$ ei vaihda merkkiään lukujen $x$ ja $a$ välillä, niin
        virheen merkki tunnetaan:
        -   jos $f''(t) > 0$ kaikilla $t\in[a,x]$ tai $t\in[x,a]$, niin
            $E(x)>0$
            -   vrt. "tangenttisuora kulkee kuvaajan $y=f(x)$
                alapuolella", $f$ on konveksi
        -   jos $f''(t) < 0$ kaikilla $t\in[a,x]$ tai $t\in[x,a]$, niin
            $E(x)<0$
            -   vrt. "tangenttisuora kulkee kuvaajan $y=f(x)$
                yläpuolella", $f$ on konkaavi
    -   jos $|f''(t)|<K$ kaikilla $t\in[a,x]$ tai $t\in[x,a]$, niin
        $|E(x)|<\frac{K}{2} (x-a)^2$

**Esimerkki**

3.  Arvioi virhettä esimerkin 2 arviossa.
    -   \[A, s. 270-271\]

## Taylorin polynomit

Johdatteluksi:

-   "Linearisaatio" $L$ edellä:
    -   "suora, joka lähellä pistettä $a$ kuvaa funktion $f$ kulkua
        paremmin kuin mikään muu suora"
    -   eli paras 1. asteen polynomiapproksimaatio
        -   pisteessä $a$ arvo on sama, $L(x)=f(x)$
        -   pisteessä $a$ derivaatta on sama, $L'(a) = f'(a)$
-   Taylorin polynomit:
    -   mikä $n$:nnen asteen polynomi kuvaa parhaiten funktion kulkua
        pisteen $a$ lähellä?
        -   pisteessä $a$ arvo on sama, $P_n (a) = f(a)$
        -   pisteessä $a$ kaikkien $n$ ensimmäisen derivaatan arvo on
            sama,
            -   $L'(a) = f'(a), \quad$
                $L''(a) = f''(a), \, \ldots, \quad L^{(n)}(a) = f^{(n)}(a)$

### Määritelmä

-   Funktion $f$ pisteessä $x=a$ kehitetty *Taylorin polynomi* astetta
    $n$ on $$
      P_n (x) = f(a) + f'(a) (x-a) + \frac{f''(a)}{2!} (x-a)^2 + \frac{f'''(a)}{3!} (x-a)^3 + \ldots + \frac{f^{(n)}(a)}{n!} (x-a)^n ,
    $$ kun $f$ on riittävän siisti (eli kun $f^{(n)}$ on olemassa
    pisteen $a$ lähistöllä).

**Huomautuksia**

-   Sanotaan myös "$P_n$ on funktion $f$ $n$-asteinen Taylorin polynomi
    pisteessä $a$".
-   Pistettä $a$ sanotaan *kehityskeskukseksi*.
    -   Samalla funktiolla $f$ on eri Taylorin polynomi $P_n$ eri
        kehityskeskuksissa!
        -   Oikeastaan pitäisi siis merkitä $P_{n,a} (x)$, mutta
            käytetään yksinkertaisempaa merkintää $P_n$ ja mainitaan
            kehityskeskus $a$ erikseen.
-   Jos $f$ on $k$:nnen asteen polynomi, sen Taylorin polynomit "astetta
    $n$" ovat itse asiassa korkeintaan $k$:nnen asteen polynomeja.
-   Taylorin polynomia nollassa sanotaan myös *Maclaurinin polynomiksi*.
-   Taylorin polynomi voidaan kirjoittaa myös summamerkinnän avulla: $$
      P_n (x) = f(a) + \sum_{k=1}^n \frac{f^{(k)}(a)}{k!} (x-a)^k
    $$

### Esimerkkejä

1.  Funktion $f(x) = x^2+5$ kolmannen asteen Taylorin polynomi pisteessä
    $x=1$?
    -   Lasketaan derivaatat: $f'(x) = 2x$, $f''(x) = 2$, $f'''(x) = 0$
    -   Lasketaan funktion ja derivaattojen arvot pisteessä $x=1$:
        $f(1) = 6, \, f'(1) = 2, \, f''(1) = 2,\, f'''(1) = 0$ $$
          P_3 (x) = f(1) + f'(1) (x-1) + \frac{f''(1)}{2} (x-1)^2 + \frac{f'''(1)}{3!} (x-1)^3 \\
          = 6 + 2(x-1) +1(x-2)^2 + 0(x-1)^3 = 6 + 2(x-1) + (x-1)^2 \\
          = \ldots = f(x)
        $$
2.  Laske $P_2$ pisteessä $x=25$ funktiolle $f(x) = \sqrt{x}$.
    -   \[A, s. 273\]

### Taylorin kaava

-   Taylorin polynomi on approksimaatio: $$
      f(x) \approx P_n(x)
    $$
    -   kuinka suuri on virhe $f(x) - P_n(x)$ ?
-   *Taylorin kaava* on tarkka: $$
      f(x) = f(a) + f'(a) (x-a) + \frac{f''(a)}{2!} (x-a)^2 + \frac{f'''(a)}{3!} (x-a)^3 + \\ 
      \ldots +  \frac{f^{(n)}(a)}{n!} (x-a)^n + \frac{f^{(n+1)}(s)}{(n+1)!} (x-a)^{n+1}
    $$ jollakin $s \in [a,x]$ tai $s\in [x, a]$.

**Huomautuksia**

-   Taylorin kaava antaa funktiolle esityksen polynomin ja nk.
    *jäännöstermin* avulla
    -   esitys ei siis ole enää polynomi (paitsi jos $f$ on polynomi)
    -   esityksessä on mukana "uusi muuttuja" $s$
-   Käyttö:
    -   virhearvioinnit $$
          E(x) = f(x) - P_n(x) = \frac{f^{(n+1)}(s)}{(n+1)!} (x-a)^{n+1}
        $$
-   Taylorin kaavalle on myös muita muotoja
    -   eli jäännöstermille on myös muita muotoja
    -   yllä mainittua jäännöstermin muotoa kutsutaan *Lagrangen
        muodoksi*

**Esimerkkejä**

3.  Arvioi lukua $\sqrt{26}$ käyttäen funktion $f(x)$ toisen asteen
    Taylorin polynomia pisteessä $x=25$. Arvioi approksimaatiossa
    tekemääsi virhettä ja etsi pienin (näillä tiedoilla löydettävä)
    väli, jolla $\sqrt{26}$ varmasti on.

### Landaun O-merkintä

-   Käytämme merkintää $$
      f(x) = O (u(x)), \quad  \text{ kun } x \to a
    $$ tarkoittamaan, että epäyhtälö $$
      |f(x)| \leq K |u(x)|
    $$ on voimassa jollakin vakiolla $K$ ja jollakin avoimella välillä
    luvun $a$ ympärillä.

-   Vastaavasti $$
      f(x) = g(x) + O (u(x)), \quad  \text{ kun } x \to a
    $$ joss $$
      f(x)-g(x) = O (u(x)), \quad  \text{ kun } x \to a .
    $$

-   Esim. $\sin x = O(x)$, kun $x \to 0$.

### Taylorin polynomien yksikäsitteisyydestä

-   Jos $f(x) = Q_n(x) + O((x-a)^{n+1})$, kun $x \to a$ jollakin
    korkeintaan $n$:nnen asteen polynomilla $Q_n$, niin tämä on funktion
    $f$ Taylorin polynomi astetta $n$ pisteessä $a$.

### Taylorin kaava, esimerkkejä

### Esimerkkejä

-   Taylorin polynomi laskematta derivaattoja

-   Taylorin polynomien käyttö raja-arvojen laskemisessa (vrt.
    l'Hospitalin sääntö)

## Tietokoneella laskemisesta

-   Tietokone laskee polynomeilla

### Pyöristys- ja katkaisuvirhe

-   Virheistä ei päästä eroon...

# Sovelluksia ja kertaus

-   derivaatan ja integraalin sovelluksia eri aloilta
-   lyhyt kertaus kurssin aiheista
-   katsaus tuleviin kursseihin
-   kyselytunti