import unittest

from timApp.document.translation.translationparser import (
    Translate,
    NoTranslate,
    get_translate_approvals,
    tex_collect,
    span_collect,
)


class TestParser(unittest.TestCase):
    def test_get_translate_approvals_attr(self):
        # TODO Add cases for identifiers, key-value -pairs and multiple classes as well
        text = "Tässä on kuva ![kissasta](/kuvat/kissa.png). [Tosi]{.red} hieno, eikös?"
        self.assertEqual(
            get_translate_approvals(text),
            [
                [
                    Translate("Tässä on kuva "),
                    NoTranslate("!["),
                    Translate("kissasta"),
                    NoTranslate("](/kuvat/kissa.png)"),
                    Translate(". "),
                    NoTranslate("["),
                    Translate("Tosi"),
                    NoTranslate("]{.red}"),
                    Translate(" hieno, eikös?"),
                ]
            ],
        )

    # For .notranslate style, might move elsewhere. For future use.
    def test_notranslate_style(self):
        text = r"tässä on [teksti]{.notranslate}, jota ei käännetä."
        self.assertEqual(
            get_translate_approvals(text),
            [
                [
                    Translate(r"tässä on "),
                    NoTranslate(r"[teksti]{.notranslate}"),
                    Translate(r", jota ei käännetä."),
                ]
            ],
        )

        text = """``` {plugin="csPlugin" #btn-tex2 .notranslate .miniSnippets}
header: Harjoittele matemaattisen vastauksen kirjoittamista.
```"""
        self.assertEqual(
            get_translate_approvals(text),
            [
                [
                    # TODO Should the plugins contain the attributes or no?
                    NoTranslate(
                        """```
header: Harjoittele matemaattisen vastauksen kirjoittamista.
```"""
                    )
                ]
            ],
        )

        text = "Jyväskylän yliopisto sijaitsee paikassa nimeltä [Keski-Suomi]{.notranslate}"
        self.assertEqual(
            get_translate_approvals(text),
            [
                [
                    Translate("Jyväskylän yliopisto sijaitsee paikassa nimeltä "),
                    NoTranslate("[Keski-Suomi]{.notranslate}"),
                ]
            ],
        )

    def test_tex_collect(self):
        # TODO Add cases for identifiers, key-value -pairs and multiple classes as well
        text = r"x^3-49x&=0 &&|\text{ erotetaan yhteinen tekijä x}\x(x^2-49)&=0 &&|\text{ käytetään tulon nollasääntöä}\x=0\;\;\;\textrm{tai}\;\;\;&x^2-49=0 &&|\textsf{ ratkaistaan x}\&\;\;\;\;\;\,\;\;x^2=49 \&\;\;\;\;\;\,\;\;\;\;x=7\;\mathsf{tai}\;x=-7"
        self.assertEqual(
            tex_collect(text),
            [
                NoTranslate(r"x^3-49x&=0 &&|\text{"),
                Translate(r" erotetaan yhteinen tekijä x"),
                NoTranslate(r"}\x(x^2-49)&=0 &&|\text{"),
                Translate(r" käytetään tulon nollasääntöä"),
                NoTranslate(r"}\x=0\;\;\;\textrm{"),
                Translate(r"tai"),
                NoTranslate(r"}\;\;\;&x^2-49=0 &&|\textsf{"),
                Translate(r" ratkaistaan x"),
                NoTranslate(
                    r"}\&\;\;\;\;\;\,\;\;x^2=49 \&\;\;\;\;\;\,\;\;\;\;x=7\;\mathsf{"
                ),
                Translate(r"tai"),
                NoTranslate(r"}\;x=-7"),
            ],
        )

    # Testing simple text inside latex
    def test_tex_collect_simple_text(self):
        text = r"\text{testi}\x"
        self.assertEqual(
            tex_collect(text),
            [NoTranslate(r"\text{"), Translate(r"testi"), NoTranslate(r"}\x")],
        ),

    # Testing text with style inside latex
    def test_tex_collect_style_text(self):
        text = r"\textrm{another test}\x"
        self.assertEqual(
            tex_collect(text),
            [NoTranslate(r"\textrm{"), Translate(r"another test"), NoTranslate(r"}\x")],
        ),

    # Testing a math function inside latex using dollar signs
    def test_tex_collect_math_function(self):
        text = r"\text{Testataan kaaviota: }\x$1\;\text{prosentti}=1\;\% =\frac{1}{100}=0,01$"
        self.assertEqual(
            tex_collect(text),
            [
                NoTranslate(r"\text{"),
                Translate(r"Testataan kaaviota: "),
                NoTranslate(r"}\x$1\;\text{"),
                Translate(r"prosentti"),
                NoTranslate(r"}=1\;\% =\frac{1}{100}=0,01$"),
            ],
        ),

    # Testing a math function mathrm inside latex using double dollar signs
    def test_tex_collect_math_function2(self):
        text = r"$$\mathrm{Muuttuja e} = \sum_{n=0}^{\infty} \dfrac{1}{n!}$$"
        self.assertEqual(
            tex_collect(text),
            [
                NoTranslate(r"$$\mathrm{"),
                Translate(r"Muuttuja e"),
                NoTranslate(r"} = \sum_{n=0}^{\infty} \dfrac{1}{n!}$$"),
            ],
        ),

    #  Testing matrices inside latex
    def test_tex_collect_math_function3(self):
        text = r""""$$M = 
        \begin{bmatrix}
        \frac{5}{6} & \frac{1}{6} & 0 \\[0.3em]
        \frac{5}{6} & 0 & \frac{1}{6} \\[0.3em]
        0 & \frac{5}{6} & \frac{1}{6}
        \end{bmatrix}
        $$"""
        self.assertEqual(
            tex_collect(text),
            [
                NoTranslate(
                    r""""$$M = 
        \begin{bmatrix}
        \frac{5}{6} & \frac{1}{6} & 0 \\[0.3em]
        \frac{5}{6} & 0 & \frac{1}{6} \\[0.3em]
        0 & \frac{5}{6} & \frac{1}{6}
        \end{bmatrix}
        $$"""
                )
            ],
        )

    # Testing bold and italics formatting
    def test_tex_collect_formatted(self):
        text = r"\textbf{oranges}\x\times 100 \textit{something}\x"
        self.assertEqual(
            tex_collect(text),
            [
                NoTranslate(r"\textbf{"),
                Translate(r"oranges"),
                NoTranslate(r"}\x\times 100 \textit{"),
                Translate(r"something"),
                NoTranslate(r"}\x"),
            ],
        ),

    # Testing multiple translate approvals
    def test_get_translate_approvals_latex(self):
        # NOTE Pandoc does not seem to account for trailing whitespace,
        # so the single space ' ' at the end of this test-text will disappear
        latexblock = r"""KING CLAUDIUS
[Aside] O, 'tis $too$ true!
$How$ $ smart$ a $$lash $$ that [speech] $$doth$$ [give] my conscience!
a) \begin{align*} asd\x^3-49x&=0 &&|\text{ erotetaan yhteinen tekijä x}
x(x^2-49)&=0 &&|\text{käytetään tulon nollasääntöä}\\
x=0\;\;\;\\text{tai}\;\;\;&x^2-49=0 && |\text{ ratkaistaan x}\\
&\;\;\;\;\;\,\;\;x^2=49 \\
&\;\;\;\;\;\,\;\;\;\;x=7\;\text{tai}\;x=-7
\end{align*} """
        self.assertEqual(
            get_translate_approvals(latexblock),
            [
                [
                    # NOTE Pandoc seems to convert a single quote into U+2019
                    Translate("KING CLAUDIUS\n[Aside] O, ’tis "),
                    NoTranslate("$too$"),
                    Translate(" true!\n"),
                    NoTranslate("$How$"),
                    Translate(" $ smart$ a "),
                    NoTranslate("$$lash $$"),
                    Translate(" that [speech] "),
                    NoTranslate("$$doth$$"),
                    Translate(" [give] my conscience!\na) "),
                    # TODO content in \text{<content>} should be marked as translate
                    NoTranslate(r"\begin{align*} asd\x^3-49x&=0 &&|\text{"),
                    Translate(" erotetaan yhteinen tekijä x"),
                    NoTranslate("}\nx(x^2-49)&=0 &&|\\text{"),
                    Translate("käytetään tulon nollasääntöä"),
                    NoTranslate(
                        r"""}\\
x=0\;\;\;\\text{"""
                    ),
                    Translate("tai"),
                    NoTranslate(r"""}\;\;\;&x^2-49=0 && |\text{"""),
                    Translate(" ratkaistaan x"),
                    NoTranslate(
                        r"""}\\
&\;\;\;\;\;\,\;\;x^2=49 \\
&\;\;\;\;\;\,\;\;\;\;x=7\;\text{"""
                    ),
                    Translate("tai"),
                    NoTranslate(
                        r"""}\;x=-7
\end{align*}"""
                    ),
                ]
            ],
        )

    def test_bulletlist(self):
        md = r"""- Mieleni minun [tekevi](www.example.com)
- [Aivoni]{.huomio} ajattelevi
- 
    - Kerran
    - [Toisen](www.esimerkki.fi)
        - Kolmannen
    - Koko kappale:\
        Mieleni minun tekevi\
        Aivoni ajattelevi\
            Kerran
            Toisen
                Kolmannen
"""
        self.assertEqual(
            get_translate_approvals(md),
            [
                [
                    # TODO/FIXME does a list need to start with newline?
                    NoTranslate("\n- "),
                    Translate("Mieleni minun "),
                    NoTranslate("["),
                    Translate("tekevi"),
                    NoTranslate("](www.example.com)\n- ["),
                    Translate("Aivoni"),
                    NoTranslate("]{.huomio}"),
                    Translate(" ajattelevi"),
                    NoTranslate("\n- \n\t- "),
                    Translate("Kerran"),
                    NoTranslate("\n\t- ["),
                    Translate("Toisen"),
                    NoTranslate("](www.esimerkki.fi)\n\t\t- "),
                    Translate("Kolmannen"),
                    NoTranslate("\n\t- "),
                    Translate("Koko kappale:"),
                    NoTranslate("\\"),
                    Translate("\nMieleni minun tekevi"),
                    NoTranslate("\\"),
                    Translate("\nAivoni ajattelevi"),
                    NoTranslate("\\"),
                    Translate("\nKerran\nToisen\nKolmannen"),
                ]
            ],
        )

    def test_ordered_list(self):
        md = r"""1. Tässä ollaan
    2. Jotain tehdään
    3. Ainakin nyt
    #) Kivaa on
    III. Roomalaisia numeroita
    IV) Ihan liikaa roomalaisia numeroita
    V) Ei olla edes Roomassa
    (ix) tai koomassa
    (a) Aakkosia
    (b) Niitäkin on liikaa
    (c) Liikaa, liikaa
    A) Ihan hirveesti
    B) Liikaa
"""
        self.assertEqual(
            get_translate_approvals(md),
            [
                [
                    # TODO/FIXME does a list need to start with newline?
                    NoTranslate("\n1. "),
                    Translate("Tässä ollaan"),
                    NoTranslate("\n\t2. "),
                    Translate("Jotain tehdään"),
                    NoTranslate("\n\t3. "),
                    Translate("Ainakin nyt"),
                    NoTranslate("\n\t#) "),
                    Translate("Kivaa on"),
                    NoTranslate("\n\tIII. "),
                    Translate("Roomalaisia numeroita"),
                    NoTranslate("\n\tIV) "),
                    Translate("Ihan liikaa roomalaisia numeroita"),
                    NoTranslate("\n\tV) "),
                    Translate("Ei olla edes Roomassa"),
                    NoTranslate("\n\t(ix) "),
                    Translate("tai koomassa"),
                    NoTranslate("\n\t(a) "),
                    Translate("Aakkosia"),
                    NoTranslate("\n\t(b) "),
                    Translate("Niitäkin on liikaa"),
                    NoTranslate("\n\t(c) "),
                    Translate("Liikaa, liikaa"),
                    NoTranslate("\n\tA) "),
                    Translate("Ihan hirveesti"),
                    NoTranslate("\n\tB) "),
                    Translate("Liikaa"),
                ]
            ],
        )

    def test_tim_plugin(self):
        md = r"""``` {plugin="csPlugin" #btn-tex2 .miniSnippets}
header: Harjoittele matemaattisen vastauksen kirjoittamista.
questionText: "Voit harjoitella
              sitä vaikkapa kirjoittamalla"
              
stem: |!!
md:
Kirjoita teksti:

>Toisen asteen ratkaisukaava on:

$$
x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}
$$

>josta on huomattava että useimmilla $a$, $b$ ja $c$ arvoilla voi tulla kaksi
eri ratkaisua.  Vain jos diskriminantti $D = \sqrt{b^2 - 4ac}$ on nolla, on ratkaisuja yksi kappale.
!!
%%matikka%%
questionTitle: 'Vinkkejä harjoitteluun'
```"""
        self.assertEqual(
            get_translate_approvals(md),
            [
                [
                    # NOTE At the moment, the attributes are discarded
                    NoTranslate(
                        r"""```
header: """
                    ),
                    Translate("Harjoittele matemaattisen vastauksen kirjoittamista."),
                    NoTranslate('\nquestionText: "'),
                    Translate(
                        "Voit harjoitella\n              sitä vaikkapa kirjoittamalla"
                    ),
                    NoTranslate(
                        '"'
                        + r"""
              
stem: |!!"""
                    ),
                    Translate(
                        r"""
md:
Kirjoita teksti:

>Toisen asteen ratkaisukaava on:

$$
x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}
$$

>josta on huomattava että useimmilla $a$, $b$ ja $c$ arvoilla voi tulla kaksi
eri ratkaisua.  Vain jos diskriminantti $D = \sqrt{b^2 - 4ac}$ on nolla, on ratkaisuja yksi kappale."""
                    ),
                    NoTranslate(
                        """
!!
%%matikka%%
questionTitle: '"""
                    ),
                    Translate("Vinkkejä harjoitteluun"),
                    NoTranslate("'\n```"),
                ]
            ],
        )

        md = r"""``` {#lasku plugin="csPlugin"}
%%laskin%%
stem: 'md:foo'
fullprogram: |!!
rad
sin(π / 2)
!!
buttons: ""
```"""
        self.assertEqual(
            get_translate_approvals(md),
            [
                [
                    NoTranslate(
                        """```
%%laskin%%
stem: '"""
                    ),
                    Translate(r"md:foo"),
                    NoTranslate(
                        """'
fullprogram: |!!
rad
sin(π / 2)
!!
buttons: ""
```"""
                    ),
                ]
            ],
        )

    # Test for multiple choice plugin.
    def test_tim_plugin2(self):
        md = r"""```{#mcq2 plugin="mmcq"}
lazy: false
answerLimit:
stem: "Vastaa seuraaviin väittämiin."
choices:
  -
    correct: false
    reason: "Väärin. Vastaus ei kelpaa."
    text: "Esimerkkiselitys."
  -
    correct: true
    reason: "Totta. Näin on." 
    text: "Esimerkkiselitys."
    
```"""
        self.assertEqual(
            get_translate_approvals(md),
            [
                [
                    NoTranslate(
                        r"""```
lazy: false
answerLimit:
stem: """
                        + '"'
                    ),
                    Translate("Vastaa seuraaviin väittämiin."),
                    NoTranslate('"\nchoices:\n  -\n    correct: false\n    reason: "'),
                    Translate("Väärin. Vastaus ei kelpaa."),
                    NoTranslate('"\n    text: "'),
                    Translate("Esimerkkiselitys."),
                    NoTranslate(
                        """\"
  -
    correct: true
    reason: \""""
                    ),
                    Translate("Totta. Näin on."),
                    NoTranslate("""\"\n    text: \""""),
                    Translate("Esimerkkiselitys."),
                    NoTranslate(
                        """"
    
```"""
                        ""
                    ),
                ]
            ],
        )


if __name__ == "__main__":
    unittest.main()
