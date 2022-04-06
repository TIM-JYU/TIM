import unittest

from timApp.document.translation.translationparser import (
    Translate,
    NoTranslate,
    TranslationParser,
    get_translate_approvals,
    tex_collect,
    span_collect,
)


class MyTestCase(unittest.TestCase):
    def test_latex_parse1(self):
        latexblock = """KING CLAUDIUS
            [Aside] O, 'tis $too$ true!
            $How$ $ smart$ a $$lash $$ that [speech] $$doth$$ [give] my conscience!
            a) \\begin{align*} asd
            x^3-49x&=0 &&|\\text{ erotetaan yhteinen tekijä x}\\
            x(x^2-49)&=0 &&|\\text{ käytetään tulon nollasääntöä}\\
            x=0\;\;\;\\text{tai}\;\;\;&x^2-49=0 &&|\\text{ ratkaistaan x}\\
            &\;\;\;\;\;\,\;\;x^2=49 \\
            &\;\;\;\;\;\,\;\;\;\;x=7\;\\text{tai}\;x=-7
            \\end{align*} """
        self.assertEqual(
            """KING CLAUDIUS
            [Aside] O, 'tis <deepl>$too$</deepl> true!
            <deepl>$How$</deepl> $ smart$ a <deepl>$$lash $$</deepl> that [speech] <deepl>$$doth$$</deepl> [give] my conscience!
            a) <deepl>\\begin{align*} asd
            x^3-49x&=0 &&|\\text</deepl>{ erotetaan yhteinen tekijä x}<deepl>\\
            x(x^2-49)&=0 &&|\\text</deepl>{ käytetään tulon nollasääntöä}<deepl>\\
            x=0\;\;\;\\text</deepl>{tai}<deepl>\;\;\;&x^2-49=0 &&|\\text</deepl>{ ratkaistaan x}<deepl>\\
            &\;\;\;\;\;\,\;\;x^2=49 \\
            &\;\;\;\;\;\,\;\;\;\;x=7\;\\text</deepl>{tai}<deepl>\;x=-7
            \\end{align*}</deepl> """,
            TranslationParser().latex_parse(latexblock, "deepl"),
        )

    def test_latex_parse2(self):
        latexblock = """$How$ $ smart$ a $$lash $$ that [speech] $$doth$$ [give] my conscience!"""
        self.assertEqual(
            """<deepl>$How$</deepl> $ smart$ a <deepl>$$lash $$</deepl> that [speech] <deepl>$$doth$$</deepl> [give] my conscience!""",
            TranslationParser().latex_parse(latexblock, "deepl"),
        )

    def test_styles_parse1(self):
        styleblock = """
        ![pieni kuusikolmio](/images/187297/kuusikulmio_esim.png){width=40%}

        [yliopiston sivut](https://www.jyu.fi/fi)

        [tämä teksti on punainen]{.red}

        [tätä tekstiä ei saisi kääntää]{.notranslate}

        ## Tämän otsikon merkinnän pitää pysyä kunnossa
        """
        self.assertEqual(
            """
        ![pieni kuusikolmio]<deepl>(/images/187297/kuusikulmio_esim.png){width=40%}</deepl>

        [yliopiston sivut]<deepl>(https://www.jyu.fi/fi)</deepl>

        [tämä teksti on punainen]<deepl>{.red}</deepl>

        <deepl>[tätä tekstiä ei saisi kääntää]{.notranslate}</deepl>

        <deepl>##</deepl> Tämän otsikon merkinnän pitää pysyä kunnossa
        """,
            TranslationParser().styles_parse(styleblock, "deepl"),
        )

    # def test_md_table_parse1(self):
    #     mdtableblock = """
    #     Yksinkertainen  Taulukko   Ilman      Rajauksia
    #     --------------  ---------  ---------  ---------
    #     1.rivi          2. sarake  3. sarake  4. sarake
    #     2.rivi          2. sarake  3. sarake  4. sarake
    #     """
    #     self.assertEqual(
    #         translationparser.TranslationParser().md_table_parse(mdtableblock, "deepl"),
    #         """
    #     <deepled>Yksinkertainen  Taulukko   Ilman      Rajauksia
    #     --------------  ---------  ---------  ---------
    #     1.rivi          2. sarake  3. sarake  4. sarake
    #     2.rivi          2. sarake  3. sarake  4. sarake</deepled>
    #     """,
    #     )

    def test_plugin_parse1(self):
        pluginblock = """``` {#p213a plugin="mcq"}
answerLimit: 1
headerText: ''
buttonText: 'Tallenna'
stem: 'a) Yhtälöllä ei ole ratkaisua luonnollisten lukujen joukossa ja se saadaan ratkeavaksi lisäämällä
negatiiviset luvut lukujoukkoomme eli ts. teemme lukulaajennuksen $\mathbb{N} \rightarrow \mathbb{Z}$.'
choices:
  -
    correct: false
    text: 'md:$x+4=7$'
    reason: 'Yhtälön ratkaisu $x=3$ kuuluu luonnollisiin lukuihin.'
  -
    correct: false
    text: 'md:$x+\dfrac{1}{2}=\dfrac{3}{2}$'
    reason: 'Yhtälön ratkaisu $x=1$ kuuluu luonnollisiin lukuihin.'
  -
    correct: true
    text: 'md:$x+3=0$'
    reason: 'Yhtälön ratkaisu $x=-3$ ei kuulu luonnollisiin lukuihin.'
```"""
        self.assertEqual(
            """<deepl>``` {#p213a plugin="mcq"}
answerLimit: 1
headerText: ''
buttonText: 'Tallenna'
stem: 'a) Yhtälöllä ei ole ratkaisua luonnollisten lukujen joukossa ja se saadaan ratkeavaksi lisäämällä
negatiiviset luvut lukujoukkoomme eli ts. teemme lukulaajennuksen $\mathbb{N} \rightarrow \mathbb{Z}$.'
choices:
  -
    correct: false
    text: 'md:$x+4=7$'
    reason: 'Yhtälön ratkaisu $x=3$ kuuluu luonnollisiin lukuihin.'
  -
    correct: false
    text: 'md:$x+\dfrac{1}{2}=\dfrac{3}{2}$'
    reason: 'Yhtälön ratkaisu $x=1$ kuuluu luonnollisiin lukuihin.'
  -
    correct: true
    text: 'md:$x+3=0$'
    reason: 'Yhtälön ratkaisu $x=-3$ ei kuulu luonnollisiin lukuihin.'
```</deepl>""",
            TranslationParser().plugin_parse(pluginblock, "deepl"),
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
    def test_span_collect(self):
        text = r"tässä on [teksti]{.notranslate}, jota ei käännetä."
        self.assertEqual(
            span_collect(text),
            [
                Translate(r"tässä on ["),
                NoTranslate(r"[teksti]{.notranslate}"),
                Translate(r", jota ei käännetä."),
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

    def test_get_translate_approvals_latex(self):
        # NOTE Pandoc does not seem to account for trailing whitespace, so the single space ' ' at the end of this test-text will disappear
        latexblock = r"""KING CLAUDIUS
[Aside] O, 'tis $too$ true!
$How$ $ smart$ a $$lash $$ that [speech] $$doth$$ [give] my conscience!
a) \begin{align*} asd
x^3-49x&=0 &&|\text{ erotetaan yhteinen tekijä x}\\
x(x^2-49)&=0 &&|\text{ käytetään tulon nollasääntöä}\\
x=0\;\;\;\\text{tai}\;\;\;&x^2-49=0 &&|\text{ ratkaistaan x}\\
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
                    NoTranslate(
                        r"""\begin{align*} asd
x^3-49x&=0 &&|\text{ erotetaan yhteinen tekijä x}\\
x(x^2-49)&=0 &&|\text{ käytetään tulon nollasääntöä}\\
x=0\;\;\;\\text{tai}\;\;\;&x^2-49=0 &&|\text{ ratkaistaan x}\\
&\;\;\;\;\;\,\;\;x^2=49 \\
&\;\;\;\;\;\,\;\;\;\;x=7\;\text{tai}\;x=-7
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


if __name__ == "__main__":
    unittest.main()
