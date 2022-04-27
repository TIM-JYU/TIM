"""
TODO: Short description of Python module
"""

__authors__ = [
    "Noora Jokela",
    "Riku Lehkonen",
    "Vili Moisala",
    "Juho Tarkkanen",
    "Sami Viitanen",
]
__license__ = "MIT"
__date__ = "25.4.2022"

import unittest

from timApp.document.translation.translationparser import (
    Translate,
    NoTranslate,
    TranslationParser,
)


class TimTranslationParserTest:
    """A Base class to run translation tests needing a parser instance."""

    parser: TranslationParser = TranslationParser()


class TestParser(unittest.TestCase, TimTranslationParserTest):
    def test_get_translate_approvals_attr(self):
        # TODO Add cases for identifiers, key-value -pairs and multiple classes as well
        text = "Tässä on kuva ![kissasta](/kuvat/kissa.png). [Tosi]{.red} hieno, eikös?"
        self.assertEqual(
            [
                Translate("\nTässä on kuva "),
                NoTranslate("!["),
                Translate("kissasta"),
                NoTranslate("](/kuvat/kissa.png)"),
                Translate(". "),
                NoTranslate("["),
                Translate("Tosi"),
                NoTranslate("]{.red}"),
                Translate(" hieno, eikös?\n"),
            ],
            self.parser.get_translate_approvals(text),
        )

    # For .notranslate style, might move elsewhere. For future use.
    def test_notranslate_style1(self):
        text = r"tässä on [teksti]{.notranslate}, jota ei käännetä."
        self.assertEqual(
            [
                Translate("\ntässä on "),
                NoTranslate("[teksti]{.notranslate}"),
                Translate(", jota ei käännetä.\n"),
            ],
            self.parser.get_translate_approvals(text),
        )

        text = """``` {plugin="csPlugin" #btn-tex2 .notranslate .miniSnippets}
header: Harjoittele matemaattisen vastauksen kirjoittamista.
```"""
        self.assertEqual(
            [
                # TODO Should the plugins contain the attributes or no?
                Translate("\n"),
                NoTranslate(
                    """```
header: Harjoittele matemaattisen vastauksen kirjoittamista.
```"""
                ),
            ],
            self.parser.get_translate_approvals(text),
        )

        text = "Jyväskylän yliopisto sijaitsee paikassa nimeltä [Keski-Suomi]{.notranslate}"
        self.assertEqual(
            [
                Translate("\nJyväskylän yliopisto sijaitsee paikassa nimeltä "),
                NoTranslate("[Keski-Suomi]{.notranslate}"),
                Translate("\n"),
            ],
            self.parser.get_translate_approvals(text),
        )

    def test_notranslate_style2(self):
        """Testing notranslate style with brackets and parenthesis inside.
        Will fail if it contains unclosed [ or ]"""
        text = "Käännettävää tekstiä[Ei(){}( { käännettävää [x],[y],[x, y] `tekstiä`]{.notranslate}"
        self.assertEqual(
            [
                Translate("\nKäännettävää tekstiä"),
                NoTranslate(
                    "[Ei(){}( { käännettävää [x],[y],[x, y] `tekstiä`]{.notranslate}"
                ),
                Translate("\n"),
            ],
            self.parser.get_translate_approvals(text),
        )

    def test_notranslate_style3(self):
        """Testing notranslate along with multiple other styles"""
        text = r"""***<u><s>Teksti, jossa on kaikki tyylit paitsi notranslate</s></u>*** 
        ***<u><s>[Ja sama myös notranslatella]{.notranslate}</s></u>***"""

        self.assertEqual(
            [
                Translate("\n<b><i>"),
                NoTranslate("""<u><s>"""),
                Translate("Teksti, jossa on kaikki tyylit paitsi notranslate"),
                NoTranslate(r"""</s></u>"""),
                Translate("</i></b>"),
                NoTranslate("\n"),
                Translate("<b><i>"),
                NoTranslate(
                    """\
<u><s>[Ja sama myös notranslatella]{.notranslate}</s></u>"""
                ),
                Translate("</i></b>\n"),
            ],
            self.parser.get_translate_approvals(text),
        )

    def test_header(self):
        """Testing different headers"""
        text = (
            """r”# otsikko1
## otsikko2
### otsikko3
#### otsikko4
##### otsikko 5
###### otsikko 6
# otsikko1.2
# otsikko jossa sana header ja ## merkkejä"""
            ""
        )
        self.assertEqual(
            [
                Translate(
                    """
r”# otsikko1"""
                ),
                NoTranslate("\n"),
                Translate("## otsikko2"),
                NoTranslate("\n"),
                Translate("### otsikko3"),
                NoTranslate("\n"),
                Translate("#### otsikko4"),
                NoTranslate("\n"),
                Translate("##### otsikko 5"),
                NoTranslate("\n"),
                Translate("###### otsikko 6"),
                NoTranslate("\n"),
                Translate("# otsikko1.2"),
                NoTranslate("\n"),
                Translate("# otsikko jossa sana header ja ## merkkejä\n" ""),
            ],
            self.parser.get_translate_approvals(text),
        )

    def test_tex_collect(self):
        # TODO Add cases for identifiers, key-value -pairs and multiple classes as well
        text = (
            r"x^3-49x&=0 &&|\text{ erotetaan yhteinen tekijä x}\x(x^2-49)&=0 &&|\text{ "
            r"käytetään tulon nollasääntöä}\x=0\;\;\;\textrm{tai}\;\;\;&x^2-49=0 &&|\textsf{ ratkaistaan x}"
            r"\&\;\;\;\;\;\,\;\;x^2=49 \&\;\;\;\;\;\,\;\;\;\;x=7\;\mathsf{tai}\;x=-7"
        )
        self.assertEqual(
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
            self.parser.tex_collect(text),
        )

    def test_tex_collect_simple_text(self):
        """Testing simple text inside latex"""
        text = r"\text{testi}\x"
        self.assertEqual(
            [NoTranslate(r"\text{"), Translate(r"testi"), NoTranslate(r"}\x")],
            self.parser.tex_collect(text),
        ),

    def test_tex_collect_style_text(self):
        """Testing text with style inside latex"""
        text = r"\textrm{another test}\x"
        self.assertEqual(
            [NoTranslate(r"\textrm{"), Translate(r"another test"), NoTranslate(r"}\x")],
            self.parser.tex_collect(text),
        ),

    def test_tex_collect_math_function1(self):
        """Testing a math function inside latex using dollar signs"""
        text = r"\text{Testataan kaaviota: }\x$1\;\text{prosentti}=1\;\% =\frac{1}{100}=0,01$"
        self.assertEqual(
            [
                NoTranslate(r"\text{"),
                Translate(r"Testataan kaaviota: "),
                NoTranslate(r"}\x$1\;\text{"),
                Translate(r"prosentti"),
                NoTranslate(r"}=1\;\% =\frac{1}{100}=0,01$"),
            ],
            self.parser.tex_collect(text),
        ),

    def test_tex_collect_math_function2(self):
        """Testing a math function mathrm inside latex using double dollar signs"""
        text = r"$$\mathrm{Muuttuja e} = \sum_{n=0}^{\infty} \dfrac{1}{n!}$$"
        self.assertEqual(
            [
                NoTranslate(r"$$\mathrm{"),
                Translate(r"Muuttuja e"),
                NoTranslate(r"} = \sum_{n=0}^{\infty} \dfrac{1}{n!}$$"),
            ],
            self.parser.tex_collect(text),
        ),

    def test_tex_collect_math_function3(self):
        """Testing matrices inside latex"""
        text = r""""$$M = 
        \begin{bmatrix}
        \frac{5}{6} & \frac{1}{6} & 0 \\[0.3em]
        \frac{5}{6} & 0 & \frac{1}{6} \\[0.3em]
        0 & \frac{5}{6} & \frac{1}{6}
        \end{bmatrix}
        $$"""
        self.assertEqual(
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
            self.parser.tex_collect(text),
        )

    def test_tex_collect_formatted(self):
        """Testing bold and italics formatting"""
        text = r"\textbf{oranges}\x\times 100 \textit{something}\x"
        self.assertEqual(
            [
                NoTranslate(r"\textbf{"),
                Translate(r"oranges"),
                NoTranslate(r"}\x\times 100 \textit{"),
                Translate(r"something"),
                NoTranslate(r"}\x"),
            ],
            self.parser.tex_collect(text),
        ),

    def test_get_translate_approvals_latex(self):
        """Test for multiple translate approvals"""
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
            [
                # NOTE Pandoc seems to convert a single quote into U+2019
                Translate("\nKING CLAUDIUS"),
                NoTranslate("\n"),
                Translate("[Aside] O, ’tis "),
                NoTranslate("$too$"),
                Translate(" true!"),
                NoTranslate("\n$How$"),
                Translate(" $ smart$ a "),
                NoTranslate("$$lash $$"),
                Translate(" that [speech] "),
                NoTranslate("$$doth$$"),
                Translate(" [give] my conscience!"),
                NoTranslate("\n"),
                Translate("a) "),
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
                Translate("\n"),
            ],
            self.parser.get_translate_approvals(latexblock),
        )

    def test_bulletlist1(self):
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
            [
                # TODO/FIXME does a list need to start with newline?
                Translate("\n"),
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
                NoTranslate("\\\n"),
                Translate("Mieleni minun tekevi"),
                NoTranslate("\\\n"),
                Translate("Aivoni ajattelevi"),
                NoTranslate("\\\n"),
                Translate("Kerran"),
                NoTranslate("\n"),
                Translate("Toisen"),
                NoTranslate("\n"),
                Translate("Kolmannen\n"),
            ],
            self.parser.get_translate_approvals(md),
        )

    def test_bulletlist2(self):
        """Testing bulletlist with different intendation and with a code block in between the list"""
        md = """- Yksi kohta
    - Kaksi kohtaa
    -   Kolme kohtaa

    -   
    ```
    Koodia välissä
    ```
    - Jotain"""
        self.assertEqual(
            [
                Translate("\n"),
                NoTranslate("\n- "),
                Translate("Yksi kohta"),
                NoTranslate("\n\t- "),
                Translate("Kaksi kohtaa\n"),
                NoTranslate("\n\t- "),
                Translate("Kolme kohtaa\n"),
                NoTranslate("\n\t- ```\nKoodia välissä\n```\n\t- "),
                Translate("Jotain\n"),
            ],
            self.parser.get_translate_approvals(md),
        )

    # TODO There is currently no reliable solution for the corner cases in this test case.
    #      Proper handling will require major refactoring of the parser engine.
    #
    # def test_bulletlist3(self):
    #     """Testing quotation marks inside bulletlist currently ' will change into ’ and " will change into ”"""
    #     md = r"""- Ilman mitään
    # - "Lainausmerkit"
    # - 'Erilaiset lainausmerkit'
    # - "Osittain" lainattu
    # - ' Vain osa löytyy
    # - '''Ja näin
    # - Ja nyt näin"
    # - ""\"Lopuksi'
    # - Add"end\'um   """
    #     self.assertEqual(
    #         get_translate_approvals(md),
    #         [
    #             [
    #                 NoTranslate("\n- "),
    #                 Translate("Ilman mitään"),
    #                 NoTranslate("\n\t- "),
    #                 Translate('"Lainausmerkit"'),
    #                 NoTranslate("\n\t- "),
    #                 Translate("'Erilaiset lainausmerkit'"),
    #                 NoTranslate("\n\t- "),
    #                 Translate('"Osittain" lainattu'),
    #                 NoTranslate("\n\t- "),
    #                 Translate("' Vain osa löytyy"),
    #                 NoTranslate("\n\t- "),
    #                 Translate("'''Ja näin"),
    #                 NoTranslate("\n\t- "),
    #                 Translate('Ja nyt näin"'),
    #                 NoTranslate("\n\t- "),
    #                 Translate('"""Lopuksi’'),
    #                 NoTranslate("\n\t- "),
    #                 Translate("Add\"end'um"),
    #             ]
    #         ],
    #     )

    def test_ordered_list1(self):
        """
        Testing orderedlist with all the different ways they can be formatted.
        """
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
            [
                # TODO/FIXME does a list need to start with newline?
                Translate("\n"),
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
                Translate("Liikaa\n"),
            ],
            self.parser.get_translate_approvals(md),
        )

    def test_ordered_list2(self):
        """
        Testing orderedlist with all the different ways they can be formatted and with indentation.
        """
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
            [
                # TODO/FIXME does a list need to start with newline?
                Translate("\n"),
                NoTranslate("\n1. "),
                Translate("Tässä ollaan"),
                NoTranslate("\n\t2. "),
                Translate("Jotain tehdään"),
                NoTranslate("\n\t3. "),
                Translate("Ainakin nyt"),
                NoTranslate("\n\t#) "),
                Translate("Kivaa on"),
                NoTranslate("\n\t\tIII. "),
                Translate("Roomalaisia numeroita"),
                NoTranslate("\n\t\tIV) "),
                Translate("Ihan liikaa roomalaisia numeroita"),
                NoTranslate("\n\t\tV) "),
                Translate("Ei olla edes Roomassa"),
                NoTranslate("\n\t(ix) "),
                Translate("tai koomassa"),
                NoTranslate("\n\t(a) "),
                Translate("Aakkosia"),
                NoTranslate("\n\t\t(b) "),
                Translate("Niitäkin on liikaa"),
                NoTranslate("\n\t\t(c) "),
                Translate("Liikaa, liikaa"),
                NoTranslate("\n\tA) "),
                Translate("Ihan hirveesti"),
                NoTranslate("\n\tB) "),
                Translate("Liikaa\n"),
            ],
            self.parser.get_translate_approvals(md),
        )

    # TODO There is currently no reliable solution for the corner cases in this test case.
    #      Proper handling will require major refactoring of the parser engine.
    #
    # def test_ordered_list3(self):
    #     """Testing quotation marks with ordered list, ' will change into ’ and " will change into ”"""
    #     md = r"""1. Tässä ollaan
    # 2. "Jotain" tehdään
    # 3. 'Ainakin' nyt
    # #) 'Kivaa on
    # III. Roomalaisia 'numeroita
    # IV) Ihan "liikaa roomalaisia numeroita
    #
    # """
    #     self.assertEqual(
    #         get_translate_approvals(md),
    #         [
    #             [
    #                 NoTranslate("\n1. "),
    #                 Translate("Tässä ollaan"),
    #                 NoTranslate("\n\t2. "),
    #                 Translate('"Jotain" tehdään'),
    #                 NoTranslate("\n\t3. "),
    #                 Translate("'Ainakin' nyt"),
    #                 NoTranslate("\n\t#) "),
    #                 Translate("'Kivaa on"),
    #                 NoTranslate("\n\tIII. "),
    #                 Translate("Roomalaisia 'numeroita"),
    #                 NoTranslate("\n\tIV) "),
    #                 Translate('Ihan "liikaa roomalaisia numeroita'),
    #             ]
    #         ],
    #     )

    # TODO There is currently no reliable solution for the corner cases in this test case.
    #      Proper handling will require major refactoring of the parser engine.
    #
    #     def test_ordered_list_codeblock(self):
    #         # FIXME NOTE This test might not be accurate or well-defined... Especially regarding indentations.
    #         md = """1. Kissa
    #
    #        eka
    #        toka
    #
    # 2. Koira"""
    #
    #         self.assertEqual(
    #             get_translate_approvals(md),
    #             [
    #                 [
    #                     NoTranslate("1. "),
    #                     Translate("Kissa"),
    #                     NoTranslate(
    #                         """
    #
    # \t```
    # \teka
    # \ttoka
    # \t```
    # 2. """
    #                     ),
    #                     Translate("Koira"),
    #                 ]
    #             ],
    #         )

    def test_tim_plugin1(self):
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
            [
                # NOTE At the moment, the attributes are discarded
                Translate("\n"),
                NoTranslate("```\nheader: "),
                Translate("Harjoittele matemaattisen vastauksen kirjoittamista."),
                NoTranslate('\nquestionText: "'),
                Translate(
                    "Voit harjoitella\n              sitä vaikkapa kirjoittamalla"
                ),
                NoTranslate('"\n              \nstem: |!!\nmd:'),
                Translate("Kirjoita teksti:\n\n"),
                NoTranslate("> "),
                # BlockQuote starts a Block, which starts with newline
                Translate("\nToisen asteen ratkaisukaava on:\n\n"),
                NoTranslate(
                    r"""$$
x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}
$$"""
                ),
                Translate("\n\n"),
                NoTranslate("> "),
                Translate("\njosta on huomattava että useimmilla "),
                NoTranslate("$a$"),
                Translate(", "),
                NoTranslate("$b$"),
                Translate(" ja "),
                NoTranslate("$c$"),
                Translate(" arvoilla voi tulla kaksi"),
                NoTranslate("\n"),
                Translate("eri ratkaisua. Vain jos diskriminantti "),
                NoTranslate(r"$D = \sqrt{b^2 - 4ac}$"),
                Translate(" on nolla, on ratkaisuja yksi kappale."),
                NoTranslate("\n!!\n%%matikka%%\nquestionTitle: '"),
                Translate("Vinkkejä harjoitteluun"),
                NoTranslate("'\n```"),
            ],
            self.parser.get_translate_approvals(md),
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
            [
                Translate("\n"),
                NoTranslate(
                    """```
%%laskin%%
stem: 'md:"""
                ),
                # Paragraph ends with a newline. TODO Is that right in this case?
                Translate("foo"),
                NoTranslate(
                    """'
fullprogram: |!!
rad
sin(π / 2)
!!
buttons: ""
```"""
                ),
            ],
            self.parser.get_translate_approvals(md),
        )

    def test_tim_plugin2(self):
        """Test for multiple choice plugin."""
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
            [
                Translate("\n"),
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
            ],
            self.parser.get_translate_approvals(md),
        )

    def test_tim_plugin3(self):
        """Test for a big sized video plugin"""
        md = r"""``` {plugin="showVideo"}
footer: "Video footer here"
#iframe: true
width: 800
height: 600
file: VIDEOURLHERE
```
"""
        self.assertEqual(
            [
                Translate("\n"),
                NoTranslate('```\nfooter: "'),
                Translate("Video footer here"),
                NoTranslate(
                    """"
#iframe: true
width: 800
height: 600
file: VIDEOURLHERE
```"""
                ),
            ],
            self.parser.get_translate_approvals(md),
        )

    def test_tim_plugin4(self):
        """Test for geogebra plugin. Currently translates only header and stem, nothing else"""
        md = r"""``` {plugin="csPlugin" #Plugin1}
type: geogebra
#tool: true
header: Otsikko
stem: Selite
-pointsRule:
   {}
width: 600
height: 320
material_id:
commands: |!!
!!
javascript: |!!
P.setDataInit = function (api, geostate) {
    timgeo.setState(api, geostate);
    timgeo.setAllLabelsVisible(api, false);  // kaikki labelit piiloon
    api.setVisible("kulmannimi", false);
    timgeo.setLabelsVisible(api, "D,E,F,β,textkulma", true);  //muutamat takaisin
    timgeo.setXmlProperty(api, 'textkulma', '<length val="5" />');
    timgeo.setPointsCoords(api, geostate.userpts); // tilan palautus
    api.evalCommand(geostate.usercmds);
}

P.getData = function(){
    return {
       "usercode": ggbApplet.getValueString('kulmannimi'),
       "userpts":  timgeo.getObjValue(ggbApplet,"D,E,F"),
       "usercmds": timgeo.getObjCommand(ggbApplet, 'kulmannimi'),
   };
}
!!
-objxml: |!!
!!
-data: |!!
<geogebra format="5.0">
<euclidianView>
    <coordSystem xZero="350" yZero="130" scale="25" yscale="25"/>
    <axis id="0" show="false" />
    <axis id="1" show="false" />
</euclidianView>
<kernel>
    <decimals val="0"/>
    <angleUnit val="degree"/>
</kernel>
</geogebra>
!!
```
"""
        self.assertEqual(
            [
                Translate("\n"),
                NoTranslate(
                    """```\ntype: geogebra
#tool: true
header: """
                ),
                Translate("Otsikko"),
                NoTranslate("\nstem: "),
                Translate("Selite"),
                NoTranslate(
                    r"""
-pointsRule:
   {}
width: 600
height: 320
material_id:
commands: |!!
!!
javascript: |!!
P.setDataInit = function (api, geostate) {
    timgeo.setState(api, geostate);
    timgeo.setAllLabelsVisible(api, false);  // kaikki labelit piiloon
    api.setVisible("kulmannimi", false);
    timgeo.setLabelsVisible(api, "D,E,F,β,textkulma", true);  //muutamat takaisin
    timgeo.setXmlProperty(api, 'textkulma', '<length val="5" />');
    timgeo.setPointsCoords(api, geostate.userpts); // tilan palautus
    api.evalCommand(geostate.usercmds);
}

P.getData = function(){
    return {
       "usercode": ggbApplet.getValueString('kulmannimi'),
       "userpts":  timgeo.getObjValue(ggbApplet,"D,E,F"),
       "usercmds": timgeo.getObjCommand(ggbApplet, 'kulmannimi'),
   };
}
!!
-objxml: |!!
!!
-data: |!!
<geogebra format="5.0">
<euclidianView>
    <coordSystem xZero="350" yZero="130" scale="25" yscale="25"/>
    <axis id="0" show="false" />
    <axis id="1" show="false" />
</euclidianView>
<kernel>
    <decimals val="0"/>
    <angleUnit val="degree"/>
</kernel>
</geogebra>
!!
```"""
                ),
            ],
            self.parser.get_translate_approvals(md),
        )

    def test_multiple_paragraphs(self):
        md = """**fet**


*kursiv stil*


<u>Understrykning</u>


<s>strykning</s>


[Färg]{.red}


[bakgrundsfärg]{.bgred}


`koodi`
kodblock


~delindex~


^högsta index^"""

        TR = Translate
        NT = NoTranslate
        self.assertEqual(
            [
                TR("\n<b>fet</b>\n\n<i>kursiv stil</i>\n\n"),
                NT("<u>"),
                TR("Understrykning"),
                NT("</u>"),
                TR("\n\n"),
                NT("<s>"),
                TR("strykning"),
                NT("</s>"),
                TR("\n\n"),
                NT("["),
                TR("Färg"),
                NT("]{.red}"),
                TR("\n\n"),
                NT("["),
                TR("bakgrundsfärg"),
                NT("]{.bgred}"),
                TR("\n\n"),
                NT("`koodi`\n"),
                TR("kodblock\n\n~delindex~\n\n^högsta index^\n"),
            ],
            self.parser.get_translate_approvals(md),
        )

    def TODO_test_latex_inside_span(self):
        # TODO This following value sometimes comes out weird from the whole parsing...
        text = "```{plugin=\"mmcq\"\nchoices:\n  -\n    text:'[$$\\sum_{i=1}^n a_i = a_1+a_2+\\ldots+a_n  = \\frac{a_1+a_n}{2}\\cdot n$$]{.red}'\n```"
        self.assertEqual(
            [Translate("\n"), NoTranslate(text), Translate("\n")],
            self.parser.get_translate_approvals(text),
        )


if __name__ == "__main__":
    unittest.main()
