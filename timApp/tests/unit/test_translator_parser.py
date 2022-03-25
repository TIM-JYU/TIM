import unittest

from timApp.document.translation import translationparser


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
            translationparser.TranslationParser().latex_parse(latexblock, "deepl"),
            """KING CLAUDIUS
            [Aside] O, 'tis <protect>$too$</protect> true!
            <protect>$How$</protect> $ smart$ a <protect>$$lash $$</protect> that [speech] <protect>$$doth$$</protect> [give] my conscience!
            a) <protect>\\begin{align*} asd
            x^3-49x&=0 &&|\\text</protect>{ erotetaan yhteinen tekijä x}<protect>\\
            x(x^2-49)&=0 &&|\\text</protect>{ käytetään tulon nollasääntöä}<protect>\\
            x=0\;\;\;\\text</protect>{tai}<protect>\;\;\;&x^2-49=0 &&|\\text</protect>{ ratkaistaan x}<protect>\\
            &\;\;\;\;\;\,\;\;x^2=49 \\
            &\;\;\;\;\;\,\;\;\;\;x=7\;\\text</protect>{tai}<protect>\;x=-7
            \\end{align*}</protect> """,
        )

    # def test_styles_parse1(self):
    #     styleblock = """
    #     ![pieni kuusikolmio](/images/187297/kuusikulmio_esim.png){width=40%}
    #
    #     [yliopiston sivut](https://www.jyu.fi/fi)
    #
    #     [tämä teksti on punainen]{.red}
    #
    #     [tätä tekstiä ei saisi kääntää]{.notranslate}
    #     """
    #     self.assertEqual(
    #         translationparser.TranslationParser().styles_parse(styleblock, "deepl"),
    #         """
    #                      ![pieni kuusikolmio]<protected>(/images/187297/kuusikulmio_esim.png){width=40%}</protected>
    #
    #                      [yliopiston sivut]<protected>(https://www.jyu.fi/fi)</protected>
    #
    #                      [tämä teksti on punainen]<protected>{.red}</protected>
    #
    #                      <protected>[tätä tekstiä ei saisi kääntää]{.notranslate}</protected>
    #                      """,
    #     )

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
    #     <protected>Yksinkertainen  Taulukko   Ilman      Rajauksia
    #     --------------  ---------  ---------  ---------
    #     1.rivi          2. sarake  3. sarake  4. sarake
    #     2.rivi          2. sarake  3. sarake  4. sarake</protected>
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
            translationparser.TranslationParser().plugin_parse(
                pluginblock, "deepl", "protect"
            ),
            """<protect>``` {#p213a plugin="mcq"}
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
```</protect>""",
        )


if __name__ == "__main__":
    unittest.main()
