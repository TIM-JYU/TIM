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
    def test_notranslate_style1(self):
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

    def test_notranslate_style2(self):
        """Testing notranslate style with brackets and parenthesis inside.
        Will fail if it contains unclosed [ or ]"""
        text = "Käännettävää tekstiä[Ei(){}( { käännettävää [x],[y],[x, y] `tekstiä`]{.notranslate}"
        self.assertEqual(
            get_translate_approvals(text),
            [
                [
                    Translate("Käännettävää tekstiä"),
                    NoTranslate(
                        "[Ei(){}( { käännettävää [x],[y],[x, y] `tekstiä`]{.notranslate}"
                    ),
                ]
            ],
        )

    def test_notranslate_style3(self):
        """Testing notranslate along with multiple other styles"""
        text = r"""***<u><s>Teksti, jossa on kaikki tyylit paitsi notranslate</s></u>*** 
        ***<u><s>[Ja sama myös notranslatella]{.notranslate}</s></u>***"""

        self.assertEqual(
            get_translate_approvals(text),
            [
                [
                    NoTranslate("""***<u><s>"""),
                    Translate("Teksti, jossa on kaikki tyylit paitsi notranslate"),
                    NoTranslate(r"""</s></u>***"""),
                    Translate("\n"),
                    NoTranslate(
                        """\
***<u><s>[Ja sama myös notranslatella]{.notranslate}</s></u>***"""
                    ),
                ]
            ],
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
            get_translate_approvals(text),
            [
                [
                    Translate(
                        """r”# otsikko1
## otsikko2
### otsikko3
#### otsikko4
##### otsikko 5
###### otsikko 6
# otsikko1.2
# otsikko jossa sana header ja ## merkkejä"""
                    )
                ]
            ],
        )

    def test_tex_collect(self):
        # TODO Add cases for identifiers, key-value -pairs and multiple classes as well
        text = r"x^3-49x&=0 &&|\text{ erotetaan yhteinen tekijä x}\x(x^2-49)&=0 &&|\text{ " \
               r"käytetään tulon nollasääntöä}\x=0\;\;\;\textrm{tai}\;\;\;&x^2-49=0 &&|\textsf{ ratkaistaan x}" \
               r"\&\;\;\;\;\;\,\;\;x^2=49 \&\;\;\;\;\;\,\;\;\;\;x=7\;\mathsf{tai}\;x=-7"
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

    def test_tex_collect_simple_text(self):
        """Testing simple text inside latex"""
        text = r"\text{testi}\x"
        self.assertEqual(
            tex_collect(text),
            [NoTranslate(r"\text{"), Translate(r"testi"), NoTranslate(r"}\x")],
        ),

    def test_tex_collect_style_text(self):
        """Testing text with style inside latex"""
        text = r"\textrm{another test}\x"
        self.assertEqual(
            tex_collect(text),
            [NoTranslate(r"\textrm{"), Translate(r"another test"), NoTranslate(r"}\x")],
        ),

    def test_tex_collect_math_function1(self):
        """Testing a math function inside latex using dollar signs"""
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

    def test_tex_collect_math_function2(self):
        """Testing a math function mathrm inside latex using double dollar signs"""
        text = r"$$\mathrm{Muuttuja e} = \sum_{n=0}^{\infty} \dfrac{1}{n!}$$"
        self.assertEqual(
            tex_collect(text),
            [
                NoTranslate(r"$$\mathrm{"),
                Translate(r"Muuttuja e"),
                NoTranslate(r"} = \sum_{n=0}^{\infty} \dfrac{1}{n!}$$"),
            ],
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

    def test_tex_collect_formatted(self):
        """Testing bold and italics formatting"""
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
            get_translate_approvals(md),
            [
                [
                    NoTranslate("\n- "),
                    Translate("Yksi kohta"),
                    NoTranslate("\n\t- "),
                    Translate("Kaksi kohtaa"),
                    NoTranslate("\n\t- "),
                    Translate("Kolme kohtaa"),
                    NoTranslate("\n\t- ```\nKoodia välissä```\n\t- "),
                    Translate("Jotain")
                ],
            ]
        )

    # TODO Test does not work, quotation mark doubles text randomly at the moment. Problem in code?
    def test_bulletlist3(self):
        """Testing quotation marks inside bulletlist"""
        md = r"""- Ilman mitään
- "Lainausmerkit"
- 'Erilaiset lainausmerkit'
- "Osittain" lainattu
- ' Vain osa löytyy
- '''Ja näin
- Ja nyt näin"
- ""\"Lopuksi'
- Add"end'um"""
        self.assertEqual(
            get_translate_approvals(md),
            [
                [
                    NoTranslate("\n- "),
                    Translate("Ilman mitään"),
                    NoTranslate("\n- "),
                    Translate('"Lainausmerkit"'),
                    NoTranslate("\n- "),
                    Translate("'Erilaiset lainausmerkit'"),
                    NoTranslate("\n- ")
                ]
            ]

        )

    def test_ordered_list1(self):
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

    def test_ordered_list2(self):
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
                    Translate("Liikaa"),
                ]
            ],
        )

    # Ordered list fails with quotation marks.
    def test_ordered_list3(self):
        """Testing quotation marks with ordered list"""
        md = r"""1. Tässä ollaan
    2. "Jotain" tehdään
    3. 'Ainakin' nyt
    #) Kivaa on
    III. "Roomalaisia" numeroita
    IV) 'Ihan' liikaa roomalaisia numeroita
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
                    NoTranslate("\n1. "),
                    Translate("Tässä ollaan"),
                    NoTranslate("\n\t2. "),
                    Translate('"Jotain" tehdään'),
                    NoTranslate("\n\t3. "),
                    Translate("'Ainakin' nyt"),
                    NoTranslate("\n\t#) "),
                    Translate("Kivaa on"),
                    NoTranslate("\n\tIII. "),
                    Translate('"Roomalaisia numeroita"'),
                    NoTranslate("\n\t\tIV) "),
                ]
            ],
        )

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
            get_translate_approvals(md),
            [
                [
                    NoTranslate('```\nfooter: "'),
                    Translate("Video footer here"),
                    NoTranslate(""""
#iframe: true
width: 800
height: 600
file: VIDEOURLHERE
```""")

                ]
            ]
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
            get_translate_approvals(md),
            [
                [
                    NoTranslate("""```\ntype: geogebra
#tool: true
header: """),
                    Translate("Otsikko"),
                    NoTranslate("\nstem: "),
                    Translate("Selite"),
                    NoTranslate(r"""
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
```""")

                ]
            ]
        )


if __name__ == "__main__":
    unittest.main()
