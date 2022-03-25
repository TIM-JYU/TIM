import re
from timApp.document.documentparser import DocumentParser

# quick manual test cases before unit tests

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

styleblock = """
![pieni kuusikolmio](/images/187297/kuusikulmio_esim.png){width=40%}

[yliopiston sivut](https://www.jyu.fi/fi)

[tämä teksti on punainen]{.red}

[tätä tekstiä ei saisi kääntää]{.notranslate}
"""

mdtableblock = """
Yksinkertainen  Taulukko   Ilman      Rajauksia
--------------  ---------  ---------  ---------
1.rivi          2. sarake  3. sarake  4. sarake
2.rivi          2. sarake  3. sarake  4. sarake
"""

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


class TranslationParser:
    """Handles parsing full text block utilizing the defined functions inside it"""

    def latex_parse(self, text: str, translator: str):
        # TODO add table for all the text areas to translate, currently only translates \text{}
        # TODO add table for all the formulas, currently only does \begin{} \end{} -pair

        newtext = text
        # finds the areas enclosed by singular $-tag, includes the tags in.
        dollartag = re.findall(
            r"(?<![\\])(?<=\s)\$(?![\s\$]).*?(?<![\\\$])\$(?!\S)", newtext
        )
        # finds the areas enclosed by double $-tag, includes the tags in.
        doubledollartag = re.findall(
            r"(?<!\\)\$(?<!\\)\$.+?(?<!\\)\$(?<!\\)\$", newtext
        )
        # TODO only finds BEGIN/END tags for LaTeX
        # saves the area as 3 variables within a tuple.
        # First is begin tag
        # Second is text within the LaTeX area
        # Third is end tag
        croptags = re.findall(r"(\\begin{.*?})(?s)(.*?)(\\end{.*?})", newtext)

        # TODO make it self.translator specific
        for formula in dollartag:
            newtext = newtext.replace(formula, "<protect>" + formula + "</protect>")
        for formula in doubledollartag:
            newtext = newtext.replace(formula, "<protect>" + formula + "</protect>")
        # protects at the start and end of found begin and end tags.
        for tags in croptags:
            for tuplepair in tags:
                # begin tag indicates protection start
                if tags.index(tuplepair) == 0:
                    newtext = newtext.replace(tuplepair, "<protect>" + tuplepair)
                # the translation protection removal for text areas within the protected areas
                # currently separates text area as { text }
                if tags.index(tuplepair) == 1:
                    textcrop = re.findall(r"(?s)\\text({.*?})", tuplepair)
                    # removing possible duplicates from the textareas to ensure format
                    textcrop = tuple(set(textcrop))
                    for textarea in textcrop:
                        # protection is swapped, since it is within another protection
                        newtext = newtext.replace(
                            textarea, "</protect>" + textarea + "<protect>"
                        )
                # ending tag indicates protection end
                elif tags.index(tuplepair) == 2:
                    newtext = newtext.replace(tuplepair, tuplepair + "</protect>")

        # manual testing of latexparse
        # print(textblock)
        return newtext

    def styles_parse(self, text: str, translator: str):
        """Parses all styles, this includes {.notranslate}, which is not yet implemented in TIM"""
        # might be good to separate or just have as separate ways for each edge case?
        # pictures, links, styles, etc. all function quite similarly

        new_text = text
        # Finds all places marked not to be translated with {.notranslate}
        no_translate = re.findall((r"\[.*?\{.notranslate\}"), new_text)

        # Finds all styles that are not {.notranslate} and IDs (which also use {})
        styles_and_ids = re.findall(r"(?<!\))\{(?!.notranslate)\S.*?\}", new_text)

        # Finds image links with styles added to the image (e.g.[image](link){width=10%})
        image_styled = re.findall(r"(?<=\])\(.*?\)(?<=\))\{.*?\}", new_text)

        # Finds all links that don't have styles added, style-less image links included
        link_styled = re.findall(r"(?<=\])\(.*?\)(?!\{)", new_text)

        # Finds all headers
        headers = re.findall(r"(\#.*?)(?=\ )", new_text)

        # Finds all linebreaks done with \
        # Doesn't work in practice right now because regex ignores the \ despite the correct line
        # linebreaks = re.findall(r"(\\)", new_text)

        for no in no_translate:
            new_text = new_text.replace(no, "<protect>" + no + "</protect>")
        for styles in styles_and_ids:
            new_text = new_text.replace(styles, "<protect>" + styles + "</protect>")
        for images in image_styled:
            new_text = new_text.replace(images, "<protect>" + images + "</protect>")
        for links in link_styled:
            new_text = new_text.replace(links, "<protect>" + links + "</protect>")
        for header in headers:
            new_text = new_text.replace(header, "<protect>" + header + "</protect>")
        # for linebreak in linebreaks:
        # new_text = new_text.replace(linebreak, "<protect>" + linebreak + "</protect>")

        # manual testing of styles_parse
        # print(styleblock)

        return new_text

    def md_table_parse(self, text: str, translator: str):
        """Parses MD tables, workflow is PanDoc => HTML => DeepL => PanDoc => MD"""
        # this works in a more unique way and might require the translator
        # to be included into the function itself, since we might have to use the beta features
        # of deepl to translate

    @staticmethod
    def plugin_parse(text: str, translator: str, protection_tag: str) -> str:
        """Parses plugins, which are always individual code blocks."""
        # TODO: extremely simple plugin protection... iterate and improve.
        # possible to use documentparser: is_beginning_of_code_block()
        # then you can just place protection at start and end, if you confirm it is a plugin
        # find {} on first row, which contains plugin?
        lines = text.split("\n")
        firstline = lines[0]
        lastline = lines[-1]
        if firstline[0:3] == "```" and lastline[0:3] == "```":
            newtext = text
            if ("{" and "}" and "plugin=") in firstline:
                newtext = f"<{protection_tag}>" + newtext + f"</{protection_tag}>"
                return newtext
        return text


# manual testing of latexparse
# textblock = TranslationParser().latex_parse(latexblock, "deepl")
# textblock = TranslationParser().plugin_parse(pluginblock, "deepl")
# print(textblock)
