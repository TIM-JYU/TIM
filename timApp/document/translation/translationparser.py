import re

textblock = """KING CLAUDIUS
[Aside] O, 'tis $too$ true!
$How$ $ smart$ a $$lash $$ that [speech] $$doth$$ [give] my conscience!
a) \\begin{align*} asd
x^3-49x&=0 &&|\\text{ erotetaan yhteinen tekijä x}\\
x(x^2-49)&=0 &&|\\text{ käytetään tulon nollasääntöä}\\
x=0\;\;\;\\text{tai}\;\;\;&x^2-49=0 &&|\\text{ ratkaistaan x}\\
&\;\;\;\;\;\,\;\;x^2=49 \\
&\;\;\;\;\;\,\;\;\;\;x=7\;\\text{tai}\;x=-7
\\end{align*} """


class TranslationParser:
    def latex_parse(self, text: str, translator: str):
        # TODO add table for all the text areas to translate, currently only translates \text{}
        # TODO add table for all the formulas, currently only does \begin{} \end{} -pair

        newtext = text
        # TODO currently dollartag and doubledollartag have overlaps
        # finds between singular $-signs correctly but also some $$.
        # areas as well
        dollartag = re.findall(r"(?<!\\)\$\S.*?(?<!\\)\S\$", newtext)
        # finds between double $-signs correctly.
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

        # manual testing of latexparse
        # textblock = TranslationParser().latex_parse(textblock, "deepl")

        def styles_parse(self, text: str, translator: str):
            """Parses all styles, this includes {.nostyle}, which is not yet implemented in TIM"""
            # might be good to separate or just have as separate ways for each edge case?
            # pictures, links, styles, etc. all function quite similarly

            # {.nostyle}

            # normal styles (pictures, links, styles)

        def md_table_parse(self, text: str, translator: str):
            """Parses MD tables, workflow is PanDoc => HTML => DeepL => PanDoc => MD"""
            # this works in a more unique way and might require the translator
            # to be included into the function itself, since we might have to use the beta features
            # of deepl to translate

        def plugin_parse(self, text: str, translator: str):
            """Parses plugins, which are always individual code blocks."""
            # possible to use documentparser: is_beginning_of_code_block()
            # then you can just place protection at start and end, if you confirm it is a plugin
            # find {} on first row, which contains plugin?
