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
    def latexparse(self, text: str, translator: str):
        newtext = text
        # TODO currently match and match2 have overlaps
        # finds between singular $-signs correctly but also some $$
        # areas as well
        match = re.findall(r"(?<!\\)\$\S.*?(?<!\\)\S\$", newtext)
        # finds between double $-signs correctly
        match2 = re.findall(r"(?<!\\)\$(?<!\\)\$.+?(?<!\\)\$(?<!\\)\$", newtext)
        # TODO only finds begin/end for latex
        match3 = re.findall(r"(\\begin{.*?})(?s).*?(\\end{.*?})", newtext)
        # TODO make it self.translator specific
        for word in match:
            newtext = newtext.replace(word, "<protect>" + word + "</protect>")
        # replaces at the start and end of found begin and end tags.
        for tags in match3:
            for tuplepair in tags:
                if tags.index(tuplepair) % 2 == 0:
                    newtext = newtext.replace(tuplepair, "<protect>" + tuplepair)
                elif tags.index(tuplepair) % 2 == 1:
                    newtext = newtext.replace(tuplepair, tuplepair + "</protect>")

        return newtext


textblock = TranslationParser().latexparse(textblock, "deepl")
print(textblock)
