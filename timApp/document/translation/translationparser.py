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
    def __init__(self, translator: str):
        translator = translator

    def latexparse(self, text: str):

        # TODO currently match and match2 have overlaps
        # finds between singular $-signs correctly but also some $$
        # areas as well
        match = re.findall(r"(?<!\\)\$\S.*?(?<!\\)\S\$", text)
        # finds between double $-signs correctly
        match2 = re.findall(r"(?<!\\)\$(?<!\\)\$.+?(?<!\\)\$(?<!\\)\$", text)
        # TODO broken and cannot find formula areas
        match3 = re.findall(r"\\begin\{.*?\}.*?\\end\{.*?\}", text)
        # TODO make it self.translator specific
        for word in match:
            text.replace(word, "<protect>" + word + "</protect>")


deepltrans = TranslationParser("deepl")
deepltrans.latexparse(textblock)
print(textblock)
