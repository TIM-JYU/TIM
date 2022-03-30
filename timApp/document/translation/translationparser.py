import re
import pypandoc
import json
from typing import Callable
from dataclasses import dataclass

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

    @staticmethod
    def parse(text: str, protection_tag: str) -> str:
        text_to_translate: str = TranslationParser.latex_parse(text, protection_tag)
        text_to_translate = TranslationParser.styles_parse(
            text_to_translate, protection_tag
        )
        text_to_translate = TranslationParser.md_table_parse(
            text_to_translate, protection_tag
        )
        text_to_translate = TranslationParser.plugin_parse(
            text_to_translate, protection_tag
        )
        return text_to_translate

    @staticmethod
    def latex_parse(text: str, protection_tag: str) -> str:
        # TODO add table for all the text areas to translate, currently only translates \text{}
        # TODO add table for all the formulas, currently only does \begin{} \end{} -pair

        newtext = text
        # finds the areas enclosed by singular $-tag, includes the tags in.
        # TODO: Can this be made more intelligent than simple an or between two almost identical lines?
        dollartag = re.findall(
            r"(?<![\\])(?<=\s)\$(?![\s\$]).*?(?<![\\\$])\$(?!\S)|^\$(?![\s\$]).*?(?<![\\\$])\$(?!\S)",
            newtext,
        )
        # finds the areas enclosed by double $-tag, includes the tags in.
        doubledollartag = re.findall(
            r"(?<!\\)\$(?<!\\)\$.+?(?<!\\)\$(?<!\\)\$", newtext
        )
        # TODO only finds BEGIN/END tags for LaTeX
        # TODO: consider format change without tables, as a single string in list
        # saves the area as 3 variables within a tuple.
        # First is begin tag
        # Second is text within the LaTeX area
        # Third is end tag
        # \\begin{.*?}(?s).*?\\end{.*?} without arrays...
        croptags = re.findall(r"(\\begin{.*?})(?s)(.*?)(\\end{.*?})", newtext)

        # TODO transfer protection tags to preprocessing.
        for formula in dollartag:
            newtext = newtext.replace(
                formula, f"<{protection_tag}>" + formula + f"</{protection_tag}>"
            )
        for formula in doubledollartag:
            newtext = newtext.replace(
                formula, f"<{protection_tag}>" + formula + f"</{protection_tag}>"
            )
        # protects at the start and end of found begin and end tags.
        # TODO: transfer protection to preprocessing
        # TODO: consider format change to without tables, as a single string in list
        for tags in croptags:
            for tuplepair in tags:
                # begin tag indicates protection start
                if tags.index(tuplepair) == 0:
                    newtext = newtext.replace(
                        tuplepair, f"<{protection_tag}>" + tuplepair
                    )
                # the translation protection removal for text areas within the protected areas
                # currently separates text area as { text }
                if tags.index(tuplepair) == 1:
                    textcrop = re.findall(r"(?s)\\text({.*?})", tuplepair)
                    # removing possible duplicates from the textareas to ensure format
                    textcrop = tuple(set(textcrop))
                    for textarea in textcrop:
                        # protection is swapped, since it is within another protection
                        newtext = newtext.replace(
                            textarea,
                            f"</{protection_tag}>" + textarea + f"<{protection_tag}>",
                        )
                # ending tag indicates protection end
                elif tags.index(tuplepair) == 2:
                    newtext = newtext.replace(
                        tuplepair, tuplepair + f"</{protection_tag}>"
                    )

        # manual testing of latexparse
        # print(textblock)
        return newtext

    @staticmethod
    def styles_parse(text: str, protection_tag: str) -> str:
        """Parses all styles, this includes {.notranslate}, which is not yet implemented in TIM"""
        # might be good to separate or just have as separate ways for each edge case?
        # pictures, links, styles, etc. all function quite similarly

        new_text = text

        everything = re.findall(
            r"\[.*?\{.notranslate\}|(?<!\))\{(?!.notranslate)\S.*?\}|(?<=\])\(.*?\)(?<=\))\{.*?\}|(?<=\])\(.*?\)(?!\{)|\#.*?(?=\ )",
            new_text,
        )

        for thing in everything:
            new_text = new_text.replace(
                thing, f"<{protection_tag}>" + thing + f"</{protection_tag}>"
            )

        # manual testing of styles_parse
        # print(styleblock)

        return new_text

    @staticmethod
    def md_table_parse(text: str, protection_tag: str) -> str:
        """Parses MD tables, workflow is PanDoc => HTML => DeepL => PanDoc => MD"""
        # this works in a more unique way and might require the translator
        # to be included into the function itself, since we might have to use the beta features
        # of deepl to translate
        return text

    @staticmethod
    def plugin_parse(text: str, protection_tag: str) -> str:
        """Parses plugins, which are always individual code blocks."""
        # TODO: extremely simple plugin protection... iterate and improve.
        # possible to use documentparser: is_beginning_of_code_block()
        # then you can just place protection at start and end, if you confirm it is a plugin
        # find {} on first row, which contains plugin?
        lines = text.strip().split("\n")
        firstline = lines[0]
        lastline = lines[-1]
        # TODO: check if document block allows empty lines before and after code block section
        if firstline[0:3] == "```" and lastline[0:3] == "```":
            newtext = text
            # TODO: transfer protection tags to preprocessing
            # TODO Handle plugin by selecting translatable parts based on its (YAML) keys
            # if ("{" and "}" and "plugin=") in firstline:
            newtext = f"<{protection_tag}>" + newtext + f"</{protection_tag}>"
            return newtext
        return text


# manual testing of latexparse
# textblock = TranslationParser().latex_parse(latexblock, "deepl")
# textblock = TranslationParser().plugin_parse(pluginblock, "deepl")
# print(textblock)


# TODO This name is kinda bad. Better would be along the lines of translate-flag or a whole new list-type data structure, that describes alternating between Yes's and No's
@dataclass
class TranslateApproval:
    """Superclass for text that should or should not be
    passed to a machine translator"""

    text: str = ""


@dataclass
class Translate(TranslateApproval):
    ...


@dataclass
class NoTranslate(TranslateApproval):
    ...


def get_translate_approvals(md: str) -> list[list[TranslateApproval]]:
    """
    By parsing the input text, identify parts that should and should not be passed to a machine translator
    :param md: The input text to eventually translate
    :return: Lists containing the translatable parts of each block in a list
    """
    # Parse the string into an ast
    ast = json.loads(pypandoc.convert_text(md, format="md", to="json"))
    # By walking the ast, glue continuous translatable parts together into Translate-object and non-translatable parts into NoTranslate object
    # Add the objects into a list where they alternate T|NT, NT, T, NT ... T|NT
    block_approvals = [collect_approvals(block) for block in ast["blocks"]]
    # Return the list
    return block_approvals


def quoted_collect(content: dict) -> list[TranslateApproval]:
    arr: list[TranslateApproval] = list()
    # TODO Are quotes translate?
    quote = (
        Translate("'")
        if content[0]["t"] == "SingleQuote"
        else Translate('"')  # == "DoubleQuote"
    )

    arr.append(quote)
    for inline in content[1]:
        arr += inline_collect(inline)
    arr.append(quote)

    return arr


def cite_collect(content: dict) -> list[TranslateApproval]:
    # At the moment not needed and will break FIXME Implement this
    arr: list[TranslateApproval] = list()
    for inline in content[1]:
        arr += inline_collect(inline)
    return arr


def code_collect(content: dict) -> list[TranslateApproval]:
    # TODO Handle "Attr"
    return [NoTranslate(content[1])]


def math_collect(content: dict) -> list[TranslateApproval]:
    # TODO Handle "MathType"
    return [NoTranslate("$"), NoTranslate(content[1]), NoTranslate("$")]


def rawinline_collect(content: dict) -> list[TranslateApproval]:
    # TODO Handle "Format"
    # TODO Should this be Translate instead? Could contain words enclosed in underline-tags (<u>text</u>)?
    return [NoTranslate(content[1])]


def link_collect(content: dict) -> list[TranslateApproval]:
    arr: list[TranslateApproval] = list()
    arr.append(NoTranslate("["))
    # TODO Handle "Attr"
    for inline in content[1]:
        arr += inline_collect(inline)
    arr.append(NoTranslate("]("))
    # Do not translate URL
    arr.append(NoTranslate(content[2][0]))
    arr.append(NoTranslate(")"))
    # TODO Handle title in "Target"
    return arr


def image_collect(content: dict) -> list[TranslateApproval]:
    # TODO This is same as link_collect -> combine the functions?
    arr: list[TranslateApproval] = list()
    # TODO Handle "Attr"
    for inline in content[1]:
        arr += inline_collect(inline)
    # Do not translate URL
    arr.append(NoTranslate(content[2][0]))
    # TODO Handle title in "Target"
    return arr


def span_collect(content: dict) -> list[TranslateApproval]:
    arr: list[TranslateApproval] = list()
    # TODO Handle "Attr"
    for inline in content[1]:
        arr += inline_collect(inline)
    return arr


def inline_collect(top_inline: dict) -> list[TranslateApproval]:
    type_ = top_inline["t"]
    content = top_inline.get("c")
    arr: list[TranslateApproval] = list()
    if type_ == "Str":
        arr.append(Translate(content))
    elif type_ == "Emph":
        # NOTE Recursion
        arr.append(NoTranslate("*"))
        for inline in content:
            arr += inline_collect(inline)
        arr.append(NoTranslate("*"))
    elif type_ == "Underline":
        # NOTE Recursion
        # TODO Is this just <u>text</u> == RawInline?
        # TODO Feed example to pypandoc to learn more
        arr.append(NoTranslate("<u>"))
        for inline in content:
            arr += inline_collect(inline)
        arr.append(NoTranslate("</u>"))
    elif type_ == "Strong":
        arr.append(NoTranslate("**"))
        for inline in content:
            arr += inline_collect(inline)
        arr.append(NoTranslate("**"))
    elif type_ == "Strikeout":
        # NOTE Recursion
        # TODO Maybe same as UnderLine?
        arr.append(NoTranslate("<s>"))
        for inline in content:
            arr += inline_collect(inline)
        arr.append(NoTranslate("</s>"))
    elif type_ == "Superscript":
        arr.append(NoTranslate("^"))
        for inline in content:
            arr += inline_collect(inline)
        arr.append(NoTranslate("^"))
    elif type_ == "Subscript":
        arr.append(NoTranslate("~"))
        for inline in content:
            arr += inline_collect(inline)
        arr.append(NoTranslate("~"))
    elif type_ == "SmallCaps":
        # TODO What even is this?
        for inline in content:
            # TODO if figured out remove this notranslate
            arr += list(map(lambda x: NoTranslate(x.text), inline_collect(inline)))
    elif type_ == "Quoted":
        arr += quoted_collect(content)
    elif type_ == "Cite":
        arr += cite_collect(content)
    elif type_ == "Code":
        arr += code_collect(content)
    elif type_ == "Space":
        arr.append(Translate(" "))
    elif type_ == "SoftBreak":
        # TODO Are newlines translated or not?
        arr.append(Translate("\n"))
    elif type_ == "LineBreak":
        arr.append(NoTranslate("\\"))
        # TODO Are newlines translated or not?
        arr.append(Translate("\n"))
    elif type_ == "Math":
        arr += math_collect(content)
    elif type_ == "RawInline":
        arr += rawinline_collect(content)
    elif type_ == "Link":
        arr += link_collect(content)
    elif type_ == "Image":
        arr += image_collect(content)
    elif type_ == "Note":
        # NOTE Scary?
        for block in content:
            arr += collect_approvals(block)
    elif type_ == "Span":
        arr += span_collect(content)

    return arr


def codeblock_collect(content: dict) -> list[TranslateApproval]:
    pass


def rawblock_collect(content: dict) -> list[TranslateApproval]:
    pass


def orderedlist_collect(content: dict) -> list[TranslateApproval]:
    pass


def definitionlist_collect(content: dict) -> list[TranslateApproval]:
    pass


def header_collect(content: dict) -> list[TranslateApproval]:
    pass


def table_collect(content: dict) -> list[TranslateApproval]:
    pass


def div_collect(content: dict) -> list[TranslateApproval]:
    pass


def collect_approvals(top_block: dict) -> list[TranslateApproval]:
    """
    Walks the whole block and appends each translatable and non-translatable string-part into a list in order.
    :param top_block: The block to collect strings from
    :return: List of strings inside the correct approval-type.
    """
    arr: list[TranslateApproval] = list()
    type_ = top_block["t"]
    content = top_block["c"]
    # Based on the pandoc AST-spec at
    # https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Block
    if type_ == "Plain" or type_ == "Para":
        # TODO Need different literals before?
        for inline in content:
            arr += inline_collect(inline)
    elif type_ == "LineBlock":
        for inline_list in content:
            for inline in inline_list:
                arr += inline_collect(inline)
    elif type_ == "CodeBlock":
        # Code blocks are not translated TODO but plugins' YAML could partly be
        arr += codeblock_collect(content)
    elif type_ == "RawBlock":
        arr += rawblock_collect(content)
    elif type_ == "BlockQuote":
        # NOTE Recursion
        for block in content:
            arr += collect_approvals(block)
    elif type_ == "OrderedList":
        # NOTE Recursion
        arr += orderedlist_collect(content)
    elif type_ == "BulletList":
        # NOTE Recursion
        for block_list in content:
            for block in block_list:
                arr += collect_approvals(block)
    elif type_ == "DefinitionList":
        arr += definitionlist_collect(content)
    elif type_ == "Header":
        arr += header_collect(content)
    elif type_ == "HorizontalRule":
        arr.append(NoTranslate("***"))
    elif type_ == "Table":
        arr += table_collect(content)
    elif type_ == "Div":
        arr += div_collect(content)
    elif type_ == "Null":
        pass

    # TODO merge all subsequent Translates or NoTranslates into continuous strings inside the correct object type
    # ie. [T("foo"), T(" "), T("bar"), NT("\n"), NT("["), T("click"), NT("](www.example.com)")]
    # ==>
    # [T("foo bar"), NT("\n["), T("click"), NT("](www.example.com)")]
    return arr
