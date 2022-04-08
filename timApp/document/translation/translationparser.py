import re
import pypandoc
import json
from typing import Tuple
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
    block_approvals = [block_collect(block) for block in ast["blocks"]]
    # Return the list
    return block_approvals


def tex_collect(content: str) -> list[TranslateApproval]:
    """
    Collect and separate translatable and untranslatable areas within
    a LaTeX element.
    :param content: String which contains LaTeX area
    :return: List containing the parsed collection of LaTeX content
    """
    # Temp content to traverse through and remove parsed sections for loop.
    edit_tex = content
    # Variable containing NoTranslate and Translate objects.
    parsed_tex: list[TranslateApproval] = list()
    # Regex table for all searched sections for translation. Expand if necessary.
    regex_to_translate = [
        r"(\\text{)(.*?)(})",  # text
        r"(\\textrm{)(.*?)(})",  # textrm
        r"(\\textsf{)(.*?)(})",  # textsf
        r"(\\textbf{)(.*?)(})",  # textbf
        r"(\\textit{)(.*?)(})",  # textit
        r"(\\textup{)(.*?)(})",  # textup
        r"(\\textnormal{)(.*?)(})",  # textnormal
        r"(\\mathrm{)(.*?)(})",  # mathrm
        r"(\\mathsf{)(.*?)(})",  # mathsf
        r"(\\mathcal{)(.*?)(})",  # mathcal
        r"(\\mathit{)(.*?)(})",  # mathit
        r"(\\mathbf{)(.*?)(})",  # mathbf
        r"(<span>)(.*?)(<\/span>)",  # span
    ]
    # Full collection of all found sections for translation as or statement.
    regex_collection = "|".join(regex_to_translate)
    # Compile collection for efficiency.
    regex_pattern = re.compile(regex_collection)
    # re.findall includes empty matches, which seems the only way of resolving the issue.
    # Contains format for each list-element in the format of [NoTranslate] and [Translate] combinations.
    no_translate_textblocks = list(re.findall(regex_pattern, content))
    # Check if given LaTeX contains any areas for translation translatable.
    if len(no_translate_textblocks) != 0:
        for block in no_translate_textblocks:
            # Filter out empty groups due to re.findall().
            block = list(filter(None, block))
            # Collect the separated regex for identification in string.
            # This area is originally split as command, text, closing statement.
            full_tex_text = block[0] + block[1] + block[2]
            # Find the first case of the collected translatable LaTeX.
            temp_tex_list = edit_tex.split(full_tex_text, 1)
            # Add LaTeX command to NoTranslate section.
            parsed_tex.append(NoTranslate(temp_tex_list[0] + block[0]))
            # Add LaTeX text to Translate section.
            parsed_tex.append(Translate(block[1]))
            # Add LaTeX closing statement to next NoTranslate section.
            edit_tex = block[2] + temp_tex_list[1]
        # Include last area left from split as NoTranslate.
        parsed_tex.append(NoTranslate(edit_tex))
        return parsed_tex
    else:
        return [NoTranslate(content)]


def attr_collect(content: dict) -> list[TranslateApproval]:
    """
    :param content: Pandoc-ASTs JSON form of Attr (attributes)
    """
    if (
        not isinstance(content[0], str)
        or not (content[1], list[str])
        or not (content[2], list[list[str]])
    ):
        assert False, "PanDoc link content is not [ str, [str], [(str, str)] ]."

    # FIXME(?) WARNING It is crucial, that the attributes do not include the TIM-identifier eg. id="SAs3EK96oQtL" from {plugin="csPlugin" id="SAs3EK96oQtL"}, because Pandoc has earlier deleted extra identifiers contained in attributes like #btn-tex2 and id="SAs3EK96oQtL" with {plugin="csPlugin" #btn-tex2 id="SAs3EK96oQtL"}
    arr: list[TranslateApproval] = list()
    # https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Attr
    identifier = content[0]
    classes = content[1]
    kv_pairs = content[2]
    # Return nothing if there's no attrs
    if not (identifier or classes or kv_pairs):
        return []

    arr.append(NoTranslate("{"))
    # TODO Are spaces needed between the attributes?
    if identifier:
        arr.append(NoTranslate(f"#{identifier} "))
    arr += [NoTranslate(f".{x} ") for x in classes]
    # NOTE Is seems to be convention with TIM to surround the value with double quotes
    arr += [NoTranslate(f'{k}="{v}" ') for k, v in kv_pairs]
    # Remove extra space from last element
    arr[-1].text = arr[-1].text.strip()
    arr.append(NoTranslate("}"))
    return arr


def quoted_collect(content: dict) -> list[TranslateApproval]:
    if not isinstance(content[0]["t"], str) or not isinstance(content[1], list):
        assert False, "PanDoc format is of wrong type"

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
    if not isinstance(content[0], list) or not isinstance(content[1], list):
        assert False, "PanDoc cite content is not [ [Citation], [Inline] ]."
    # At the moment not needed and will break FIXME Implement this
    arr: list[TranslateApproval] = list()
    for inline in content[1]:
        arr += inline_collect(inline)
    return arr


def code_collect(content: dict) -> list[TranslateApproval]:
    if not isinstance(content[0], list) or not isinstance(content[1], str):
        assert False, "PanDoc Attr content is not [ Attr, Text ]."
    # TODO Handle "Attr"
    return [NoTranslate(content[1])]


def math_collect(content: dict) -> list[TranslateApproval]:
    """
    Collect and separate translatable and untranslatable areas within
    a math element.
    :param content: TeX math (literal) from Inline
    :return: List containing the parsed collection of math content
    """
    if not isinstance(content[0]["t"], str) or not isinstance(content[1], str):
        assert False, "PanDoc math content is not [ MathType, Text ]."
    arr: list[TranslateApproval] = list()
    mathtype = content[0]["t"]
    # TODO Go deeper to find translatable text
    # Double $-sign LaTeX area is InlineMath -type
    if mathtype == "DisplayMath":
        arr.append(NoTranslate("$$"))
        arr += tex_collect(content[1])
        arr.append(NoTranslate("$$"))
        return arr
    # Single $-sign LaTeX area is InlineMath -type
    elif mathtype == "InlineMath":
        arr.append(NoTranslate("$"))
        arr += tex_collect(content[1])
        arr.append(NoTranslate("$"))
        return arr
    # Only mathtypes in haskell are DisplayMath and InlineMath
    else:
        raise NotImplementedError


def rawinline_collect(content: dict) -> list[TranslateApproval]:
    """
    Collect and separate translatable and untranslatable areas within
    a rawinline element.
    :param content: RawInline from Inline
    :return: List containing the parsed collection of rawinline content
    """
    if not isinstance(content[0], str) or not isinstance(content[1], str):
        assert False, "PanDoc rawinline content is not [ Format, Text ]."
    # HTML currently as "else" path (<u></u> and <s></s>)
    format_ = content[0]
    if format_ == "tex":
        return tex_collect(content[1])
    else:
        return [NoTranslate(content[1])]


def link_collect(content: dict) -> list[TranslateApproval]:
    if (
        not isinstance(content[0], list)
        or not isinstance(content[1], list)
        or not isinstance(content[2], list)
    ):
        assert False, "PanDoc link content is not [ Attr, [Inline], Target ]."
    return link_or_image_collect(content, True)


def image_collect(content: dict) -> list[TranslateApproval]:
    if (
        not isinstance(content[0], list)
        or not isinstance(content[1], list)
        or not isinstance(content[2], list)
    ):
        assert False, "PanDoc image content is not [ Attr, [Inline], Target ]."
    return link_or_image_collect(content, False)


def link_or_image_collect(content: dict, islink: bool) -> list[TranslateApproval]:
    arr: list[TranslateApproval] = list()
    arr.append(NoTranslate("[" if islink else "!["))
    for inline in content[1]:
        arr += inline_collect(inline)
    arr.append(NoTranslate("]("))
    # Do not translate URL
    arr.append(NoTranslate(content[2][0]))
    arr.append(NoTranslate(")"))

    arr += attr_collect(content[0])

    # TODO Handle title in "Target"
    return arr


def span_collect(content: dict) -> list[TranslateApproval]:
    # Attr check in attr_collect
    if not isinstance(content[1], list):
        assert False, "PanDoc link content is not [ [Inline] ]."

    # TODO Generalize this func like with links and images
    arr: list[TranslateApproval] = list()
    arr.append(NoTranslate("["))
    for inline in content[1]:
        arr += inline_collect(inline)
    arr.append(NoTranslate("]"))

    arr += attr_collect(content[0])

    return arr


def inline_collect(top_inline: dict) -> list[TranslateApproval]:
    type_ = top_inline["t"]
    # TODO Dynamic typing would be easy here but Mypy doesn't like this
    content = top_inline.get("c", list())
    arr: list[TranslateApproval] = list()
    # Change to str
    if type_ == "Str":
        if not isinstance(content, str):
            assert False, "PanDoc inline content is not [ str ]."
        arr.append(Translate(content))
    # What?
    if type_ == "Emph":
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
    elif type_ == "Note":
        # NOTE Scary?
        for block in content:
            arr += block_collect(block)
    elif type_ == "Quoted":
        arr += quoted_collect(content)
    elif type_ == "Cite":
        arr += cite_collect(content)
    elif type_ == "Space":
        arr.append(Translate(" "))
    elif type_ == "SoftBreak":
        # TODO Are newlines translated or not?
        arr.append(Translate("\n"))
    elif type_ == "LineBreak":
        arr.append(NoTranslate("\\"))
        # TODO Are newlines translated or not?
        arr.append(Translate("\n"))
    elif type_ == "Code":
        arr += code_collect(content)
    elif type_ == "Math":
        arr += math_collect(content)
    elif type_ == "RawInline":
        arr += rawinline_collect(content)

    # Dict[any, any, any]
    elif type_ == "Link":
        arr += link_collect(content)
    elif type_ == "Image":
        arr += image_collect(content)

    elif type_ == "Span":
        arr += span_collect(content)

    return arr


def notranslate_all(type_: str, content: dict) -> list[TranslateApproval]:
    """
    Mark the whole element as non-translatable.

    TODO NOTE This function does not seem to produce markdown consistent with TIM's practices, and using this should eventually be replaced with the specific *_collect -functions!
    :param type_: Pandoc AST-type of the content
    :param content: Pandoc AST-content of the type
    :return: List of single NoTranslate -element containing Markdown representation of content
    """
    # The conversion requires the pandoc-api-version TODO This feels kinda hacky...
    ast_content = {
        "pandoc-api-version": json.loads(
            pypandoc.convert_text("", to="json", format="md")
        )["pandoc-api-version"],
        "meta": {},
        "blocks": [{"t": type_, "c": content}],
    }
    # TODO Test this. Could have problems with newlines or other coding of characters?
    json_str = json.dumps(ast_content)
    # FIXME This conversion needs to know the pandoc api version -> add into a parser-class?
    md = pypandoc.convert_text(json_str, to="md", format="json")
    return [NoTranslate(md)]


# TODO make attrs into a class
def collect_tim_plugin(attrs: dict, content: str) -> list[TranslateApproval]:
    """
    Special case to collect translatable and non-translatable parts of a TIM-plugin based on its (YAML) contents
    :param attrs: Pandoc-AST defined Attr -attributes of the plugin-block for example plugin="csPlugin"
    :param content: The raw markdown content of the plugin-defined paragraph.
    :return: List of the translatable and non-translatable parts
    """
    keys = [
        "stem",
        "buttonText",
        "button",
        "text",
        "reason",
        "placeholder",
        "header",
        "headerText",
        "questionText",
        "questionTitle",
        # Starts a list of translatable texts
        # "rows", # TODO Handle separately?
        "button",
        "inputstem",
        "inputplaceholder",
        "argsstem",
        "argsplaceholder",
        "showCodeOn",
        "showCodeOff",
        "footer",
        # "id", # TODO Allowing translation unsure
        "hidetext",
        "videoname",
        "correctText",
        "wrongText",
    ]

    arr: list[TranslateApproval] = list()
    for line in content.splitlines():
        for key in map(lambda x: f"{x}:", keys):
            if line.lstrip().startswith(key):
                nt, text = line.split(key)
                arr.append(NoTranslate(nt))
                if not text.lstrip().startswith("|"):
                    arr.append(NoTranslate(key))
                    arr.append(Translate(text))
                else:
                    # TODO handle multiline strings
                    arr.append(NoTranslate(line))
                break
        # No keys were matched if the inner for-loop does not terminate early -> the line does not contain a known key for translatable text
        else:
            arr.append(NoTranslate(line))
        arr.append(NoTranslate("\n"))
    return arr


def codeblock_collect(content: dict) -> list[TranslateApproval]:
    # NOTE Attr identifier is set to the last occurrence and rest are discarded in Pandoc-parsing eg. from #foo id=bar id=baz only baz is saved and foo and bar are lost! To fix this in regard to TIM's block ids, the id needs to be saved and injected into md after parsing with Pandoc NOTE that such an approach requires that the critical information is saved BEFORE giving the md to Pandoc
    # TODO Different plugins can be identified by Attr's 3rd index; key-value -pair for example plugin="csplugin", but this information might be unnecessary
    attr = content[0]
    attr_kv_pairs = dict(attr[2])
    if "plugin" in attr_kv_pairs:
        # NOTE Here, the attributes of codeblock are DISCARDED (as mentioned in comment above) and will not be included in the result when markdown is reconstructed ie. caller should save needed attributes
        arr: list[TranslateApproval] = list()
        arr.append(NoTranslate("```"))
        arr.append(NoTranslate("\n"))

        # TODO Maybe parse the YAML to be translated based on keys for more exact translations?
        arr += collect_tim_plugin(attr, content[1])

        arr.append(NoTranslate("```"))
        return arr

    # TODO Handle "fenced code block" (https://www.markdownguide.org/extended-syntax/#syntax-highlighting). Maybe if there is just 1 class (for example "cs" in "```cs\nvar x;\n```") then do NOT put it inside braces (like "{.cs}")
    return notranslate_all("CodeBlock", content)  # TODO


def rawblock_collect(content: dict) -> list[TranslateApproval]:
    return notranslate_all("RawBlock", content)


def orderedlist_collect(content: dict, depth: int) -> list[TranslateApproval]:
    return list_collect(content[1], depth, content[0])


def bulletlist_collect(content: dict, depth: int) -> list[TranslateApproval]:
    return list_collect(content, depth, None)


def list_collect(
    blocks: list[list[dict]], depth: int, attrs: Tuple[int, str, str] | None
) -> list[TranslateApproval]:
    """
    General method for handling both bullet- and ordered lists.

    :param blocks: The [[Block]] found in Pandoc definition for the lists.
    :param depth: The depth of recursion with lists (can contain lists of lists of lists ...).
    :param attrs: Information related to the style of the OrderedList items.
    :return: List containing the translatable parts of the list.
    """
    # Select the string that list items are prepended with ie. -, 1., i) etc.
    if attrs:
        start_num, num_style, num_delim = attrs
        # TODO Implement the rest of list styles
        list_style = "1. "
    else:
        list_style = "- "

    arr: list[TranslateApproval] = list()
    for block_list in blocks:
        # Next list item
        arr.append(NoTranslate("\n"))
        # Indentation and list item start
        arr.append(NoTranslate(("\t" * depth) + list_style))
        for block in block_list:
            arr += block_collect(block, depth + 1)
    return arr


def definitionlist_collect(content: dict) -> list[TranslateApproval]:
    return notranslate_all("DefinitionList", content)  # TODO


def header_collect(content: dict) -> list[TranslateApproval]:
    return notranslate_all("Header", content)  # TODO


def table_collect(content: dict) -> list[TranslateApproval]:
    return notranslate_all("Table", content)  # TODO


def div_collect(content: dict) -> list[TranslateApproval]:
    return notranslate_all("Div", content)  # TODO


def block_collect(top_block: dict, depth: int = 0) -> list[TranslateApproval]:
    """
    Walks the whole block and appends each translatable and non-translatable string-part into a list in order.

    Based on the pandoc AST-spec at:
    https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Block
    :param top_block: The block to collect strings from
    :param depth: The depth of the recursion if it is needed for example with list-indentation
    :return: List of strings inside the correct approval-type.
    """
    arr: list[TranslateApproval] = list()
    type_ = top_block["t"]
    content = top_block["c"]
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
            arr += block_collect(block)
    elif type_ == "OrderedList":
        # NOTE Recursion
        arr += orderedlist_collect(content, depth)
    elif type_ == "BulletList":
        # NOTE Recursion
        arr += bulletlist_collect(content, depth)
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

    # TODO this function could be good for unit-testing
    # ie. [T("foo"), T(" "), T("bar"), NT("\n"), NT("["), T("click"), NT("](www.example.com)")]
    # ==>
    # [T("foo bar"), NT("\n["), T("click"), NT("](www.example.com)")]
    if len(arr) > 0:
        merged_arr = [arr[0]]
        for elem in arr[1:]:
            if isinstance(elem, type(merged_arr[-1])):
                # Combine the texts of element and previously added if they are the same type
                merged_arr[-1].text = merged_arr[-1].text + elem.text
            else:
                # If element is different type to last, add new one
                merged_arr.append(elem)
        return merged_arr

    return arr
