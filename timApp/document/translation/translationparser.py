"""
This module contains the main functions needed for marking parts of the
Markdown used in TIM into translatable text (human-spoken language) and
non-translatable text (syntax of Markdown and TIM-plugins for example).

Basically only the get_translate_approvals -function should be called directly
by users.
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

import json
import re
import string
from dataclasses import dataclass
from itertools import chain
from typing import Tuple, Iterable

import pypandoc

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
    r"(\\mbox{)(.*?)(})",  # mbox
]
# Full collection of all found sections for translation as or statement.
regex_collection = "|".join(regex_to_translate)
# Compile collection for efficiency.
regex_pattern = re.compile(regex_collection)

# Below are the plugin attributes which have their values translated. Currently the values which have "md:" in the
# beginning go through the parser. Parser does not handle attributes values, if they are formatted with YAML-syntax.
TRANSLATE_PLUGIN_ATTRIBUTES = [
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

PLUGIN_MD_PREFIX = "md:"
"""Prefix in plugin's values that can be parsed into Markdown. The prefix does
not contain delimiters and is not preceded by spaces."""

NOTRANSLATE_STYLE_LONG = "notranslate"
"""Longer string used for marking non-translatable text in TIM's Markdown"""

NOTRANSLATE_STYLE_SHORT = "nt"
"""Shorter string used for marking non-translatable text in TIM's Markdown"""


# TODO This name is kinda bad. Better would be along the lines of
#  translate-flag or a whole new list-type data structure, that describes
#  alternating between Yes's and No's
@dataclass
class TranslateApproval:
    """Superclass for text that should or should not be passed to a machine translator."""

    text: str = ""


@dataclass
class Translate(TranslateApproval):
    """Subclass of TranslateApproval, which indicates that the string value of the class will be translated."""

    ...


@dataclass
class NoTranslate(TranslateApproval):
    """Subclass of TranslateApproval, which indicates that the string value of the class will not be translated."""

    ...


@dataclass
class Table(TranslateApproval):
    """
    Hacky way to translate tables by identifying them at translation and setting html-tag handling on.
    """

    ...


@dataclass
class TranslationParser:
    quote: str = '"'

    def get_translate_approvals(self, md: str) -> list[TranslateApproval]:
        """
        By parsing the input text, identify parts that should and should not be
        passed to a machine translator.

        TODO Does this need to return list of lists, when the function of this is
         to split markdown into parts that can be translated or not?

        :param md: The input text to eventually translate.
        :return: Lists containing the translatable parts of each block in a list.
        """
        # Parse the string into the abstract syntax tree (AST).
        ast = json.loads(pypandoc.convert_text(md, format="md", to="json"))
        # By walking the ast, glue continuous translatable parts together into
        # Translate-object and non-translatable parts into NoTranslate object.
        # Add the objects into a list where they alternate eg.:
        # T, NT, T, NT ... T|NT
        block_approvals = list(
            self.merge_consecutive(
                chain.from_iterable(
                    self.block_collect(block) for block in ast["blocks"]
                )
            )
        )
        # NOTE Asserting that the NoTranslate and Translate alternate in the list
        # could maybe be suited here, but cannot really be done, when there are
        # objects like Table for hacky reasons...

        # Return the list
        return block_approvals

    def tex_collect(self, content: str) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within a LaTeX
        element.

        :param content: String which contains LaTeX area.
        :return: List containing the parsed collection of LaTeX content.
        """
        # Temp content to traverse through and remove parsed sections for loop.
        edit_tex = content
        # Variable containing NoTranslate and Translate objects.
        parsed_tex: list[TranslateApproval] = list()
        # re.findall includes empty matches, which seems the only way of resolving
        # the issue.
        # Contains format for each list-element in the format of [NoTranslate] and
        # [Translate] combinations.
        no_translate_textblocks = list(re.findall(regex_pattern, content))
        # Check if given LaTeX contains any areas for translation translatable.
        if len(no_translate_textblocks) != 0:
            for block in no_translate_textblocks:
                # Filter out empty groups due to re.findall().
                block = list(filter(None, block))
                # Collect the separated regex for identification in string.
                # This area is originally split as command, text, closing
                # statement.
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

    def attr_collect(
        self,
        content: list,
    ) -> Tuple[list[TranslateApproval], bool]:
        """
        Collect the parts of Attr into Markdown.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Inline

        :param content: Pandoc-ASTs JSON form of Attr (attributes): [ str, [str], [(str, str)] ].
        :return: List of non/translatable parts and boolean indicating, whether.
         the .notranslate -style was found in the element.
        """
        if (
            not isinstance(content[0], str)
            or not (content[1], list[str])
            or not (content[2], list[list[str]])
        ):
            raise Exception("PanDoc Attr content is not [ str, [str], [(str, str)] ].")

        # NOTE Attr identifier is set to the last occurrence and rest are
        # discarded in Pandoc-parsing e.g. from "#foo id=bar id=baz" only "baz"
        # is saved and "foo" and "bar" are lost! To fix this in regard to
        # TIM's block ids, the id needs to be saved before and added back into
        # md after parsing with Pandoc.

        arr: list[TranslateApproval] = list()
        # https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Attr
        identifier = content[0]
        classes = content[1]
        kv_pairs = content[2]

        # Handle the special styles in TIM, that allow user to opt out from
        # translating some text.
        is_notranslate = (
            NOTRANSLATE_STYLE_LONG in classes or NOTRANSLATE_STYLE_SHORT in classes
        )

        # Return nothing if there's no attrs
        if not (identifier or classes or kv_pairs):
            return [], is_notranslate

        # Collect the needed pieces of Markdown syntax marked to not be
        # translated.
        arr.append(NoTranslate("{"))
        # TODO Are spaces needed between the attributes?
        if identifier:
            arr.append(NoTranslate(f"#{identifier} "))
        arr += [NoTranslate(f".{x} ") for x in classes]
        # Use the specific quotes that was initialized with (related to md: in
        # plugin values).
        arr += [NoTranslate(f"{k}={self.quote}{v}{self.quote} ") for k, v in kv_pairs]
        # Remove extra space from last element
        arr[-1].text = arr[-1].text.strip()
        arr.append(NoTranslate("}"))
        return arr, is_notranslate

    def quoted_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within
        quotation marks.
        Quotation element is delimited by quotation marks.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Inline

        :param content: The types of quotation marks used and the text (list
         of inlines) from Inline element: [ QuoteType, [Inline] ].
        :return: List containing the parsed collection of Quoted content.
        """
        if not isinstance(content[0]["t"], str) or not isinstance(content[1], list):
            raise Exception("PanDoc format is of wrong type")

        arr: list[TranslateApproval] = list()
        # TODO Are quotes translate?
        quote = (
            Translate("'")
            if content[0]["t"] == "SingleQuote"
            else Translate('"')  # == "DoubleQuote"
        )

        arr.append(quote)
        for inline in content[1]:
            arr += self.inline_collect(inline)
        arr.append(quote)

        return arr

    def cite_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within a citation
        element. Citation element is delimited by citation marks.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Inline

        :param content: Citation (list of inlines) from Inline element: [ [Citation], [Inline] ].
        :return: List containing the parsed collection of Citation content.
        """
        if not isinstance(content[0], list) or not isinstance(content[1], list):
            raise Exception("PanDoc cite content is not [ [Citation], [Inline] ].")
        # At the moment not needed and will break FIXME Implement this
        arr: list[TranslateApproval] = list()
        for inline in content[1]:
            arr += self.inline_collect(inline)
        return arr

    def code_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collect everything within an Inline code element as untranslatable areas due to no clear context if the text
        should remain in the origin language or not element. Inline Code element is defined through spacing before the
        string.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Inline

        :param content: Inline code (literal) from Inline element: [ Attr, Text ].
        :return: List containing the collection of Inline code content.
        """
        if not isinstance(content[0], list) or not isinstance(content[1], str):
            raise Exception("PanDoc code content is not [ Attr, Text ].")
        # TODO Handle "Attr"
        arr: list[TranslateApproval] = list()
        arr.append(NoTranslate(f"`{content[1]}`"))

        attrs, is_notranslate = self.attr_collect(content[0])
        arr += attrs
        if is_notranslate:
            return [
                x if isinstance(x, NoTranslate) else NoTranslate(x.text) for x in arr
            ]

        return arr

    def math_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within a math
        element.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Inline

        :param content: TeX math (literal) from Inline: [ MathType, Text ].
        :return: List containing the parsed collection of math content.
        """
        if not isinstance(content[0]["t"], str) or not isinstance(content[1], str):
            raise Exception("PanDoc math content is not [ MathType, Text ].")
        arr: list[TranslateApproval] = list()
        mathtype = content[0]["t"]
        # TODO Go deeper to find translatable text
        # Double $-sign LaTeX area is InlineMath -type
        if mathtype == "DisplayMath":
            arr.append(NoTranslate("$$"))
            arr += self.tex_collect(content[1])
            arr.append(NoTranslate("$$"))
            return arr
        # Single $-sign LaTeX area is InlineMath -type
        elif mathtype == "InlineMath":
            arr.append(NoTranslate("$"))
            arr += self.tex_collect(content[1])
            arr.append(NoTranslate("$"))
            return arr
        # Only mathtypes in haskell are DisplayMath and InlineMath
        else:
            raise NotImplementedError

    def rawinline_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within a
        rawinline element.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Inline

        :param content: RawInline from Inline: [ Format, Text ].
        :return: List containing the parsed collection of rawinline content.
        """
        if not isinstance(content[0], str) or not isinstance(content[1], str):
            raise Exception("PanDoc rawinline content is not [ Format, Text ].")
        # HTML currently as "else" path (<u></u> and <s></s>)
        format_ = content[0]
        if format_ == "tex":
            return self.tex_collect(content[1])
        else:
            return [NoTranslate(content[1])]

    def link_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within a link element.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Inline

        :param content: Attr, alt text (list of inlines), target: [ Attr, [Inline], Target ].
        :return: List containing the parsed collection of link content.
        """
        if (
            not isinstance(content[0], list)
            or not isinstance(content[1], list)
            or not isinstance(content[2], list)
        ):
            raise Exception("PanDoc link content is not [ Attr, [Inline], Target ].")
        return self.link_or_image_collect(content, True)

    def image_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within an image element.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Inline

        :param content: Attr, alt text (list of inlines), target: [ Attr, [Inline], Target ].
        :return: List containing the parsed collection of image content.
        """
        if (
            not isinstance(content[0], list)
            or not isinstance(content[1], list)
            or not isinstance(content[2], list)
        ):
            raise Exception("PanDoc image content is not [ Attr, [Inline], Target ].")
        return self.link_or_image_collect(content, False)

    def link_or_image_collect(
        self, content: dict, islink: bool
    ) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within a link or image element.
        Universal collector for both link and image collect due to them having the same outline in markdown, except
        for "[" or "![" prepend.

        :param content: Attr, alt text (list of inlines), target: [ Attr, [Inline], Target ].
        :param islink: True-state if content is link-element (true=link, false=image).
        :return: List containing the parsed collection of link or image content.
        """
        arr: list[TranslateApproval] = list()
        arr.append(NoTranslate("[" if islink else "!["))
        for inline in content[1]:
            arr += self.inline_collect(inline)
        arr.append(NoTranslate("]("))
        # Do not translate URL
        arr.append(NoTranslate(content[2][0]))
        arr.append(NoTranslate(")"))

        attrs, is_notranslate = self.attr_collect(content[0])
        arr += attrs
        if is_notranslate:
            return [
                x if isinstance(x, NoTranslate) else NoTranslate(x.text) for x in arr
            ]

        # TODO Handle title in "Target"
        # Title is the text pop-up/tooltip which occurs when you hover over a link
        # For an example: [link](www.example.com "This is an example link"), where the quoted area is the title.

        return arr

    def span_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within a generic inline container with attributes.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Inline

        :param content: Generic inline container with attributes: [Attr, [Inline] ].
        :return: List containing the parsed collection of span area.
        """
        # Attr check in self.attr_collect
        if not isinstance(content[1], list):
            raise Exception("PanDoc span content is not [ [Inline] ].")

        # TODO Generalize this func like with links and images
        arr: list[TranslateApproval] = list()
        # Prepend of span area
        arr.append(NoTranslate("["))
        # Translated text within the span area
        for inline in content[1]:
            arr += self.inline_collect(inline)
        # Append of span area
        arr.append(NoTranslate("]"))

        attrs, is_notranslate = self.attr_collect(content[0])
        arr += attrs
        if is_notranslate:
            return [
                x if isinstance(x, NoTranslate) else NoTranslate(x.text) for x in arr
            ]

        return arr

    def inline_collect(self, top_inline: dict) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within an Inline element.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Inline
        Types are listed as emphasized text in the list and the values after it is the content.

        :param top_inline: Made out of type and content. Type defines the case and content is the value of that type.
        :return: List of translatable and untranslatable areas within an Inline element.
        """
        type_ = top_inline["t"]
        content = top_inline.get("c", list())
        arr: list[TranslateApproval] = list()
        match type_:
            case "Str":
                if not isinstance(content, str):
                    raise Exception("PanDoc inline content is not [ str ].")
                arr.append(Translate(content))
            case "Emph":
                # NOTE Recursion
                # TODO Marking these as Translate is DeepL-specific. Refactor into
                #  translator's preprocessing or inject the translator into parsing
                #  with a filtering-list for example.
                # These can be changed to NoTranslate but the quality of the text will suffer. The only benefits are
                # that the extreme edge cases of multiple styles nested inside each other will hold the specific
                # styles, but it will require manual translation regardless because the quality is awful.
                arr.append(Translate("<i>"))
                for inline in content:
                    arr += self.inline_collect(inline)
                arr.append(Translate("</i>"))
            case "Underline":
                # NOTE Recursion
                # TODO Is this just <u>text</u> == RawInline?
                # TODO Feed example to pypandoc to learn more
                arr.append(NoTranslate("<u>"))
                for inline in content:
                    arr += self.inline_collect(inline)
                arr.append(NoTranslate("</u>"))
            case "Strong":
                # TODO Marking these as Translate is DeepL-specific. Refactor into
                #  translator's preprocessing or inject the translator into parsing
                #  with a filtering-list for example.
                # These could be changed to NoTranslate but the quality of the text will suffer. Benefits are
                # that the extreme edge cases of multiple styles nested inside each other will hold the specific
                # styles, but it will require manual translation regardless because the quality is awful.
                arr.append(Translate("<b>"))
                for inline in content:
                    arr += self.inline_collect(inline)
                arr.append(Translate("</b>"))
            case "Strikeout":
                # NOTE Recursion
                # TODO Maybe same as UnderLine?
                arr.append(NoTranslate("<s>"))
                for inline in content:
                    arr += self.inline_collect(inline)
                arr.append(NoTranslate("</s>"))
            case "Superscript":
                # TODO Not related to this module, but spaces inside break TIM
                #  render of Superscript. eg. ^yläindeksi^ can translate into
                #  English as ^top index^, which renders badly, but rendering
                #  could be fixed by encoding spaces like ^top\ index^.

                # TODO Marking these as Translate is DeepL-specific. Refactor into
                #  translator's preprocessing or inject the translator into parsing
                #  with a filtering-list for example.
                arr.append(Translate("^"))
                for inline in content:
                    arr += self.inline_collect(inline)
                arr.append(Translate("^"))
            case "Subscript":
                # TODO Marking these as Translate is DeepL-specific. Refactor into
                #  translator's preprocessing or inject the translator into parsing
                #  with a filtering-list for example.
                arr.append(Translate("~"))
                for inline in content:
                    arr += self.inline_collect(inline)
                arr.append(Translate("~"))
            case "SmallCaps":
                # For more info see: https://pandoc.org/MANUAL.html#small-caps
                # In Markdown this is equivalent to [Small caps]{.smallcaps}
                # and in TeX it produces \textsc{Small caps}.
                # TODO Add actual handling for this type of element.
                arr += self.notranslate_all(type_, content)
            case "Note":
                # NOTE Scary?
                for block in content:
                    arr += self.block_collect(block)
            case "Quoted":
                arr += self.quoted_collect(content)
            case "Cite":
                arr += self.cite_collect(content)
            case "Space":
                arr.append(Translate(" "))
            case "SoftBreak":
                # TODO Are newlines translated or not?
                arr.append(NoTranslate("\n"))
            case "LineBreak":
                arr.append(NoTranslate("\\"))
                # TODO Are newlines translated or not?
                arr.append(NoTranslate("\n"))
            case "Code":
                arr += self.code_collect(content)
            case "Math":
                arr += self.math_collect(content)
            case "RawInline":
                arr += self.rawinline_collect(content)

            # Dict[any, any, any]
            case "Link":
                arr += self.link_collect(content)
            case "Image":
                arr += self.image_collect(content)

            case "Span":
                arr += self.span_collect(content)

            case other:
                raise Exception(f"Parser encountered unexpected type: '{other}'")

        return arr

    def notranslate_all(self, type_: str, content: dict) -> list[TranslateApproval]:
        """
        Mark the whole element as non-translatable.

        TODO NOTE This function does not seem to produce Markdown consistent with
         TIM's practices, and using this should eventually be replaced with the
         specific *_collect -functions!

        :param type_: Pandoc AST-type of the content.
        :param content: Pandoc AST-content of the type.
        :return: List of single NoTranslate -element containing Markdown
         representation of content.
        """
        # The conversion requires the pandoc-api-version which is extracted here a
        # bit hacky...
        # TODO The information could be added into a Parser-class containing the
        #  module's functions.
        ast_content = {
            "pandoc-api-version": json.loads(
                pypandoc.convert_text("", to="json", format="md")
            )["pandoc-api-version"],
            "meta": {},
            "blocks": [{"t": type_, "c": content}],
        }
        json_str = json.dumps(ast_content)
        md = pypandoc.convert_text(json_str, to="md", format="json")
        return [Table(md) if type_ == "Table" else NoTranslate(md)]

    # TODO make attrs into a class
    def collect_tim_plugin(self, attrs: dict, content: str) -> list[TranslateApproval]:
        """
        Special case to collect translatable and non-translatable parts of a
        TIM-plugin based on its (YAML) contents.

        :param attrs: Pandoc-AST defined Attr -attributes of the plugin-block for
            example plugin="csPlugin".
            TODO Add handling for this if necessary.
        :param content: The raw markdown content of the plugin-defined paragraph.
        :return: List of the translatable and non-translatable parts.
        """

        arr: list[TranslateApproval] = list()
        lines = iter(content.splitlines())
        while (line := next(lines, None)) is not None:
            for key_s in map(lambda x: f"{x}:", TRANSLATE_PLUGIN_ATTRIBUTES):
                # Current line contains an attribute for translation
                if line.lstrip().startswith(key_s):
                    # Extract text from the line for translation
                    nt, text = line.split(key_s, 1)
                    arr.append(NoTranslate(nt))
                    arr.append(NoTranslate(key_s))
                    # Save leading whitespace from value to avoid wasted translation
                    if text_start := text.lstrip():
                        text_start_i = text.index(text_start)
                        nt, text = text[:text_start_i], text[text_start_i:]
                        arr.append(NoTranslate(nt))
                        start_char = text_start[0]
                    else:
                        # Value is whitespace.
                        arr.append(NoTranslate(text))
                        break
                    if start_char == '"' or start_char == "'":
                        # Line is a multiline (quoted) value.
                        arr.append(NoTranslate(start_char))
                        # Skip the start_char
                        text = text[1:]
                        # Get text until next occurrence of the quote.
                        # NOTE An area prone to bugs seems to be when
                        # multiline-values are after each other
                        # NOTE Translation could worsen because of YAML indentation
                        while (
                            start_char not in text
                            and (line := next(lines, None)) is not None
                        ):
                            text += "\n" + line
                        # Add the lines content, that included the start_char
                        text = text[: text.index(start_char)]
                        # If the text contains Markdown, select the correct
                        # quote to use as not to break the syntax.
                        self.add_value_with_prefix(
                            text, arr, "'" if start_char == '"' else '"'
                        )
                        arr.append(NoTranslate(start_char))
                    elif start_char == "|":
                        # Line is a multiline (ie. "|xx .* xx") value.
                        start_symbol = text_start[1:]
                        arr.append(NoTranslate(start_char + start_symbol))
                        # Remove until the starting symbol
                        text = text[text.index(start_symbol) + len(start_symbol) :]
                        while (line := next(lines, None)) is not None:
                            if start_symbol in line:
                                break
                            text += "\n" + line
                        self.add_value_with_prefix(text, arr)
                        # Add the end of multiline
                        # TODO If line is None here, should raise an exception
                        #  because of malformed TIM-YAML?
                        arr.append(NoTranslate("\n" + (line or "")))
                    else:
                        # Line is a single line value.
                        self.add_value_with_prefix(text, arr)
                    break
            # No keys were matched if the inner for-loop does not terminate early
            # -> the line does not contain a known key for translatable text
            else:
                arr.append(NoTranslate(line))
            arr.append(NoTranslate("\n"))
        return arr

    def add_value_with_prefix(
        self, text: str, arr: list[TranslateApproval], plugin_quote: str = '"'
    ) -> None:
        """
        Separates the contents of a YAML string-prefix and value found in plugins
        and adds to the list.

        The text can possibly start with the "md:" prefix (NoTranslate) for
        content that is Markdown, and the rest after that is the value (Translate).

        :param text: The text that can be contained with (plugin).
        :param arr: The list that the results will be added to.
        :param plugin_quote: The quote to use inside the potential Markdown.
        :return: None, the result is inserted into the arr-parameter.
        """
        if text.lstrip().startswith(PLUGIN_MD_PREFIX):
            # Save the original leading whitespace.
            leading_wspace = text[: len(text) - len(text.lstrip())]
            text = text.removeprefix(leading_wspace + PLUGIN_MD_PREFIX)
            arr.append(NoTranslate(leading_wspace + PLUGIN_MD_PREFIX))
            # Remove leading whitespace from each line of the text, because
            # otherwise it might get interpreted as a CodeBlock, which will
            # not be translated at all.
            # TODO Save the original indentation somehow (use the YAML-parser?)
            text = "\n".join(line.lstrip() for line in text.splitlines())
            # Parse Markdown here. Select the correct quote to use, if the
            # Markdown-string is inside quotes (i.e. it is a multiline value).
            sub_parser = TranslationParser(quote=plugin_quote)
            parsed_md = sub_parser.get_translate_approvals(text)
            # Skip the initial newline(s) that is prepended into blocks,
            # because it can break when the plugin value is on a single line.
            parsed_md[0].text = parsed_md[0].text.lstrip("\n")
            parsed_md[-1].text = parsed_md[-1].text.rstrip("\n")
            arr += parsed_md
        else:
            # Translate normal text.
            arr.append(Translate(text))

    def codeblock_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Pick translatable and non-translatable parts off of a codeblock.

        NOTE/WARNING In regard to plugins:

        It is critical that the attributes do not include the TIM-identifier eg.
        id="SAs3EK96oQtL" from {plugin="csPlugin" id="SAs3EK96oQtL"}, because
        Pandoc deletes extra identifiers contained in attributes like #btn-tex2
        and id="SAs3EK96oQtL" in {plugin="csPlugin" #btn-tex2 id="SAs3EK96oQtL"}.
        Here, the attributes of a plugin-codeblock are DISCARDED and will not be
        included in the result when markdown is reconstructed i.e. caller should
        save the attributes if needed.

        :param content: List with the attributes and text-content of the codeblock.
        :return: List marking the Markdown representation of the element into
                translatable and non-translatable parts.
        """
        # TODO where to typecheck Attr?
        if not isinstance(content[1], str):
            raise Exception("PanDoc codeblock content is not [ Attr, [Block] ].")

        attr = content[0]
        attr_kv_pairs = dict(attr[2])
        is_plugin = "plugin" in attr_kv_pairs
        attrs, is_notranslate = self.attr_collect(attr)

        arr: list[TranslateApproval] = list()

        # NOTE Pandoc does not tell us what syntax (N amount of `-marks or
        # indentation) the original markdown used with the codeblock.
        arr.append(NoTranslate("```"))

        if is_plugin:
            # TODO Maybe parse the YAML to be translated based on the attrs for
            #  more exact translations?
            arr.append(NoTranslate("\n"))
            arr += self.collect_tim_plugin(attr, content[1])
        else:
            # FIXME The syntax-style (e.g. "```cs ..." for C#) is moved into
            #  Attr-classes in result instead of appended to the starting "```".
            #  This is apparently called a "fenced code block"
            #  (https://www.markdownguide.org/extended-syntax/#syntax-highlighting).
            #  Maybe if there is just 1 class (for example "cs" in "```cs\nvar
            #  x;\n```") then do NOT put it inside braces (like "{.cs}")
            arr += attrs

            # TODO To handle the case, where content of a code block contains
            #  codeblock syntax, parse until no codeblocks are found. Ie.
            # ````                  ==> ```` # Level 0 code block
            # ```                   ==> ``` # The bottom-level code block (3 * "`")
            # My codeblock example  ==> My codeblock example
            # ```                   ==> ```
            # ````                  ==> ````
            arr.append(NoTranslate("\n"))
            arr.append(NoTranslate(content[1]))
            arr.append(NoTranslate("\n"))

        arr.append(NoTranslate("```"))

        if is_notranslate:
            return [
                x if isinstance(x, NoTranslate) else NoTranslate(x.text) for x in arr
            ]

        return arr

    def rawblock_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Pick translatable and non-translatable parts from a rawblock.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Block

        :param content: The Raw block [ Format, Text ].
        :return: List of single NoTranslate -element containing Markdown representation of rawblock element.
        """
        return self.notranslate_all("RawBlock", content)

    def orderedlist_collect(self, content: dict, depth: int) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within an ordered list element through recursion.
        Calls to list_collect to handle recursion through block_collect.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Block

        :param content: Ordered list (attributes and a list of items, each a list of blocks):
                        [ ListAttributes, [[Block]] ].
        :param depth: The current depth of the list, used for indentation.
        :return: List of translatable and untranslatable areas within an ordered list element.
        """
        # TODO: how to check the tuple.
        # For an example: isinstance(v, tuple) and list(map(type, v)) == [str, int]
        if (
            not isinstance(content[0], list)
            or not isinstance(content[0][0], int)
            or "t" not in content[0][1]
            and isinstance(content[0][1]["t"], str)
            or "t" not in content[0][2]
            and isinstance(content[0][2]["t"], str)
            or not isinstance(content[1], list)
        ):
            raise Exception(
                "PanDoc orderedlist content is not [ ListAttributes, [[Block]] ]."
            )
        # TODO: fix after tuple check as content[0].
        return self.list_collect(
            content[1],
            depth,
            (int(content[0][0]), str(content[0][1]), str(content[0][2])),
        )

    def bulletlist_collect(self, content: dict, depth: int) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within a bullet list element through recursion.
        Calls to list_collect to handle recursion through block_collect.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Block

        :param content: Bullet list (attributes and a list of items, each a list of blocks):
                        [ ListAttributes, [[Block]] ].
        :param depth: The current depth of the list, used for indentation.
        :return: List of translatable and untranslatable areas within a bullet list element.
        """
        if not isinstance(content, list):
            raise Exception("PanDoc bulletlist content is not [ [Block] ].")
        return self.list_collect(content, depth, None)

    def ordered_list_styling(
        self, start_num: int, num_style: str, num_delim: str
    ) -> str:
        """
        Makes the style for the ordered lists.

        Different styles for ordered lists:
        num_styles - Decimal (1,2,3), LowerRoman(i,ii,iii), LowerAlpha(a,b,c),
                     UpperRoman (I,III,III), UpperAlpha(A,B,C), DefaultStyle (#)
        num_delims - Period( . ), OneParen( ) ), DefaultDelim ( . ),
                     TwoParens ( (#) )

        :param start_num: The number that starts the list.
        :param num_style: The numbering style.
        :param num_delim: The punctuation for list.
        :returns: The list style that needs to be used.
        """

        list_style = ""
        if "DefaultStyle" in num_style:
            list_style = "#"
        elif "Decimal" in num_style:
            list_style = str(start_num)
        elif "LowerRoman" in num_style:
            list_style = to_roman_numeral(start_num).lower()
        elif "UpperRoman" in num_style:
            list_style = to_roman_numeral(start_num).upper()
        elif "LowerAlpha" in num_style:
            list_style = to_alphabet(start_num).lower()
        elif "UpperAlpha" in num_style:
            list_style = to_alphabet(start_num).upper()

        if "Period" in num_delim or "DefaultDelim" in num_delim:
            list_style = list_style + ". "
        elif "OneParen" in num_delim:
            list_style = list_style + ") "
        elif "TwoParens" in num_delim:
            list_style = "(" + list_style + ") "

        return list_style

    def list_collect(
        self, blocks: list[list[dict]], depth: int, attrs: Tuple[int, str, str] | None
    ) -> list[TranslateApproval]:
        """
        General method for handling both bullet- and ordered lists.

        :param blocks: The [[Block]] found in Pandoc definition for the lists.
        :param depth: The depth of recursion with lists (can contain lists of lists.
                of lists ...).
        :param attrs: The information related to the style of the OrderedList items.
        :return: List containing the translatable parts of the list.
        """

        # Select the string that list items are prepended with ie. -, 1., i) etc.
        list_style = ""
        start_num = 0
        if attrs:
            start_num, num_style, num_delim = attrs

        else:
            list_style = "- "

        arr: list[TranslateApproval] = list()
        for block_list in blocks:
            # Next list item
            arr.append(NoTranslate("\n"))
            # Indentation and list item start
            arr.append(
                NoTranslate(
                    ("\t" * depth)
                    + (
                        self.ordered_list_styling(start_num, num_style, num_delim)
                        if not list_style
                        else list_style
                    )
                )
            )

            for block in block_list:
                # The paragraphs in items except the 1st need to be as indented as
                # the item start (ie. "- " or "1. " etc).
                # TODO Handle the indentation inside list item's element.
                #  It seems hard to control the indentation of paragraphs at
                #  this level and in some cases the list-items break.
                arr += self.block_collect(block, depth + 1)

            start_num = start_num + 1

        # Handle edge case of separation between Markdown lists and paragraphs.
        if depth == 0:
            arr.append(Translate("\n"))

        return arr

    def definitionlist_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collect definition list areas as untranslatable.
        Each list item is a pair consisting of a term (a list of inlines) and one or more definitions
        (each a list of blocks).

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Block

        :param content: Definition list. : [([Inline], [[Block]])].
        :return: List of single NoTranslate -element containing Markdown representation of definition list.
        """
        return self.notranslate_all(
            "DefinitionList", content
        )  # TODO: Uncertain what definition lists are in Markdown.

    def header_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collect and separate translatable and untranslatable areas within a header from a block.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Block

        :param content: Header's level (integer) and text (inlines): [ int, Attr, [Inline]  ].
        :return: List of translatable and untranslatable areas within a header element.
        """
        # Attr check in self.attr_collect
        # TODO Should we follow this convention? ATM other funcs like link_collect
        #  make the check at their level...
        if not isinstance(content[0], int) or not isinstance(content[2], list):
            raise Exception(
                "PanDoc OrderedList content is not [ int, Attr, [Inline]  ]"
            )

        level = content[0]
        arr: list[TranslateApproval] = list()

        # Prepend Headers with an empty line (see block_collect, where the first
        # newline is added)
        arr.append(NoTranslate(f"\n{'#' * level} "))
        for inline in content[2]:
            arr += self.inline_collect(inline)

        # NOTE Apparently Pandoc likes to add to headers their text-content as
        # identifier, which does not seem to be a TIM-convention.
        # TODO If it turns out to be problematic, remove the extra identifier that
        #  Pandoc automatically adds (Needs handling [Inline])

        attrs, is_notranslate = self.attr_collect(content[1])
        arr += attrs
        if is_notranslate:
            return [
                x if isinstance(x, NoTranslate) else NoTranslate(x.text) for x in arr
            ]
        return arr

    def table_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collect table areas as untranslatable.

        Refer to Pandoc definition for tables:
        https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Block

        :param content: Table content as dict.
        :return: List of single NoTranslate -element containing Markdown representation of table.
        """
        # TODO This is handled and translated separately for DeepL but when you include other machine translators
        #  this will need to be handled for them.
        return self.notranslate_all("Table", content)

    def div_collect(self, content: dict) -> list[TranslateApproval]:
        """
        Collects generic block container with attributes as untranslatable.

        Pandoc: https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Block

        :param content: Generic block container with attributes: [ Attr [Block] ].
        :return: List of single NoTranslate -element containing Markdown representation of div element.
        """
        # This is not handled as there are no div cases to replicate in TIM.
        return self.notranslate_all("Div", content)

    def block_collect(self, top_block: dict, depth: int = 0) -> list[TranslateApproval]:
        """
        Walks the whole block and appends each translatable and non-translatable
        string-part into a list in order.
        Adds newlines to the start of each block and end of some specific blocks, for Markdown syntax.
        These newlines are required due to Pandoc removing the newlines in formatting.

        Based on the pandoc AST-spec at:
        https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Block

        :param top_block: The block to collect strings from.
        :param depth: The depth of the recursion if it is needed for example with
                    list-indentation.
        :return: List of strings inside the correct approval-type.
        """
        arr: list[TranslateApproval] = list()
        type_ = top_block["t"]
        content = top_block.get("c", None)

        # All blocks should be prepended by newline
        # TODO figure out the "correct" way to include newlines. The Combination of Pandoc markdown and DeepL
        #  creates and inconsistent structure on when and how to add newlines. Currently they are included into
        #  the Markdown handling in parser.
        # TODO: Unordered list might have issues with this.
        if depth == 0:
            arr.append(Translate("\n"))

        match type_:
            case "Plain":
                # TODO Need different literals before?
                for inline in content:
                    arr += self.inline_collect(inline)
            case "Para":
                for inline in content:
                    arr += self.inline_collect(inline)
                # "A paragraph is simply one or more consecutive lines of text,
                # separated by one or more blank lines."
                # https://daringfireball.net/projects/markdown/syntax#p
                arr.append(Translate("\n"))
            case "LineBlock":
                for inline_list in content:
                    for inline in inline_list:
                        arr += self.inline_collect(inline)
            case "CodeBlock":
                # Code blocks are not translated
                arr += self.codeblock_collect(content)
            case "RawBlock":
                arr += self.rawblock_collect(content)
            case "BlockQuote":
                # NOTE Recursion
                # FIXME The block_collect adds an unneeded newline (but it does not
                #  seem to matter for TIM-rendering). A HACK would be to pass
                #  depth=1 to it...
                arr.append(NoTranslate("> "))
                for block in content:
                    arr += self.block_collect(block)
            case "OrderedList":
                # NOTE Recursion
                arr += self.orderedlist_collect(content, depth)
            case "BulletList":
                # NOTE Recursion
                arr += self.bulletlist_collect(content, depth)
            case "DefinitionList":
                arr += self.definitionlist_collect(content)
            case "Header":
                arr += self.header_collect(content)
                # Add newline after Header so an empty line is produced if a block
                # follows
                arr.append(Translate("\n"))
            case "HorizontalRule":
                arr.append(NoTranslate("***"))
            case "Table":
                arr += self.table_collect(content)
            case "Div":
                arr += self.div_collect(content)
            case "Null":
                pass
            case x:
                raise Exception(f"Parser encountered unexpected type: '{x}'")

        return arr

    def merge_consecutive(
        self, arr: Iterable[TranslateApproval]
    ) -> list[TranslateApproval]:
        """
        Merge consecutive elements of the same type into each other to reduce
        length of the list.

        The merging is as follows (T = Translate, NT = NoTranslate):

        [T("foo"), T(" "), T("bar"), NT("\n"), NT("["), T("click"),
        NT("](www.example.com)")]

        ==>

        [T("foo bar"), NT("\n["), T("click"), NT("](www.example.com)")]

        :param arr: The list of objects to merge.
        :return: Merged list.
        """
        # TODO this function could be good for unit-testing
        merged_arr: list[TranslateApproval] = list()
        arr_iter = iter(arr)
        elem = next(arr_iter, None)
        if elem:
            last_elem_s, last_elem_t = elem.text, type(elem)
            while True:
                elem = next(arr_iter, None)
                if isinstance(elem, last_elem_t):
                    # Combine the texts of element and previously added if they
                    # are the same type.
                    last_elem_s += elem.text
                else:
                    # If element is different type to last, save last and start
                    # collecting the new one.
                    last_obj = last_elem_t(last_elem_s)
                    merged_arr.append(last_obj)
                    if elem is None:
                        break
                    last_elem_t = type(elem)
                    last_elem_s = elem.text
        return merged_arr


def to_roman_numeral(num: int) -> str:
    """
    Converts the start number from Pandoc's Roman number list to the
    corresponding number.
    Source:
    https://stackoverflow.com/questions/28777219/basic-program-to-convert-integer-to-roman-numerals

    :param num: The list's starting number.
    :returns: The Roman number corresponding the starting number.
    """
    romans = [
        (1000, "M"),
        (900, "CM"),
        (500, "D"),
        (400, "CD"),
        (100, "C"),
        (90, "XC"),
        (50, "L"),
        (40, "XL"),
        (10, "X"),
        (9, "IX"),
        (5, "V"),
        (4, "IV"),
        (1, "I"),
    ]

    roman = ""

    while num > 0:
        for i, r in romans:
            while num >= i:
                roman += r
                num -= i

    return roman


def to_alphabet(num: int) -> str:
    """
    Converts the start number from Pandoc's alphabet list to the corresponding
    character.

    :param num: The list's starting number.
    :returns: The alphabet corresponding the starting number.
    """
    alphabet = string.ascii_lowercase
    n = len(alphabet)

    # Adapted from:
    # https://codereview.stackexchange.com/questions/182733/base-26-letters-and-base-10-using-recursion
    result = ""

    while num > 0:
        # Minus one in order to prevent (N * 26) % 26.
        num, remainder = (num - 1) // n, (num - 1) % n
        # The first letter A matches 1 which here is indexed as 0.
        result += alphabet[remainder]

    return "".join(result)[::-1]
