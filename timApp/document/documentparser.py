import re
from contracts import contract
from document.attributeparser import AttributeParser


class SplitterException(Exception):
    pass


class DocReference:
    """
    :type lines: list[str]
    :type i: int
    :param lines:
    :param i:
    """

    def __init__(self, lines, i=0):
        self.lines = lines
        self.current_line = i

    def peek_line(self):
        return self.lines[self.current_line]

    def get_line_and_advance(self):
        result = self.peek_line()
        self.current_line += 1
        return result

    def has_more_lines(self):
        return self.current_line < len(self.lines)


class DocumentParser:
    """Splits documents into paragraphs."""
    def __init__(self):
        pass

    @contract
    def parse_document(self, doc_text: str,
                       id_func: 'str -> str',
                       hash_func) -> list(dict):
        results = []

        lines = doc_text.split("\n")
        doc = DocReference(lines)
        funcs = [self.try_parse_code_block, self.try_parse_header_block, self.parse_normal_block]
        while doc.has_more_lines():
            for func in funcs:
                result = func(doc)
                if result:
                    results.append(result)
                    # TODO: Check if the block is missing attributes (like ID/hash)
                else:
                    assert False, 'Should be impossible to come here'

        return results

    def parse_attributes(self, attribute_str: str):
        pass

    def is_beginning_of_code_block(self, doc):
        if doc.peek_line().startswith('```'):
            code_start_char = '`'
        elif doc.peek_line().startswith('~~~'):
            code_start_char = '~'
        else:
            return False, None
        match = re.match('^' + code_start_char + '+', doc.peek_line()).group(0)
        return True, match

    def is_beginning_of_header_block(self, doc):
        return doc.peek_line().startswith('#')

    def try_parse_code_block(self, doc):
        is_code_block, code_block_marker = self.is_beginning_of_code_block(doc)
        if not is_code_block:
            return None
        block_lines = [doc.get_line_and_advance()]
        tokens, start = AttributeParser(block_lines[0]).get_attributes()
        while True:
            line = doc.get_line_and_advance()
            block_lines.append(line)
            if line.startswith(code_block_marker):
                break
            if not doc.has_more_lines():
                raise SplitterException("Missing end of code block")
        return '\n'.join(block_lines), tokens

    def try_parse_header_block(self, doc):
        if not self.is_beginning_of_header_block(doc):
            return None
        block_lines = [doc.get_line_and_advance()]
        tokens, start = AttributeParser(block_lines[0]).get_attributes()
        block_lines.append(self.parse_normal_block(doc))
        return '\n'.join(block_lines), tokens

    def parse_normal_block(self, doc):
        block_lines = []
        while doc.has_more_lines():
            if self.is_beginning_of_header_block(doc) or self.is_beginning_of_code_block(doc)[0]:
                break
            block_lines.append(doc.get_line_and_advance())
        return '\n'.join(block_lines), {}