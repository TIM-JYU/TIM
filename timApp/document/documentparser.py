import re
from contracts import contract
from document.attributeparser import AttributeParser


class SplitterException(Exception):
    pass


class DocReference:
    """
    :type lines: list[str]
    :type i: int
    :type current_line: int
    :param lines:
    :param i:
    """

    def __init__(self, lines, i=0):
        self.lines = lines
        self.current_line = i

    def peek_line(self):
        """

        :rtype: str
        :return:
        """
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

    def parse_document(self, doc_text):
        results = []

        lines = doc_text.split("\n")
        doc = DocReference(lines)
        funcs = [self.eat_whitespace,
                 self.try_parse_code_block,
                 self.try_parse_header_block,
                 self.parse_normal_block]
        while True:
            self.eat_whitespace(doc)
            if not doc.has_more_lines():
                break
            for func in funcs:
                result = func(doc)
                if result:
                    results.append(result)
                    # TODO: Check if the block is missing attributes (like ID/hash)
                    break
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
        start_line = doc.get_line_and_advance()
        block_lines = []
        tokens, start = AttributeParser(start_line).get_attributes()
        block_lines.append(start_line[:start].strip())
        while True:
            line = doc.get_line_and_advance()
            block_lines.append(line)
            if line.startswith(code_block_marker):
                break
            if not doc.has_more_lines():
                raise SplitterException("Missing end of code block")
        tokens['md'] = '\n'.join(block_lines)
        return tokens

    def try_parse_header_block(self, doc):
        """
        :type doc: DocReference
        :param doc:
        :return:
        """
        if not self.is_beginning_of_header_block(doc):
            return None
        header_line = doc.get_line_and_advance()
        block_lines = []
        tokens, start = AttributeParser(header_line).get_attributes()
        if not header_line.startswith('#-'):
            block_lines.append(header_line[:start].strip())
        block_lines.append(self.parse_normal_block(doc)['md'])
        tokens['md'] = '\n'.join(block_lines)
        return tokens

    def parse_normal_block(self, doc):
        block_lines = []
        while doc.has_more_lines():
            if self.is_beginning_of_header_block(doc) or self.is_beginning_of_code_block(doc)[0]:
                break
            block_lines.append(doc.get_line_and_advance())
        return {'md': '\n'.join(block_lines)}

    def eat_whitespace(self, doc):
        """
        :type doc: DocReference
        """
        if not doc.has_more_lines():
            return None
        if doc.peek_line().isspace() or doc.peek_line() == '':
            doc.get_line_and_advance()
        return None
