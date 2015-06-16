import re
from contracts import contract
from document.attributeparser import AttributeParser
from document.randutils import hashfunc, random_id, is_valid_id


class SplitterException(Exception):
    pass


class ValidationException(Exception):
    pass


class DocReader:
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
    """Splits documents into paragraphs.

    :type blocks: list[dict]
    :type doc_text: str
    """
    def __init__(self, doc_text=''):
        """

        :type doc_text: str
        """
        self.doc_text = doc_text
        self.blocks = None

    def set_text(self, doc_text):
        """

        :type doc_text: str
        """
        self.doc_text = doc_text
        return self

    def add_missing_attributes(self, hash_func=hashfunc, id_func=random_id):
        for r in self.blocks:
            r['t'] = hash_func(r['md'])
            if not r.get('id'):
                r['id'] = id_func()

    def validate_ids(self, id_validator_func=is_valid_id):
        for r in self.blocks:
            curr_id = r.get('id')
            if curr_id is not None:
                if not id_validator_func(curr_id):
                    raise ValidationException('Invalid paragraph id: ' + curr_id)
        return True

    def parse_document(self):
        self.blocks = []

        lines = self.doc_text.split("\n")
        doc = DocReader(lines)
        funcs = [self.try_parse_code_block,
                 self.try_parse_header_block,
                 self.parse_normal_block]
        while True:
            self.eat_whitespace(doc)
            if not doc.has_more_lines():
                break
            for func in funcs:
                result = func(doc)
                if result:
                    self.blocks.append(result)
                    break
        return self.blocks

    def is_beginning_of_code_block(self, doc):
        """

        :type doc: DocReader
        """
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
        """

        :type doc: DocReader
        :rtype: dict
        """
        is_code_block, code_block_marker = self.is_beginning_of_code_block(doc)
        if not is_code_block:
            return None
        start_line = doc.get_line_and_advance()
        block_lines = []
        tokens, start = AttributeParser(start_line).get_attributes()
        is_atom = tokens.get('atom', False)
        if is_atom:
            tokens.pop('atom')
        else:
            first_line = start_line[:start].strip()
            block_lines.append(first_line)
        while True:
            if not doc.has_more_lines():
                raise SplitterException("Missing end of code block")
            line = doc.get_line_and_advance()
            if line.startswith(code_block_marker):
                break
            block_lines.append(line)
        if not is_atom:
            block_lines.append(line)
        tokens['md'] = '\n'.join(block_lines)
        return tokens

    def try_parse_header_block(self, doc):
        """

        :rtype: dict
        :type doc: DocReader
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
        """

        :type doc: DocReader
        """
        block_lines = []
        while doc.has_more_lines():
            if self.is_beginning_of_header_block(doc) or self.is_beginning_of_code_block(doc)[0]:
                break
            block_lines.append(doc.get_line_and_advance())
        return {'md': '\n'.join(block_lines)}

    def eat_whitespace(self, doc):
        """

        :rtype: NoneType
        :type doc: DocReader
        """
        if not doc.has_more_lines():
            return None
        if doc.peek_line().isspace() or doc.peek_line() == '':
            doc.get_line_and_advance()
        return None
