import re

from documentmodel.attributeparser import AttributeParser
from documentmodel.randutils import hashfunc, random_id, is_valid_id


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

    :type _blocks: list[dict]
    :type _doc_text: str
    :type _last_setting: tuple
    """

    def __init__(self, doc_text=''):
        """

        :type doc_text: str
        """
        self._doc_text = doc_text
        self._blocks = None
        self._break_on_empty_line = False
        self._last_setting = ()

    def get_blocks(self, break_on_empty_line=False,
                   break_on_code_block=True,
                   break_on_header=True,
                   break_on_normal=True):
        self._parse_document(break_on_empty_line=break_on_empty_line,
                             break_on_code_block=break_on_code_block,
                             break_on_header=break_on_header,
                             break_on_normal=break_on_normal)
        return self._blocks

    def add_missing_attributes(self, hash_func=hashfunc, id_func=random_id):
        self._parse_document(*self._last_setting)
        for r in self._blocks:
            r['t'] = hash_func(r['md'])
            if not r.get('id'):
                r['id'] = id_func()
        return self

    def validate_structure(self, id_validator_func=is_valid_id):
        self._parse_document(*self._last_setting)
        found_ids = set()
        found_areas = set()
        classed_areas = []
        found_area_ends = set()
        for r in self._blocks:
            curr_id = r.get('id')
            if curr_id is not None:
                if curr_id in found_ids:
                    raise ValidationException('Duplicate paragraph id: ' + curr_id)
                found_ids.add(curr_id)
                if not id_validator_func(curr_id):
                    raise ValidationException('Invalid paragraph id: ' + curr_id)
            attrs = r.get('attrs', {})
            area = attrs.get('area')
            if area:
                if area in found_areas:
                    raise ValidationException('Cannot have multiple areas with same name: ' + area)
                has_classes = len(attrs.get('classes', [])) > 0
                if has_classes:
                    classed_areas.append(area)
                found_areas.add(area)
            area_end = attrs.get('area_end')
            if area_end:
                if area_end == area:
                    raise ValidationException('Cannot have a zero-length area')
                if area_end in classed_areas:
                    if area_end != classed_areas[-1]:
                        raise ValidationException('Classed areas cannot overlap ("{}" and "{}")'
                                                  .format(classed_areas[-1], area))
                    classed_areas.pop()
                if area_end not in found_areas:
                    raise ValidationException('No start found for area "{}"'.format(area_end))
                if area_end in found_area_ends:
                    raise ValidationException('Area already ended: ' + area_end)
                found_area_ends.add(area_end)
        unended_areas = found_areas - found_area_ends
        if unended_areas:
            raise ValidationException('Some areas were not ended: ' + str(unended_areas))
        return self

    def _parse_document(self, break_on_empty_line=False,
                        break_on_code_block=True,
                        break_on_header=True,
                        break_on_normal=True):
        if self._last_setting == (break_on_empty_line, break_on_code_block, break_on_header, break_on_normal):
            return
        self._blocks = []
        self._break_on_empty_line = break_on_empty_line
        self._last_setting = (break_on_empty_line, break_on_code_block, break_on_header, break_on_normal)
        lines = self._doc_text.split("\n")
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
                    result['md'] = result['md'].rstrip().strip('\r\n')
                    if ((result['type'] == 'code' and not break_on_code_block)
                        or (result['type'] == 'header' and not break_on_header)
                        or (result['type'] == 'autonormal' and not break_on_normal)) \
                            and not result.get('attrs') and len(self._blocks) > 0 \
                            and not self._blocks[-1].get('attrs', {}).get('plugin') \
                            and self._blocks[-1]['type'] != 'atom':
                        self._blocks[-1]['md'] += '\n\n' + result['md']
                    else:
                        self._blocks.append(result)
                    break

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

    def is_empty_line(self, doc):
        """

        :type doc: DocReader
        """
        return doc.peek_line().isspace() or doc.peek_line() == ''

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
        line = None
        while True:
            if not doc.has_more_lines():
                break
            line = doc.get_line_and_advance()
            if line.startswith(code_block_marker):
                break
            block_lines.append(line)
        if not is_atom and line is not None:
            block_lines.append(line)
        result = {'md': '\n'.join(block_lines), 'type': 'atom' if is_atom else 'code'}
        self.extract_attrs(result, tokens)
        return result

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
        block_type = 'normal'
        if not header_line.startswith('#-'):
            block_type = 'header'
            block_lines.append(header_line[:start].strip())
        block_lines.append(self.parse_normal_block(doc)['md'])
        result = {'md': '\n'.join(block_lines), 'type': block_type}
        self.extract_attrs(result, tokens)
        return result

    def parse_normal_block(self, doc):
        """

        :type doc: DocReader
        """
        block_lines = []
        while doc.has_more_lines():
            if self.is_beginning_of_header_block(doc) \
                    or self.is_beginning_of_code_block(doc)[0] \
                    or (self._break_on_empty_line and self.is_empty_line(doc)):
                break
            block_lines.append(doc.get_line_and_advance())
        return {'md': '\n'.join(block_lines), 'type': 'autonormal'}

    def extract_attrs(self, result, tokens):
        for builtin in ('id', 't'):
            if builtin in tokens:
                result[builtin] = tokens.pop(builtin)
        if len(tokens) > 0:
            result['attrs'] = tokens

    def eat_whitespace(self, doc):
        """

        :rtype: NoneType
        :type doc: DocReader
        """
        while doc.has_more_lines() and self.is_empty_line(doc):
            doc.get_line_and_advance()
        return None
