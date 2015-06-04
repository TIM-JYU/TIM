

class AttributeParser:
    """
    :type string: str
    """
    def __init__(self, string):
        self.str = string
        self.current_pos = 0

    def current_char(self):
        return self.str[self.current_pos]

    def get_char(self):
        result = self.current_char()
        self.current_pos += 1
        return result

    def has_chars(self):
        return self.current_pos < len(self.str)

    def get_attributes(self):
        self.current_pos = 0
        if not self.find_attr_list_start_char():
            return [], -1
        start_index = self.current_pos - 1
        tokens = {}
        end_found = False
        while self.has_chars():
            self.eat_whitespace()
            if self.try_parse_attr_list_end_char():
                end_found = True
                break
            token = self.try_parse_hash()
            if token:
                tokens['plugin'] = token
                continue
            token = self.try_parse_class()
            if token:
                if not tokens.get('classes'):
                    tokens['classes'] = []
                tokens['classes'].append(token)
                continue
            key, val = self.try_parse_keyvaluepair()
            if key:
                tokens[key] = val
                continue
        if end_found:
            return tokens, start_index
        else:
            return [], -1

    @staticmethod
    def attr_list_start_char():
        return '{'

    def find_attr_list_start_char(self):
        self.current_pos = 0
        while self.has_chars():
            if self.get_char() == self.attr_list_start_char():
                return True
            if self.get_char() == '\\':
                self.get_char()
        return False

    def try_parse_attr_list_end_char(self):
        if self.current_char() == self.attr_list_end_char():
            return True
        return False

    def try_parse_helper(self, char):
        if self.current_char() != char:
            return None
        self.get_char()
        i = self.str.index(' ', self.current_pos)
        i2 = self.str.index(self.attr_list_end_char(), self.current_pos)
        if i < 0:
            i = i2
        if i < 0:
            raise AttributeParserException('Unexpected end of line')
        value = self.str[self.current_pos:i]
        self.current_pos = i
        return value

    def try_parse_hash(self):
        return self.try_parse_helper('#')

    def try_parse_class(self):
        return self.try_parse_helper('.')

    def try_parse_keyvaluepair(self):
        key_name = ''
        saved_pos = self.current_pos
        while True:
            if not self.has_chars():
                self.current_pos = saved_pos
                return None, None
            curr = self.get_char()
            if curr == ' ':
                self.current_pos = saved_pos
                return None, None
            if curr == '=':
                break
            key_name += curr
        if key_name == '':
            self.current_pos = saved_pos
            return None, None
        value = ''
        quote_enabled = False
        if self.get_char() == '"':
            quote_enabled = True
        while self.has_chars():
            curr = self.get_char()
            if not quote_enabled and curr == ' ':
                break
            if quote_enabled and curr == '\\':
                curr = self.get_char()
                if curr != '"' and curr != '\\':
                    raise Exception("Unknown escape character encountered: " + curr)
            if quote_enabled and curr == '"':
                return key_name, value
            value += curr
        if not quote_enabled:
            return key_name, value
        else:
            raise Exception("Missing ending quotation character")

    def eat_whitespace(self):
        while self.current_char() == ' ':
            self.current_pos += 1

    @staticmethod
    def attr_list_end_char():
        return '}'


class AttributeParserException(Exception):
    pass