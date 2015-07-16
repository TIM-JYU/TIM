

class AttributeParser:
    """
    :type string: str
    :type current_pos: int
    """
    def __init__(self, string=''):
        self.str = string
        self.current_pos = 0

    def set_str(self, string):
        self.str = string
        return self

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
            return {}, None
        start_index = self.current_pos - 1
        tokens = {}
        end_found = False
        while self.has_chars():
            self.eat_whitespace()
            if not self.has_chars():
                break
            if self.try_parse_attr_list_end_char():
                end_found = True
                break
            token = self.try_parse_hash()
            if token:
                if 'taskId' in tokens:
                    break
                tokens['taskId'] = token
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
            break
        if end_found:
            return tokens, start_index
        else:
            return {}, None

    @staticmethod
    def attr_list_start_char():
        return '{'

    def find_attr_list_start_char(self):
        self.current_pos = 0
        while self.has_chars():
            curr = self.get_char()
            if curr == self.attr_list_start_char():
                return True
            if curr == '\\':
                if not self.has_chars():
                    return False
                self.get_char()
        return False

    def try_parse_attr_list_end_char(self):
        if self.current_char() == self.attr_list_end_char():
            return True
        return False

    def try_parse_helper(self, char):
        if self.current_char() != char:
            return None
        find_pos = self.current_pos + 1
        i = self.str.find(' ', find_pos)
        i2 = self.str.find(self.attr_list_end_char(), find_pos)
        if i < 0:
            i = i2
        if i < 0:
            return None
        value = self.str[find_pos:i]
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
        if self.current_char() == '"':
            quote_enabled = True
            self.get_char()
        while self.has_chars():
            curr = self.get_char()
            if not quote_enabled:
                if curr == ' ':
                    break
                elif curr == self.attr_list_end_char():
                    self.current_pos -= 1
                    break
            if quote_enabled and curr == '\\':
                curr = self.get_char()
                if curr != '"' and curr != '\\':
                    self.current_pos = saved_pos
                    return None, None
            elif quote_enabled and curr == '"':
                return key_name, value
            value += curr
        if not quote_enabled:
            return key_name, value
        else:
            self.current_pos = saved_pos
            return None, None

    def eat_whitespace(self):
        while self.has_chars() and self.current_char() == ' ':
            self.current_pos += 1

    @staticmethod
    def attr_list_end_char():
        return '}'


class AttributeParserException(Exception):
    pass