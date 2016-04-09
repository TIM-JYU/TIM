class DocumentParserOptions:
    def __init__(self, break_on_empty_line=False,
                 break_on_code_block=True,
                 break_on_header=True,
                 break_on_normal=True
                 ):
        self.break_on_empty_line = break_on_empty_line
        self.break_on_code_block = break_on_code_block
        self.break_on_header = break_on_header
        self.break_on_normal = break_on_normal

    def __eq__(self, other):
        if type(other) is type(self):
            return self.__dict__ == other.__dict__
        return False

    @staticmethod
    def single_paragraph() -> 'DocumentParserOptions':
        return DocumentParserOptions(break_on_code_block=False,
                                     break_on_header=False,
                                     break_on_normal=False)

    @staticmethod
    def whole_document() -> 'DocumentParserOptions':
        return DocumentParserOptions(break_on_code_block=True,
                                     break_on_header=True,
                                     break_on_normal=True)

    @staticmethod
    def break_on_empty_lines() -> 'DocumentParserOptions':
        return DocumentParserOptions(break_on_empty_line=True)
