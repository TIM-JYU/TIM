"""Defines the DocumentWriter class."""
from documentmodel.documentparser import DocumentParser


class DocumentWriter:
    """Converts a sequence of document blocks to text.
    """

    def __init__(self, pars):
        """Initializes a DocumentWriter object.

        :param pars: A sequence of paragraphs representing the document.
        """
        self.pars = pars

    def get_text(self):
        """Gets the full text for the document.

        :return: The full text of the document.
        """
        text = ''
        for p in self.pars:
            blocks = DocumentParser(p['md']).get_blocks()

            if len(blocks) > 1:
                atomized = p.copy()
                atomized['atom'] = 'true'
                text += '``` {' + DocumentWriter.attrs_to_str(atomized) + '}\n' + p['md'] + '\n' + '```'
            else:
                attrs_str = DocumentWriter.attrs_to_str(p)
                if not attrs_str:
                    text += p['md']
                else:
                    if blocks[0]['type'] == 'normal':
                        text += '#-' + ' {' + attrs_str + '}\n' + p['md']
                    else:
                        first_line, rest = blocks[0]['md'].split('\n', 1)
                        text += first_line + ' {' + attrs_str + '}\n' + rest + '\n'
            text += '\n'
        return text

    @staticmethod
    def attrs_to_str(attrs):
        """

        :type attrs: dict
        """
        attr_str = ''
        for k, v in attrs.items():
            if k in ['md', 'type']:
                continue
            elif k == 'taskId':
                attr_str += '#' + v
            elif k == 'classes':
                attr_str += ' '.join(['.' + cl for cl in v])
            elif isinstance(v, dict):
                attr_str += DocumentWriter.attrs_to_str(v)
            else:
                attr_str += k + '="'
                for char in v:
                    if char in ('"', '\\'):
                        attr_str += '\\'
                    attr_str += char
                attr_str += '"'
            attr_str += ' '
        return attr_str.strip()
