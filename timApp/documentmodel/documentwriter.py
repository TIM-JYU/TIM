"""Defines the DocumentWriter class."""
from documentmodel.documentparser import DocumentParser


class DocumentWriter:
    """Converts a sequence of document blocks to text.
    """

    def __init__(self, pars, export_hashes=False, export_ids=True):
        """Initializes a DocumentWriter object.

        :param export_hashes: Whether to include hash attributes in the exported markdown.
        :param export_ids: Whether to include id attributes in the exported markdown.
        :param pars: A sequence of paragraphs representing the document.
        """
        self.pars = pars
        self.ignored_attrs = ['md', 'type', 'html', 'links', 'doc_id']
        if not export_hashes:
            self.ignored_attrs.append('t')
        if not export_ids:
            self.ignored_attrs.append('id')

    def get_text(self):
        """Gets the full text for the document.

        :return: The full text of the document.
        """
        text = ''
        for p in self.pars:
            blocks = DocumentParser(p['md']).get_blocks()
            text += '\n'
            if len(blocks) > 1:
                atomized = p.copy()
                atomized['atom'] = 'true'
                text += '``` {' + self.attrs_to_str(atomized) + '}\n' + p['md'] + '\n' + '```'
            else:
                attrs_str = self.attrs_to_str(p)
                if not attrs_str:
                    text += p['md']
                else:
                    if blocks[0]['type'] == 'normal' or blocks[0]['type'] == 'autonormal':
                        text += '#-' + ' {' + attrs_str + '}\n' + p['md']
                    else:
                        parts = blocks[0]['md'].split('\n', 1)
                        first_line, rest = parts[0], parts[1] if len(parts) > 1 else ''
                        text += first_line + ' {' + attrs_str + '}\n' + rest
            if text[-1] != '\n':
                text += '\n'
        return text[1:]

    def attrs_to_str(self, attrs):
        """

        :type attrs: dict
        """
        attr_str = ''
        for k, v in attrs.items():
            if k in self.ignored_attrs:
                continue
            elif k == 'taskId':
                attr_str += '#' + v
            elif k == 'classes':
                attr_str += ' '.join(['.' + cl for cl in v])
            elif isinstance(v, dict):
                attr_str += self.attrs_to_str(v)
            else:
                attr_str += k + '="'
                for char in v:
                    if char in ('"', '\\'):
                        attr_str += '\\'
                    attr_str += char
                attr_str += '"'
            attr_str += ' '
        return attr_str.strip()
