from contracts import contract, new_contract
from utils import parse_yaml
from documentmodel.docparagraph import DocParagraph
from markdownconverter import expand_macros


class DocSettings:
    global_plugin_attrs_key = 'global_plugin_attrs'
    css_key = 'css'
    macros_key = 'macros'
    macro_delimiter_key = 'macro_delimiter'
    source_document_key = "source_document"

    @classmethod
    def is_valid_paragraph(cls, par):
        if par.is_reference():
            par = par.get_referenced_pars(set_html=False)[0]
        if not par.is_setting():
            return True

        md = par.get_markdown().replace('```', '').replace('~~~', '')
        return parse_yaml(md).__class__ != str

    @classmethod
    def from_paragraph(cls, par):
        """Constructs DocSettings from the given DocParagraph.

        :param par: The DocParagraph to extract settings from.
        :type par: DocParagraph
        :return: The DocSettings object.
        """
        if par.is_reference():
            par = par.get_referenced_pars(set_html=False)[0]
        if par.is_setting():
            md = par.get_markdown().replace('```', '').replace('~~~', '')
            yaml_vals = parse_yaml(md)
            if yaml_vals.__class__ == str:
                #raise ValueError("DocSettings yaml parse error: " + yaml_vals)
                #print("DocSettings yaml parse error: " + yaml_vals)
                return DocSettings()
            else:
                return DocSettings(settings_dict=yaml_vals)
        else:
            return DocSettings()

    @contract
    def __init__(self, settings_dict: 'dict|None' = None):
        self.__dict = settings_dict if settings_dict else {}

    @contract
    def to_paragraph(self, doc) -> 'DocParagraph':
        text = "\n".join(['{}: {}'.format(k, self.__dict[k]) for k in self.__dict ])
        return DocParagraph.create(doc, md=text, attrs={"settings": ""})

    @contract
    def get_settings(self) -> 'dict':
        return self.__dict

    def global_plugin_attrs(self):
        return self.__dict.get(self.global_plugin_attrs_key)

    def css(self):
        return self.__dict.get(self.css_key)

    def get_macros(self):
        return self.__dict.get(self.macros_key)

    def get_macro_delimiter(self):
        return self.__dict.get(self.macro_delimiter_key, '%%')

    @contract
    def get_source_document(self) -> 'int|None':
        return self.__dict.get(self.source_document_key)

    @contract
    def set_source_document(self, source_docid: 'int|None'):
        self.__dict[self.source_document_key] = source_docid

new_contract('DocSettings', DocSettings)
