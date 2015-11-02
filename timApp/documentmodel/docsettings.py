from contracts import new_contract
from utils import parse_plugin_values
from documentmodel.docparagraph import DocParagraph


class DocSettings:
    global_plugin_attrs_key = 'global_plugin_attrs'
    css_key = 'css'
    macros_key = 'macros'
    macro_delimiter_key = 'macro_delimiter'

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
            yaml_vals = parse_plugin_values(par)
            if 'error' in yaml_vals:
                return DocSettings()
            else:
                return DocSettings(settings_dict=yaml_vals['markup'])
        else:
            return DocSettings()

    def __init__(self, settings_dict=None):
        self.__dict = settings_dict if settings_dict else {}

    def get_settings(self):
        return self.__dict

    def global_plugin_attrs(self):
        return self.__dict.get(self.global_plugin_attrs_key)

    def css(self):
        return self.__dict.get(self.css_key)

    def get_macros(self):
        return self.__dict.get(self.macros_key)

    def get_macro_delimiter(self):
        return self.__dict.get(self.macro_delimiter_key)

new_contract('DocSettings', DocSettings)
