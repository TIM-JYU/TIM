from contracts import new_contract
from pluginControl import parse_plugin_values


class DocSettings:
    global_plugin_attrs_key = 'global_plugin_attrs'
    css_key = 'css'

    @classmethod
    def from_paragraph(cls, par):
        if par.is_setting():
            yaml_vals = parse_plugin_values(par)
            if 'error' in yaml_vals:
                return DocSettings()
            else:
                return DocSettings(dict=yaml_vals['markup'])
        else:
            return DocSettings()

    def __init__(self, dict=None):
        self.__dict = dict if dict else {}

    def get_settings(self):
        return self.__dict

    def global_plugin_attrs(self):
        return self.__dict.get(self.global_plugin_attrs_key)

    def css(self):
        return self.__dict.get(self.css_key)



new_contract('DocSettings', DocSettings)
