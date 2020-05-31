import attr

import timApp

CONTENT_FIELD_NAME_MAP = {
    'csPlugin': 'usercode',
    'pali': 'userword',
    'numericfield': 'c',
    'textfield': 'c',
}


@attr.s(auto_attribs=True)
class PluginType:
    type: str

    def get_content_field_name(self):
        return CONTENT_FIELD_NAME_MAP.get(self.type, 'content')

    def can_give_task(self):
        plugin_class = timApp.plugin.containerLink.get_plugin(self.type)
        return plugin_class.get('canGiveTask', False)