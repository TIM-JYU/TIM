from dataclasses import dataclass

import attr

import timApp

CONTENT_FIELD_NAME_MAP = {
    'csPlugin': 'usercode',
    'pali': 'userword',
    'numericfield': 'c',
    'textfield': 'c',
}


@dataclass
class PluginType:
    type: str

    def get_content_field_name(self) -> str:
        return CONTENT_FIELD_NAME_MAP.get(self.type, 'content')

    def can_give_task(self) -> bool:
        plugin_class = timApp.plugin.containerLink.get_plugin(self.type)  # type: ignore[attr-defined]
        return plugin_class.can_give_task
