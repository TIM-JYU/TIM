from dataclasses import dataclass
from typing import Dict, Any

import timApp
from timApp.timdb.sqa import db

CONTENT_FIELD_NAME_MAP = {
    'csPlugin': 'usercode',
    'pali': 'userword',
    'numericfield': 'c',
    'textfield': 'c',
}


class PluginTypeBase:
    def get_type(self) -> str:
        raise NotImplementedError

    def get_content_field_name(self) -> str:
        return CONTENT_FIELD_NAME_MAP.get(self.get_type(), 'content')

    def can_give_task(self) -> bool:
        plugin_class = timApp.plugin.containerLink.get_plugin(self.get_type())  # type: ignore[attr-defined]
        return plugin_class.can_give_task

    def to_json(self) -> Dict[str, Any]:
        return {
            'type': self.get_type()
        }


# TODO: Right now values are added dynamically to the table when saving answers. Instead add them on TIM start.
class PluginType(db.Model, PluginTypeBase):
    __tablename__ = 'plugintype'
    id = db.Column(db.Integer, primary_key=True)

    type = db.Column(db.Text, nullable=False, unique=True)

    @staticmethod
    def resolve(p_type: str) -> 'PluginType':
        return PluginType.query.filter_by(type=p_type).first() or PluginType(type=p_type)

    def get_type(self) -> str:
        return self.type


@dataclass
class PluginTypeLazy(PluginTypeBase):
    type: str

    def get_type(self) -> str:
        return self.type

    def resolve(self) -> PluginType:
        return PluginType.resolve(self.type)
