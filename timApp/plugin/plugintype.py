from dataclasses import dataclass

import timApp
from timApp.timdb.sqa import db

CONTENT_FIELD_NAME_MAP = {
    'csPlugin': 'usercode',
    'pali': 'userword',
    'numericfield': 'c',
    'textfield': 'c',
}


# TODO: Right now values are added dynamically to the table when saving answers. Instead add them on TIM start.
class PluginType(db.Model):
    __tablename__ = 'plugintype'
    id = db.Column(db.Integer, primary_key=True)

    type = db.Column(db.Text, nullable=False, unique=True)

    def get_content_field_name(self) -> str:
        return CONTENT_FIELD_NAME_MAP.get(self.type, 'content')

    def can_give_task(self) -> bool:
        plugin_class = timApp.plugin.containerLink.get_plugin(self.type)  # type: ignore[attr-defined]
        return plugin_class.can_give_task

    @staticmethod
    def resolve(p_type: str) -> 'PluginType':
        return PluginType.query.filter_by(type=p_type).first() or PluginType(type=p_type)
