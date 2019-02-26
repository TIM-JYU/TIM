import re
from typing import Optional

import attr

from timApp.plugin.pluginexception import PluginException


KNOWN_FIELD_NAMES = {'points'}


@attr.s(auto_attribs=True)
class TaskId:
    doc_id: Optional[int]
    task_name: str
    block_id_hint: Optional[str]
    field: Optional[str] = None

    @staticmethod
    def parse(s: str, require_doc_id=True) -> 'TaskId':
        m = re.fullmatch(r'((\d+)\.)?([a-zA-Z0-9_-]+)(\.([a-zA-Z0-9_-]+))?', s)
        if not m:
            raise PluginException('The format of task id is invalid.')
        doc_id = m.group(2)
        if require_doc_id and not doc_id:
            raise PluginException('The format of task id is invalid. Missing doc id.')
        task_id_name = m.group(3)
        if task_id_name.isdigit():
            raise PluginException('Task name cannot be only a number.')
        block_hint_or_field_access = m.group(5)
        par_id = None
        field = None
        if block_hint_or_field_access in KNOWN_FIELD_NAMES:
            field = block_hint_or_field_access
        else:
            par_id = block_hint_or_field_access
        return TaskId(
            doc_id=int(doc_id) if doc_id else None,
            task_name=task_id_name,
            block_id_hint=par_id,
            field=field,
        )

    @property
    def doc_task(self):
        if not self.doc_id:
            raise PluginException('Task id does not have doc id.')
        return f'{self.doc_id}.{self.task_name}'

    @property
    def extended(self):
        if not self.block_id_hint:
            raise PluginException('Task id does not have block id hint.')
        return f'{self.doc_task}.{self.block_id_hint}'

    @property
    def extended_or_doc_task(self):
        if self.block_id_hint:
            return self.extended
        return self.doc_task
