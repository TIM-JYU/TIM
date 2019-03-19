import re
from enum import Enum
from typing import Optional

import attr

from timApp.document.docparagraph import DocParagraph
from timApp.document.randutils import is_valid_id
from timApp.plugin.pluginexception import PluginException

KNOWN_FIELD_NAMES = {'points'}


@attr.s(auto_attribs=True)
class UnvalidatedTaskId:
    s: str

    def validate(self):
        return TaskId.parse(self.s, require_doc_id=False, allow_block_hint=False)


class TaskIdAccess(Enum):
    ReadWrite = 'readwrite'
    ReadOnly = 'readonly'


@attr.s(auto_attribs=True)
class TaskId:
    doc_id: Optional[int]
    task_name: str = attr.ib()
    block_id_hint: Optional[str]
    field: Optional[str]
    plugin_type: Optional[str] = None
    access_specifier: Optional[TaskIdAccess] = None

    @task_name.validator
    def _check_name(self, attribute, value):
        if '.' in value:
            raise PluginException('Task name cannot contain dots.')
        if value.isdigit():
            raise PluginException('Task name cannot be only a number.')

    @staticmethod
    def parse(s: str, require_doc_id=True, allow_block_hint=True) -> 'TaskId':
        m = re.fullmatch(r'((\d+)\.)?([a-zA-Z0-9_-]+)(\.([a-zA-Z0-9_-]+))?(:([a-zA-Z]*)(:(readonly|readwrite))?)?', s)
        if not m:
            raise PluginException('Task name can only have characters a-z, 0-9, "_" and "-".')
        doc_id = m.group(2)
        if require_doc_id and not doc_id:
            raise PluginException('The format of task id is invalid. Missing doc id.')
        task_id_name = m.group(3)
        block_hint_or_field_access = m.group(5)
        plugin_type = m.group(7)
        access = m.group(9)
        par_id = None
        field = None
        if block_hint_or_field_access in KNOWN_FIELD_NAMES:
            field = block_hint_or_field_access
        elif allow_block_hint:
            if block_hint_or_field_access and not is_valid_id(block_hint_or_field_access):
                raise PluginException(f'Invalid field access: {block_hint_or_field_access}')
            par_id = block_hint_or_field_access
        elif block_hint_or_field_access:
            raise PluginException(f'Invalid field access: {block_hint_or_field_access}')
        return TaskId(
            doc_id=int(doc_id) if doc_id else None,
            task_name=task_id_name,
            block_id_hint=par_id,
            field=field,
            plugin_type=plugin_type,
            access_specifier=TaskIdAccess(access) if access else None,
        )

    @property
    def doc_task(self):
        if not self.doc_id:
            raise PluginException('Task id does not have doc id.')
        return f'{self.doc_id}.{self.task_name}'

    @property
    def doc_task_with_field(self):
        if not self.field:
            return self.doc_task
        return f'{self.doc_task}.{self.field}'

    @property
    def extended(self):
        if not self.block_id_hint and not self.field:
            raise PluginException('Task id does not have block id hint.')
        return f'{self.doc_task}.{self.block_id_hint or self.field}'

    @property
    def extended_or_doc_task(self):
        if self.block_id_hint or self.field:
            return self.extended
        return self.doc_task

    def maybe_set_hint(self, hint: str):
        if not self.field:
            self.block_id_hint = hint

    @property
    def is_points_ref(self):
        return self.field == 'points'

    def validate(self):
        pass  # already validated at __init__

    def update_doc_id_from_block(self, par: DocParagraph):
        self.doc_id = par.ref_doc.doc_id if par.ref_doc else par.doc.doc_id
