import re
from dataclasses import dataclass
from enum import Enum
from typing import Optional, Any

from timApp.document.docparagraph import DocParagraph
from timApp.document.randutils import is_valid_id
from timApp.plugin.pluginexception import PluginException

KNOWN_FIELD_NAMES = {"points", "datetime", "ALL"}


@dataclass
class UnvalidatedTaskId:
    s: str

    def validate(self) -> "TaskId":
        return TaskId.parse(self.s, require_doc_id=False, allow_block_hint=False)


class TaskIdAccess(Enum):
    ReadWrite = "readwrite"
    ReadOnly = "readonly"


@dataclass
class TaskId:
    doc_id: int | None
    task_name: str
    block_id_hint: str | None = None
    field: str | None = None
    plugin_type: str | None = None
    access_specifier: TaskIdAccess | None = None

    def __post_init__(self):
        value = self.task_name
        if "." in value:
            raise PluginException("Task name cannot contain dots.")
        if value.isdigit():
            raise PluginException("Task name cannot be only a number.")

    @staticmethod
    def parse(
        s: str,
        *,
        require_doc_id=True,
        allow_block_hint=True,
        allow_custom_field=False,
        allow_type=True,
    ) -> "TaskId":
        m = re.fullmatch(
            r"((?P<docid>\d+)\.)?(?P<name>[a-zåäöA-ZÅÄÖ0-9_-]+)(\.(?P<field>[a-zA-Z0-9_-]+))?(:(?P<type>[a-zA-Z]*)(:(?P<rw>readonly|readwrite))?)?",
            s,
        )
        if not m:
            raise PluginException(
                'Task name can only have characters a-z, 0-9, "_" and "-".'
            )
        doc_id = m.group("docid")
        if require_doc_id and not doc_id:
            raise PluginException("The format of task id is invalid. Missing doc id.")
        if not allow_type and m.group("type"):
            raise PluginException("Plugin type not allowed here.")
        task_id_name = m.group("name")
        block_hint_or_field_access = m.group("field")
        plugin_type = m.group("type")
        access = m.group("rw")
        par_id = None
        field = None
        if block_hint_or_field_access in KNOWN_FIELD_NAMES or allow_custom_field:
            field = block_hint_or_field_access
        elif allow_block_hint:
            if block_hint_or_field_access and not is_valid_id(
                block_hint_or_field_access
            ):
                raise PluginException(
                    f"Invalid field access: {block_hint_or_field_access}"
                )
            par_id = block_hint_or_field_access
        elif block_hint_or_field_access:
            raise PluginException(f"Invalid field access: {block_hint_or_field_access}")
        return TaskId(
            doc_id=int(doc_id) if doc_id else None,
            task_name=task_id_name,
            block_id_hint=par_id,
            field=field,
            plugin_type=plugin_type,
            access_specifier=TaskIdAccess(access) if access else None,
        )

    @staticmethod
    def try_parse(
        s: str, *args: Any, **kwargs: Any
    ) -> tuple[Optional["TaskId"], Optional[str]]:
        try:
            return TaskId.parse(s, *args, **kwargs), None
        except PluginException as e:
            return None, str(e)

    @property
    def doc_task(self):
        if not self.doc_id:
            raise PluginException("Task id does not have doc id.")
        return f"{self.doc_id}.{self.task_name}"

    @property
    def doc_task_with_field(self):
        if not self.field:
            return self.doc_task
        return f"{self.doc_task}.{self.field}"

    @property
    def extended(self):
        if not self.block_id_hint and not self.field:
            raise PluginException("Task id does not have block id hint.")
        return f"{self.doc_task}.{self.block_id_hint or self.field}"

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
        return self.field == "points"

    @property
    def is_global(self):
        return self.task_name.startswith("GLO_")

    def validate(self):
        pass  # already validated at __init__

    def update_doc_id_from_block(self, par: DocParagraph):
        self.doc_id = par.ref_doc.doc_id if par.ref_doc else par.doc.doc_id

    @staticmethod
    def parse_doc_id(tid: str) -> int:
        tid = TaskId.parse(tid, require_doc_id=True)
        assert tid.doc_id is not None
        return tid.doc_id
