from typing import Optional

import attr

from timApp.plugin.pluginexception import PluginException


@attr.s(auto_attribs=True)
class TaskId:
    doc_id: Optional[int]
    task_name: str
    block_id_hint: Optional[str]

    @staticmethod
    def parse(s: str, require_doc_id=True) -> 'TaskId':
        pieces = s.split('.')
        required_pieces = 2 if require_doc_id else 1
        num_pieces = len(pieces)
        if not required_pieces <= num_pieces <= 3:
            raise PluginException(f'The format of task_id is invalid. Expected {required_pieces - 1}-2 dot characters.')
        if num_pieces == 1:
            return TaskId(
                doc_id=None,
                task_name=pieces[0],
                block_id_hint=None,
            )
        try:
            doc_id = int(pieces[0])
        except ValueError:
            raise PluginException(f'The format of task_id is invalid. Expected integral doc id but got {pieces[0]}.')
        task_id_name = pieces[1] if pieces[1] else None
        par_id = pieces[2] if num_pieces == 3 else None
        return TaskId(
            doc_id=doc_id,
            task_name=task_id_name,
            block_id_hint=par_id,
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
