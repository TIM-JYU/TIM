from __future__ import annotations

from sqlalchemy import select, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.item.block import Block, BlockType, insert_block
from timApp.timdb.sqa import db, run_sql
from timApp.user.usergroup import UserGroup


class TaskBlock(db.Model):
    id: Mapped[int] = mapped_column(ForeignKey("block.id"), primary_key=True)
    task_id: Mapped[str] = mapped_column(primary_key=True)

    block: Mapped[Block] = relationship(lazy="select")

    @staticmethod
    def get_by_task(task_id: str) -> TaskBlock | None:
        return run_sql(select(TaskBlock).filter_by(task_id=task_id)).scalars().first()

    @staticmethod
    def get_block_by_task(task_id: str) -> Block | None:
        task_block = (
            run_sql(select(TaskBlock).filter_by(task_id=task_id)).scalars().first()
        )
        if task_block is not None:
            return task_block.block
        else:
            return None


def insert_task_block(task_id: str, owner_groups: list[UserGroup]) -> TaskBlock:
    # owner_groups may be redundant, would need to be kept up-to-date with task doc owners
    b = insert_block(BlockType.Task, description=task_id, owner_groups=owner_groups)
    task_block = TaskBlock(id=b.id, task_id=task_id, block=b)
    db.session.add(task_block)
    return task_block
