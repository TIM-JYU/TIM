from __future__ import annotations


from timApp.timdb.sqa import db
from timApp.item.block import Block, BlockType, insert_block
from timApp.user.usergroup import UserGroup


class TaskBlock(db.Model):

    __tablename__ = "taskblock"
    id = db.Column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    task_id = db.Column(db.Text, primary_key=True)

    block = db.relationship("Block", lazy="joined")

    @staticmethod
    def get_by_task(task_id: str) -> TaskBlock | None:
        return TaskBlock.query.filter_by(task_id=task_id).first()

    @staticmethod
    def get_block_by_task(task_id: str) -> Block | None:
        task_block = TaskBlock.query.filter_by(task_id=task_id).first()
        if task_block is not None:
            return task_block.block
        else:
            return None


def insert_task_block(task_id: str, owner_groups: list[UserGroup]) -> TaskBlock:
    b = insert_block(BlockType.Task, description=task_id, owner_groups=owner_groups)
    task_block = TaskBlock(id=b.id, task_id=task_id, block=b)
    db.session.add(task_block)
    return task_block
