from datetime import timezone
from enum import Enum

import dateutil
import dateutil.parser

from timApp.document.version import Version
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class OperationType(Enum):
    Add = "Added"
    Delete = "Deleted"
    Insert = "Inserted"
    Modify = "Modified"


class Operation:
    def __init__(self, op: OperationType) -> None:
        self.op = op

    @staticmethod
    def from_type_dict(op: OperationType, d: dict) -> "Operation":
        if op == OperationType.Add:
            return AddOperation(op)
        elif op == OperationType.Delete:
            return DeleteOperation(op)
        elif op == OperationType.Insert:
            return InsertOperation(op, d.get("before_id"))
        elif op == OperationType.Modify:
            return ModifyOperation(op, old_hash=d["old_hash"], new_hash=d["new_hash"])
        else:
            assert False, "Unknown OperationType"

    def to_json(self):
        return None


class ModifyOperation(Operation):
    def to_json(self):
        return {"old_hash": self.old_hash, "new_hash": self.new_hash}

    def __init__(self, op: OperationType, old_hash: str, new_hash: str) -> None:
        super().__init__(op)
        self.old_hash = old_hash
        self.new_hash = new_hash


class DeleteOperation(Operation):
    def __init__(self, op: OperationType) -> None:
        super().__init__(op)


class AddOperation(Operation):
    def __init__(self, op: OperationType) -> None:
        super().__init__(op)


class InsertOperation(Operation):
    def to_json(self):
        return {"before_id": self.before_id}

    def __init__(self, op: OperationType, before_id: str) -> None:
        super().__init__(op)
        self.before_id = before_id


class ChangelogEntry:
    def __init__(
        self,
        par_id: str,
        ver: Version,
        op: str,
        time: str,
        group_id: int,
        op_params: dict,
    ):
        self.version = ver
        self.time = dateutil.parser.parse(time).replace(tzinfo=timezone.utc)
        self.op = Operation.from_type_dict(OperationType(op), op_params)
        self.group_id = group_id
        self.par_id = par_id

    def to_json(self):
        return {
            "ver": self.version,
            "time": self.time,
            "op": self.op.op.value,
            "group": db.session.get(UserGroup, self.group_id).name,
            "par_id": self.par_id,
            "op_params": self.op.to_json(),
        }
