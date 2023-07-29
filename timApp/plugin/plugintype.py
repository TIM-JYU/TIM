from dataclasses import dataclass
from typing import Any

import filelock
from sqlalchemy import select
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import mapped_column, Mapped, Session

import timApp
from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import DbModel

CONTENT_FIELD_NAME_MAP = {
    "csPlugin": "usercode",
    "pali": "userword",
    "numericfield": "c",
    "textfield": "c",
    "reviewcanvas": "uploadedFiles",
}


class PluginTypeBase:
    def get_type(self) -> str:
        raise NotImplementedError

    def get_content_field_name(self) -> str:
        return CONTENT_FIELD_NAME_MAP.get(self.get_type(), "content")

    def can_give_task(self) -> bool:
        plugin_class = timApp.plugin.containerLink.get_plugin(self.get_type())  # type: ignore[attr-defined]
        return plugin_class.can_give_task

    def to_json(self) -> dict[str, Any]:
        return {"type": self.get_type()}


# TODO: Right now values are added dynamically to the table when saving answers. Instead add them on TIM start.
class PluginType(DbModel, PluginTypeBase):
    id: Mapped[int] = mapped_column(primary_key=True)

    type: Mapped[str] = mapped_column(unique=True)

    @staticmethod
    def resolve(p_type: str) -> "PluginType":
        pt = run_sql(select(PluginType).filter_by(type=p_type)).scalars().first()
        if pt:
            return pt

        # Add plugin type to database via separate session to preserve the original one created by Flask-SQLAlchemy
        # Use a lock to prevent concurrent access
        with filelock.FileLock("/tmp/plugin_type_create.lock"):
            try:
                with Session(db.engine) as session:
                    session.add(PluginType(type=p_type))
                    session.commit()
            except IntegrityError as e:
                # TODO: Try to debug why this still happens even after locking
                if (
                    'duplicate key value violates unique constraint "plugintype_type_key"'
                    not in str(e)
                ):
                    raise

        # We have to re-query the database since the other session was closed
        return run_sql(select(PluginType).filter_by(type=p_type)).scalars().one()

    def get_type(self) -> str:
        return self.type


@dataclass
class PluginTypeLazy(PluginTypeBase):
    type: str

    def get_type(self) -> str:
        return self.type

    def resolve(self) -> PluginType:
        return PluginType.resolve(self.type)
