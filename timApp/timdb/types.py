from datetime import datetime

from flask_sqlalchemy.model import Model
from sqlalchemy import Text, DateTime
from sqlalchemy.orm import DeclarativeBase, declared_attr, has_inherited_table
from typing_extensions import Annotated

datetime_tz = Annotated[datetime, "datetime_tz"]


class DbModel(DeclarativeBase, Model):
    """
    Base class for all TIM database models.
    """

    type_annotation_map = {
        str: Text,
        datetime_tz: DateTime(timezone=True),
    }

    @declared_attr.directive
    def __tablename__(cls) -> str | None:
        if has_inherited_table(cls):
            return None
        return cls.__name__.lower()
