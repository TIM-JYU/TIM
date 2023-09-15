from datetime import datetime
from typing import TYPE_CHECKING

# flask_sqlalchemy stubs are not up-to-date to support mypy >1.0
from flask_sqlalchemy.model import Model  # type: ignore
from sqlalchemy import Text, DateTime
from sqlalchemy.orm import DeclarativeBase, declared_attr, has_inherited_table
from sqlalchemy.orm import registry
from typing_extensions import Annotated

datetime_tz = Annotated[datetime, "datetime_tz"]


class DbModel(DeclarativeBase):
    """
    Base class for all TIM database models.
    """

    registry = registry(
        type_annotation_map={
            str: Text,
            datetime_tz: DateTime(timezone=True),
        }
    )

    # Add check for mypy to suppress __tablename__ error when it's overridden as a string and not a method
    if not TYPE_CHECKING:

        @declared_attr.directive
        def __tablename__(cls) -> str | None:
            if has_inherited_table(cls):
                return None
            return cls.__name__.lower()
