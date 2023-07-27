from datetime import datetime

from flask_sqlalchemy import SQLAlchemy
from typing_extensions import Annotated

datetime_tz = Annotated[datetime, "datetime_tz"]


def add_tim_types(db: SQLAlchemy) -> None:
    # In TIM, we always use TEXT by default for strings
    db.Model.registry.update_type_annotation_map(
        {
            str: db.Text,
            datetime_tz: db.DateTime(timezone=True),
        }
    )
