from dataclasses import dataclass
from typing import Any, List

from tim_common.marshmallow_dataclass import class_schema


@dataclass
class ExportData:
    plugin: str
    data: Any
    save: bool


@dataclass
class WithExportData:
    exportdata: List[ExportData]


@dataclass
class WithOutData:
    outdata: WithExportData


WithOutDataSchema = class_schema(WithOutData)()
