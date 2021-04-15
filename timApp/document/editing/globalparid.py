from dataclasses import dataclass


@dataclass(frozen=True)
class GlobalParId:
    doc_id: int
    par_id: str
