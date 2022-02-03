from types import UnionType

import typing_inspect  # type: ignore


def is_union_type(tp: type) -> bool:
    return (isinstance(tp, UnionType)) or typing_inspect.is_union_type(tp)


def get_args(tp: type, evaluate: bool | None = None) -> tuple[type, ...]:
    if isinstance(tp, UnionType):
        return tp.__args__
    return typing_inspect.get_args(tp, evaluate)


def is_optional_type(tp: type) -> bool:
    if isinstance(tp, UnionType):
        return any(is_optional_type(arg) for arg in tp.__args__)
    return typing_inspect.is_optional_type(tp)


def get_origin(tp: type) -> type:
    if isinstance(tp, UnionType):
        return UnionType
    return typing_inspect.get_origin(tp)
