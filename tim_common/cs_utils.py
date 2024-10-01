from typing import TypeVar, Protocol


class AllSubclasses(Protocol):
    @classmethod
    def all_subclasses(cls) -> list:
        return []


T = TypeVar("T", bound=type[AllSubclasses])


def populated(
    base_class: T,
) -> dict[str, T]:
    """
    Populate a dictionary with all subclasses of a base class that have the AllSubclasses protocol implemented
    and have a ttype attribute defined.

    :param base_class: The base class to populate the dictionary with

    :return: A dictionary with the ttype as the key and the class as the value
    """
    dictionary: dict[str, T] = {}
    classes = [base_class] + base_class.all_subclasses()

    def add(new_cls: T, new_ttype: str) -> None:
        if new_ttype in dictionary:
            raise Exception(
                f"{base_class.__name__} {cls.__name__} has a duplicate ttype ({new_ttype}) with {dictionary[new_ttype].__name__}"
            )
        dictionary[new_ttype] = new_cls

    for cls in classes:
        if not hasattr(cls, "ttype"):
            raise Exception(
                f"{base_class.__name__} {cls.__name__} hasn't defined ttype"
            )
        if cls.ttype is None:
            continue
        if isinstance(cls.ttype, list):
            if len(cls.ttype) == 0:
                raise Exception(
                    f"{base_class.__name__} {cls.__name__} hasn't defined ttype"
                )
            for ttype in cls.ttype:
                add(cls, ttype.lower())
        else:
            add(cls, cls.ttype.lower())
    return dictionary
