def populated(base_class):
    dictionary = {}
    classes = [base_class] + base_class.all_subclasses()

    def add(new_cls, new_ttype):
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
