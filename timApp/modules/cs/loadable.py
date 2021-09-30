class Loadable:
    Schema = None

    @classmethod
    def load(cls, *kargs, **kwargs):
        """Load from dict"""
        if cls.Schema is None:
            raise ValueError("Schema is None")
        return cls.Schema().load(*kargs, **kwargs)

    @classmethod
    def loads(cls, *kargs, **kwargs):
        """Load from json string"""
        if cls.Schema is None:
            raise ValueError("Schema is None")
        return cls.Schema().loads(*kargs, **kwargs)

    @classmethod
    def dump(cls, obj, *kargs, exclude=None, **kwargs):
        """Create a dict from object"""
        if cls.Schema is None:
            raise ValueError("Schema is None")
        return cls.Schema(exclude=exclude if exclude is not None else []).dump(
            obj, *kargs, **kwargs
        )

    @classmethod
    def dumps(cls, obj, *kargs, **kwargs):
        """Create a json string from object"""
        if cls.Schema is None:
            raise ValueError("Schema is None")
        return cls.Schema().dumps(obj, *kargs, **kwargs)
