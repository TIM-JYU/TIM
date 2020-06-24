from typing import List, Dict, get_type_hints
from json import JSONEncoder

class LoadableJSONEncoder(JSONEncoder):
    def default(self, o):
        if issubclass(type(o), Loadable):
            return o.to_dict()
        return super().encode(o)

class Loadable:
    @staticmethod
    def realtype(type):
        if hasattr(type, '__origin__'):
            return type.__origin__
        else:
            return type
    
    @staticmethod
    def convert(value, type):
        type = Loadable.realtype(type)
        if isinstance(value, type):
            return value
        else:
            return type(value)
    
    @staticmethod
    def isconvertible(value, type):
        try:
            convert(value, type)
        except:
            return False
        return True
    
    def helper(self, key, value, req):
        if isinstance(value, list) and Loadable.realtype(req) is List:
            out = []
            for item in value:
                out.append(self.helper(f"{key} list content", item, req.__args__[0]))
            return out
        elif issubclass(req, Loadable) and isinstance(value, dict):
            return req(value)
        
        try:
            return Loadable.convert(value, req)
        except:
            pass
        
        if isinstance(value, dict) and Loadable.realtype(req) is Dict:
            if value and not (Loadable.isconvertible(next(iter(value.keys())), req.__args__[0]) and Loadable.isconvertible(next(iter(value.values())), req.__args__[1])):
                raise Exception(f"{key} is of wrong type. Should be {req} but is {type(value)}")
            out = {}
            for key, val in value.items():
                out[key] = self.helper(key, val, req.__args__[1])
            return out
        elif hasattr(req, '__origin__') and Loadable.isconvertible(value, req.__origin__):
            return req.__origin__(value)
        else:
            raise Exception(f"{key} is of wrong type. Should be {req} but is {type(value)}")
    
    def __init__(self, data = None, set_defaults: bool = True):
        type_hints = get_type_hints(self)
        self.fields = {
            key: value for key, value in type_hints.items()
            if not callable(getattr(self.__class__, key)) and not key.startswith("__")
        }
        if set_defaults:
            for key in self.fields.keys():
                self.__dict__[key] = type(self).__dict__.get(key)
        if data is not None:
            for key, value in data.items():
                if key not in self.fields:
                    raise Exception(f"Field {key} does not exist in class {self.__class__.__name__}")
                req = self.fields[key]
                self.__dict__[key] = self.helper(key, value, req)
    
    def to_dict(self) -> dict:
        out = {}
        for key in self.fields.keys():
            if hasattr(self, key):
                out[key] = getattr(self, key)
        
        return out
    
    def update(self, data):
        instance = type(self)(data, False)
        self.__dict__.update(instance.__dict__)
