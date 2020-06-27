from typing import List, Dict, Union, Optional
from dataclasses import field
from marshmallow_dataclass import dataclass
from marshmallow import missing

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
    def dump(cls, obj):
        """Create a dict from object"""
        if cls.Schema is None:
            raise ValueError("Schema is None")
        return cls.Schema().dump(obj)
    @classmethod
    def dumps(cls, obj):
        """Create a json string from object"""
        if cls.Schema is None:
            raise ValueError("Schema is None")
        return cls.Schema().dumps(obj)

@dataclass
class AngularComponent(Loadable):
    template: str = field(default="")

@dataclass
class AngularModule(Loadable):
    components: Dict[str, AngularComponent] = field(default_factory=dict)
    entry: str = field(default="")

@dataclass
class DivContent(Loadable):
    classes: str = field(default="")
    content: str = field(default="")

@dataclass
class OutputContainer(Loadable):
    title: Optional[DivContent] = field(default=None)
    text: Optional[DivContent] = field(default=None)
    angular: Optional[AngularModule] = field(default=None)
    hide: bool = field(default=False) # whether to hide by default

@dataclass
class RunResult(Loadable):
    output_boxes: List[OutputContainer] = field(default_factory=list)
    penalties: Dict[str, Union[bool, str]] = field(default_factory=dict)
    points: float = field(default=0.0)
    max_points: Optional[float] = field(default=None)
    
    def penalize(self, key: str) -> bool:
        return self.penalties and self.penalties.get(key, False)
