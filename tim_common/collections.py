from collections import defaultdict
from typing import Any, Callable


class keydefaultdict(defaultdict):
    """
    Extended defaultdict that invokes the default factory while passing the key to it.
    """

    def __init__(
        self, default_factory: Callable[[Any], Any], *args: Any, **kwargs: Any
    ) -> None:
        super().__init__(default_factory, *args, **kwargs)  # type: ignore

    def __missing__(self, key: Any) -> Any:
        if self.default_factory is None:
            raise KeyError(key)
        else:
            def_key: Callable[[Any], Any] = self.default_factory  # type: ignore
            ret = self[key] = def_key(key)
            return ret
