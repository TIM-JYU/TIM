from collections import OrderedDict
from dataclasses import dataclass, fields
from functools import wraps
from inspect import signature, Signature
from typing import Any, Callable

from flask import Blueprint

from timApp.util.flask.requesthelper import use_model


class TypedBlueprint(Blueprint):
    """Adds support for declaring route parameters directly in the route function signature."""

    def route(self, rule: str, **options: Any) -> Callable:
        def decorator(f: Callable) -> Callable:
            wrapped = use_typed_params(sum(1 for c in rule if c == '<'))(f)
            return super(TypedBlueprint, self).route(rule, **options)(wrapped)

        return decorator


def use_typed_params(num_path_params: int = 0) -> Callable:
    def decorator(func: Callable) -> Callable:
        sig = signature(func)
        params = OrderedDict(sig.parameters)
        for i in range(0, num_path_params):
            params.popitem(last=False)
        if not params:
            return func
        class_attrs = {k: v.default for k, v in params.items() if v.default is not Signature.empty}
        anns = {
            k: v.annotation for k, v in params.items() if v.annotation is not Signature.empty
        }
        class_attrs['__annotations__'] = anns
        missing_annotations = set(params.keys()) - set(anns.keys())
        if missing_annotations:
            raise Exception(f'Some parameter type annotations are missing from {func.__name__}: {missing_annotations}')
        new_dataclass: Any = dataclass(type(f'{func.__name__}_params', (), class_attrs))

        @use_model(new_dataclass)
        def extract_params(inst: Any) -> Any:
            return inst

        @wraps(func)
        def extract_and_call(*args: Any, **kwargs: Any) -> Any:
            extracted = extract_params()  # type: ignore[call-arg]
            return func(*args, **kwargs, **{f.name: getattr(extracted, f.name) for f in fields(extracted)})

        return extract_and_call

    return decorator
