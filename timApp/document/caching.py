import hashlib
from dataclasses import dataclass
from typing import Optional, Union, TYPE_CHECKING, overload

import redis
from redis import ResponseError

from timApp.document.docinfo import DocInfo
from timApp.document.docrenderresult import DocRenderResult
from timApp.document.document import Document
from timApp.document.docviewparams import DocViewParams
from timApp.document.viewcontext import ViewRoute, ViewContext
from timApp.user.usergroup import UserGroup
from timApp.util.utils import dataclass_to_bytearray

if TYPE_CHECKING:
    from timApp.user.user import User

rclient = redis.Redis(host="redis")

allowed_cache_routes = {
    ViewRoute.View,
}

DEFAULT_EXPIRE_SECS = 3600 * 24 * 7


# DocInfoOrDocument = Union[DocInfo, Document]
DocInfoOrDocument = Union["DocInfo", "Document"]


@dataclass
class CacheResult:
    key: str
    doc: DocRenderResult | None


def check_doc_cache(
    doc_info: DocInfo,
    current_user: "User",
    view_ctx: ViewContext,
    m: DocViewParams,
    nocache: bool,
) -> CacheResult:
    cache_key = get_doc_cache_key(doc_info, current_user, view_ctx, m)
    not_cached = CacheResult(key=cache_key, doc=None)
    if view_ctx.route not in allowed_cache_routes:
        return not_cached

    # Skip and clear cache if requested.
    if nocache:
        clear_doc_cache(doc_info, user=None)
        return not_cached

    try:
        cached: tuple[bytes, bytes, bytes, bytes] = rclient.lrange(cache_key, 0, -1)  # type: ignore
    except ResponseError:
        return not_cached
    if cached:
        try:
            head_b, content_b, override_b, hide_readmarks_b = cached
            head = head_b.decode()
            content = content_b.decode()
            override = override_b.decode() if override_b else None
            hide_readmarks = bool(int(hide_readmarks_b))
        except ValueError:
            # If for whatever reason the cache is corrupted, just ignore it.
            return not_cached
        return CacheResult(
            doc=DocRenderResult(
                head_html=head,
                content_html=content,
                allowed_to_cache=True,
                override_theme=override,
                hide_readmarks=hide_readmarks,
            ),
            key=cache_key,
        )
    return not_cached


def get_doc_cache_key(
    doc: DocInfo, user: "User", view_ctx: ViewContext, m: DocViewParams
) -> str:
    # We can't use builtin hash(...) here because the hash value changes between restarts.
    h = hashlib.shake_256()
    for part in doc.document.get_version():
        h.update(part.to_bytes(4, "little", signed=True))
    h.update(dataclass_to_bytearray(m))
    h.update(dataclass_to_bytearray(view_ctx))
    return f"timdoc-{doc.id}-{user.id}-{h.hexdigest(10)}"


@overload
def clear_doc_cache(doc: DocInfoOrDocument | int, user: "User") -> None:
    ...


@overload
def clear_doc_cache(doc: DocInfoOrDocument | int, user: None) -> None:
    ...


@overload
def clear_doc_cache(doc: None, user: "User") -> None:
    ...


def clear_doc_cache(
    doc: DocInfoOrDocument | int | None, user: Optional["User"]
) -> None:
    if not doc:
        prefix = "timdoc-*-"
    else:
        doc_id: int = doc if isinstance(doc, int) else doc.id
        prefix = f"timdoc-{doc_id}-"
    if user:
        prefix += f"{user.id}-"
    prefix += "*"
    for key in rclient.scan_iter(match=prefix, count=1000):
        rclient.delete(key)


def set_doc_cache(
    key: str, value: DocRenderResult, ex: int = DEFAULT_EXPIRE_SECS
) -> None:
    rclient.delete(key)
    rclient.rpush(
        key,
        value.head_html,
        value.content_html,
        # Redis doesn't accept None value, so convert it to empty string.
        value.override_theme or "",
        int(value.hide_readmarks),
    )
    refresh_doc_expire(key, ex)


def refresh_doc_expire(key: str, ex: int = DEFAULT_EXPIRE_SECS) -> None:
    rclient.expire(key, ex)


def set_style_timestamp_hash(style_name: str, hash_val: str) -> None:
    rclient.set(f"tim-style-hash-{style_name}", hash_val)


def get_style_timestamp_hash(style_name: str) -> str | None:
    res = rclient.get(f"tim-style-hash-{style_name}")
    return res.decode(encoding="utf-8") if res else None


def set_user_global_message(user: "User", message: str | None) -> None:
    if message:
        rclient.set(f"tim-global-message-{user.id}", message)
    else:
        rclient.delete(f"tim-global-message-{user.id}")


def set_usergroup_global_message(ug: UserGroup, message: str | None) -> None:
    if message:
        rclient.mset({f"tim-global-message-{u.id}": message for u in ug.users})
    else:
        rclient.delete(*[f"tim-global-message-{u.id}" for u in ug.users])


def get_user_global_message(user: "User") -> str | None:
    res = rclient.get(f"tim-global-message-{user.id}")
    return res.decode(encoding="utf-8") if res else None
