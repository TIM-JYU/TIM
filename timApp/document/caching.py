import hashlib
from dataclasses import dataclass
from typing import Optional, Union, Tuple

import redis
from redis import ResponseError

from timApp.document.docinfo import DocInfo
from timApp.document.docrenderresult import DocRenderResult
from timApp.document.document import Document
from timApp.document.docviewparams import DocViewParams
from timApp.document.viewcontext import ViewRoute, ViewContext
from timApp.user.user import User
from timApp.util.utils import dataclass_to_bytearray

rclient = redis.Redis(host='redis')

allowed_cache_routes = {
    ViewRoute.View,
}

DEFAULT_EXPIRE_SECS = 3600 * 24 * 7


DocInfoOrDocument = Union[DocInfo, Document]


@dataclass
class CacheResult:
    key: str
    doc: Optional[DocRenderResult]


def check_doc_cache(
        doc_info: DocInfo,
        current_user: User,
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
        cached: Tuple[bytes, bytes, bytes] = rclient.lrange(cache_key, 0, -1)  # type: ignore
    except ResponseError:
        return not_cached
    if cached:
        try:
            head_b, content_b, override_b = cached
            head = head_b.decode()
            content = content_b.decode()
            override = override_b.decode() if override_b else None
        except ValueError:
            # If for whatever reason the cache is corrupted, just ignore it.
            return not_cached
        return CacheResult(
            doc=DocRenderResult(head_html=head, content_html=content, allowed_to_cache=True, override_theme=override),
            key=cache_key,
        )
    return not_cached


def get_doc_cache_key(doc: DocInfo, user: User, view_ctx: ViewContext, m: DocViewParams) -> str:
    # We can't use builtin hash(...) here because the hash value changes between restarts.
    h = hashlib.shake_256()
    for part in doc.document.get_version():
        h.update(part.to_bytes(4, 'little', signed=True))
    h.update(dataclass_to_bytearray(m))
    h.update(dataclass_to_bytearray(view_ctx))
    return f'timdoc-{doc.id}-{user.id}-{h.hexdigest(10)}'


def clear_doc_cache(doc: DocInfoOrDocument, user: Optional[User]) -> None:
    prefix = f'timdoc-{doc.id}-'
    if user:
        prefix += f'{user.id}-'
    prefix += '*'
    for key in rclient.scan_iter(match=prefix):
        rclient.delete(key)


def set_doc_cache(key: str, value: DocRenderResult, ex: int = DEFAULT_EXPIRE_SECS) -> None:
    rclient.delete(key)
    rclient.rpush(
        key,
        value.head_html,
        value.content_html,
        value.override_theme or '',  # Redis doesn't accept None value, so convert it to empty string.
    )
    refresh_doc_expire(key, ex)


def refresh_doc_expire(key: str, ex: int = DEFAULT_EXPIRE_SECS) -> None:
    rclient.expire(key, ex)
