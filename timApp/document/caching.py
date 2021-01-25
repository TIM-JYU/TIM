import hashlib
import json
from dataclasses import dataclass
from typing import Optional, Union

import redis
from json import JSONDecodeError

from timApp.document.docinfo import DocInfo
from timApp.document.docrenderresult import DocRenderResult
from timApp.document.document import Document
from timApp.document.viewcontext import ViewRoute, ViewContext
from timApp.document.docviewparams import DocViewParams
from timApp.user.user import User
from timApp.util.flask.responsehelper import to_json_str
from timApp.util.utils import dataclass_to_bytearray

rclient = redis.Redis(host='redis')

allowed_cache_routes = {
    ViewRoute.View,
}

# Parameters that shouldn't affect caching.
ignore_url_params = {
    'unlock',
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
    if view_ctx.route not in allowed_cache_routes:
        return CacheResult(key=cache_key, doc=None)

    # Skip and clear cache if requested.
    if nocache:
        clear_doc_cache(doc_info, user=None)
        return CacheResult(key=cache_key, doc=None)

    cached: bytes = rclient.get(cache_key)
    if cached is not None:
        try:
            head, content, override = json.loads(cached)
        except (JSONDecodeError, ValueError):
            # If for whatever reason the cache is corrupted, just ignore it.
            return CacheResult(key=cache_key, doc=None)
        return CacheResult(
            doc=DocRenderResult(head_html=head, content_html=content, allowed_to_cache=True, override_theme=override),
            key=cache_key,
        )
    return CacheResult(key=cache_key, doc=None)


def get_doc_cache_key(doc: DocInfo, user: User, view_ctx: ViewContext, m: DocViewParams) -> str:
    # We can't use builtin hash(...) here because the hash value changes between restarts.
    h = hashlib.shake_256()
    h.update(bytes(doc.document.get_version()))
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
    rclient.set(key, to_json_str((value.head_html, value.content_html, value.override_theme)), ex=ex)


def refresh_doc_expire(key: str, ex: int = DEFAULT_EXPIRE_SECS) -> None:
    rclient.expire(key, ex)
