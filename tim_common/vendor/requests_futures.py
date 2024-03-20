"""
requests_futures
~~~~~~~~~~~~~~~~
This module provides a small add-on for the requests http library. It makes use
of python 3.3's concurrent.futures or the futures backport for previous
releases of python.
    from requests_futures.sessions import FuturesSession
    session = FuturesSession()
    # request is run in the background
    future = session.get('http://httpbin.org/get')
    # ... do other stuff ...
    # wait for the request to complete, if it hasn't already
    response = future.result()
    print('response status: {0}'.format(response.status_code))
    print(response.content)

Copyright 2013 Ross McFarland

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
"""
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor, Future
from functools import partial
from logging import getLogger
from pickle import dumps, PickleError
from threading import local

from requests import Session
from requests.adapters import DEFAULT_POOLSIZE, HTTPAdapter


def wrap(self, sup, background_callback, *args_, **kwargs_):
    """A global top-level is required for ProcessPoolExecutor"""
    resp = sup(*args_, **kwargs_)
    return background_callback(self, resp) or resp


PICKLE_ERROR = (
    "Cannot pickle function. Refer to documentation: https://"
    "github.com/ross/requests-futures/#using-processpoolexecutor"
)

DEFAULT_MAX_WORKERS = 8


class FuturesSession(Session):
    def __init__(
        self,
        executor=None,
        max_workers=DEFAULT_MAX_WORKERS,
        session_factory=Session,
        adapter_kwargs=None,
        *args,
        **kwargs
    ) -> None:
        """Creates a FuturesSession
        Notes
        ~~~~~
        * `ProcessPoolExecutor` may be used with Python > 3.4;
          see README for more information.
        * If you provide both `executor` and `max_workers`, the latter is
          ignored and provided executor is used as is.
        """
        _adapter_kwargs = {}
        super().__init__(*args, **kwargs)
        self._owned_executor = executor is None
        if executor is None:
            executor = ThreadPoolExecutor(max_workers=max_workers)
            # set connection pool size equal to max_workers if needed
            if max_workers > DEFAULT_POOLSIZE:
                _adapter_kwargs.update(
                    {"pool_connections": max_workers, "pool_maxsize": max_workers}
                )

        _adapter_kwargs.update(adapter_kwargs or {})

        if _adapter_kwargs:
            self.mount("https://", HTTPAdapter(**_adapter_kwargs))
            self.mount("http://", HTTPAdapter(**_adapter_kwargs))

        self.executor = executor
        self.session_factory = session_factory
        self.tls = local()
        self.sessions = []
        self.session_args = args
        self.session_kwargs = kwargs

    def request(self, *args, **kwargs):
        """Maintains the existing api for Session.request.
        Used by all of the higher level methods, e.g. Session.get.
        The background_callback param allows you to do some processing on the
        response in the background, e.g. call resp.json() so that json parsing
        happens in the background thread.

        :rtype : concurrent.futures.Future
        """

        def func(*args, **kwargs):
            session = getattr(self.tls, "session", None)
            if session is None:
                session = self.session_factory(
                    *self.session_args, **self.session_kwargs
                )
                self.tls.session = session
                self.sessions.append(session)
            return session.request(*args, **kwargs)

        background_callback = kwargs.pop("background_callback", None)
        if background_callback:
            logger = getLogger(self.__class__.__name__)
            logger.warning(
                "`background_callback` is deprecated and will be "
                "removed in 1.0, use `hooks` instead"
            )
            func = partial(wrap, self, func, background_callback)

        if isinstance(self.executor, ProcessPoolExecutor):
            # verify function can be pickled
            try:
                dumps(func)
            except (TypeError, PickleError):
                raise RuntimeError(PICKLE_ERROR)

        return self.executor.submit(func, *args, **kwargs)

    def close(self):
        super().close()
        if self._owned_executor:
            self.executor.shutdown()
        for session in self.sessions:
            session.close()

    def get(self, url, **kwargs):
        """
        Sends a GET request. Returns :class:`Future` object.

        :param url: URL for the new :class:`Request` object.
        :rtype : concurrent.futures.Future
        """
        return super().get(url, **kwargs)

    def options(self, url, **kwargs):
        """
        Sends a OPTIONS request. Returns :class:`Future` object.

        :param url: URL for the new :class:`Request` object.
        :rtype : concurrent.futures.Future
        """
        return super().options(url, **kwargs)

    def head(self, url, **kwargs):
        """Sends a HEAD request. Returns :class:`Future` object.

        :param url: URL for the new :class:`Request` object.
        :rtype : concurrent.futures.Future
        """
        return super().head(url, **kwargs)

    def post(self, url, data=None, json=None, **kwargs) -> Future:
        """Sends a POST request. Returns :class:`Future` object.

        :param url: URL for the new :class:`Request` object.
        :param data: (optional) Dictionary, list of tuples, bytes, or file-like
            object to send in the body of the :class:`Request`.
        :param json: (optional) json to send in the body of the :class:`Request`.
        :rtype : concurrent.futures.Future
        """
        return super().post(url, data=data, json=json, **kwargs)

    def put(self, url, data=None, **kwargs) -> Future:
        """Sends a PUT request. Returns :class:`Future` object.

        :param url: URL for the new :class:`Request` object.
        :param data: (optional) Dictionary, list of tuples, bytes, or file-like
            object to send in the body of the :class:`Request`.
        :rtype : concurrent.futures.Future
        """
        return super().put(url, data=data, **kwargs)

    def patch(self, url, data=None, **kwargs):
        """Sends a PATCH request. Returns :class:`Future` object.

        :param url: URL for the new :class:`Request` object.
        :param data: (optional) Dictionary, list of tuples, bytes, or file-like
            object to send in the body of the :class:`Request`.
        :rtype : concurrent.futures.Future
        """
        return super().patch(url, data=data, **kwargs)

    def delete(self, url, **kwargs):
        """Sends a DELETE request. Returns :class:`Future` object.

        :param url: URL for the new :class:`Request` object.
        :rtype : concurrent.futures.Future
        """
        return super().delete(url, **kwargs)
