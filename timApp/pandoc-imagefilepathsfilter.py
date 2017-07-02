#!/usr/bin/env python3

"""
Pandoc filter to convert image sources to latex graphics source paths considering
the images location according to the set of following rules:

- If an image has an absolute path that points to the TIM machine, e.g. "http://<TIM-domain>/imagepath"
  or "<tim-domain>/imagepath", then....
- If an image has a relative path, e.g. "/images/1239854102", then....
- If an image points to a resource that resides at another host, simply convert the image
  to a simple link at the output. This is due to possible copyright infringements, as the images
  would othewrise be unrightly copied to the output document.

TODO: BETTER DOCUMENTATION

"""
import os

from defaultconfig import TIM_HOST, FILES_PATH

from pandocfilters import toJSONFilter, RawInline, Image, Link, Str

# This of course, requires that this module resides in the timApp root folder
APP_ROOT = os.path.dirname(os.path.abspath(__file__))

IMAGE_ROOT = os.path.join(APP_ROOT, FILES_PATH, 'blocks')

# protocol + hostname
CURRENT_HOST_MACHINE = TIM_HOST


def handle_images(key, value, fmt, meta):

    if key == 'Image' and fmt == 'latex':
        (attrs, alt_text_inlines, target) = value
        (url, title) = target

        # For debugging:
        # return Image(attrs, alt_text_inlines, ["notarealhost.juupahuu.com/image.png", ""])

        image_path = ""

        parsed_url = urlparse(url)

        scheme = parsed_url.scheme or ''
        host = parsed_url.hostname or ''
        path = parsed_url.path or ''

        # The first slash needs to be removed from the path in order for the joins to work properly
        if path.startswith('/'):
            path = path[1:]

        # handle internal absolute urls
        base_address = scheme + '://' + host
        if base_address == CURRENT_HOST_MACHINE:
            image_path = os.path.join(APP_ROOT, path)

        # handle internal relative urls
        elif (host == "") and \
            (parsed_url.path.startswith(("images/", "/images/"))):

            image_path = os.path.join(IMAGE_ROOT, path)

        # handle external urls
        else:
            return [
                RawInline('latex', "\externalimagelink{"),
                Link(attrs, [Str(url)], [url, title]),
                RawInline('latex', "}")
            ]

        # Makes sure the paths are in the UNIX form, as that is what LaTeX uses for paths even on Windows
        image_path = image_path.replace('\\', '/')

        return Image(attrs, alt_text_inlines, [image_path, title])


if __name__ == "__main__":

    # Needs to import different package based on python version, as the urlparse method
    # was moved from urlparse module to urllib.parse between python2.7 -> python3
    try:
        from urllib.parse import urlparse
    except ImportError:
        from urlparse import urlparse

    toJSONFilter(handle_images)
