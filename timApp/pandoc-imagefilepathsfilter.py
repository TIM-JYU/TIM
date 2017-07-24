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
import tempfile
import urllib
import uuid

from timApp.defaultconfig import FILES_PATH

from pandocfilters import toJSONFilter, RawInline, Image, Link, Str

# This of course, requires that this module resides in the timApp root folder
APP_ROOT = os.path.dirname(os.path.abspath(__file__))

IMAGE_ROOT = os.path.join(APP_ROOT, FILES_PATH, 'blocks')

# protocol + hostname
CURRENT_HOST_MACHINE = os.environ.get('TIM_HOST', None)

ALLOWED_EXTERNAL_HOSTS = []

PRINTING_WHITELIST_FILE = os.path.join(APP_ROOT, '.printing_whitelist.config')


def init_whitelist():
    """ Init whitelist for trusted image source domains. """
    if not os.path.exists(PRINTING_WHITELIST_FILE):
        try:
            os.makedirs(os.path.dirname(PRINTING_WHITELIST_FILE))
        except OSError:
            pass

        try:
            open(PRINTING_WHITELIST_FILE, 'a').close()
        except IOError:
            pass

    content = []
    try:
        with open(PRINTING_WHITELIST_FILE, 'r') as f:
            content = f.readlines()
    except IOError:
        pass

    return [x.strip() for x in content]


# Get the os temp directoryls
TEMP_DIR_PATH = tempfile.gettempdir()
DOWNLOADED_IMAGES_ROOT = os.path.join(TEMP_DIR_PATH, 'tim-img-dls')


def handle_images(key, value, fmt, meta):
    # open("Output.txt", "a").write("Key:" + key + " fmt:" + fmt + " value:" + str(value) + "\n")
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
        base_address = scheme + '://' if scheme != '' else ''
        base_address += host + '/' if host != '' else ''
        if (CURRENT_HOST_MACHINE is not None) and base_address == CURRENT_HOST_MACHINE:
            image_path = os.path.join(APP_ROOT, path)

        # handle internal relative urls
        elif (host == "") and os.path.exists(os.path.join(APP_ROOT, path)):
            image_path = os.path.join(APP_ROOT, path)

        elif (host == "") and os.path.exists(os.path.join(IMAGE_ROOT, path)):
            image_path = os.path.join(IMAGE_ROOT, path)

        # handle external urls
        else:

            # Download images from allowed external urls to be attached to the document.
            if host in ALLOWED_EXTERNAL_HOSTS:

                # create folder for image dls, if it does not exist already
                if not os.path.exists(DOWNLOADED_IMAGES_ROOT):
                    os.makedirs(DOWNLOADED_IMAGES_ROOT, )

                # download img to the folder and give the file a random but unique name
                img_uuid = str(uuid.uuid1()).replace('-', '')  # remove hyphens
                try:
                    _, ext = os.path.splitext(url)
                    img_dl_path = os.path.join(DOWNLOADED_IMAGES_ROOT, img_uuid + ext)
                    urllib.URLopener().retrieve(url, img_dl_path)

                    img_dl_path = img_dl_path.replace('\\', '/')  # Ensure UNIX form
                    return Image(attrs, alt_text_inlines, [img_dl_path, title])

                except IOError:
                    # could not download image, so display the image as a link to the imageURL
                    return [
                        RawInline('latex', "\externalimagelink{"),
                        Link(attrs, [Str(url)], [url, title]),
                        RawInline('latex', "}")
                    ]

            # For other external images, transform the element to appear as a link
            # to the image resource in the LaTeX-output.
            return [
                RawInline('latex', "\externalimagelink{"),
                Link(attrs, [Str(url)], [url, title]),
                RawInline('latex', "}")
            ]

        # Makes sure the paths are in the UNIX form, as that is what LaTeX uses for paths even on Windows
        image_path = image_path.replace('\\', '/')
        # if len(alt_text_inlines) == 0: # with this it is possible to take the empty caption away
        #    title = ''                  # but then centered is lost. On should return raw inline TeX

        return Image(attrs, alt_text_inlines, [image_path, title])


if __name__ == "__main__":

    # Needs to import different package based on python version, as the urlparse method
    # was moved from urlparse module to urllib.parse between python2.7 -> python3
    try:
        from urllib.parse import urlparse
    except ImportError:
        from urlparse import urlparse

    ALLOWED_EXTERNAL_HOSTS = init_whitelist()

    toJSONFilter(handle_images)
