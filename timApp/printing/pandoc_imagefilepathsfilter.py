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
import re
import tempfile
import urllib.request
from subprocess import check_output, STDOUT

from pandocfilters import toJSONFilter, RawInline, Image, Link, Str

from timApp.defaultconfig import FILES_PATH
from timApp.document.randutils import hashfunc
from timApp.util.file_utils import guess_image_type

APP_ROOT = "/service/timApp"

IMAGE_ROOT = os.path.join(APP_ROOT, FILES_PATH, "blocks")

# protocol + hostname
CURRENT_HOST_MACHINE = os.environ.get("TIM_HOST", None)

ALLOWED_EXTERNAL_HOSTS = []

PRINTING_WHITELIST_FILE = os.path.join(APP_ROOT, ".printing_whitelist.config")

urlmaps = [
    {"url": "/csstatic/", "dir": "/service/timApp/modules/cs/static/"},
    {"url": "/csgenerated/", "dir": "/service/timApp/modules/cs/generated/"},
    {"url": "/static/", "dir": "/service/timApp/static/"},
    {"url": "/images/", "dir": "/tim_files/blocks/images/"},
    {"url": "/files/", "dir": "/tim_files/blocks/files/"},
]


def init_whitelist():
    """Init whitelist for trusted image source domains."""

    # s = ""  # just a test for env variables
    # for a in os.environ:
    #     s += 'Var: ' + a + ' Value: ' +  os.getenv(a) + "\n"
    # open("Output.txt", "a").write("Environment:" + s)

    if not os.path.exists(PRINTING_WHITELIST_FILE):
        try:
            os.makedirs(os.path.dirname(PRINTING_WHITELIST_FILE))
        except OSError:
            pass

        try:
            open(PRINTING_WHITELIST_FILE, "a").close()
        except OSError:
            pass

    content = []
    try:
        with open(PRINTING_WHITELIST_FILE) as f:
            content = f.readlines()
    except OSError:
        pass

    return [x.strip() for x in content]


# Get the os temp directoryls
TEMP_DIR_PATH = tempfile.gettempdir()
DOWNLOADED_IMAGES_ROOT = os.path.join(TEMP_DIR_PATH, "tim-img-dls")

texdocid = None


def convert_svg_to_pdf(image_path):
    path = os.path.dirname(image_path)
    # TODO: muista tarkistaa että jos pdf jo on, niin ei tehdä uudelleen!!!
    temp = image_path.replace(".svg", ".tmp.html")
    pdf = image_path.replace(".svg", ".pdf")
    if os.path.isfile(pdf):
        pdftime = os.path.getmtime(pdf)
        svgtime = os.path.getmtime(image_path)
        if pdftime > svgtime:
            return pdf

    # Make html to avoid headers and footers from PDF-image
    html = """<html>
  <head>
    <style>
body {
  margin: 0;
}
    </style>
    <script>
function init() {
  const element = document.getElementById('targetsvg');
  const positionInfo = element.getBoundingClientRect();
  const height = positionInfo.height;
  const width = positionInfo.width;
  const style = document.createElement('style');
  style.innerHTML = `@page {margin: 0; size: ${width}px ${height+1}px}`;
  document.head.appendChild(style);
}
window.onload = init;
    </script>
  </head>
  <body>
    <img id="targetsvg" src="SVGIMAGE">
  </body>
</html>
"""
    # string.format does not work because {} is needed for js
    html = html.replace("SVGIMAGE", image_path)
    open(temp, "w").writelines(html)
    cmd = [
        "/opt/google/chrome/chrome",
        "--no-sandbox",
        "--headless",
        "--disable-gpu",
        f"--print-to-pdf={pdf}",
        temp,
    ]
    # cmd = "./svg2pdf.sh {} {}".format(image_path, pdf)
    output = check_output(cmd, stderr=STDOUT, cwd=path)
    os.remove(temp)
    return pdf


def handle_images(key, value, fmt, meta):
    # open("Output.txt", "a").write("Meta:" + str(meta) + "\n")

    if key == "Image" and fmt == "latex":
        (attrs, alt_text_inlines, target) = value
        (url, title) = target

        # For debugging:
        # return Image(attrs, alt_text_inlines, ["notarealhost.juupahuu.com/image.png", ""])

        image_path = ""

        parsed_url = urlparse(url)
        parsed_cur = urlparse(CURRENT_HOST_MACHINE + "/kukku")
        curhost = parsed_cur.hostname or ""

        scheme = parsed_url.scheme or ""
        host = parsed_url.hostname or ""
        path = parsed_url.path or ""

        image_path = ""

        for urlmap in urlmaps:
            urlbeg = urlmap.get("url")
            if path.startswith(urlbeg):
                image_path = path.replace(urlbeg, urlmap.get("dir"))
                break

        # open("Output.txt", "a").write("image_path: " + image_path + " host: " + host + "CHM: " + curhost + "\n")
        if host != "" and host != curhost:
            image_path = ""

        if image_path != "" and os.path.exists(image_path):
            image_path = image_path.replace("\\", "/")
            if image_path.endswith(".svg"):
                image_path = convert_svg_to_pdf(image_path)
            return Image(attrs, alt_text_inlines, [image_path, title])

        """
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
            # open("Output.txt", "a").write("host: " + host + "\n")

        # handle external urls
        else:
        """

        # Download images from allowed external urls to be attached to the document.
        allow = False
        for h in ALLOWED_EXTERNAL_HOSTS:
            # open("Output.txt", "a").write("try image: " + h + " -> " + url + "\n")
            if re.match(h, url):
                allow = True
                break

        if allow:
            # open("Output.txt", "a").write("Check texdocid \n")
            global texdocid  # check if we already have path for doc id
            if not texdocid:
                m = meta.get(
                    "texdocid", None
                )  # if we do not have, get the path from meta data
                # open("Output.txt", "a").write("m:" + str(m) + "\n")
                if m:
                    texdocid = str(m.get("c", "xx"))
                # open("Output.txt", "a").write("texdocid:" + texdocid + "\n")

            images_root = os.path.join(DOWNLOADED_IMAGES_ROOT, texdocid)
            # create folder for image dls, if it does not exist already
            if not os.path.exists(images_root):
                os.makedirs(images_root)

            # download img to the folder and give the file a unique name (hash the url)
            img_uid = hashfunc(url)
            try:
                _, ext = os.path.splitext(url)
                img_dl_path = os.path.join(images_root, str(img_uid) + ext)
                # open("Output.txt", "a").write("img_dl_path = " + img_dl_path + "\n")

                if not os.path.exists(img_dl_path):
                    # open("Output.txt", "a").write("retrieve: " + url + " -> " + img_dl_path + "\n")
                    urllib.request.urlretrieve(url, img_dl_path)
                    if not ext:
                        img_type = guess_image_type(img_dl_path)
                        if img_type:
                            img_dl_path_ext = f"{img_dl_path}.{img_type}"
                            # open("Output.txt", "a").write("img_dl_path_ext = " + img_dl_path_ext + "\n")
                            os.symlink(img_dl_path, img_dl_path_ext)
                            img_dl_path = img_dl_path_ext

                    # urllib.URLopener().retrieve(url, img_dl_path)

                img_dl_path = img_dl_path.replace(
                    "\\", "/"
                )  # Ensure UNIX form for pandoc
                if img_dl_path.endswith(".svg"):
                    img_dl_path = convert_svg_to_pdf(img_dl_path)
                return Image(attrs, alt_text_inlines, [img_dl_path, title])

            except OSError:
                # could not download image, so display the image as a link to the imageURL
                pass
            except:
                pass

            # For other external images, transform the element to appear as a link
            # to the image resource in the LaTeX-output.
            return [
                RawInline("latex", r"\externalimagelink{"),
                Link(attrs, [Str(url)], [url, title]),
                RawInline("latex", "}"),
            ]

        # Makes sure the paths are in the UNIX form, as that is what LaTeX uses for paths even on Windows
        image_path = image_path.replace("\\", "/")

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
