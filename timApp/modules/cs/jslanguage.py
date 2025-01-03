import json
import os
from languages import Language, get_by_id, get_by_id_and_pop

from tim_common.fileParams import get_url_lines_as_string, do_sed_replacements

from urllib.parse import urlparse, urljoin

JSREADYHTML = {}

JSREADYOPTIONS = {}


def add_host_if_missing(url: str, default_host: str) -> str:
    parsed_url = urlparse(url)
    if not parsed_url.netloc:
        # if host is missing, add default host
        # parsed_url = parsed_url._replace(netloc=default_host)
        # AI suggeset unurlparse made extra // to beginning of url
        # url = urlunparse(parsed_url)
        url = urljoin(default_host, url)
    return str(url)


def get_from_url(url: str, usecache: bool = True) -> str:
    """
    Get html from url but use cache if already there.
    For that remember to empty csplugin cache if needed (/cs/refresh)!
    """
    print(f"get_from_url: {url}")
    url = add_host_if_missing(url, os.environ["TIM_HOST"])
    # change URL in local development because inside docker container localhost is not valid
    url = url.replace("localhost/", "host.docker.internal/")
    return get_url_lines_as_string(url, usecache=usecache)


class JSWithHTML(Language):
    """
    Base for JS like languages that need html for iframe.

    Currently base for old "js" that is temporarily now in class JShtml.
    Also JSFrame inherites from this by moving it's modify_query
    to this class.  Idea is that JS and JSFrame can use same
    attributes.

    Many classes is inherited from JS-class and they are not
    yet tested to be inherited from JSWithHTML.  For that the old
    ttype="js" is moved here and JShtml is created.

    As soon all JS-based classes are tested to be inherited from JShtml,
    JShtml can be removed and JS can be inherited from JSWithHTML.
    """

    ttype = None  # "base_jawithhtml"
    load_original_data = False

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.jsobject = "window."
        self.query_modified = False

    def modify_query(self):
        """
        This method is called when html needed for jsframe.
        Maybe called twice when task saved and iframehtml: true.
        The default version tries to change any way to get html
        to srchtml attribute.  And html is modified to have
        jsparams in <script> before </body>.  And to do also
        other modifications for srchtml.
        :return: void
        """
        if self.query_modified:
            return

        ma = self.query.jso["markup"]
        readyhtml = get_by_id(ma, "readyHtml", "")
        readyoptions = get_by_id(ma, "readyOptions", readyhtml)
        src = ma.get("srchtml", "")
        if not src and readyhtml:
            src = JSREADYHTML.get(readyhtml, "")

        if not src:
            src = get_by_id_and_pop(ma, "fullhtml", None)

        if not src:
            url = get_by_id_and_pop(ma, "fullhtmlurl", None)
            if url:
                url = url.replace("COMPS", "/print/tim/components")
                src = get_from_url(url, get_by_id(ma, "usecache", True))

        if not src:
            src = ""

        if src.find("TIMJS") >= 0:
            self.jsobject = "TIMJS."

        src = do_sed_replacements(self.query, src)

        opt = get_by_id(ma, "options", None)
        if not opt:
            opt = JSREADYOPTIONS.get(readyoptions, None)
        if opt:
            src = src.replace(
                "//OPTIONS", self.jsobject + "options = " + json.dumps(opt) + ";"
            )

        contstyle = (
            'style="' + get_by_id(ma, "contStyle", "width: 100%; margin: auto; ") + '"'
        )

        src = src.replace("CONTSTYLE", contstyle)

        src = src.replace("##TIM_HOST##", os.environ["TIM_HOST"])

        src = src.replace("##HOST_URL##", ma.get("hosturl", os.environ["TIM_HOST"]))

        if self.load_original_data:
            original_data = get_by_id(ma, "data", None)
            if original_data:
                src = src.replace(
                    "//ORIGINALDATA",
                    self.jsobject + "originalData = " + json.dumps(original_data) + ";",
                )

        javascript = get_by_id(ma, "javascript", None)
        if javascript:
            src = src.replace("//JAVASCRIPT", javascript)

        src = src.replace("##CHARTJSVERSION##", ma.get("chartjsversion", "2.8.0"))

        jsparams = get_by_id(ma, "jsparams", None)

        if jsparams:
            jsframedata = {
                "params": jsparams,
            }
            init_data = f"<script>{self.jsobject}jsframedata = {json.dumps(jsframedata)} </script>"
            src = src.replace("</body>", init_data + "\n</body>")

        ma["srchtml"] = src
        return


class JShtml(JSWithHTML):
    ttype = "js"

    def run(self, result, sourcelines, points_rule):
        return 0, "", "", ""
