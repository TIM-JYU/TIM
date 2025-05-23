import binascii
import codecs
import datetime
import glob
import hashlib
import http.server
import io
import json
import yaml
import logging
import os
import random
import re
import shlex
import shutil
import signal
import socketserver
import subprocess
import time
from base64 import b64encode
from os.path import splitext
from pathlib import Path
from subprocess import Popen, PIPE, check_output
from traceback import print_exc
from typing import Any
from urllib.request import urlopen

from cs_logging import get_logger
from cs_utils import replace_code, check_parsons, text_value_replace
from file_handler import FileHandler
from file_util import write_safe, rm, rm_safe
from languages import dummy_language, sanitize_cmdline
from manager import all_js_files, all_css_files
from tim_common.cs_points_rule import (
    return_points,
    get_points_rule,
    check_number_rule,
    give_points,
)
from run import generate_filename, run2_subdir
from tim_common.cs_sanitizer import cs_min_sanitize, svg_sanitize, tim_sanitize
from tim_common.dumboclient import (
    call_dumbo,
    DumboOptions,
)
from tim_common.fileParams import (
    encode_json_data,
    replace_random,
    get_url_lines_as_string,
    get_json_eparam,
    str_to_int,
    NOLAZY,
    is_lazy,
    is_review,
    get_tiny_surrounding_headers,
    get_surrounding_headers,
    LAZYSTART,
    LAZYEND,
    get_param_del,
    query_params_to_map_check_parts,
    get_param_table,
    get_clean_param,
    get_params,
    do_headers,
    post_params,
    multi_post_params,
    get_all_templates,
    file_to_string,
    get_template,
    get_cache_keys,
    clear_cache,
    replace_scripts,
    string_to_string_replace_url,
    FileParams,
    get_file_to_output,
    mkdirs,
    get_json_param,
    get_param,
    QueryClass,
    replace_program_tokens,
)
from ttype import TType

#  uid = pwd.getpwnam('agent')[2]
#  os.setuid(uid)

# cs.py: WWW-palvelin portista 5000 (ulospäin 56000) joka palvelee csPlugin pyyntöjä
#
# masterPath-kansioiden lisääminen copyFiles, jsFiles, cssFiles ja fromFile-attribuutteja varten:
#   - Luo kansio /cs/masters/-hakemistoon.
#   Voit käyttää tätä kansiota masterPathissä (masterPath: <kansio-nimi>)
#   - Lisää kansioon tai sen alikansioihin haluamasi js, css ja markup-tiedostot (fromFile).
#   - Viittaa näihin tiedostoihin attribuuteilla. Jos fromFile on tosi, eikä merkkijono, oletetaan
#   markup tiedoston nimeksi 'csmarkup.json'
#
# Uuden kielen lisäämiseksi
# 1. Mene tiedostoon languages.py ja kopioi sieltä luokka
#        class Lang(Language):
#      ja vaihda sille nimi, toteuta metodit ja täytä 'ttype' muuttuja.
# 2. Tee tarvittava lisäys myös js/dir.js tiedoston kieliluetteloon.
# 3. Lisää kielen kääntäjä/tulkki vastaavaan konttiin, ks. Dockerfile
#
# ExtCheck-kielen käyttämiseen katso ohjeet extcheck.py-tiedoston alun kommentista.
#
# Ensin käynistettävä
# ./startPlugins.sh             - käynnistää dockerin cs.py varten
# ./startAll.sh                 - ajetaan dockerin sisällä cs.py (ajetaan edellisestä)
# Muut tarvittavat skriptit:
# rcmd.sh          - käynistetään ajettavan ohjelman kontin sisälle ajamaan
# cs.py tuottama skripti
#
# Hakemistot:
#  tim-koneessa
#     <tim polku>/timApp/modules/cs               - varsinainen csPluginin hakemisto, skirptit yms
#     <tim polku>/timApp/modules/cs/masters       - masterPath-attribuutin juurikansio
#     <tim polku>/timApp/modules/cs/templates     - pluginin templatet editoria varten
#     <tim polku>/timApp/modules/cs/java          - javan tarvitsemat tavarat
#     <tim polku>/timApp/modules/cs/images/cs     - kuvat jotka syntyvät csPlugin ajamista ohjelmista
#     /tmp/uhome                                  - käyttäjän hakemistoja ohjelmien ajamisen ajan
#     /tmp/uhome/user                             - käyttäjän hakemistoja ohjelmien ajamisen ajan
#     /tmp/uhome/user/HASH                        - yhden käyttäjän hakemisto joka säilyy ajojen välillä
#
#
# tim-koneesta käynnistetään cs docker-kontti nimelle csPlugin (./startPlugins.sh), jossa
# mountataan em. hakemistoja seuraavasti:
#
#   <tim polku>/timApp/modules/cs  ->          /cs/          (ohjelmat ja polut, read only)
#   <tim polku>/timApp/modules/cs/static  ->   /csstatic     (julkiset tiedostot, read only)
#   <lokien polku>/cs                    -> /logs            (lokitiedostot, read/write)
#   /tmp/uhome:       -> /tmp/                               käyttäjän jutut tänne
#
# Lisäksi konttiin lisätään volumet:
#   csplugin_data  -> /cs_data                              (data, read/write)
#   csplugin_data_generated -> /csgenerated                 käyttäjän ohjelmien kuvat ja muut julkiset tiedostot
#
# Käyttäjistä (csPlugin-kontissa) tehdään /tmp/user/HASHCODE
# tai /tmp/HASHCODE nimiset hakemistot (USERPATH=user/HASHCODE tai HASHCODE),
# joihin tehdään ohjelmien käännökset ja joista ohjelmat ajetaan.
# HASHCODE = käyttäjätunnuksesta muodostettu hakemiston nimi.
# Mikäli käyttäjätunnusta ei ole, on tiedoston nimi satunnainen.
#
# Kääntämisen jälkeen luodaan /tmp/USERPATH hakemistoon
# täydellinen ajokomento run/URNDFILE.sh
# Tämä jälkeen tehdään /tmp/run -hakemistoon tiedosto
# RNDNAME johon on kirjoitettu "USERPATH run/URNDFILE.sh"
# Tähän ajokonttiin mountataan tim-koneesta
#  (udir = userhash + "/"  + docid  jos on path: user)
#
#   /opt/cs          -> /cs/          read only
#   /tmp/uhome/udir  -> /home/agent   käyttäjän "kotihakemisto"
#
# Docker-kontin käynnistyessä suoritetaan komento /cs/rcmd.sh
# (TIMissä /opt/cs/rcmd.sh)
# joka alustaa "näytön" ja luo sopivat ympäristömuuttajat mm.
# javan polkuja varten ja sitten vaihtaa hakemistoon /home/agent
# ja ajaa sieltä komennon ./run/URNDFILE.sh
# stdout ja stderr tulevat tiedostoihin ./run/URNDFILE.in ja ./run/URNDFILE.err
# Kun komento on ajettu, docker-kontti häviää.  Ajon lopuksi tuohotaan
# ./run/URNDFILE.sh
# ja kun tämä on hävinnyt, luetaan stdin ja stderr ja tiedot lähetetään
# selaimelle (Timin kautta)
#

PORT = 5000


def get_process_children(pid):
    p = Popen(
        "ps --no-headers -o pid --ppid %d" % pid, shell=True, stdout=PIPE, stderr=PIPE
    )
    stdout, stderr = p.communicate()
    return [int(p) for p in stdout.split()]


def print_lines(file, lines, n1, n2):
    linefmt = "{0:03d} "
    n = len(lines)
    if n1 < 0:
        n1 = 0
    if n2 >= n:
        n2 = n - 1

    for i in range(n1, n2 + 1):
        line = lines[i]
        ln = linefmt.format(i + 1)
        file.write(ln + line + "\n")


def write_json_error(file, err, result, points_rule=None):
    return_points(points_rule, result)

    result["web"] = {"error": err}
    result_str = json.dumps(result)
    file.write(result_str.encode())
    print("ERROR:======== ", err.encode("UTF8"))
    print(result_str)


def removedir(dirname):
    # noinspection PyBroadException
    try:
        # os.chdir('/tmp')
        shutil.rmtree(dirname)
    except:
        return


def save_extra_files(query, extra_files, prgpath):
    if not extra_files:
        return
    ie = 0
    for extra_file in extra_files:
        extra_file_perms = extra_file.get("perms")
        ie += 1
        efilename = prgpath + "/extrafile" + str(ie)
        if "name" in extra_file:
            efilename = prgpath + "/" + extra_file["name"]

        mkdirs(os.path.dirname(efilename))
        if "text" in extra_file:
            # noinspection PyBroadException
            try:
                s = replace_random(query, extra_file["text"])
                if not write_safe(efilename, s, others_permissions=extra_file_perms):
                    print(f"Tried to write to unsafe path: {efilename}")
            except:
                print("Can not write", efilename)
        if "file" in extra_file:
            try:
                if extra_file.get("type", "") != "bin":
                    headers = extra_file.get("headers", {})
                    lines = replace_random(
                        query, get_url_lines_as_string(extra_file["file"], headers)
                    )
                    write_safe(efilename, lines, others_permissions=extra_file_perms)
                else:
                    write_safe(
                        efilename,
                        urlopen(extra_file["file"]).read(),
                        "wb",
                        others_permissions=extra_file_perms,
                    )
            except Exception as e:
                print(str(e))
                print("XXXXXXXXXXXXXXXXXXXXXXXX Could no file cache: \n", efilename)


def delete_extra_files(extra_files, prgpath):
    if not extra_files:
        return
    ie = 0
    for extra_file in extra_files:
        ie += 1
        if extra_file.get("delete", False):
            efilename = prgpath + "/extrafile" + str(ie)
            if "name" in extra_file:
                efilename = prgpath + "/" + extra_file["name"]
            # noinspection PyBroadException
            try:
                rm(efilename)
            except:
                print("Can not delete: ", efilename)


def get_md(ttype: TType, query):
    _, bycode, js, runner = handle_common_params(query, ttype)

    usercode = None
    user_print = get_json_param(query.jso, "userPrint", None, False)
    if user_print:
        usercode = get_json_eparam(query.jso, "state", "usercode", None, False)
    if usercode is None:
        usercode = bycode

    # s = '\\begin{verbatim}\n' + usercode + '\n\\end{verbatim}'
    header = str(get_param(query, "header", ""))
    stem = str(get_param(query, "stem", ""))
    footer = str(get_param(query, "footer", ""))

    rows = get_param(query, "rows", None)

    target_format = get_param(query, "targetFormat", "latex")

    if target_format == "html":
        s = """
<h4>{}</h4>
<p>{}</p>
<pre>
{}
</pre>
<p>{}</p>
""".format(
            header, stem, usercode, footer
        )
        return s

    if target_format == "md":
        s = """
#### {}
{}
```
{}
```
{}
""".format(
            header, stem, usercode, footer
        )
        return s

    if target_format == "latex":
        code = "\\begin{lstlisting}\n" + str(usercode) + "\n" + "\\end{lstlisting}\n"

        if "mathcheck" in ttype and str(usercode) == "=":
            code = ""

        if "text" in ttype and rows is not None and str(usercode) == "":
            r = ""  # for text make a verbatim with number of rows empty lines
            rows = str_to_int(rows, 1)
            for i in range(0, rows):
                r += "\n"
            code = "\\begin{verbatim}\n" + r + "\\end{verbatim}\n"

        s = (
            "\\begin{taskenv}{"
            + header
            + "}{"
            + stem
            + "}{"
            + footer
            + "}"
            + "\\lstset{language=[Sharp]C, numbers=left}\n"
            + code
            + "\\end{taskenv}"
        )

        return s

    # plain
    s = """
{}

{}

{}

{}
""".format(
        header, stem, usercode, footer
    )
    return s


def get_cache_footer(query):
    cache_footer = get_param(query, "cacheFooter", "")
    if not cache_footer:
        cache_footer = get_param(query, "footer", "")

    if not cache_footer:
        return ""

    cache_footer = tim_sanitize(cache_footer)
    return "<figcaption>" + cache_footer + "</figcaption>"


def get_graphviz_data(query):
    return get_param(query, "gvData", None)


def convert_graphviz(query):
    gv_data = get_graphviz_data(query)
    if gv_data:  # convert graphViz plugin to csPlugin
        gv_type = get_param(query, "gvType", "dot")
        query.jso["markup"]["type"] = "run"
        query.jso["markup"]["cmds"] = gv_type + " -Tsvg {0}"
        query.jso["markup"]["isHtml"] = True
        query.query["isHtml"] = [True]
        query.query["type"] = ["run"]
        query.jso["markup"]["fullprogram"] = gv_data
        query.jso["markup"]["cacheClass"] = "figure graphviz "
        if query.jso.get("input", None):
            query.jso["input"]["type"] = "run"
            if not query.jso["input"].get("markup", None):
                query.jso["input"]["markup"] = {}
            query.jso["input"]["markup"]["type"] = "run"
        if get_param(query, "cache", True):
            query.jso["markup"]["cache"] = True
        else:
            check_fullprogram(query, True)


def get_html(self: "TIMServer", ttype: TType, query: QueryClass):
    htmldata = get_param(query, "cacheHtml", None)  # check if constant html
    if htmldata:
        return tim_sanitize(htmldata) + get_cache_footer(query)
    video_start = '<video src="'
    convert_graphviz(query)
    markup = query.jso.get("markup", {})
    if get_param(query, "cache", False):  # check if we should take the html from cache
        cache_root = "/tmp"
        cache_clear = get_param(query, "cacheClear", True)
        if not cache_clear:
            cache_root = "/tmp/ucache"  # maybe user dependent cache that may grow bigger, so to different place
        h = hashlib.new("ripemd160")
        h.update((str(markup) + str(query.deleted)).encode())
        task_id = get_param(query, "taskID", False)
        filepath = cache_root + "/imgcache/" + task_id.replace(".", "/")
        if filepath.endswith("/"):
            task_id = get_param(query, "taskIDExt", False)
            filepath = "/tmp/imgcache/" + task_id.replace("..", "/")

        contenthash = h.hexdigest()
        filename = filepath + "/" + contenthash + ".html"
        if os.path.isfile(filename):  # if we have cache, use that
            with open(filename) as fh:
                htmldata = fh.read()
            video_index = htmldata.find(video_start)
            if video_index >= 0:
                video_i1 = video_index + len(video_start)
                video_i2 = htmldata.find('"', video_i1)
                video_name = htmldata[video_i1:video_i2]
                if os.path.isfile(video_name):
                    return htmldata
            else:
                return htmldata

        markup["imgname"] = "/csgenerated/" + task_id  # + "/" + hash
        query.jso["state"] = None

        ret = self.do_all(query)  # otherwise generate new image
        # if is_html, then web.console is sanitized
        # web.err is not sanitized
        # web.console may not be sanitized and should be put inside pre-element.

        htmldata = NOLAZY

        error = ret["web"].get("error", None)
        if not ret:
            return ""

        if error:
            htmldata += "<pre>" + cs_min_sanitize(str(error)) + "</pre>"

        is_html = get_param(query, "isHtml", False)
        default_class = "console"
        if is_html:
            default_class = "htmlresult"
        cache_class = get_param(query, "cacheClass", default_class)

        cache_elem = "div"
        if cache_class == "console":
            cache_elem = "pre"

        if cache_class:
            cache_class = 'class="' + cs_min_sanitize(cache_class) + '"'
        else:
            cache_class = ""

        image_attributes = cs_min_sanitize(get_param(query, "imageAttributes", ""))

        console = ret["web"].get("console", None)
        if console:
            if not is_html:
                console = cs_min_sanitize(str(console))
            # else:
            #     console = svg_sanitize(console)
            htmldata += (
                "<"
                + cache_elem
                + " "
                + cache_class
                + ">"
                + str(console)
                + "</"
                + cache_elem
                + ">"
            )

        img = ret["web"].get("image", None)
        if img:
            qidx = img.find("?")
            if qidx >= 0:  # remove timestamp with ?
                img = img[0:qidx]
            with open(img, "rb") as fh:
                pngdata = fh.read()
            pngenc = b64encode(pngdata)
            _, imgext = splitext(img)
            imgext = imgext[1:]
            htmldata += (
                '<img src="data:image/'
                + imgext
                + ";base64, "
                + pngenc.decode()
                + '" '
                + image_attributes
                + "/>"
            )
            rm_safe(img)

        video = ret["web"].get("video", None)
        if isinstance(video, str) and video:
            htmldata += (
                video_start
                + video
                + '" '
                + 'type="video/mp4" controls="" style = "width: 100%;"></video>'
            )
        htmldata += get_cache_footer(query)

        if cache_clear:
            files = glob.glob(filepath + "/*")
            for f in files:
                os.remove(f)

        os.makedirs(filepath, exist_ok=True)

        write_safe(filename, htmldata)

        # return '<img src="' + img + '">'
        return htmldata

    user_id = get_param(query, "user_id", "--")
    # print("UserId:", user_id)
    if user_id == "Anonymous":
        allow_anonymous = str(get_param(query, "anonymous", "false")).lower()
        # jump = get_param(query, "taskID", "")
        # print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX jump: ", jump)
        if allow_anonymous != "true":
            return (
                NOLAZY
                + '<p class="pluginError">Please <tim-login-menu>'
                + "</tim-login-menu> to interact with this component</p>"
                + '<pre class="csRunDiv">'
                + get_param(query, "byCode", "")
                + "</pre>"
            )
    do_lazy = is_lazy(query)
    # do_lazy = False
    # print("do_lazy",do_lazy,type(do_lazy))

    language, bycode, js, runner = handle_common_params(query, ttype)

    state = query.jso.get("state")
    if state:
        state_copy = language.state_copy()
        for key in state_copy:
            # value = get_json_eparam(query.jso, "state", key, "")
            value = state.get(key, None)
            if value is not None:
                js["markup"][key] = value

    before_open = markup.get("beforeOpen", "")
    is_rv = is_review(query)

    if do_lazy and not before_open:
        before_open = language.get_default_before_open()

    usercode = get_json_eparam(query.jso, "state", "usercode", None)

    parsons_options = markup.get("parsons", {})
    if parsons_options.get("shuffleHost", False):
        if usercode is None:
            bycode_lines = bycode.split("\n")
            random.shuffle(bycode_lines)
            bycode = "\n".join(bycode_lines)
            js["by"] = bycode
        js["markup"].pop("byCode", None)

    if before_open or is_rv:
        susercode = language.modify_usercode(usercode or "")
        before_open = before_open.replace("{USERCODE}", susercode)
        js["markup"]["beforeOpen"] = before_open

    doc_addr = doc_address(query, True)
    if doc_addr:
        js["markup"]["docurl"] = doc_addr["dochtml"]

    parsons_md = parsons_options.get(
        "md", parsons_options.get("math_type", None) is not None
    )
    if parsons_md:
        code = bycode
        if usercode:
            code = usercode
        bymd = code.split("\n")
        dopts = DumboOptions.from_dict(parsons_options)
        htmls = call_dumbo(bymd, options=dopts)
        parsons_html = []
        for i in range(0, len(bymd)):
            parsons_html.append({"t": bymd[i], "h": htmls[i]})
        js["markup"]["parsons"]["html"] = parsons_html
    jso = json.dumps(js)

    if is_rv:
        userinput = get_json_eparam(query.jso, "state", "userinput", "")
        userargs = get_json_eparam(query.jso, "state", "userargs", "")
        uploaded_file = get_json_eparam(query.jso, "state", "uploadedFile", None)
        uploaded_files = get_json_param(query.jso, "state", "uploadedFiles", None)
        submitted_files = get_json_param(query.jso, "state", "submittedFiles", None)
        s = ""
        if "input" in ttype:
            s = s + "<p>Input:</p><pre>" + userinput + "</pre>"
        if "args" in ttype:
            s = s + "<p>Args:</p><pre>" + userargs + "</pre>"
        if uploaded_files is not None:
            for f in uploaded_files:
                s = s + f'<p>File:</p><pre>{os.path.basename(f["path"])}</pre>'
        elif uploaded_file is not None:
            s = s + "<p>File:</p><pre>" + os.path.basename(uploaded_file) + "</pre>"
        if submitted_files is not None:
            for f in submitted_files:
                s = s + f'<pre>{f["path"]}</pre>'
                if "content" not in f:
                    continue
                ucode = language.get_review(f["content"])
                if isinstance(ucode, str):
                    s = (
                        s
                        + f"<pre>---------------------FILE START---------------------\n"
                    )
                    s = s + ucode
                    s = (
                        s
                        + "\n----------------------FILE END----------------------</pre>"
                    )
        else:
            ucode = language.get_review(usercode)
            if isinstance(ucode, str) and "drawio" not in ttype:
                s = "<pre>" + ucode + "</ pre>" + s
            elif "drawio" in ttype:
                return ucode
        if not s:
            s = "<pre>No answer</pre>"
        result = NOLAZY + '<div class="review" ng-non-bindable>' + s + "</div>"
        return result

    r = runner
    s = "<" + r + " ng-cloak>xxxJSONxxx" + jso + "</" + r + ">"
    # print(s)
    lazy_visible = ""
    lazy_class = ""
    lazy_start = ""
    lazy_end = ""

    if do_lazy:
        # r = LAZYWORD + r;
        code = bycode
        if isinstance(usercode, str):
            code = usercode
        if not isinstance(code, str):
            print("Ei ollut string: ", code, jso)
            code = "" + str(code)
            # ebycode = html.escape(code)
        # ebycode = code.replace("</pre>", "</pre>")  # prevent pre ending too early
        ebycode = code.replace("<", "&lt;").replace(">", "&gt;")
        cs_class = "csRunDiv "
        if not get_param(query, "borders", True):
            cs_class = ""

        if before_open:
            ebycode = before_open
        if "tiny" in ttype:
            lazy_visible = (
                '<div class="lazyVisible '
                + cs_class
                + 'csTinyDiv no-popup-menu" >'
                + get_tiny_surrounding_headers(query, "" + ebycode + "")
                + "</div>"
            )
        else:
            # btn = '<p class="csRunMenu"><button class="tim-button"></button></p>'
            prebeg = ""
            preend = ""
            lazyclasses = ""
            if not before_open:
                prebeg = "<pre>"
                preend = "</pre>"
                lazyclasses = "csRunCode csEditorAreaDiv csrunEditorDiv csRunArea csInputArea csLazyPre"
            btn = '<p class="csRunMenu">&nbsp;</p>'
            lazy_visible = (
                '<div class="lazyVisible '
                + cs_class
                + "no-popup-menu"
                + (" cs-has-header" if get_param(query, "header", None) else "")
                + '">'
                + get_surrounding_headers(
                    query,
                    ('<div class="' + lazyclasses + '"' ' " ng-non-bindable>' + prebeg)
                    + ebycode
                    + preend
                    + "</div>",
                )
                + btn
                + "</div>"
            )
        # lazyClass = ' class="lazyHidden"'
        lazy_start = LAZYSTART
        lazy_end = LAZYEND

    if "c1" in ttype or True:  # c1 oli testejä varten ettei sinä aikana rikota muita.
        s = f'{lazy_start}<{r}{lazy_class} ng-cloak json="{encode_json_data(jso)}"></{r}>{lazy_end}{lazy_visible}'
    return s


def handle_common_params(query: QueryClass, ttype: TType):
    language = ttype.get_language()
    runner = ttype.runner_name()

    if query.hide_program:
        get_param_del(query, "program", "")
    ttype.modify_query()
    js = query_params_to_map_check_parts(query, language.deny_attributes())

    if str(ttype) not in [
        "tauno",
        "parsons",
    ]:  # TODO: make tauno, simcir and parsons work without this
        if "markup" not in js or js["markup"] is None:
            js["markup"] = {}
        js["markup"]["type"] = str(ttype)

    if not ttype.success:
        return language, "", js, runner

    # print(js)

    q_bycode = get_param(query, "byCode", None)
    q_byfile = get_param(query, "byFile", None)
    not_found_error = get_param(query, "notFoundError", None)
    if q_byfile is not None and q_bycode is None:
        # TODO: Better error message for missing file
        js["by"] = get_url_lines_as_string(
            q_byfile,
            not_found_error=not_found_error,
            usecache=get_param(query, "usecache", True),
        )
    bycode = ""
    if q_bycode is not None:
        bycode = q_bycode
    else:
        if "by" in js:
            bycode = js["by"]
    if get_param(query, "noeditor", False):
        bycode = ""
    uf = get_param(query, "uploadedFile", None)
    ut = get_param(query, "uploadedType", None)
    uf = get_json_eparam(query.jso, "state", "uploadedFile", uf)
    ut = get_json_eparam(query.jso, "state", "uploadedType", ut)
    if uf and ut:
        js["uploadedFile"] = uf
        js["uploadedType"] = ut
    ufs = get_param(query, "uploadedFiles", None)
    ufs = get_json_param(query.jso, "state", "uploadedFiles", ufs)
    if ufs:
        js["uploadedFiles"] = ufs
    # jso)
    # print(ttype)
    if "simcir" in ttype:
        bycode = ""

    return language, bycode, js, runner


def wait_file(f1):
    """Wait until the file is ready or 10 tries has been done.

    :param f1: filename to wait
    :return: the file status if it became ready, otherwise False

    """
    count = 0
    while count < 10:
        count += 1
        if os.path.isfile(f1):
            s1 = os.stat(f1)
            if s1.st_size > 50:
                return s1
            print(s1.st_size, " ??????????????????????? ")
        time.sleep(0.05)
    return False


def debug_str(s):
    t = datetime.datetime.now()
    print(t.isoformat(" ") + ": " + s)


def check_code(out, err, compiler_output, ttype):
    err = err + compiler_output
    if ttype == "fs":
        err = err.replace(
            "F# Compiler for F# 4.0 (Open Source Edition)\n"
            "Freely distributed under the Apache 2.0 Open Source License\n",
            "",
        )

    if str != type(err):
        err = err.decode()
    # if type(out) != type(''): out = out.decode()
    # noinspection PyBroadException
    try:
        if out and out[0] in [254, 255]:
            out = out.decode("UTF16")
        elif str != type(out):
            out = out.decode("utf-8-sig")
    except:
        # out = out.decode('iso-8859-1')
        pass

    return out, err


def check_fullprogram(query, cut_errors=False):
    # Try to find fullprogram or fullfile attribute and if found,
    # do program, bycode and replace attributes from that
    query.cut_errors = get_param_table(query, "cutErrors")
    # -cutErrors:
    # - "(\n[^\n]*REMOVEBEGIN.* REMOVEEND[^\n]*)"
    # - "(\n[^\n]*REMOVELINE[^\n]*)"
    if not query.cut_errors:
        query.cut_errors = [
            {"replace": r"(\n[^\n]*REMOVEBEGIN.*? REMOVEEND[^\n]*)", "by": ""},
            {"replace": r"(\n[^\n]*REMOVELINE[^\n]*)", "by": ""},
        ]

    query.hide_program = False
    fullprogram = get_param(query, "-fullprogram", None)
    if not fullprogram:
        fullprogram = get_param(query, "fullprogram", None)
    else:
        query.hide_program = True
    if not fullprogram:
        fullfile = get_param(query, "-fullfile", None)
        if not fullfile:
            fullfile = get_param(query, "fullfile", None)
        else:
            query.hide_program = True
        if not fullfile:
            return False
        fullprogram = get_url_lines_as_string(
            fullfile, usecache=get_param(query, "usecache", True)
        )

    if not fullprogram:
        return False

    # fullprogram = do_sed_replacements(query, fullprogram)
    get_param_del(query, "fullprogram", "")
    get_param_del(query, "fullfile", "")

    fullprogram = replace_program_tokens(query, fullprogram)
    program = fullprogram

    program = replace_random(query, program)
    by_code_replace = [
        {"replace": r"(\n[^\n]*DELETEBEGIN.*? DELETEEND[^\n]*)", "by": ""}
    ]
    program = replace_code(by_code_replace, program)
    delete_line = [{"replace": r"(\n[^\n]*DELETELINE[^\n]*)", "by": ""}]
    program = replace_code(delete_line, program)
    delete_line = get_param_table(query, "deleteLine")
    if delete_line:
        program = replace_code(delete_line, program)
    m = re.search(r"BYCODEBEGIN[^\n]*\n(.*)\n.*?BYCODEEND", program, flags=re.S)
    # m = re.search("BYCODEBEGIN[^\\n]\n(.*)\n.*?BYCODEEND", program, flags=re.S)
    by_code = ""
    if m:
        by_code = m.group(1)
        by_code_replace = [
            {
                "replace": r"((\n|)[^\n]*BYCODEBEGIN.*?BYCODEEND[^\n]*)",
                "by": "\nREPLACEBYCODE",
            }
        ]
    else:  # no BYCODEBEGIN
        m = re.search(r"BYCODEBEGIN[^\\n]*\n(.*)", program, flags=re.S)
        if m:
            by_code = m.group(1)
            by_code_replace = [
                {"replace": r"((\n|)[^\n]*BYCODEBEGIN.*)", "by": "\nREPLACEBYCODE"}
            ]
        else:
            if (
                program.find("BYCODEEND") >= 0
            ):  # TODO: for some reason next regexp is slow in not found case
                m = re.search(r"[^\n]*\n(.*)\n.*?BYCODEEND", program, flags=re.S)
                if m:
                    by_code = m.group(1)
                    by_code_replace = [
                        {
                            "replace": r"((\n|)[^\n]*.*?BYCODEEND[^\n]*)",
                            "by": "\nREPLACEBYCODE",
                        }
                    ]
            else:
                by_code = fullprogram
                program = "REPLACEBYCODE"
    program = replace_code(by_code_replace, program)
    if program.startswith("\nREPLACEBYCODE") and not fullprogram.startswith("\n"):
        program = program[1:]  # remove extra \n from begin
    if cut_errors:
        program = replace_code(query.cut_errors, program)
    query.query["replace"] = ["REPLACEBYCODE"]
    query.query["program"] = [program]
    query.query["by"] = [by_code]
    # query.jso.markup["byCode"] = by_code
    # query.jso.markup["program"] = program
    return True


def doc_address(query, check=False):
    docaddr = get_clean_param(query, "docaddr", "")
    if not docaddr:
        return None
    docaddr = re.sub(r"(https:)|([^a-zA-Z0-9])", "", docaddr)
    task_id = str(query.query.get("taskID", ""))
    task_id = re.sub(r"(https:)|([^a-zA-Z0-9])", "", task_id)
    userdoc = "/csgenerated/docs/%s" % task_id
    docrnd = docaddr
    dochtml = "{}/{}/html/{}".format(userdoc, docrnd, "files.html")
    if check:
        if not os.path.isfile(dochtml):
            return None

    return {"userdoc": userdoc, "docrnd": docrnd, "dochtml": dochtml}


def update_markup_from_file(query):
    file = get_param(query, "fromFile", False)
    if not file:
        return query

    if isinstance(file, bool):
        file = ""

    if file.startswith("/"):
        file = Path(file)
    else:
        master_path = get_param(query, "masterPath", None)
        if master_path is None:
            raise Exception(
                "Cannot fetch markup from file as masterPath is not specified"
            )
        file = "/cs/masters/" / Path(master_path) / file

    if file.is_dir():
        file = file / "csmarkup.json"

    if not file.is_file():
        raise FileNotFoundError("Markup file does not exist")

    if query.jso is None:
        query.jso = {}
    if "markup" not in query.jso:
        query.jso["markup"] = {}

    with open(str(file)) as f:
        query.jso["markup"] = dict(
            list(json.load(f).items()) + list(query.jso["markup"].items())
        )


# see: http://stackoverflow.com/questions/366682/how-to-limit-execution-time-of-a-function-call-in-python
# noinspection PyUnusedLocal
def signal_handler(signum, frame):
    print("Timed out1!")
    raise Exception("Timed out1!")


type_splitter = re.compile("[^+a-z0-9]")


def handle_iframes(
    iframes, web_iframes, out, err, prgpath, sandbox=None, recurse=False
):
    """
    Handle list of iframes.  If íframe has readIframes attribute,
    read the iframes dict from file and continue by that list
    :param iframes: list of iframes to use
    :param web_iframes: result variable for iframes
    :param out: output string until now
    :param err: error string until now
    :param prgpath: program path directory
    :param sandbox: if recursice call, use this sandbox
    :param recurse: if recursive call, do not read iframes dict from file
    :return: possibly modified out and err
    """
    for iframe in iframes:
        content = None
        filename = ""
        fn = ""
        # noinspection PyBroadException
        try:
            fn = iframe.get("readIframes", None)
            if fn:
                if recurse:
                    continue  # do not read iframes recursively
                # read iframes from file but only when sandbox is not set
                filename = prgpath + "/" + fn
                with open(filename, encoding="utf-8") as f:
                    content = f.read()
                # content_iframes = json.loads(content)
                content_iframes = yaml.safe_load(content)
                sbox = iframe.get("sandbox", None)
                out, err = handle_iframes(
                    content_iframes, web_iframes, out, err, prgpath, sbox, True
                )
                continue

            fn = iframe.get("filename", None)
            if fn:
                if fn == "stdout":
                    content = out
                    out = ""
                else:
                    filename = prgpath + "/" + fn
                    with open(filename, encoding="utf-8") as f:
                        content = f.read()
        except Exception:  # pylint: disable=broad-except
            if not iframe.get("ignoreError", False):
                err += " Error reading iframe file: " + fn
        if content:
            if iframe.get("remove", True):
                try:
                    os.remove(filename)
                except FileNotFoundError:
                    pass
            iframe["content"] = content
            if recurse:
                # if sandbox when read iframes from file, use top level sandbox
                iframe.pop("sandbox", None)
                if sandbox is not None:
                    iframe["sandbox"] = sandbox
            web_iframes.append(iframe)
    return out, err


def check_iframes(query, web, out, err, prgpath):
    """
    Handle iframes in the query.
    This function will read the iframe files and put them to web
    :param query: all the parameters
    :param web: result
    :param out: output string until now
    :param err: error string until now
    :param prgpath: path to the program directory
    :return possibly modified out and err, for example
            out may change to empty string
            if the iframe is stdout and err may get new error messages
    """
    iframes = get_param(query, "iframes", None)
    if not iframes:
        return out, err
    web_iframes = []
    web["iframes"] = web_iframes
    out, err = handle_iframes(iframes, web_iframes, out, err, prgpath)
    return out, err


class TIMServer(http.server.BaseHTTPRequestHandler):
    def __init__(self, request, client_address, _server):
        # print(request,client_address)
        super().__init__(request, client_address, _server)
        self.user_id = "--"

    def log(self):
        msg_logger = get_logger()
        agent = self.headers.get("User-Agent", "unknown")
        if agent.find("ython") >= 0:
            agent = ""

        msg_logger.info(self.path, extra={"user_id": self.user_id, "useragent": agent})

    def do_OPTIONS(self):
        print("do_OPTIONS ==============================================")
        self.send_response(200, "ok")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "GET, PUT, POST, OPTIONS")
        self.send_header(
            "Access-Control-Allow-Headers", "version, X-Requested-With, Content-Type"
        )
        print(self.path)
        print(self.headers)

    def do_GET(self):
        # print("do_GET ==================================================")
        self.do_all(get_params(self))

    def do_POST(self):
        # print("do_POST =================================================")
        if self.path == "/cs/fetchExternal":
            do_headers(self, "application/json")
            response = {}
            try:
                response["files"] = FileHandler(post_params(self)).get_external_files()
            except Exception as e:
                response["error"] = str(e)
                print_exc()
            self.wout(json.dumps(response))
            return

        multimd = self.path.find("/multimd") >= 0

        if self.path.find("/multihtml") < 0 and not multimd:
            self.do_all(post_params(self))
            return

        # print("do_POST MULTIHML ==========================================")
        t1 = time.perf_counter()
        t1t = time.time()
        queries = multi_post_params(self)
        do_headers(self, "application/json")
        is_tauno = self.path.find("/tauno") >= 0
        is_simcir = self.path.find("/simcir") >= 0
        is_parsons = self.path.find("/parsons") >= 0
        htmls = []
        self.user_id = get_param(queries[0], "user_id", "--")
        if self.user_id != "--":
            print("UserId:", self.user_id)
        self.log()
        # print(queries)

        global_anonymous = False
        for query in queries:
            is_graphviz = get_graphviz_data(query) is not None
            # noinspection PyBroadException
            try:
                update_markup_from_file(query)
            except Exception:  # pylint: disable=broad-except
                pass  # TODO: show error to user

            ga = get_param(query, "GlobalAnonymous", None)
            if ga:
                global_anonymous = True
            if global_anonymous:
                query.query["anonymous"] = [True]
            # print(query.jso)
            # print(str(query))
            usercode = get_json_param(query.jso, "state", "usercode", None)
            if isinstance(usercode, str):
                query.query["usercode"] = [usercode]

            submitted_files = get_json_param(query.jso, "state", "submittedFiles", None)
            if submitted_files is not None:
                query.query["submittedFiles"] = [submitted_files]

            userinput = get_json_param(query.jso, "state", "userinput", None)
            if userinput is None:
                userinput = get_json_param(query.jso, "markup", "userinput", None)

            if userinput is not None:
                query.query["userinput"] = [str(userinput)]
            userargs = get_json_param(query.jso, "state", "userargs", None)
            if userargs is None:
                userargs = get_json_param(query.jso, "markup", "userargs", None)

            if userargs is not None:
                query.query["userargs"] = [str(userargs)]
            selected_language = get_json_param(
                query.jso, "state", "selectedLanguage", None
            )
            if selected_language:
                query.query["selectedLanguage"] = [selected_language]
            timeout = get_json_param(query.jso, "markup", "timeout", None)
            if timeout:
                query.query["timeout"] = [timeout]
            ttype = get_param(query, "type", None)
            if is_tauno:
                ttype = "tauno"
            if is_simcir:
                ttype = "simcir"
            if is_parsons:
                ttype = "parsons"
            if is_graphviz:
                ttype = "cs"
            # The graphviz conversion happens later, so need to check that here before giving an error.
            if ttype is None:
                s = '<div class="pluginError">Attribute "type" is required.</div>'
            else:
                ttype = TType(ttype, query)
                check_fullprogram(query, True)
                if multimd:
                    # noinspection PyBroadException
                    try:
                        s = get_md(ttype, query)
                    except Exception as ex:
                        print("ERROR: " + str(ex) + " " + json.dumps(query))
                        continue
                else:
                    s = get_html(self, ttype, query)
                # print(s)
            htmls.append(s)

        # print(htmls)
        sresult = json.dumps(htmls)
        self.wout(sresult)
        self.log()
        t2 = time.perf_counter()
        t2t = time.time()
        ts = f"multihtml: {t2 - t1:7.4f} {t2t - t1t:7.4f}"
        print(ts)

    def do_PUT(self):
        # print("do_PUT =================================================")
        t1put = time.time()
        self.do_all(post_params(self))
        t2 = time.time()
        ts = "do_PUT: %7.4f" % (t2 - t1put)
        print(ts)

    def wout(self, s):
        self.wfile.write(s.encode("UTF-8"))

    def do_reqs(self, query):
        path = self.path
        qindex = self.path.find("?")
        if qindex >= 0:
            path = self.path[:qindex]

        is_cache = get_param(query, "cache", False)
        is_graphviz = path.find("/graphviz") >= 0
        is_tauno = path.find("/tauno") >= 0
        is_simcir = path.find("/simcir") >= 0
        is_parsons = path.find("/parsons") >= 0
        is_rikki = path.find("rikki") >= 0

        if not is_cache:
            content_type = "application/json"
            do_headers(self, content_type)

        templs = {}
        jslist = all_js_files()
        csslist = all_css_files()

        result_json = {
            "js": jslist,
            "css": csslist,
            "multihtml": True,
            "multimd": True,
            "canGiveTask": True,
        }

        if not (is_tauno or is_rikki or is_parsons or is_simcir or is_graphviz):
            templs = get_all_templates("templates")

        if is_parsons:
            result_json = {
                "js": [
                    "/cs/js/build/csPlugin.js",
                    "jqueryui-touch-punch",
                    "/cs/js-parsons/lib/underscore-min.js",
                    "/cs/js-parsons/lib/lis.js",
                    "/cs/js-parsons/parsons.js",
                    "/cs/js-parsons/lib/skulpt.js",
                    "/cs/js-parsons/lib/skulpt-stdlib.js",
                    "/cs/js-parsons/lib/prettify.js",
                ],
                "css": [
                    "/csstatic/css/cs.css",
                    "/cs/js-parsons/parsons.css",
                    "/cs/js-parsons/lib/prettify.css",
                ],
                "multihtml": True,
                "multimd": True,
            }
        if is_simcir:
            result_json = {
                "js": jslist,
                "css": csslist,
                "multihtml": True,
                "multimd": True,
            }
        result_json.update(templs)
        result_str = json.dumps(result_json)
        return self.wout(result_str)

    def do_all(self, query):
        timeout = get_param(query, "timeout", 20)
        if not isinstance(timeout, int):
            # noinspection PyBroadException
            try:
                timeout = int(timeout)
            except:  # pylint: disable=broad-except
                timeout = 20
        timeout = (
            timeout + 3
        )  # +3 because we want languages to realize the timeout first
        # noinspection PyBroadException
        try:
            signal.signal(signal.SIGALRM, signal_handler)
            signal.alarm(timeout)  # Ten seconds
        except Exception:  # pylint: disable=broad-except
            # print("No signal", e)  #  TODO; why is this signal at all when it always comes here?
            pass
        try:
            return self.do_all_t(query)
        except Exception as e:
            print("Timed out2!", e)
            logging.exception("Timed out 2 trace")
            self.wout(str(e))

    def do_all_t(self, query: QueryClass):
        convert_graphviz(query)

        t1start = time.time()
        t_run_time = 0
        times_string = ""
        # print("t:", time.time() - t1start)

        query.randomcheck = binascii.hexlify(os.urandom(16)).decode()
        pwddir = ""
        # print(threading.currentThread().getName())
        result = {}  # query.jso
        save: dict[str, Any] = {}
        web = {}
        tim_info = {}
        result["web"] = web
        result["save"] = save
        result["tim_info"] = tim_info

        # print("doAll ===================================================")
        # print(self.path)
        # print(self.headers)

        if self.path.find("/favicon.ico") >= 0:
            self.send_response(404)
            return

        # print(query)
        self.user_id = get_param(query, "user_id", "--")
        if self.user_id != "--":
            print("UserId:", self.user_id)
        self.log()
        """
        if self.path.find('/login') >= 0:
            username = check_korppi_user(self,"tim")
            if not username: return

            self.send_response(200)
            content_type = 'text/plain'
            self.send_header('Content-type', content_type)
            self.end_headers()
            self.wout("Username = " + username)
            return
        """

        is_test = ""
        path = self.path
        qindex = self.path.find("?")
        if qindex >= 0:
            path = self.path[:qindex]

        if "/reqs" in path:
            return self.do_reqs(query)

        is_cache = get_param(query, "cache", False)
        is_template = path.find("/template") >= 0
        is_fullhtml = path.find("/fullhtml") >= 0
        is_gethtml = path.find("/gethtml") >= 0
        is_html = (
            path.find("/html/") >= 0 or path.find(".html") >= 0
        ) and not is_gethtml
        is_css = path.find(".css") >= 0
        is_js = path.find(".js") >= 0 or path.find(".ts") >= 0
        # is_graphviz = path.find("/graphviz") >= 0
        is_iframe_param = get_param_del(query, "iframe", "")
        is_iframe = (path.find("/iframe") >= 0) or is_iframe_param
        is_answer = path.find("/answer") >= 0
        is_tauno = path.find("/tauno") >= 0
        is_simcir = path.find("/simcir") >= 0
        # is_parsons = path.find("/parsons") >= 0
        is_ptauno = path.find("/ptauno") >= 0
        is_rikki = path.find("rikki") >= 0
        print_file = get_param(query, "print", "")

        try:
            if is_css:
                if os.path.isfile(self.path):
                    do_headers(self, "text/css")
                return self.wout(file_to_string(self.path))

            if is_js:
                if is_rikki:
                    return self.wout(file_to_string("js/dirRikki.js"))
                if os.path.isfile(self.path):
                    do_headers(self, "application/javascript")
                return self.wout(file_to_string(self.path))
            if is_html:
                p = self.path.split("?")[0]
                if os.path.isfile(p):
                    do_headers(self, "text/html")
                return self.wout(file_to_string(p))

        except Exception as e:
            if str(e).find("[Errno 2]") >= 0:
                self.send_response(404)
            else:
                self.send_response(400)
            self.end_headers()
            self.wout(str(e))
            # self.send_error(404, str(e))
            return

        content_type = "text/plain"
        if is_fullhtml or is_gethtml or is_html or is_ptauno or is_tauno:
            content_type = "text/html; charset=utf-8"
        if is_answer:
            content_type = "application/json"
        if not is_cache:
            do_headers(self, content_type)

        if is_template:
            tempfile = get_param(query, "file", "")
            tidx = get_param(query, "idx", "0")
            # print("tempfile: ", tempfile, tidx)
            # return self.wout(file_to_string('templates/' + tempfile))
            return self.wout(get_template("templates", tidx, tempfile))

        if self.path.find("refresh") >= 0:
            print(f"Cleaning cache")
            keys, mem_cache_len, disk_cache_len = get_cache_keys()
            clear_cache()
            print(keys)
            self.wout(
                f"Removed {mem_cache_len} in-memory cache items and {disk_cache_len} cached files"
            )
            return

        if is_gethtml:
            scripts = get_param(query, "scripts", "")
            inchtml = get_param(query, "html", "")
            p = self.path.split("?")
            # print(p, scripts)
            htmlstring = file_to_string(p[0])
            htmlstring = replace_scripts(htmlstring, scripts, "%INCLUDESCRIPTS%")
            htmlstring = htmlstring.replace("%INCLUDEHTML%", inchtml)
            self.wout(htmlstring)
            return

        if is_ptauno:
            # print("PTAUNO: " + content_type)
            p = self.path.split("?")
            self.wout(file_to_string(p[0]))
            return

        if is_tauno and not is_answer:
            # print("PTAUNO: " + content_type)
            p = self.path.split("?")
            self.wout(file_to_string(p[0]))
            return

        # answer-route

        task_id = get_param(query, "taskID", None)
        if task_id:
            print("taskID:", task_id)

        points_rule = None

        is_doc = False
        extra_files = None
        warnmessage = ""
        language = dummy_language
        # print("t:", time.time() - t1start)

        try:
            update_markup_from_file(query)
            # Get the template type
            ttype = TType.split(get_json_param(query.jso, "markup", "type", "cs"))
            is_iframe = is_iframe or "js" in ttype

            if is_tauno and not is_answer:
                ttype[0] = "tauno"  # answer is newer tauno
            if is_simcir:
                ttype[0] = "simcir"

            # if ( query.jso != None and query.jso.has_key("state") and query.jso["state"].has_key("usercode") ):
            uploaded_file = get_json_param(query.jso, "input", "uploadedFile", None)
            uploaded_type = get_json_param(query.jso, "input", "uploadedType", None)
            if uploaded_file and uploaded_type:
                save["uploadedFile"] = uploaded_file
                save["uploadedType"] = uploaded_type
            usercode = get_json_param(query.jso, "input", "usercode", None)
            if isinstance(usercode, str):
                query.query["usercode"] = [usercode]

            submitted_files = get_json_param(query.jso, "input", "submittedFiles", None)
            if submitted_files is not None:
                query.query["submittedFiles"] = [submitted_files]

            userinput = get_json_param(query.jso, "input", "userinput", None)
            if userinput is None:
                userinput = get_json_param(
                    query.jso, "state", "userinput", None
                )  # this might be needles??
            if userinput is None:
                userinput = get_json_param(query.jso, "markup", "userinput", None)
            if userinput is not None:
                userinput = str(userinput)
                markupuserinput = get_json_param(query.jso, "markup", "userinput", "")
                if userinput != markupuserinput:
                    save["userinput"] = userinput
                if len(userinput) > 0 and userinput[-1:] != "\n":
                    userinput += "\n"
                query.query["userinput"] = [userinput]
            else:
                userinput = ""

            selected_language = get_json_param(
                query.jso, "input", "selectedLanguage", None
            )
            if selected_language:
                save["selectedLanguage"] = selected_language

            userargs = get_json_param(query.jso, "input", "userargs", None)
            if userargs is None:
                userargs = get_json_param(query.jso, "markup", "userargs", None)
            if userargs is not None:
                markupuserargs = get_json_param(query.jso, "markup", "userargs", "")
                userargs = str(userargs)
                if userargs != markupuserargs:
                    save["userargs"] = userargs
            else:
                userargs = ""

            is_doc = get_json_param(query.jso, "input", "document", False)

            extra_files = get_json_param(query.jso, "markup", "extrafiles", None)
            if not extra_files:
                extra_files = get_json_param(query.jso, "markup", "-extrafiles", None)

            # print("t:", time.time() - t1start)

            # print("USERCODE: XXXXX = ", usercode)

            # print("Muutos ========")
            # pprint(query.__dict__, indent=2)

            ttype = TType("/".join(ttype), query)
            ttype.modify_query()

            if is_html and not is_iframe:
                # print("HTML:==============")
                s = get_html(self, ttype, query)
                # print(s)
                return self.wout(s)

            if is_fullhtml:
                self.wout(file_to_string("begin.html"))
                self.wout(get_html(self, ttype, query))
                self.wout(file_to_string("end.html"))
                return

            if (
                is_iframe
                and not print_file
                and not ttype.has_any_of(
                    ["js", "glowscript", "vpython", "processing", "viz", "vars"]
                )
            ):
                s = string_to_string_replace_url(
                    '<iframe frameborder="0"  src="https://tim.jyu.fi/cs/fullhtml?##QUERYPARAMS##" '
                    + 'style="overflow:hidden;" height="##HEIGHT##" width="100%"  seamless></iframe>',
                    "##QUERYPARAMS##",
                    query,
                )
                return self.wout(s)

            nosave = get_param(query, "nosave", None)
            nosave = get_json_param(query.jso, "input", "nosave", nosave)

            if nosave:
                result["save"] = {}

            check_fullprogram(query, print_file)

            # Check query parameters
            p0 = FileParams(query, "", "")

            usercode_edit_rules = get_param(query, "usercodeEdit", None)
            if p0.by and usercode_edit_rules:
                p0.by = replace_code(usercode_edit_rules, p0.by)

            if p0.url == "" and p0.replace == "":
                p0.replace = "XXXX"

            s = get_file_to_output(query, False and print_file, p0)
            slines = ""

            # Open the file and write it
            # print(print_file,"Haetaan")
            if print_file:
                s = replace_code(query.cut_errors, s)
                return self.wout(s)

            # /answer-path comes here

            fhandler = FileHandler(query, save)
            submitted_files = fhandler.get_files(s)

            # code ahead needs the unmodified usercode, so take it here
            # TODO: refactor to not need this
            usercode = get_json_param(query.jso, "input", "usercode", None)
            if usercode is None:
                usercode = submitted_files[0].content

            # TODO: get_param is potentially unsafe as the client can change its value
            # get_json_param from markup would be better, but all language and test
            # languages change the type on client side
            # get_param type also doesn't contain the modifiers
            ttype = TType(get_param(query, "type", "cs"), query, submitted_files)
            if not ttype.success:
                raise Exception(f"Could not get language from type {ttype}")

            language = ttype.get_language()

            if not language.can_give_task() and query.jso.get("input", {}).get(
                "getTask", False
            ):
                raise Exception(f"Give task not allowed for {ttype}")

            filesaveattribute = get_param(query, "filesaveattribute", None)
            if filesaveattribute:
                if len(language.sourcefiles) > 1:
                    raise Exception(
                        "Cannot have multiple files with filesaveattribute"
                    )  # TODO
                attrnames = filesaveattribute.split(",")
                usercode = ""
                for aname in attrnames:
                    usercode += (
                        get_json_param(query.jso, "input", aname.strip(), "") + "\n"
                    )
                language.sourcefiles[0].content = usercode

            errorcondition = get_json_param(
                query.jso, "markup", "errorcondition", False
            )
            warncondition = get_json_param(query.jso, "markup", "warncondition", False)
            for file in language.sourcefiles:
                if file.content is None:
                    continue

                if is_doc:
                    file.content = replace_code(query.cut_errors, file.content)

                if not file.content.startswith("File not found"):
                    if errorcondition and re.search(
                        errorcondition, file.content, flags=re.S
                    ):
                        errormessage = get_json_param(
                            query.jso,
                            "markup",
                            "errormessage",
                            "Not allowed to use: " + errorcondition,
                        )
                        return write_json_error(self.wfile, errormessage, result)

                    if warncondition and re.search(
                        warncondition, file.content, flags=re.S
                    ):
                        warnmessage = "\n" + get_json_param(
                            query.jso,
                            "markup",
                            "warnmessage",
                            "Not recommended to use: " + warncondition,
                        )

                    # print(os.path.dirname(language.sourcefilename))
                    # print("Write file: " + language.sourcefilename)
                    if file.content == "":
                        file.content = "\n"

                    file.content = language.before_save(
                        language.before_code + file.content
                    )
                    slines = file.content

            # Write the program to the file =======================================================
            nofilesave = get_param(query, "nofilesave", False)
            if not nofilesave:
                mkdirs(language.prgpath)
                fhandler.save_files(language.sourcefiles, language)

            save_extra_files(query, extra_files, language.prgpath)

            if not nofilesave:
                for file in language.sourcefiles:
                    path = Path(file.path)
                    if not path.is_file():
                        return write_json_error(
                            self.wfile, "Could not get the source file", result
                        )
                    if path.stat().st_size == 0:
                        return write_json_error(
                            self.wfile,
                            "Could not get the source file (file is empty)",
                            result,
                        )
                # self.wfile.write("Could not get the source file\n")
                # print "=== Could not get the source file"

            nocode = get_param(query, "nocode", False)

            is_test = ""
            if "test" in str(ttype):  # str(ttype) to include all test types
                is_test = "test"
            points_rule = get_param(query, "pointsRule", None)
            # if points_rule is None and language.readpoints_default:
            #    points_rule = {}
            if points_rule is not None:
                points_rule["points"] = get_json_param(
                    query.jso, "state", "points", None
                )
                points_rule["result"] = 0
                if points_rule["points"]:
                    if is_test:
                        points_rule["points"]["test"] = 0
                    elif is_doc:
                        points_rule["points"]["doc"] = 0
                        # ("doc points: ", points_rule["points"]["doc"])
                    else:
                        points_rule["points"]["run"] = 0
                if not is_doc:
                    is_plain = False
                    expect_code = get_points_rule(
                        points_rule, is_test + "expectCode", None
                    )
                    if not expect_code:
                        expect_code = get_points_rule(
                            points_rule, is_test + "expectCodePlain", None
                        )
                        is_plain = True

                    parsons_options = get_param(query, "parsons", {})

                    if expect_code:
                        if expect_code == "byCode":
                            expect_code = get_param(query, "byCode", "")
                            if expect_code == "":
                                expect_code = query.query.get("by", [""])[0]
                        maxn = parsons_options.get("maxCheck", 0)
                        if maxn > 0:
                            p, parsons_correct = check_parsons(
                                expect_code,
                                usercode,
                                maxn,
                                parsons_options.get("notOrderMatters", False),
                                usercode_edit_rules,
                            )
                            if p > 0:
                                give_points(points_rule, "code", 1)
                            web["parsons_correct"] = parsons_correct
                        else:
                            excode = expect_code.rstrip("\n")
                            match = False
                            if is_plain:
                                match = usercode == excode
                            elif usercode is not None:
                                excode = re.compile(excode, re.M)
                                match = excode.match(usercode)
                            if match:
                                give_points(points_rule, "code", 1)
                    number_rule = get_points_rule(
                        points_rule, is_test + "numberRule", None
                    )
                    if number_rule:
                        give_points(
                            points_rule,
                            "code",
                            check_number_rule(usercode, number_rule),
                        )
                    points_key = get_points_rule(points_rule, "pointsKey", None)
                    if points_key:
                        p = query.jso.get("input", {}).get(points_key, None)
                        if p is not None:
                            give_points(points_rule, "code", p)

            # print(points_rule)

            # uid = pwd.getpwnam("agent").pw_uid
            # gid = grp.getgrnam("agent").gr_gid
            # os.chown(prgpath, uid, gid)
            # print("t:", time.time() - t1start)

            # print(ttype)
            # ########################## Compiling programs ###################################################
            self.log()
            cmdline = ""
            if get_param(query, "justCompile", False) and "comtest" not in str(
                ttype
            ):  # str(ttype) to include _comtest types
                language.just_compile = True

            if is_doc:
                # doxygen
                # ./doxygen/csdoc.sh /tmp/user/4d85...17114/3 /csgenerated/docs/vesal/abcd /csgenerated/docs/vesal
                # http://tim3/csgenerated/docs/vesal/abcd/html/index.html
                #
                userdoc = "/csgenerated/docs/%s" % self.user_id
                docrnd = generate_filename()
                locsource = language.prgpath
                locpath = re.sub(
                    r"(https:)|([^/a-zA-Z0-9])",
                    "",
                    get_clean_param(query, "locpath", ""),
                )
                if locpath:
                    locsource += "/" + locpath

                doc_addr = doc_address(query)
                if doc_addr:
                    userdoc = doc_addr["userdoc"]
                    docrnd = doc_addr["docrnd"]
                doccmd = (
                    f"/cs/doxygen/csdoc.sh {locsource} {userdoc}/{docrnd} {userdoc}"
                )
                doccmd = sanitize_cmdline(doccmd)
                p = re.compile(r"\.java")
                docfilename = p.sub("", language.filename)
                p = re.compile(r"[^.]*\.")
                docfilename = p.sub("", docfilename)
                docfilename = docfilename.replace(
                    "_", "__"
                )  # jostakin syystä tekee näin
                dochtml = "{}/{}/html/{}_8{}.html".format(
                    userdoc, docrnd, docfilename, language.fileext
                )
                docfile = (
                    f"{userdoc}/{docrnd}/html/{docfilename}_8{language.fileext}.html"
                )
                # print("XXXXXXXXXXXXXXXXXXXXXX", language.filename)
                # print("XXXXXXXXXXXXXXXXXXXXXX", docfilename)
                # print("XXXXXXXXXXXXXXXXXXXXXX", dochtml)
                # print("XXXXXXXXXXXXXXXXXXXXXX", docfile)
                check_output([doccmd], stderr=subprocess.STDOUT, shell=True).decode(
                    "utf-8"
                )
                if not os.path.isfile(
                    docfile
                ):  # There is maybe more files with same name and it is difficult to guess the name
                    dochtml = "{}/{}/html/{}".format(userdoc, docrnd, "files.html")
                    # print("XXXXXXXXXXXXXXXXXXXXXX", dochtml)

                web["docurl"] = dochtml
                give_points(points_rule, "doc")

            else:
                cmdline = language.get_cmdline()

            if get_param(query, "justSave", False) or get_param(
                query, "justCmd", False
            ):
                cmdline = ""

            """
            # old unsafe compile
            compiler_output = ""
            t1startrun = time.time()
            if cmdline:
                compiler_output = check_output(["cd " + language.prgpath + " && " + cmdline],
                                               stderr=subprocess.STDOUT,
                                               shell=True).decode("utf-8")
                compiler_output = compiler_output.replace(language.prgpath, "")
                if language.hide_compile_out:
                    compiler_output = ""
                give_points(points_rule, is_test + "compile")

            # self.wfile.write("*** Success!\n")
            print("*** Compile Success")
            t_run_time = time.time() - t1startrun
            times_string = "\n" + str(t_run_time)
            """

            # change old long file names to relatice paths
            language.compile_commandline = cmdline.replace(
                language.prgpath, "/home/agent"
            )

            language.prgpath = sanitize_cmdline(language.prgpath)

            if nocode and "jcomtest" not in ttype:
                print("Poistetaan ", ttype, language.sourcefilename)
                # remove(language.sourcefilename)
                # print(compiler_output)
                # TODO(?): doesn't support multi file submissions
                language.compile_commandline += (
                    " && rm "
                    + language.sourcefilename.replace(language.prgpath, "/home/agent")
                )

            # ########################## Running programs ###################################################
            # delete_tmp = False

            lang = ""
            plang = get_param(query, "lang", "")
            env = dict(os.environ)
            if plang:
                if plang.find("fi") == 0:
                    lang = "fi_FI.UTF-8"
                if plang.find("en") == 0:
                    lang = "en_US.UTF-8"

            if lang:
                env["LANG"] = lang
                env["LC_ALL"] = lang
            # print("Lang= ", lang)

            err = ""
            out = ""

            pwddir = ""

            t1startrun = time.time()

            if get_param(query, "iframehtml", False):
                rs = language.iframehtml(result, slines, points_rule)
                rjson = {"iframehtml": rs}
                sresult = json.dumps(rjson)
                if is_cache:
                    return rjson
                self.wout(sresult)
                return

            if is_doc:
                pass  # jos doc ei ajeta
            elif get_param(query, "justSave", False):
                showname = ", ".join(
                    name if name != "prg" else "" for name in language.filenames
                )
                saved_text = get_param(query, "savedText", "Saved {0}")
                code, out, err, pwddir = (0, "", saved_text.format(showname), "")
            # elif get_param(query, "justCompile", False) and ttype.find("comtest") < 0:
            #    language.just_compile = True
            # code, out, err, pwddir = (0, "".encode("utf-8"), ("Compiled " + filename).encode("utf-8"), "")
            #    code, out, err, pwddir = (0, "", ("Compiled " + language.filename), "")

            else:  # run cmd wins all other run types
                language.set_stdin(userinput)
                language.add_uploaded_files()
                runcommand = get_param(query, "cmd", "")
                if (
                    "run" not in ttype
                    and (runcommand or get_param(query, "cmds", ""))
                    and not is_test
                ):
                    # print("runcommand: ", runcommand)
                    # code, out, err, pwddir = run2([runcommand], cwd=prgpath, timeout=10, env=env, stdin=stdin,
                    #                               uargs=get_param(query, "runargs", "") + " " + userargs)
                    cmd = shlex.split(runcommand)
                    uargs = userargs
                    extra = get_param(query, "cmds", "").format(
                        language.pure_exename, uargs
                    )
                    if extra != "":
                        cmd = []
                        uargs = ""
                    # print("run: ", cmd, extra, language.pure_exename, language.sourcefilename)
                    try:
                        code, out, err, pwddir = run2_subdir(
                            cmd,
                            dir=language.rootpath,
                            cwd=language.prgpath,
                            timeout=language.timeout,
                            env=env,
                            stdin=language.stdin,
                            uargs=get_param(query, "runargs", "") + " " + uargs,
                            extra=extra,
                            no_x11=language.no_x11,
                            ulimit=language.ulimit,
                            compile_commandline=language.compile_commandline,
                            escape_pipe=get_param(query, "escapePipe", False),
                        )
                    except Exception as e:
                        print(e)
                        code, out, err = (-1, "", str(e))
                    # print("Run2: ", language.imgsource, language.pngname)
                    out, err = language.copy_image(result, code, out, err, points_rule)
                    language.save(result)
                else:  # Most languages are run from here
                    # print(query.jso.get("markup").get("byCode"))
                    code, out, err, pwddir = language.run(result, slines, points_rule)

                t_run_time = time.time() - t1startrun
                # print(out[590:650])
                # noinspection PyBroadException
                try:
                    times_string += (
                        codecs.open(
                            language.fullpath + "/run/time.txt", "r", "iso-8859-15"
                        ).read()
                        or ""
                    )
                except:  # pylint: disable=broad-except
                    pass

                if err.find("Compile error") >= 0:
                    # print("directory = " + os.curdir)
                    # error_str = "!!! Error code " + str(e.returncode) + "\n"
                    # error_str += e.output.decode("utf-8") + "\n"
                    # error_str += e.output.decode("utf-8") + "\n"
                    # errorStr = re.sub("^/tmp/.*cs\(\n", "tmp.cs(", errorStr, flags=re.M)
                    error_str = err.replace(language.prgpath, "")
                    error_str += out
                    output = io.StringIO()
                    file = codecs.open(language.sourcefilename, "r", "utf-8")
                    lines = file.read().splitlines()
                    file.close()
                    if not nocode:
                        print_lines(output, lines, 0, 10000)
                    error_str += output.getvalue()
                    output.close()
                    error_str = replace_code(query.cut_errors, error_str)

                    if language.delete_tmp:
                        removedir(language.prgpath)
                    give_points(points_rule, is_test + "notcompile")
                    return write_json_error(self.wfile, error_str, result, points_rule)

                if code == -9:
                    out = "Runtime exceeded, maybe loop forever\n" + out
                else:
                    give_points(points_rule, is_test + "compile")
                    if (
                        not err and not language.run_points_given
                    ):  # because test points are already given
                        give_points(points_rule, "run")
                    # print(code, out, err, pwddir)
                    print(err)
                    # err = err + compiler_output
                    err = language.clean_error(err)

                    # if type(out) != type(''): out = out.decode()
                    # noinspection PyBroadException
                    try:
                        if out and out[0] in [254, 255]:
                            out = out.decode("UTF16")
                            # elif type('') != type(out):
                            #    out = out.decode('utf-8-sig')
                    except:
                        # out = out.decode('iso-8859-1')
                        pass
        except Exception as e:
            print("run: ", e)
            code, out, err = (-1, "", str(e))  # .encode())

        if is_doc:
            pass  # jos doc, ei ajeta
        else:
            expect_output = get_points_rule(points_rule, is_test + "expectOutput", None)
            if expect_output:
                exout = re.compile(expect_output.rstrip("\n"), re.M)
                if exout.match(out):
                    give_points(points_rule, "output", 1)
            expect_output = get_points_rule(
                points_rule, is_test + "expectOutputPlain", None
            )
            if expect_output:
                exout = expect_output.rstrip("\n")
                if exout == out.rstrip("\n"):
                    give_points(points_rule, "output", 1)

            readpoints = get_points_rule(
                points_rule, "readpoints", language.readpoints_default
            )
            readpointskeep = get_points_rule(points_rule, "readpointskeep", False)
            if readpoints:
                try:
                    readpoints = replace_random(query, readpoints)
                    m = re.search(readpoints, out)
                    m2 = re.findall(readpoints, out)
                    if m and m.group(1):
                        p = 0
                        if not readpointskeep:
                            out = re.sub(m.group(0), "", out)
                        # noinspection PyBroadException
                        try:
                            if len(m2) == 1:  # Only one line found
                                p = float(m.group(1))
                            else:
                                err += "Pattern is more than once: " + readpoints + "\n"
                        except:
                            p = 0
                        give_points(points_rule, "output", p)
                except Exception as e:
                    msg = str(e)
                    if isinstance(e, IndexError):
                        msg = "no group ()"
                    err += "readpoints pattern error: " + msg + "\n"
                    err += "Pattern: " + readpoints + "\n"

        return_points(points_rule, result)

        delete_extra_files(extra_files, language.prgpath)
        if language.delete_tmp:
            removedir(language.prgpath)

        out = out[0 : get_param(query, "maxConsole", 20000)]
        out = text_value_replace(query, out, "outReplace", "outBy")
        err = text_value_replace(query, err, "errReplace", "errBy")

        stdout = get_param(query, "stdout", None)
        if stdout:
            outname = f"{language.filepath}/{stdout}"
            if not write_safe(outname, out):
                err += f"\nThe stdout path is not allowed: {outname}"
            else:
                stdtee = get_param(query, "stdtee", True)
                if not stdtee:
                    out = ""

        is_html = get_param(query, "isHtml", False)
        if is_html:
            out = svg_sanitize(out)
        # else:
        #    out = tim_sanitize(out)

        out, err = check_iframes(query, web, out, err, language.prgpath)

        web["console"] = out
        web["error"] = err + warnmessage
        web["pwd"] = cs_min_sanitize(pwddir.strip())
        web["language"] = language.web_data()

        t2 = time.time()
        ts = f"{(t2 - t1start):7.3f} {t_run_time:7.3f}"
        ts += times_string
        # print(ts)
        web["runtime"] = cs_min_sanitize(ts)
        if result.get("nosave", False):  # Language has decided not to save
            del result["save"]

        result["web"] = web
        # print(result)

        # Clean up
        # print("FILE NAME:", sourcefilename)
        # remove(sourcefilename)

        # self.wfile.write(out)
        # self.wfile.write(err)
        sresult = json.dumps(result)
        if is_cache:
            return result
        self.wout(sresult)
        # print("Result ========")
        # print(sresult)
        # print(out)
        # print(err)


def init_directories():
    mkdirs("/tmp/user")
    mkdirs("/tmp/tmp")


# Kun debuggaa Windowsissa, pitää vaihtaa ThreadingMixIn
# Jos ajaa Linuxissa ThreadingMixIn, niin chdir vaihtaa kaikkien hakemistoa?
# Ongelmaa korjattu siten, että kaikki run-kommennot saavat prgpathin käyttöönsä

if __debug__:
    # if True:
    class ThreadedHTTPServer(socketserver.ThreadingMixIn, http.server.HTTPServer):
        """Handle requests in a separate thread."""

    print("Debug mode/ThreadingMixIn")
else:

    class ThreadedHTTPServer(socketserver.ForkingMixIn, http.server.HTTPServer):
        """Handle requests in a separate thread."""

    print("Normal mode/ForkingMixIn")

if __name__ == "__main__":
    init_directories()
    server = ThreadedHTTPServer(("", PORT), TIMServer)
    print("Starting server, use <Ctrl-C> to stop")
    server.serve_forever()
