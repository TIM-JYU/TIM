from flask import Response, redirect, current_app
# import requests
import re
from timApp.auth.sessioninfo import user_context_with_logged_in_or_anon
from timApp.document.docentry import DocEntry
from timApp.document.viewcontext import default_view_ctx
from timApp.markdown.markdownconverter import expand_macros_info
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.typedblueprint import TypedBlueprint
# from timApp.printing.print import get_printed_document, pull_doc_path

redirect_route = TypedBlueprint("redirect", __name__, url_prefix="")


@redirect_route.get("/r/<alias>")
def redirect_by_alias_file(
    alias: str
) -> Response:
    # Seuraavasta tulee max retries vaikka manuaalisesti olisi
    # suora linkki.  localhostin ulkopuolelle toimii
    hostname = current_app.config["TIM_HOST"]
    # res = requests.get(f"{hostname}/print/redirect/{alias}")
    # res = requests.get("http://localhost/print/redirect/ohj1")
    # res = requests.get("http://users.jyu.fi/~vesal/")

    # seuraava ei oikein toimi, mistä saisi itse tuloksen?
    # pull_doc_path('print.get_printed_document', {'doc_path': f'redirect/{alias}'})
    # res = get_printed_document(f"redirect/{alias}")

    doc_entry = DocEntry.find_by_path(f"redirect/{alias}")
    if not doc_entry:
        raise RouteException(f"Alias '{alias}' does not exist!")

    res = doc_entry.document.export_markdown(export_ids=False, export_settings=False).strip("\n")

    # Hakee käyttäjäkontekstin (mm. käyttäjäkohtaiset makrot)
    user_ctx = user_context_with_logged_in_or_anon()
    # Haetaan makroasetukset dokumentille
    settings = doc_entry.document.get_settings()
    macro_info = settings.get_macroinfo(default_view_ctx, user_ctx)
    # Lasketaan makrot auki
    res = expand_macros_info(res, macro_info, ignore_errors=True).strip("\n").strip(" ")
    alias_doc_name = f"{hostname}/view/redirect/{alias}"

    if res.find("\n") >= 0:
        # should make new errortypes to get better html with link
        raise RouteException(f'Alias {alias_doc_name}-file should include only one line! Now:\n\n{res}')

    match = re.match("^(http)|(/).*$", res)
    if not match:
        raise RouteException(f'Alias {alias_doc_name}-file should start with http or /! Now:\n\n{res}')

    return redirect(res)

    # return json_response({"data": res, "status_code": 200})
