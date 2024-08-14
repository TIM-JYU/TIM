from flask import Response, redirect

from timApp.document.docentry import DocEntry
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.typedblueprint import TypedBlueprint
# from timApp.printing.print import get_printed_document, pull_doc_path

redirect_route = TypedBlueprint("redirect", __name__, url_prefix="")


@redirect_route.get("/r/<alias>")
def redirect_by_alias_file(
    alias: str
) -> Response:
    # Seuraavasta tulee max retries:
    # hostname = current_app.config["TIM_HOST"]
    # res = requests.get(f"http://{hostname}/print/redirect/{alias}")
    # res = requests.get("http://localhost/print/redirect/ohj1")

    # seuraava ei oikein toimi, mistä saisi itse tuloksen?
    # pull_doc_path('print.get_printed_document', {'doc_path': f'redirect/{alias}'})
    # res = get_printed_document(f"redirect/{alias}")

    # Seuraavalla toimii, mutta makroja ei suoriteta
    # mutta jostakin syystä niitä ei suoriteta edes print-reitissä?
    doc_entry = DocEntry.find_by_path(f"redirect/{alias}")
    if not doc_entry:
        raise RouteException(f"Alias '{alias}' does not exist!")

    res = doc_entry.document.export_markdown(export_ids=False, export_settings=False).strip("\n")

    return redirect(res)

    # return json_response({"data": res, "status_code": 200})
