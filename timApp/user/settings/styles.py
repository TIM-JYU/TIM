from flask import Response

from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import get_documents, DocEntry
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

styles = TypedBlueprint("styles", __name__, url_prefix="/styles")

OFFICIAL_STYLES_PATH = "styles/official"
USER_STYLES_PATH = "styles/user"


@styles.get("")
def get_styles() -> Response:
    candidates = get_documents(
        filter_user=get_current_user_object(),
        custom_filter=(
            DocEntry.name.like(f"{OFFICIAL_STYLES_PATH}%")
            | DocEntry.name.like(f"{USER_STYLES_PATH}%")
        ),
    )

    result = []
    for doc in candidates:
        style_description = doc.document.get_settings().get("description", None)
        if not style_description:
            continue
        result.append(
            {"name": doc.title, "path": doc.name, "description": style_description}
        )

    return json_response(result)
