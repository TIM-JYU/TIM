from flask import request, render_template

from timApp.auth.accesshelper import verify_view_access, AccessDenied
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import (
    get_templates_for_folder,
    apply_template,
    create_document,
)
from timApp.document.specialnames import FORCED_TEMPLATE_NAME
from timApp.document.viewcontext import ViewRoute
from timApp.folder.folder import Folder
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import grant_access
from timApp.util.flask.requesthelper import get_option


def try_return_folder(item_name):
    f = Folder.find_by_path(item_name, fallback_to_id=True)
    from timApp.item.routes import view, get_items

    if f is None:
        f = Folder.find_first_existing(item_name)
        templates = get_templates_for_folder(f)
        template_to_find = get_option(request, "template", FORCED_TEMPLATE_NAME)
        template_item = None
        for t in templates:
            if t.short_name == template_to_find:
                template_item = t

        force_create = get_option(request, "force_create", False) or (
            template_item and template_item.short_name == FORCED_TEMPLATE_NAME
        )
        create_public = get_option(request, "create_public", False)

        if force_create:
            ind = item_name.rfind("/")
            if ind >= 0:
                check_username = get_option(request, "check_username", None)
                if check_username and get_current_user_object().name != check_username:
                    raise AccessDenied(
                        "This document is reserved for another user. Create a document for your username."
                    )
                title = get_option(request, "title", item_name[ind + 1 :])
                item = create_document(item_name, title)
                if template_item:
                    apply_template(item, template_item.short_name)
                if create_public:
                    db.session.flush()
                    grant_access(UserGroup.get_anonymous_group(), item, AccessType.view)
                db.session.commit()
                return view(item_name, ViewRoute.View)

        return (
            render_template(
                "create_new.jinja2",
                show_create_new=get_current_user_object().can_write_to_folder(f),
                new_item=item_name,
                found_item=f,
                forced_template=template_to_find if template_item else None,
            ),
            404,
        )
    verify_view_access(f)
    return render_template(
        "index.jinja2",
        item=f,
        items=get_items(item_name),
    )
