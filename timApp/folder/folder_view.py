from flask import request, render_template

from timApp.auth.accesshelper import verify_view_access
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import get_templates_for_folder, apply_template, create_document
from timApp.document.specialnames import FORCED_TEMPLATE_NAME
from timApp.document.viewcontext import ViewRoute
from timApp.folder.folder import Folder
from timApp.timdb.sqa import db
from timApp.util.flask.requesthelper import get_option


def try_return_folder(item_name):
    f = Folder.find_by_path(item_name, fallback_to_id=True)
    from timApp.item.routes import view, get_items

    if f is None:
        f = Folder.find_first_existing(item_name)
        templates = get_templates_for_folder(f)
        template_to_find = get_option(request, 'template', FORCED_TEMPLATE_NAME)
        template_item = None
        for t in templates:
            if t.short_name == template_to_find:
                template_item = t

        if template_item and template_item.short_name == FORCED_TEMPLATE_NAME:
            ind = item_name.rfind('/')
            if ind >= 0:
                item = create_document(item_name, item_name[ind + 1:])
                apply_template(item, template_item.path)
                db.session.commit()
                return view(item_name, ViewRoute.View)

        return render_template('create_new.html',
                               show_create_new=get_current_user_object().can_write_to_folder(f),
                               new_item=item_name,
                               found_item=f,
                               forced_template=template_to_find if template_item else None), 404
    verify_view_access(f)
    return render_template(
        'index.html',
        item=f,
        items=get_items(item_name),
    )
