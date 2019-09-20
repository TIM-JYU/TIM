"""
Routes related to handling faculty council documents, such as meeting invitations and minutes
"""
import ast
from pathlib import Path

from flask import Blueprint, send_file, request
from flask import abort

import timApp.plugin
import timApp.util

from timApp.auth.accesshelper import verify_manage_access, verify_edit_access
from timApp.document.create_item import create_or_copy_item, create_document
from timApp.document.docsettings import DocSettings
from timApp.item.block import BlockType
from timApp.util.flask.requesthelper import verify_json_params
from timApp.util.flask.responsehelper import safe_redirect, json_response
from timApp.document.docentry import DocEntry
from timApp.timdb.sqa import db
import timApp.util.pdftools
import timApp.plugin.plugin

minutes_blueprint = Blueprint('minutes',
                              __name__,
                              url_prefix='/minutes')


@minutes_blueprint.route('/createMinuteExtracts/<path:doc>')
def create_minute_extracts(doc):
    """
    A route for creating extracts of faculty council minutes.
    :param doc:
    :return:
    """
    d = DocEntry.find_by_path(doc)
    if not d:
        abort(404)
    verify_manage_access(d)

    # figure out the index of the minute, get the value of the 'knro' macro

    macros = d.document.get_settings().get_macroinfo().get_macros()
    minute_number = macros.get("knro")
    if not minute_number:
        return abort(400, "Error creating extracts: the document is not a minute document (no 'knro' macro found)")

    if not isinstance(minute_number, int):
        return abort(400, "Error creating extracts: the value of the 'knro' macro is not a valid integer")

    paragraphs = d.document.get_paragraphs()

    extract_dict = dict()
    current_paragraphs = []
    current_extract_index = -1
    current_extract_title = ""

    # we build a dict of the extracts first before creating any new files
    # for each extract, we need its list index / extract index, related paragraphs, and its title

    # we detect an extract by searching the document's non-expanded markdown for the extract macro
    # all paragraphs between two such macros belong to the same extract
    # the rest of the document after the last occurence of the macro belong to the last extract

    markdown_to_find = "%%lista("

    for par in paragraphs:

        if par.is_setting():
            continue

        markdown = par.get_markdown()
        macro_position = markdown.find(markdown_to_find)

        if macro_position > -1:
            # if the extract macro exists in the current paragraph, figure out the extract index from the markdown
            # if we can't parse the extract index, just ignore the paragraph
            comma_position = markdown.find(",", macro_position + len(markdown_to_find))
            if comma_position == -1:
                continue
                # sometimes there's intentionally a macro without the extract index
                # abort(400, f"Failed to parse extract index from macro, from paragraph: \n{markdown}")

            new_extract_index = 0
            try:
                new_extract_index = int(ast.literal_eval(markdown[macro_position + len(markdown_to_find):comma_position]))
            except ValueError:
                abort(400, f"Failed to parse extract index from macro, from paragraph: \n{markdown}")

            if current_extract_index > -1:
                # if we were in another extract's paragraph before, save the previous extract's paragraphs into the dict
                # don't allow duplicate extract numbers
                if current_extract_index in extract_dict:
                    return abort(400, f"Error creating extracts: the same extract entry ({current_extract_index}) " +
                                 "cannot exist multiple times in the document.")
                extract_dict[current_extract_index] = (current_extract_title, current_paragraphs)
                current_extract_title = ""
                current_paragraphs = []

            current_extract_index = new_extract_index

        if current_extract_index > -1:
            # if the macro doesn't exist in the current paragraph but it existed in some previous paragraph,
            # we're in a paragraph related to an extract -> add the paragraph to the extract's list of paragraphs
            current_paragraphs.append(par)

            # find the extract's title from after the first number sign (#) that we find
            if not current_extract_title:
                title_search_string = "# "
                number_sign_position = markdown.find(title_search_string)
                if number_sign_position > -1:
                    current_extract_title = markdown[number_sign_position + len(title_search_string):]
                    # if there's other content in the same paragraph than just the title, cut the other content out
                    linebreak_position = current_extract_title.find("\n")
                    if linebreak_position > -1:
                        current_extract_title = current_extract_title[:linebreak_position]

    # after the loop has ended, check if we're still within an extract
    # if so, add the last extract to the dict
    if current_extract_index > -1:
        if current_extract_index in extract_dict:
            return abort(400, f"Error creating extracts: the same extract entry ({current_extract_index}) cannot " +
                         "exist multiple times in the document.")
        extract_dict[current_extract_index] = (current_extract_title, current_paragraphs)

    if not extract_dict:
        return abort(400, "The document has no extract macros!")

    base_path = f"{d.location}/otteet/kokous{minute_number}/"

    # create the composite document that has links to all the extract documents
    composite_docentry = create_or_get_and_wipe_document(f"{base_path}kokous{minute_number}", f"kokous{minute_number}")
    composite_docentry.document.add_paragraph("## Pöytäkirjan asiakohtien otteet",
                                              attrs=dict([("area", f"kokous{minute_number}")]))

    composite_paragraph = composite_docentry.document.add_paragraph("")

    # loop through the extracts and create new documents for them
    for extract_number, (extract_title, paragraphs) in extract_dict.items():
        docentry = create_or_get_and_wipe_document(f"{base_path}lista{extract_number}", f"lista{extract_number}")

        #  Next must be add_text to avoid first coming as text and then changing to different paragraph
        #  and #- must be on the column 1!
        docentry.document.add_text(f"""
PÖYTÄKIRJANOTE - Lista {extract_number} -  {extract_title}      
\        
#- {{rd="{d.id}" ra="ETUSIVU"}}
           """)
        for par in paragraphs:
            docentry.document.add_paragraph_obj(par.create_reference(docentry.document, add_rd=True))
        docentry.document.add_paragraph("%%ALLEKIRJOITUKSET%%")
        docentry.update_last_modified()

        # add into the composite document a link leading to the new extract document
        composite_paragraph.set_markdown(f"{composite_paragraph.get_markdown()}\n" +
                                         f"- [Lista {extract_number}](lista{extract_number}), " +
                                         f"([PDF](/print/{docentry.path_without_lang})) - {extract_title}")

    composite_paragraph.save()
    composite_docentry.document.add_paragraph("", attrs=dict([("area_end", f"kokous{minute_number}")]))
    composite_docentry.update_last_modified()
    db.session.commit()
    return safe_redirect(f"/view/{composite_docentry.path_without_lang}")


@minutes_blueprint.route("/createMinutes", methods=["POST"])
def create_minutes_route():
    """
    Creates a base document for minutes from an IT faculty council meeting invitation.
    :return: A web response for the new document.
    """

    item_path, item_title, copy_id = verify_json_params('item_path', 'item_title', 'copy')

    d = DocEntry.find_by_id(copy_id)
    if not d:
        abort(404)

    verify_manage_access(d)

    item = create_or_copy_item(
        item_path,
        BlockType.Document,
        item_title,
        copy_id=copy_id,
        use_template=False,
        copy_uploads=False,
    )
    item.document.add_setting(DocSettings.memo_minutes_key, "minutes")
    db.session.commit()
    return json_response(item)


def create_or_get_and_wipe_document(path: str, title: str):
    """ Creates a document to the given path and returns the DocEntry.
    If a document already exists in the given path, the already existing document is wiped clean and then its DocEntry
    is returned.
    :param path: The path to the document.
    :param title: The title of the document.
    :return: The DocEntry of a new document or an already existing document that has been wiped clean.
    """
    d = DocEntry.find_by_path(path)

    if not d:
        return create_document(path, title)
    else:
        d.title = title  # update title of existing document

    d.document.update("", d.document.export_markdown())

    return d


@minutes_blueprint.route('/checkAttachments/<path:doc>', methods=['GET'])
def get_attachment_list(doc):
    """
    Gets the list of valid attachments in the document and whether there was any invalid ones.
    :param doc:
    :return: List of valid attachments and whether the list is incomplete.
    """
    try:
        d = DocEntry.find_by_path(doc)
        if not d:
            abort(404)
        verify_edit_access(d)

        paragraphs = d.document.get_paragraphs(d)
        attachments = timApp.util.pdftools.get_attachments_from_pars(paragraphs)

    except Exception as err:
        message = str(err)
        abort(404, message)
    else:
        return json_response(attachments)


@minutes_blueprint.route('/mergeAttachments/<path:doc>', methods=['GET'])
def merge_attachments(doc):
    """
    A route for merging all the attachments in a document.
    :param doc: Document path.
    :return: Merged pdf-file.
    """
    try:
        d = DocEntry.find_by_path(doc)
        if not d:
            abort(404)
        verify_edit_access(d)
        # TODO: Partial merging: add an attachment list to the route.
        # TODO: Route params for adding/removing perusliite.
        macro_list = ["%%liite"]
        paragraphs = d.document.get_paragraphs(d)

        pdf_paths, errors = timApp.util.pdftools.get_attachments_from_paragraphs(paragraphs, macro_list)
        # Uses document name as the base for the merged file name and tmp as folder.
        doc_name = Path(doc).name
        merged_pdf_path = Path(timApp.util.pdftools.temp_folder_default_path) / f"{doc_name}_merged.pdf"

        timApp.util.pdftools.merge_pdfs(pdf_paths, merged_pdf_path)

    except Exception as err:
        message = str(err)
        abort(404, message)
    else:
        return send_file(merged_pdf_path.absolute().as_posix(), mimetype="application/pdf")


@minutes_blueprint.route('/mergeAttachments/<path:doc>', methods=['POST'])
def get_attachments(doc):
    """
    A route for getting merged document.
    :param doc: document path
    :return: merged pdf-file
    """
    try:
        d = DocEntry.find_by_path(doc)
        if not d:
            abort(404)
        verify_edit_access(d)

    except Exception as err:
        message = str(err)
        abort(404, message)
    else:
        merge_access_url = request.url
        return json_response({'success': True, 'url': merge_access_url}, status_code=201)
