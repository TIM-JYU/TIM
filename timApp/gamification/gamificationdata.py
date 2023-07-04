"""Collection of gamification functions."""
from collections import defaultdict
from operator import itemgetter, attrgetter

from sqlalchemy import select

from timApp.answer.answers import get_users_for_tasks
from timApp.auth.sessioninfo import get_current_user_id, user_context_with_logged_in
from timApp.document.docentry import DocEntry
from timApp.document.viewcontext import default_view_ctx
from timApp.document.yamlblock import YamlBlock
from timApp.plugin.plugin import find_task_ids
from timApp.timdb.sqa import db


def gamify(initial_data: YamlBlock):
    # Find document IDs from json, and place them in their appropriate arrays(lectures and demos separately)
    lecture_table, demo_table, button_text = get_doc_data(initial_data)

    # Insert document, IDs, paths, and points in a dictionary
    gamification_data = {
        "lectures": lecture_table,
        "demos": demo_table,
        "buttonText": button_text,
    }

    return gamification_data


def get_doc_data(gamify_data: YamlBlock):
    """
    Parses json to find and link appropriate data into lecture and demo documents.

    :param gamify_data = Checked documents in JSON
    :returns: Arrays of lecture and demo documents

    """
    lecture_paths = gamify_data.get("lectures", [])
    demo_paths = gamify_data.get("demos", [])
    default_max = gamify_data.get("defaultMax", 5)
    button_text = gamify_data.get("buttonText", "Show map")

    lectures = get_lecture_data(lecture_paths)
    demos = get_demo_data(demo_paths, default_max)

    return lectures, demos, button_text


def get_lecture_data(lecture_paths):
    """
    Configure data of all lecture documents.

    :param lecture_paths: Lecture path dicts.
    :return: List of lecture data dicts.
    """
    results = []
    lectures, docs = get_sorted_lists(lecture_paths, "lecture")
    # TODO: There was a starts with "http" check, but is it necessary here?
    for doc, lecture in zip(docs, lectures):
        # If short_name has been input, use it. Otherwise get document attribute short name.
        short_name = lecture.get("short_name")
        if short_name is None:
            short_name = doc.short_name
        temp_dict = dict(id=doc.id, name=short_name, link=doc.url)
        results.append(temp_dict)
    return results


def get_sorted_lists(items, item_name: str):
    """
    Fetches document list from a dictionary list and sorts it and the dictionary list.
    Checks also whether every path was founf from database and raises error accordingly.

    :param items: Dictionary list with 'path' keys.
    :param item_name: Name of the item type (demo, lecture, etc.).
    :return: Documents and original items as lists sorted by paths.
    """
    item_path_list = []
    filtered_items = []

    # Separate paths to be used for database fetch and filter None values.
    for item in items:
        path = item["path"]
        # Leave out demos with None paths.
        if path is not None:
            item_path_list.append(path)
            filtered_items.append(item)
        # Sort both so they can be looped simultaneusly without value mismatches.
    docs = sorted(
        db.session.execute(select(DocEntry).filter(DocEntry.name.in_(item_path_list))).scalars().all(),
        key=attrgetter("path"),
    )
    items = sorted(filtered_items, key=itemgetter("path"))

    # If demos don't match the documents fetched from their paths, there's an error.
    invalid_paths = {d["path"] for d in items} - {d.path for d in docs}
    if invalid_paths:
        raise GamificationException(
            f"Failed to fetch following {item_name} document(s): {invalid_paths}"
        )
    return items, docs


def get_demo_data(demo_paths, default_max):
    """
    Configure data of all demos.

    :param demo_paths: Demo path dicts.
    :param default_max: default maximum points.
    :return: List of demo data dicts.
    """
    results = []
    task_id_list = []

    demos, docs = get_sorted_lists(demo_paths, "demo")

    for doc in docs:
        task_id_list += (
            find_task_ids(
                doc.document.get_paragraphs(),
                default_view_ctx,
                user_context_with_logged_in(None),
            )
        )[0]
    task_info_list = get_users_for_tasks(
        task_id_list, [get_current_user_id()], group_by_doc=True
    )
    task_info_dict = defaultdict(float)
    for task in task_info_list:
        pts = task.get("total_points", 0.0)
        if pts is not None:
            task_info_dict[task.get("doc_id")] += pts

    for doc, demo in zip(docs, demos):
        doc_max_points = demo.get("max_points")
        short_name = demo.get("short_name")
        if short_name is None:
            short_name = doc.short_name
        temp_dict = dict(id=doc.id, name=short_name, link=doc.url)
        if doc_max_points is None:
            doc_set = doc.document.get_settings()
            doc_max_points = doc_set.max_points() or default_max
        if doc_max_points is not None:
            temp_dict["maxPoints"] = doc_max_points
        temp_dict["gotPoints"] = task_info_dict[str(doc.id)]
        results.append(temp_dict)
    return results


class GamificationException(Exception):
    """The exception that is thrown when an error occurs during a gamification check."""

    pass
