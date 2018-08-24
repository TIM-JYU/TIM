"""Collection of gamification functions."""
from collections import defaultdict
from operator import itemgetter, attrgetter

from flask import request

import timApp.plugin.pluginControl
from timApp.auth.sessioninfo import get_current_user_id
from timApp.document.docentry import DocEntry
from timApp.document.yamlblock import YamlBlock
from timApp.timdb.dbaccess import get_timdb


def gamify(initial_data: YamlBlock):
    # Find document IDs from json, and place them in their appropriate arrays(lectures and demos separately)
    lecture_table, demo_table, button_text = get_doc_data(initial_data)

    # Insert document, IDs, paths, and points in a dictionary
    gamification_data = {"lectures": lecture_table, "demos": demo_table, "buttonText": button_text}

    return gamification_data


def get_doc_data(gamify_data: YamlBlock):
    """Parses json to find and link appropriate data into lecture and demo documents.

    :param gamify_data = Checked documents in JSON
    :returns: Arrays of lecture and demo documents

    """
    lecture_paths = gamify_data.get('lectures', [])
    demo_paths = gamify_data.get('demos', [])
    default_max = gamify_data.get('defaultMax', 5)
    button_text = gamify_data.get('buttonText', "Show map")

    # Configure data of lecture documents
    lectures = []
    for path in lecture_paths:
        p = path.get('path', None)
        if not p:
            continue
        if p.startswith('http'):
            doc = path.get('shortname', 'doc')
            temp_dict = dict()
            temp_dict['id'] = 0
            temp_dict['name'] = doc
            temp_dict['link'] = p
            lectures.append(temp_dict)
        else:
            lecture = DocEntry.find_by_path(path['path'])
            if lecture is not None:
                doc = path.get('shortname', lecture.short_name)
                temp_dict = dict()
                temp_dict['id'] = lecture.id
                temp_dict['name'] = doc
                temp_dict['link'] = request.url_root + 'view/' + lecture.path
                lectures.append(temp_dict)

    demos = get_demo_data(demo_paths, default_max)

    return lectures, demos, button_text


def get_demo_data(demos, default_max):
    """
    Forms point data for all demos.
    :param demos: Demo path dicts.
    :param default_max: default maximum points.
    :return: List of demo data dicts.
    """
    demo_path_list = []
    filtered_demos = []
    results = []
    task_id_list = []

    for demo in demos:
        path = demo['path']
        # Leave out demos with None paths.
        if path is not None:
            demo_path_list.append(path)
            filtered_demos.append(demo)
    docs = sorted(DocEntry.query.filter(DocEntry.name.in_(demo_path_list)).all(), key=attrgetter('path'))
    demos = sorted(filtered_demos, key=itemgetter('path'))

    # If demos don't match the documents fetched from their paths, there's an error.
    invalid_paths = set(d['path'] for d in demos) - set(d.path for d in docs)
    if invalid_paths:
        raise GamificationException(f"Failed to fetch following demo document(s): {invalid_paths}")

    for doc in docs:
        task_id_list += (timApp.plugin.pluginControl.find_task_ids(doc.document.get_paragraphs()))[0]
    task_info_list = get_timdb().answers.get_users_for_tasks(task_id_list, [get_current_user_id()], group_by_doc=True)
    task_info_dict = defaultdict(float)
    for task in task_info_list:
        task_info_dict[task.get('doc_id')] += float(task.get('total_points'))
    for doc, demo in zip(docs, demos):
        doc_max_points = demo.get('max_points')
        temp_dict = dict(id=doc.id, name=doc.short_name, link=doc.url)
        if doc_max_points is None:
            doc_set = doc.document.get_settings()
            doc_max_points = doc_set.max_points() or default_max
        if doc_max_points is not None:
            temp_dict['maxPoints'] = doc_max_points
        temp_dict['gotPoints'] = task_info_dict[str(doc.id)]
        results.append(temp_dict)
    return results


class GamificationException(Exception):
    """The exception that is thrown when an error occurs during a gamification check."""
    pass
