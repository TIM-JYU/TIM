"""Collection of gamification functions."""
from typing import List, Union, Tuple, Optional, Any, Dict

from flask import request

import timApp.plugin.pluginControl
from timApp.timdb.dbaccess import get_timdb
from timApp.document.yamlblock import YamlBlock
from timApp.auth.sessioninfo import get_current_user_id
from timApp.document.docinfo import DocInfo
from timApp.document.docentry import DocEntry


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

    # Configure data of demo documents
    # demos = []
    # for path in demo_paths:
    #     demo = DocEntry.find_by_path(path['path'])
    #
    #     if demo is not None:
    #         doc = path.get('shortname', demo.short_name)
    #         doc_max_points = path.get('max_points', None)
    #         temp_dict = dict()
    #         temp_dict['id'] = demo.id
    #         temp_dict['name'] = doc
    #         temp_dict['link'] = request.url_root + 'view/' + demo.path
    #         if doc_max_points is None:
    #             doc_set = demo.document.get_settings()
    #             doc_max_points = doc_set.max_points() or default_max
    #         if doc_max_points is not None:
    #             temp_dict['maxPoints'] = doc_max_points
    #         temp_dict['gotPoints'] = get_points_for_doc(demo)  # TODO: must pick all docs at once
    #         demos.append(temp_dict)

    return lectures, demos, button_text


def get_points(doc_id, task_info_list):
    """
    Get total points for one document from a list of all tasks.
    :param doc_id: Demo document id.
    :param task_info_list: All tasks including their document ids.
    :return: Total points for the demo document.
    """
    user_points = 0
    for entrys in task_info_list:
        try:
            if str(entrys['doc_id']) == str(doc_id):
                user_points += (entrys['total_points'])
        except KeyError:
            continue
    return user_points


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
        if path is not None:
            demo_path_list.append(path)
            filtered_demos.append(demo)
    demos = filtered_demos   # Leave out demos with None paths.
    docs = DocEntry.query.filter(DocEntry.name.in_(demo_path_list)).all()

    for doc in docs:
        task_id_list += (timApp.plugin.pluginControl.find_task_ids(doc.document.get_paragraphs()))[0]
    task_info_list = get_timdb().answers.get_users_for_tasks(task_id_list, [get_current_user_id()], group_by_doc=True)

    for doc, demo in zip(docs, demos):
        doc_max_points = demo.get('max_points', None)
        temp_dict = dict()
        temp_dict['id'] = doc.id
        temp_dict['name'] = doc.short_name
        temp_dict['link'] = request.url_root + 'view/' + doc.path
        if doc_max_points is None:
            doc_set = doc.document.get_settings()
            doc_max_points = doc_set.max_points() or default_max
        if doc_max_points is not None:
            temp_dict['maxPoints'] = doc_max_points
        temp_dict['gotPoints'] = get_points(doc.id, task_info_list)
        results.append(temp_dict)
    return results


def get_points_for_doc(d: DocInfo):
    """Finds the current users point information for a specific document.

    :param d: The document.
    :returns: The current users points for the document

    """
    document = d.document
    timdb = get_timdb()
    user_points = 0
    task_id_list = (timApp.plugin.pluginControl.find_task_ids(document.get_paragraphs()))

    users_task_info = timdb.answers.get_users_for_tasks(task_id_list[0], [get_current_user_id()])

    for entrys in users_task_info:
        if entrys['total_points'] is not None:
            user_points += (entrys['total_points'])

    return user_points


class GamificationException(Exception):
    """The exception that is thrown when an error occurs during a gamification check."""
    pass
