# Collection of gamification functions
import json

import yaml
from flask import request

from timApp.dbaccess import get_timdb
import timApp.pluginControl
from timApp.sessioninfo import get_current_user_id
from timApp.timdb.models.docentry import DocEntry


def gamify(initial_data):

    # Convert initial data to JSON format
    initial_json = convert_to_json(initial_data)

    # Find document IDs from json, and place them in their appropriate arrays(lectures and demos separately)
    lecture_table, demo_table = get_doc_data(initial_json)

    # Insert document, IDs, paths, and points in a dictionary
    gamification_data = {"lectures": lecture_table, "demos": demo_table}

    return gamification_data


def convert_to_json(md_data):
    """Converts the YAML paragraph in gamification document to JSON.

    :param md_data = the data read from paragraph in YAML
    :returns: same data in JSON

    """
    temp = yaml.load(md_data[3:len(md_data) - 3])  # TODO: does not work if used other separtor than 3 `
    return json.loads(json.dumps(temp))


def get_doc_data(json_to_check):
    """Parses json to find and link appropriate data into lecture and demo documents.

    :param json_to_check = Checked documents in JSON
    :returns: Arrays of lecture and demo documents

    """
    if json_to_check is None:
        raise GamificationException('JSON is None')

    lecture_paths = json_to_check.get('lectures', [])
    demo_paths = json_to_check.get('demos', [])
    default_max = json_to_check.get('defaultMax', 5)

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

    # Configure data of demo documents
    demos = []
    for path in demo_paths:
        demo = DocEntry.find_by_path(path['path'])

        if demo is not None:
            doc = path.get('shortname', demo.short_name)
            doc_max_points = path.get('max_points', None)
            temp_dict = dict()
            temp_dict['id'] = demo.id
            temp_dict['name'] = doc
            temp_dict['link'] = request.url_root + 'view/' + demo.path
            if doc_max_points is None:
                doc_set = demo.document.get_settings()
                doc_max_points = doc_set.max_points() or default_max
            if doc_max_points is not None:
                temp_dict['maxPoints'] = doc_max_points
            temp_dict['gotPoints'] = get_points_for_doc(demo)  # TODO: must pick all docs at once
            demos.append(temp_dict)

    return lectures, demos


def get_points_for_doc(d):
    """Finds the current users point information for a specific document.

    :param d The document as a DocEntry
    :returns: The current users points for the document

    """
    document = d.document
    timdb = get_timdb()
    user_points = 0
    task_id_list = (timApp.pluginControl.find_task_ids(document.get_paragraphs()))

    users_task_info = timdb.answers.get_users_for_tasks(task_id_list[0], [get_current_user_id()])

    for entrys in users_task_info:
        if entrys['total_points'] is not None:
            user_points += (entrys['total_points'])

    return user_points


class GamificationException(Exception):
    """The exception that is thrown when an error occurs during a gamification check."""
    pass
