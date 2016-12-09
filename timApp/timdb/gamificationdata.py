# Collection of gamification functions
from timdb.models.docentry import DocEntry
import yaml
import json
from flask import request
import pluginControl
from routes.dbaccess import get_timdb
from routes import common


def gamify(initial_data):

    # Convert initial data to JSON format
    initial_json = convert_to_json(initial_data)

    # Find document IDs from json, and place them in their appropriate arrays(lectures and demos separately)
    lecture_table, demo_table = get_doc_data(initial_json)

    # Insert document, IDs, paths, and points in a dictionary
    gamification_data = place_in_dict(lecture_table, demo_table)

    return gamification_data


def convert_to_json(md_data):
    """Converts the YAML paragraph in gamification document to JSON
    :param md_data = the data read from paragraph in YAML
    :returns: same data in JSON
    """
    temp = yaml.load(md_data[3:len(md_data) - 3])
    return json.loads(json.dumps(temp))


def get_doc_data(json_to_check):
    """Parses json to find and link appropriate data into lecture and demo documents.
    :param json_to_check = Checked documents in JSON
    :returns: Arrays of lecture and demo documents
    """
    if json_to_check is None:
        raise GamificationException('JSON is None')

    lecture_paths = json_to_check['lectures']
    demo_paths = json_to_check['demos']

    # Configure data of lecture documents
    lectures = []
    for path in lecture_paths:
        lecture = DocEntry.find_by_path(path['path'])
        if lecture is not None:
            temp_dict = dict()
            temp_dict['id'] = lecture.id
            temp_dict['name'] = lecture.short_name
            temp_dict['link'] = request.url_root+'view/' + lecture.path
            lectures.append(temp_dict)

    # Configure data of demo documents
    demos = []
    for path in demo_paths:
        demo = DocEntry.find_by_path(path['path'])

        if demo is not None:
            doc_set = demo.document.get_settings()
            temp_dict = dict()
            temp_dict['id'] = demo.id
            temp_dict['name'] = demo.short_name
            temp_dict['link'] = request.url_root+'view/' + demo.path
            doc_max_points = doc_set.max_points()
            if doc_max_points is None:
                temp_dict['maxPoints'] = 0
            else:
                temp_dict['maxPoints'] = doc_max_points
            temp_dict['gotPoints'] = get_points_for_doc(demo)
            demos.append(temp_dict)

    return lectures, demos


def place_in_dict(l_table, d_table):
    """
    :param l_table Array of lecture IDs
    :param d_table Array of demo IDs
    :returns: A dictionary of combined lecture and demo documents
    """
    document_dict = {'lectures': [], 'demos': []}

    temp1 = document_dict['lectures']
    for i in range(len(l_table)):
        temp1.append(l_table[i])

    temp2 = document_dict['demos']
    for j in range(len(d_table)):
        temp2.append(d_table[j])

    document_dict['lectures']=temp1
    document_dict['demos'] = temp2

    return json.dumps(document_dict)


def get_points_for_doc(d):
    """
    Finds the current users point information for a specific document
    :param d The document as a DocEntry
    :returns: The current users points for the document
    """
    document = d.document
    timdb = get_timdb()
    user_points = 0
    task_id_list = (pluginControl.find_task_ids(document.get_paragraphs()))
    users_task_info = timdb.answers.get_users_for_tasks(task_id_list[0], [common.get_current_user_id()])

    for entrys in users_task_info:
        if users_task_info is not None:
            user_points += (users_task_info[0]['total_points'])

    return user_points


class GamificationException(Exception):
    """The exception that is thrown when an error occurs during a gamification check."""
    pass
