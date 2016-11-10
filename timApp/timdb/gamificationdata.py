"Collection of gamification functions"
from timdb.models.docentry import DocEntry
import yaml
import json
from flask import request


def gamify(initial_data):

    # Convert initial data to JSON format
    initial_json = convert_to_json(initial_data)

    # Find document IDs from json, and place them in their appropriate arrays(lectures and demos separately)
    lecture_table, demo_table = get_doc_ids(initial_json)

    # Insert document, IDs, paths, and points in a dictionary
    place_in_dict(lecture_table, demo_table)
    # get_demo_maxpoints(demo_name)


def convert_to_json(md_data):
    """Converts the YAML paragraph in gamification document to JSON
    :param md_data = the data read from paragraph in YAML
    :returns: same data in JSON
    """
    temp = yaml.load(md_data[3:len(md_data) - 3])
    return json.loads(json.dumps(temp))


def get_doc_ids(json_to_check):
    """Parses json to find names of lecture and demo documents
    :param json_to_check = Checked documents in JSON
    :returns:
    """
    if json_to_check is None:
        raise GamificationException('JSON is None')

    lecture_paths = json_to_check['lectures']
    demo_paths = json_to_check['demos']

    lectures = []
    for path in lecture_paths:
        lecture = DocEntry.find_by_path(path['path'])
        if lecture is not None:
            temp_dict1 = {}
            temp_dict1['id']   = lecture.id
            temp_dict1['path'] = lecture.get_short_name()
            temp_dict1['url']  = request.url_root+'/view/'+ lecture.get_path()
            lectures.append(temp_dict1)

    demos = []
    for path in demo_paths:
        demo = DocEntry.find_by_path(path['path'])
        if demo is not None:
            temp_dict2 = {}
            temp_dict2['id']   = demo.id
            temp_dict2['path'] = demo.get_short_name()
            temp_dict2['url']  = request.url_root+'/view/'+ demo.get_path()
            demos.append(temp_dict2)

    return lectures, demos


def place_in_dict(l_table, d_table):
    """
    :param l_table Array of lecture IDs
    :param d_table Array of demo IDs
    :returns: maxPoints of the demo document
    """
    document_dict = {'lectures': [], 'demos': []}

    temp1 = document_dict['lectures']
    for i in range(len(l_table)):
        temp1.append(l_table[i])

    temp2 = document_dict['demos']
    for j in range(len(d_table)):
        temp2.append(d_table[j])

    print(document_dict)
    return


class GamificationException(Exception):
    """The exception that is thrown when an error occurs during a gamification check."""
    pass

