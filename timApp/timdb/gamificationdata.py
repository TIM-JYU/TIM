"Collection of gamification functions"
from timdb.models.docentry import DocEntry
import yaml
import json


def gamify(initial_data):
    initial_json = convert_to_json(initial_data)
    checked_json = check_for_duplicate_docs(initial_json)
    return
    parse_json(checked_json)
    get_doc_name()
    get_maximum_points()
    get_url()


def convert_to_json(md_data):
    """Converts the YAML paragraph in gamification document to JSON
    :param md_data = the data read from paragraph in YAML
    :returns: same data in JSON
    """
    temp = yaml.load(md_data[3:len(md_data) - 3])
    return json.loads(json.dumps(temp))


def check_for_duplicate_docs(json_to_check):
    """Checks that all documents in json are unique
    :param json = Checked documents in JSON
    :returns: same data in JSON
    """
    is_valid = True
    if json_to_check is None:
        raise GamificationException('JSON is None')

    paths = json_to_check['lectures']

    for path in paths:
        print(path['path'].split('/')[-1])


    return is_valid


def parse_json(initial_json):
    """Parses JSON for document ids, document points and user points
    :param initial_json = the JSON to be parsed
    :returns: Updated JSON, ready for map generator
    """
    lectures = initial_json[1]['lectures']
    index = 0
    for lecture in lectures:
        name = lectures[index]['name']
        maximum_points = lectures[index]['maxPoints']
        got_points = lectures[index]['gotPoints']
        # self.addPointsToDocument(name, maxPoints)
        index += 1
        print(name, maximum_points, got_points)


def get_doc_name(initial_yaml):
    """Converts the gamification
    :param initial_yaml = the data read from block in yaml
    :returns: Updated JSON, ready for map generator
    """
    DocEntry.find_by_path()


class GamificationException(Exception):
    """The exception that is thrown when an error occurs during a gamification check."""
    pass
