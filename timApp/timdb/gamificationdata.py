from timdb.timdbbase import TimDbBase
from timdb.models.docentry import DocEntry
import yaml
import json


def gamify(initial_data):
    initial_json = convert_to_json(initial_data)
    parse_json(initial_json)
    get_doc_name()
    #get_maximum_points()
    #get_url()


def convert_to_json(md_data):
    """Converts the YAML paragraph in gamification document to JSON
    :param initial_data = the data read from paragraph in YAML
    :returns: same data in JSON
    """
    temp = yaml.load(md_data[3:len(md_data) - 3])
    return json.dumps(temp)


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
    :param initial_data = the data read from block in yaml
    :returns: Updated JSON, ready for map generator
    """
    DocEntry.find_by_path()
