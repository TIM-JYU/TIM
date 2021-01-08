import json
from copy import deepcopy

from flask import Blueprint
from flask import request

from timApp.gamification import gamificationdata
from timApp.gamification.gamificationdata import GamificationException
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import json_response

generateMap = Blueprint('generateMap', __name__, url_prefix='')


@generateMap.route('/generateMap', methods=["POST"])
def generate_map():
    # Configuring the data is here to make the map load lazier.
    try:
        coursedata = gamificationdata.gamify(request.get_json())
    except GamificationException as e:
        raise RouteException(str(e))
    # Get lectures from json
    lecturejson = coursedata['lectures']
    # Get demos from json
    demojson = coursedata['demos']
    # Get map
    mapname = select_map(len(lecturejson), len(demojson))
    # Read map
    with open(mapname, "r") as f:
        g_map = json.loads(f.read())
    # Dict for properties added to every layer.
    properties = {
        'studentpoints': 0,  # Points student got from this demo/lecture
        'maxpoints': 0,  # Maximum points attainable from this demo/lecture
        'site': "",  # Link to this demo/lecture
        'title': ""  # Title of this demo/lecture
    }
    # Width of the map, needed for calculating tiles.
    mapwidth = g_map['width']
    g_map['layers'] = add_properties(g_map['layers'], properties)
    # The number of demos and lectures in the map.
    demosinmap = 0
    lecturesinmap = 0

    # Replace all of the tables with dicts and get number of lectures and demos in map.
    for layer in g_map['layers']:
        layer = replace_table_with_dict(layer)
        if layer['name'][0:1] == 'l':
            lecturesinmap += 1
        if layer['name'][0:1] == 'd':
            demosinmap += 1

    # Indexes for lectures and demos so correct information can be sent to layers in a loop.
    # Starts at -abs(len - number of demos/lectures) so that excess layers can be removed.
    lectureindex = -abs(len(lecturejson) - lecturesinmap)
    demoindex = -abs(len(demojson) - demosinmap)
    # Index for iterating over the layers.
    layerindex = 0
    # Table of layers to be removed due to them not being needed.
    dellayers = []

    # Add layers for buildings.
    for layer in g_map['layers']:
        if layer['name'][0:1] == 'l':
            # Attach properties and create buildings if there are still lectures to be added.
            if len(lecturejson) - 1 >= lectureindex >= 0:
                points = 1
                maxpoints = 1
                title = lecturejson[len(lecturejson) - lectureindex - 1]['name']
                lectureproperties = {
                    'studentpoints': 0,  # Points student got from this demo/lecture
                    'maxpoints': 0,  # Maximum points attainable from this demo/lecture
                    'site': lecturejson[len(lecturejson) - lectureindex - 1]['link'],  # Li$
                    'title': title  # Title of this demo/lecture
                }
                # Change the name of the layer so it doesnt get caught in the loop a$
                layer['name'] = 'L' + layer['name'][1:]
                layer['properties'] = lectureproperties
                g_map['layers'] = create_lecture_layers(g_map['layers'], layerindex, int(points), int(maxpoints),
                                                      mapwidth, lectureproperties)
                # Delete layer if no more lectures need to be added.
            else:
                dellayers.append(layer)
            lectureindex += 1
        if layer['name'][0:1] == 'd':
            # Attach properties and create buildings if there are still demos to be added.
            if len(demojson) - 1 >= demoindex >= 0:
                points = demojson[len(demojson) - demoindex - 1]['gotPoints']
                maxpoints = demojson[len(demojson) - demoindex - 1]['maxPoints']
                demoproperties = {
                    'studentpoints': points,  # Points student got from this demo/lecture
                    'maxpoints': maxpoints,  # Maximum points attainable from this demo/lecture
                    'site': demojson[len(demojson) - demoindex - 1]['link'],  # Link to this demo/lecture
                    'title': demojson[len(demojson) - demoindex - 1]['name']  # Title of this demo/lecture
                }
                # Change the name of the layer so it doesnt get caught in the loop again.
                layer['name'] = 'D' + layer['name'][1:]
                layer['properties'] = demoproperties
                g_map['layers'] = create_demo_layers(g_map['layers'], layerindex, points, maxpoints,
                                                   mapwidth, demoproperties)
            # If there is no need for the layer delete it.
            else:
                dellayers.append(layer)
            demoindex += 1
        layerindex += 1

    # If a layer has no data remove it.
    for layer in g_map['layers']:
        if len(layer['data']) == 0:
            dellayers.append(layer)

    # Remove unused layers.
    for dellayer in dellayers:
        g_map['layers'].remove(dellayer)

    # layers = create_lecture_layers(map['layers'],2,mapwidth)
    return json_response(g_map)


# Creates zero to three layers on top of a demo layer
# layers are the layers which need to be appended with more layers
# layerid is the id for the layer above which the layers need to be appended
# layerdict is the dictionary for the layer above which the layers need to be added
def create_lecture_layers(layers, layerid, points, maxpoints, mapwidth, properties):
    # Lets not divide by zero.
    if maxpoints == 0:
        maxpoints = 1
    if float(points / maxpoints) > 0:
        layers.insert(layerid + 1, deepcopy(layers[layerid]))
        layers[layerid + 1]['name'] += "b1"
        layers[layerid + 1]['data'] = make_build_blocks(layers[layerid + 1]['data'], 0)
        layers[layerid + 1]['properties'] = properties
    if float(points / maxpoints) > 0.5:
        layers.insert(layerid + 2, deepcopy(layers[layerid]))
        layers[layerid + 2]['name'] += "b2"
        layers[layerid + 2]['data'] = make_build_blocks(layers[layerid + 2]['data'], 3)
        layers[layerid + 2]['properties'] = properties
    # Delete unused keys, create offset for roof.
    deltable = []
    if float(points / maxpoints) > 0:
        for key in layers[layerid + 2]['data'].keys():
            deltable.append(key)

    if float(points / maxpoints) > 0.5:
        for key in deltable:
            layers[layerid + 2]['data'][str(int(key) - mapwidth)] = layers[layerid + 2]['data'][key]
            del layers[layerid + 2]['data'][key]

    return layers


# Creates zero to three layers on top of a demo layer
# layers are the layers which need to be appended with more layers
# layerid is the id for the layer above which the layers need to be appended
# layerdict is the dictionary for the layer above which the layers need to be added
# percentage is the percentage of points gotten from the demo.
def create_demo_layers(layers, layerid, points, maxpoints, mapwidth, properties):
    if maxpoints == 0 and points == 0:
        maxpoints = 1
        points = 1
    if maxpoints <= 0:
        maxpoints = 1
    max_tiles = 12
    totaltiles = max_tiles * points / maxpoints
    if totaltiles > max_tiles:
        totaltiles = max_tiles
    totaltiles = round(totaltiles)
    floors = totaltiles // 3 + 1
    tiles = totaltiles % 3
    # index used to track progress of loop
    floorindex = 0

    while floorindex < floors:
        layers.insert(layerid + 1 + floorindex, deepcopy(layers[layerid]))
        layers[layerid + 1 + floorindex]['name'] = layers[layerid]['name'] + "b" + str(1 + floorindex)
        layers[layerid + 1 + floorindex]['data'] = make_build_blocks(layers[layerid + 1 + floorindex]['data'],
                                                                     floorindex)
        layers[layerid + 1 + floorindex]['properties'] = properties
        if floorindex != 0:
            deltable = []
            # Insert layers into the deletable table if they have no data in them
            for key in layers[layerid + 1 + floorindex]['data'].keys():
                deltable.append(key)
            for key in deltable:
                layers[layerid + 1 + floorindex]['data'][str(int(key) - mapwidth * floorindex)] = layers[
                    layerid + 1 + floorindex]['data'][key]
                del layers[layerid + 1 + floorindex]['data'][key]
        floorindex += 1
        # Remove extra tiles if this is the last run of the loop.
        deltiles = 3
        if floorindex == floors:
            while tiles < deltiles:
                # Delete keys starting with the latest so that the frames look better.
                for key in reversed(list(layers[layerid + floorindex]['data'].keys())):
                    del layers[layerid + floorindex]['data'][key]
                    deltiles -= 1
                    break

    return layers


# Makes buildblocks for tiles, dict is the dict to be modified,
# floor defines the blocks to be added. 0 = ground floor, 1 = middle, 2 = middle, 3 = roof
def make_build_blocks(data, floor):
    if floor == 0:
        for key in data.keys():
            data[key] = 23
    if floor == 1 or floor == 2:
        for key in data.keys():
            data[key] = 20
    if floor == 3:
        for key in data.keys():
            data[key] = 101
    return data


# Select a map based on the number of lectures and demos.
def select_map(lectures, demos):
    map_file = "static/map_files/d"
    # If there are more than twice as many demos as there are lectures select map based on lectures
    if 2 * demos < lectures:
        if lectures % 2 == 0:
            map_file += str(int(lectures / 2)) + ".json"
        if lectures % 2 == 1:
            map_file += str(int(int(lectures / 2) + 1)) + ".json"
        # Otherwise select map based on demos.
    else:
        map_file += str(demos if demos > 0 else 1) + ".json"
    return map_file


# Add properties to every layer in the map. Properties dont contain anything of actual value at this point.
def add_properties(layers, properties):
    for layer in layers:
        layer['properties'] = properties
    return layers


# replaces data table of layer with a dict containing tile and image id.
def replace_table_with_dict(layer):
    toptiles = find_tiles(layer)
    # Dictionary the data will be replaced with.
    data = {}
    for tile in toptiles:
        # Dict is updated with line tile:image id
        data.update({u"" + str(tile): layer['data'][tile]})

    # Replace data with dict
    layer['data'] = data
    return layer


# This method returns a list of tiles in the given data.
def find_tiles(layer):
    data = layer['data']
    # Table of the tiles
    ret = []
    i = 0
    for n in data:
        if n != 0:
            ret.append(i)
        i += 1
    return ret
