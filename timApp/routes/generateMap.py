from flask import Blueprint
from copy import deepcopy
from .common import *


generateMap = Blueprint('generateMap', __name__, url_prefix='')

@generateMap.route('/generateMap', methods=["GET","POST"])
def generate_map():
    coursedata = request.get_json()
    #Get lectures from json
    lecturejson = coursedata['lectures']
    #Get demos from json
    demojson = coursedata['demos']
    #Get map
    mapname = select_map(len(lecturejson),len(demojson))
    #Read map
    with open(mapname,"r") as f:
        map = json.loads(f.read())
    #Dict for properties added to every layer.
    properties = {
        'studentpoints':0, #Points student got from this demo/lecture
        'maxpoints':0, # Maximum points attainable from this demo/lecture
        'site':"", #Link to this demo/lecture
        'title':"" # Title of this demo/lecture

    }
    # Width of the map, needed for calculating tiles.
    mapwidth = map['width']
    map['layers'] = add_properties(map['layers'],properties)
    #The number of demos and lectures in the map.
    demosinmap = 0
    lecturesinmap = 0

    # Replace all of the tables with dicts and get number of lectures and demos in map.
    for layer in map['layers']:
        layer = replace_table_with_dict(layer)
        if layer['name'][0:1] == 'l':
            lecturesinmap +=1
        if layer['name'][0:1] == 'd':
            demosinmap += 1


    # Indexes for lectures and demos so correct information can be sent to layers in a loop.
    # Starts at -abs(len - number of demos/lectures) so that excess layers can be removed.
    lectureindex = -abs(len(lecturejson)-lecturesinmap)
    demoindex = -abs(len(demojson)-demosinmap)
    # Index for iterating over the layers.
    layerindex = 0
    # Table of layers to be removed due to them not being needed.
    dellayers = []

    # Add layers for buildings.
    for layer in map['layers']:
        if layer['name'][0:1] == 'l':
            # Attach properties and create buildings if there are still lectures to be added.
            if(len(lecturejson) -1 >= lectureindex and lectureindex >= 0):
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
                map['layers'] = create_lecture_layers(map['layers'], layerindex, int(points),int(maxpoints),mapwidth,lectureproperties)
                # Delete layer if no more lectures need to be added.
            else:
                dellayers.append(layer)
            lectureindex += 1
        if layer['name'][0:1] =='d':
            # Attach properties and create buildings if there are still demos to be added.
            if(len(demojson)-1 >= demoindex and demoindex >=0):
                points = int(demojson[len(demojson) - demoindex - 1]['gotPoints'])
                maxpoints = int(demojson[len(demojson) - demoindex - 1]['maxPoints'])
                demoproperties = {
                    'studentpoints': points,  # Points student got from this demo/lecture
                    'maxpoints': maxpoints,  # Maximum points attainable from this demo/lecture
                    'site': demojson[len(demojson) - demoindex - 1]['link'],  # Link to this demo/lecture
                    'title': demojson[len(demojson) - demoindex - 1]['name']  # Title of this demo/lecture
                }
                # Change the name of the layer so it doesnt get caught in the loop again.
                layer['name'] = 'D' + layer['name'][1:]
                layer['properties'] = demoproperties
                map['layers'] = create_demo_layers(map['layers'],layerindex,int(points),int(maxpoints),mapwidth,demoproperties)
            # If there is no need for the layer delete it.
            else:
                dellayers.append(layer)
            demoindex +=1
        layerindex += 1

    #If a layer has no data remove it.
    for layer in map['layers']:
        if len(layer['data']) == 0:
            dellayers.append(layer)

    #Remove unused layers.
    for dellayer in dellayers:
        map['layers'].remove(dellayer)

    # Python for some reason modifies certain characters in the JSON and it has to be modified back to the proper form.
    map = str.replace(str(map), "\'", "\"")
    map = str.replace(str(map), "True", "true")
   # layers = create_lecture_layers(map['layers'],2,mapwidth)
    print(map)
    return str(map)

# Creates zero to three layers on top of a demo layer
# layers are the layers which need to be appended with more layers
# layerid is the id for the layer above which the layers need to be appended
# layerdict is the dictionary for the layer above which the layers need to be added
def create_lecture_layers(layers,layerid,points,maxpoints,mapwidth,properties):
    # Lets not divide by zero.
    if maxpoints == 0:
        maxpoints = 1
    if(float(points/maxpoints) > 0):
        layers.insert(layerid + 1, deepcopy(layers[layerid]))
        layers[layerid+1]['name'] = layers[layerid+1]['name'] +"b1"
        layers[layerid + 1]['data'] = make_build_blocks(layers[layerid+1]['data'],0)
        layers[layerid + 1]['properties'] = properties
    if(float(points/maxpoints) > 0.5):
        layers.insert(layerid + 2, deepcopy(layers[layerid]))
        layers[layerid + 2]['name'] = layers[layerid + 2]['name'] + "b2"
        layers[layerid + 2]['data'] = make_build_blocks(layers[layerid+2]['data'],3)
        layers[layerid + 2]['properties'] = properties
    #Delete unused keys, create offset for roof.
    deltable = []
    if(float(points/maxpoints) > 0):
        for key in layers[layerid + 2]['data'].keys():
            deltable.append(key)

    if(float(points/maxpoints) > 0.5):
        for key in deltable:
            layers[layerid + 2]['data'][str(int(key) - mapwidth)] = layers[layerid + 2]['data'][key]
            del layers[layerid + 2]['data'][key]

    return layers


# Creates zero to three layers on top of a demo layer
# layers are the layers which need to be appended with more layers
# layerid is the id for the layer above which the layers need to be appended
# layerdict is the dictionary for the layer above which the layers need to be added
# percentage is the percentage of points gotten from the demo.
def create_demo_layers(layers,layerid,points,maxpoints,mapwidth,properties):
    if maxpoints == 0 and points == 0:
        maxpoints = 1
        points = 1
    if maxpoints <= 0:
        maxpoints = 1
    percentage = float(points/maxpoints)
    floors = 0
    tiles = 0
    #set number of floors
    if 0 <= percentage < 0.25:
        floors = 1
        tiles = 0
        if 0 < percentage < 0.075:
            tiles = 1
        if 0.075 <= percentage < 0.15:
            tiles = 2
        if 0.15 <= percentage:
            tiles = 3
    if 0.25 <= percentage < 0.50:
        floors = 2
        tiles = 0
        if 0.25 < percentage < 0.325:
            tiles = 1
        if 0.325 <= percentage < 0.4:
            tiles = 2
        if 0.4 <= percentage:
            tiles = 3
    if 0.5 <= percentage < 0.75:
        floors = 3
        tiles = 0
        if 0.5 < percentage < 0.575:
            tiles = 1
        if 0.575 <= percentage < 0.65:
            tiles = 2
        if 0.725 <= percentage:
            tiles = 3
    if 0.75 <= percentage:
        floors = 4
        if percentage < 0.825:
            tiles = 1
        if 0.825 <= percentage < 0.9:
            tiles = 2
        if 0.975 <= percentage:
            tiles = 3
    #index used to track progress of loop
    floorindex = 0


    while floorindex < floors:
        layers.insert(layerid + 1 + floorindex, deepcopy(layers[layerid]))
        layers[layerid + 1+ floorindex]['name'] = layers[layerid]['name'] + "b" + str(1+floorindex)
        layers[layerid + 1+ floorindex]['data'] = make_build_blocks(layers[layerid + 1+ floorindex]['data'], floorindex)
        layers[layerid + 1+ floorindex]['properties'] = properties
        if floorindex != 0:
            deltable = []
            #Insert layers into the deletable table if they have no data in them
            for key in layers[layerid +1+ floorindex]['data'].keys():
                deltable.append(key)
            for key in deltable:
                layers[layerid + 1 + floorindex]['data'][str(int(key) - mapwidth*floorindex)] = layers[layerid + 1 + floorindex]['data'][key]
                del layers[layerid + 1 + floorindex]['data'][key]
        floorindex +=1
        # Remove extra tiles if this is the last run of the loop.
        deltiles = 3
        if floorindex == floors:
            while tiles < deltiles:
                #Delete keys starting with the latest so that the frames look better.
                for key in reversed(list(layers[layerid + floorindex]['data'].keys())):
                    del layers[layerid + floorindex]['data'][key]
                    deltiles -=1
                    break

    return  layers


# Makes buildblocks for tiles, dict is the dict to be modified,
# floor defines the blocks to be added. 0 = ground floor, 1 = middle, 2 = middle, 3 = roof
def make_build_blocks(dict,floor):
    if floor == 0:
        for key in dict.keys():
            dict[key] = 23
    if floor == 1 or floor ==2:
        for key in dict.keys():
            dict[key] = 20
    if floor == 3:
        for key in dict.keys():
            dict[key] = 101
    return dict


# Select a map based on the number of lectures and demos.
def select_map(lectures,demos):
    map = "static/map_files/d"
    # If there are more than twice as many demos as there are lectures select map based on lectures
    if (2*demos < lectures):
        if lectures%2 == 0:
            map += str(int(lectures/2)) + ".json"
        if lectures%2 == 1:
            map += str(int(int(lectures/2) +1)) + ".json"
        # Otherwise select map based on demos.
    else:
        map += str(demos) +".json"
    return map


# Add properties to every layer in the map. Properties dont contain anything of actual value at this point.
def add_properties(layers,properties):
    for layer in layers:
        layer['properties'] = properties
    return layers


# replaces data table of layer with a dict containing tile and image id.
def replace_table_with_dict(layer):
    toptiles = find_tiles(layer)
    # Dictionary the data will be replaced with.
    dict = {}
    for tile in toptiles:
        #Dict is updated with line tile:image id
        dict.update({u"" + str(tile):layer['data'][tile]})

    #Replace data with dict
    layer['data'] = dict
    return layer


# This method returns a list of tiles in the given data.
def find_tiles(layer):
    data = layer['data']
    #Table of the tiles
    ret = []
    i = 0
    for id in data:
        if id != 0:
            ret.append(i)
        i+=1
    return ret