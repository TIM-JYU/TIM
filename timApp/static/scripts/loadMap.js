define(['jquery'], function ($) {

$(document).ready(function () {
    // Scale of the map
    var scale = 0.5;

    // Show all building frames
    var showAll = false;

    // Alpha value for the building frames
    var alpha = 0.2;

    var json;

    // Get the div that will hold canvases
    var $container = $(".mapContainer");

    // don't try to do anything if there are no maps on the page
    if ($container.length === 0) {
        return;
    }

    var container = $container[0];
    $container.css({
        "z-index": 0,
        "display": "none",
        "position": "relative"
    });

    // Set properties for zoom input
    var zoomInput = $(".mapZoom");
    zoomInput.attr({"max": 1, "min": 0.3, "step": 0.05});
    zoomInput.val(scale);
    zoomInput.width(300);

    // Set properties for alpha input
    var alphaInput = $(".alphaRange");
    alphaInput.attr({"max": 1, "min": 0, "step": 0.05});
    alphaInput.val(alpha);
    alphaInput.width(300);

    // Title and description elements for the info box
    var title = $("<p></p>");
    title.addClass("infoBoxTitle");
    title.css("position", "absolute");
    title.appendTo(container);

    var description = $("<p></p>");
    description.addClass("infoBoxDescription");
    description.css("position", "absolute");
    description.appendTo(container);

    /**
     * Function for preloading the images
     * @param sources File paths to the images
     * @param callback Callback function
     */
    function loadImages(sources, callback) {
        var images = {};
        var loadedImages = 0;
        var onload = function () {
            if (++loadedImages >= sources.length) {
                callback(images);
            }
        };
        for (var i = 0; i < sources.length; i++) {
            images[i] = new Image();
            images[i].onload = onload;
            images[i].src = sources[i].replace("..", "/static");
        }
    }

    // Get the JSON map file
    $.ajax({
        url: "/generateMap",
        type: "post",
        datatype : "application/json",
        contentType: "application/json",
        data: $container.attr('data-mapdata'),
        charset: 'utf-8',
        success: function (newJson) {

            json = JSON.parse(newJson);

            /**
             * Create canvases for drawing the map, frames and event listeners
             */
            function createCanvases() {
                // If earlier canvases exist, remove them
                if (canvases.length > 0) {
                    $("#topCanvas").remove();
                    $("#middleCanvas").remove();
                    $("#bottomCanvas").remove();
                    canvases = [];
                }

                var canvas;

                // Canvases used to draw the final map
                for (var j = 0; j < 3; j++) {
                    canvas = document.createElement('canvas');
                    container.appendChild(canvas);

                    // Set some attributes for the canvas
                    if (j === 0) {
                        canvas.setAttribute("id", "bottomCanvas");
                        canvas.setAttribute("style", "padding-bottom: 1em; z-index: " + j + ";");
                        bottomCanvas = canvas;
                    } else if (j === 1) {
                        canvas.setAttribute("id", "middleCanvas");
                        canvas.setAttribute("style", "position: absolute; left: 0; top: 0; z-index: " + j + ";");
                        middleCanvas = canvas;
                    } else {
                        canvas.setAttribute("id", "topCanvas");
                        canvas.setAttribute("style", "position: absolute; left: 0; top: 0; z-index: " + j + ";");
                        topCanvas = canvas;
                    }
                    canvas.width = json.width * json.tilewidth * scale;
                    canvas.height = (json.height * json.tileheight + json.tileheight * 3) * scale;
                }

                // Canvases used to draw the layers
                for (j = 0; j < json.layers.length; j++) {
                    canvas = document.createElement('canvas');

                    // Set some attributes for the canvas and get it's context
                    canvas.setAttribute("id", "layer" + j);
                    canvas.width = json.width * json.tilewidth * scale;
                    canvas.height = (json.height * json.tileheight + json.tileheight * 3) * scale;
                    canvases.push(canvas);
                }
            }

            // Array for the image sources
            var sources = [];

            // Fill the sources array with tileset image sources
            for (var k = 0; k < json.tilesets.length; k++) {
                sources.push(json.tilesets[k].image);
            }

            // List of canvases that are used to help with drawing the full map
            var canvases = [];

            // Used for event listener
            var topCanvas;
            // Used for dynamic drawing
            var middleCanvas;
            // Used for drawing the map
            var bottomCanvas;

            createCanvases();

            // Preload the images and draw the map
            loadImages(sources, function (images) {

                /**
                 * Find the position of a tile on spreadsheet
                 * @param tileset Used tileset
                 * @param offset Number of tiles that come after the searched tile on the spreadsheet
                 * @returns {{x: *, y: *}} Position of the tile on spreadsheet
                 */
                function findTileImagePos(tileset, offset) {
                    var currentX = 0;
                    var currentY = 0;

                    var x;
                    var y;

                    // Find the position of gray tile on a spreadsheet
                    for (var j = 0; j < tileset.columns - offset; j++) {
                        x = currentX++ * tileset.tilewidth;
                        y = currentY * tileset.tileheight;

                        if (currentX == tileset.columns) {
                            currentX = 0;
                            currentY++;
                        }
                    }

                    return {
                        x: x,
                        y: y
                    };
                }


                function hasOwnProperty(json, tile, layerdelta, datadelta) {
                    if ( !json.layers[tile.layerNo + layerdelta] ) return false;
                    if ( !json.layers[tile.layerNo + layerdelta].data ) return false;
                    return ( json.layers[tile.layerNo + layerdelta].data.hasOwnProperty((tile.dataIndex + datadelta).toString()) );
                }

                /**
                 * Function for drawing a frame of a building on given position
                 * @param tile Tile that the frame is drawn over
                 */
                function drawFrame(tile) {
                    var context = middleCanvas.getContext("2d");

                    // if clicked on a "top tile", draw a full frame
                    if (tile.tileset.properties !== undefined &&
                        tile.tileset.properties.frameProperty == 1  && !hasOwnProperty(json, tile, 1, 0) ) {
/*
                        && json.layers[tile.layerNo + 1] && json.layers[tile.layerNo + 1].data &&
                        !json.layers[tile.layerNo + 1].data.hasOwnProperty((tile.dataIndex).toString()))  {
*/
                        context.globalAlpha = alpha;
                        context.drawImage(tileImage, grayTile.x, grayTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, tile.y * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(tileImage, grayTile.x, grayTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(tileImage, grayTile.x, grayTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight * 2) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(roofImage, roofTile.x, roofTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight * 3) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.globalAlpha = 1;

                        context.drawImage(tileImage, frameTile.x, frameTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, tile.y * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(tileImage, frameTile.x, frameTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(tileImage, frameTile.x, frameTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight * 2) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(roofImage, roofFrameTile.x, roofFrameTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight * 3) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);
                        // If clicked on a bulding with one floor, draw two frame tiles and a roof frame
                    } else if (tile.tileset.properties !== undefined &&
                        tile.tileset.properties.buildingProperty === 1  && //&& json.layers[tile.layerNo + 1] &&

                        !hasOwnProperty(json, tile, 1, -json.width) &&
                        hasOwnProperty(json, tile, -1, 0) ) {
                        /*
                        !json.layers[tile.layerNo + 1].data.hasOwnProperty((tile.dataIndex - json.width).toString()) &&
                        json.layers[tile.layerNo - 1].data.hasOwnProperty((tile.dataIndex).toString())) {
                        */
                        context.globalAlpha = alpha;

                        context.drawImage(tileImage, grayTile.x, grayTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(tileImage, grayTile.x, grayTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight * 2) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(roofImage, roofTile.x, roofTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight * 3) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.globalAlpha = 1;

                        context.drawImage(tileImage, frameTile.x, frameTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(tileImage, frameTile.x, frameTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight * 2) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(roofImage, roofFrameTile.x, roofFrameTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight * 3) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);
                        // If clicked on a bulding with two floors, draw one frame tile and a roof frame
                    } else if (tile.tileset.properties !== undefined &&
                        tile.tileset.properties.buildingProperty == 1 && // json.layers[tile.layerNo + 1] &&
                        !hasOwnProperty(json, tile, 1, -json.width) &&
                        hasOwnProperty(json, tile, -1, json.width) &&
                        hasOwnProperty(json, tile, -2, json.width) ) {
                        /*
                        !json.layers[tile.layerNo + 1].data.hasOwnProperty((tile.dataIndex - json.width).toString()) &&
                        json.layers[tile.layerNo - 1].data.hasOwnProperty((tile.dataIndex + json.width).toString()) &&
                        json.layers[tile.layerNo - 2].data.hasOwnProperty((tile.dataIndex + json.width).toString())) {
                        */
                        context.globalAlpha = alpha;

                        context.drawImage(tileImage, grayTile.x, grayTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(roofImage, roofTile.x, roofTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight * 2) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.globalAlpha = 1;

                        context.drawImage(tileImage, frameTile.x, frameTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.drawImage(roofImage, roofFrameTile.x, roofFrameTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight * 2) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);
                        // If clicked on a building with three floors and no roof, draw a roof frame
                    } else if (tile.tileset.properties !== undefined &&
                        tile.tileset.properties.buildingProperty == 1 && // json.layers[tile.layerNo + 1] &&
                        !hasOwnProperty(json, tile, 1, -json.width) &&
                        hasOwnProperty(json, tile, -1, json.width) &&
                        hasOwnProperty(json, tile, -2, json.width*2) &&
                        hasOwnProperty(json, tile, -3, json.width*2) ) {
                        /*
                        !json.layers[tile.layerNo + 1].data.hasOwnProperty((tile.dataIndex - json.width).toString()) &&
                        json.layers[tile.layerNo - 1].data.hasOwnProperty((tile.dataIndex + json.width).toString()) &&
                        json.layers[tile.layerNo - 2].data.hasOwnProperty((tile.dataIndex + json.width * 2).toString()) &&
                        json.layers[tile.layerNo - 3].data.hasOwnProperty((tile.dataIndex + json.width * 2).toString())) {
                        */
                        context.globalAlpha = alpha;

                        context.drawImage(roofImage, roofTile.x, roofTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);

                        context.globalAlpha = 1;

                        context.drawImage(roofImage, roofFrameTile.x, roofFrameTile.y,
                            tileFrameSet.tilewidth, tileFrameSet.tileheight, tile.x * scale, (tile.y - json.tileheight) * scale,
                            tileFrameSet.tilewidth * scale, tileFrameSet.tileheight * scale);
                    }

                }

                /**
                 * Draws a frame on all active tiles
                 * @param tiles All tiles on the map
                 */
                function drawOnActive(tiles) {
                    tiles.forEach(function (t) {
                        if (t.active && !t.frame) {
                            t.frame = true;
                            drawFrame(t);
                        }
                    });
                }


                function rround(d, des) {
                    var mult = Math.pow(10,des);
                    return Math.round(d*mult)/mult;
                }

                /**
                 * Draws an info box
                 * @param x X coordinate for the box
                 * @param y Y coordinate for the box
                 * @param tile Clicked tile
                 */
                function drawInfo(x, y, tile) {
                    var tscale = scale;
                    if ( tscale < 0.7 ) tscale = 0.7;
                    // Size and position of the box
                    var height = json.tileheight * 10 * tscale;
                    var width = json.tilewidth * 8 * tscale;
                    var rectX = x - json.tilewidth * 4 * scale;
                    var rectY = y - json.tileheight * 6 * scale - height;

                    if (rectY < json.tileheight * scale) {
                        rectY = y + json.tileheight * scale;
                    }

                    if (rectX < json.tilewidth * scale) {
                        rectX = json.tilewidth * scale;
                    }

                    if (rectX >= (json.tilewidth * json.width) * scale - width) {
                        rectX = json.tilewidth * json.width * scale - json.tilewidth * scale - width;
                    }

                    // Set style settings for info box text elements

                    title.css({
                        "position": "absolute",
                        "left": (rectX + 10 * tscale) + "px",
                        "top": (rectY + 5 * tscale) + "px",
                        "min-width": (width - 20 * tscale) + "px",
                        "max-width": (width - 20 * tscale) + "px",
                        "max-height": (2 * tscale) + "em",
                        "min-height": (2 * tscale) + "em",
                        "overflow": "hidden",
                        "font-weight": "bold",
                        "font-size": (1.1 * tscale) + "em",
                        "z-index": 4
                    });
                    description.css({
                        "min-width": (width - 20 * tscale) + "px",
                        "max-width": (width - 20 * tscale) + "px",
                        "max-height": (height - 60 * tscale) + "px",
                        "position": "absolute",
                        "overflow": "auto",
                        "left": (rectX + 10 * tscale) + "px",
                        "top": (rectY + 40 * tscale) + "px",
                        "font-size": (tscale) + "em",
                        "text-align": "left",
                        "z-index": 4
                    });

                    var totalMax = 0;
                    var totalPoints = 0;
                    var titles = [];

                    for (var i = 0; i < json.layers.length; i++) {
                        if(!titles.includes(json.layers[i].properties.title)) {
                            totalMax += json.layers[i].properties.maxpoints;
                            titles.push(json.layers[i].properties.title);
                        }
                        if (json.layers[i].name.length == 2) {
                            totalPoints += json.layers[i].properties.studentpoints;
                        }
                    }

                    // Set info box text

                    title.html("<a href='" + json.layers[tile.layerNo].properties.site + "'>" + json.layers[tile.layerNo].properties.title + "</a>");
		                if(json.layers[tile.layerNo].properties.maxpoints !== 0) {
                            description.html("Dokumentin pisteet: " + rround(json.layers[tile.layerNo].properties.studentpoints,2) + "/" +
                                json.layers[tile.layerNo].properties.maxpoints + "<br>Kurssin pisteet yhteensä: " + rround(totalPoints,2) + "/" + totalMax);
			                } else {
			                    description.html("Kurssin pisteet yhteensä: " + rround(totalPoints,2) + "/" + totalMax);
			                }

                    // Draw the box on the canvas
                    var context = middleCanvas.getContext("2d");

                    context.beginPath();
                    context.fillStyle = "rgba(132,225,225,0.8)";
                    context.fillRect(rectX, rectY, width, height);
                }

                /**
                 * Add click-event listener to the top canvas
                 */
                function addClickListener() {
                    // Add event listener for `click` events on top canvas.
                    topCanvas.addEventListener('click', function (event) {
                        var pos = absolutePosition(topCanvas);
                        var x = event.pageX - pos.left;
                        var y = event.pageY - pos.top;

                        for (var i = 0; i < tiles.length; i++) {
                            var tile = tiles[i];
                            // Check if clicked tile
                            if (tile.isPointInside(x, y)) {
                                // Clear earlier selections
                                clearSelection(tiles);

                                // If clicked tile is a "top tile" or a building
                                if (tile.tileset.properties !== undefined &&
                                    (tile.tileset.properties.frameProperty === 1 ||
                                    tile.tileset.properties.buildingProperty === 1 ||
                                    tile.tileset.properties.buildingProperty === 2)) {
                                    if (!tile.active) {
                                        if (!showAll) {
                                            // Activate clicked tile cluster
                                            activateCluster(tile, tiles);
                                        } else {
                                            activateAll(tiles);
                                        }

                                        // Draw a frame on activated tiles
                                        drawOnActive(tiles);
                                    }
                                    // Draw info box over the clicked point
                                    drawInfo(x, y, tile);
                                } else if (tile.tileset.properties.buildingProperty === 0 &&
                                    tile.tileset.properties.frameProperty === 0) {
                                    if (showAll) {
                                        activateAll(tiles);
                                        drawOnActive(tiles);
                                    }
                                }
                                break;
                            }
                        }
                    }, false);
                }

                /**
                 * Activate a cluster of "top tiles" and building tiles on the map
                 * @param tile A tile that is part of the cluster
                 * @param tiles All tiles on the map
                 */
                function activateCluster(tile, tiles) {
                    tile.active = true;

                    // Boolean for checking if iterated tile is next to the given tile
                    var isNextTo;

                    tiles.forEach(function (t) {
                        isNextTo = (t.x * scale <= (tile.x + json.tilewidth * 2) * scale &&
                        t.x * scale >= (tile.x - json.tilewidth * 2) * scale &&
                        (t.y * scale <= (tile.y + json.tileheight * 2) * scale &&
                        t.y * scale >= (tile.y - json.tileheight * 2) * scale));

                        // If the iterated tile is a building or a "top tile" and it is next to
                        // or above the given tile, call this function in  recursion.
                        if (t.tileset.properties !== undefined &&
                            (t.tileset.properties.buildingProperty == 1 ||
                            t.tileset.properties.frameProperty == 1) && !t.active &&
                            ( isNextTo || ((t.layerNo === tile.layerNo + 1 ||
                            t.layerNo === tile.layerNo - 1) &&
                            t.x === tile.x && (t.y === tile.y - json.tileheight ||
                            t.y === tile.y + json.tileheight)))) {
                            activateCluster(t, tiles);
                        }
                    });

                }

                /**
                 * Activate all "top tiles" and building tiles on the map.
                 * @param tiles All tiles on the map
                 */
                function activateAll(tiles) {
                    tiles.forEach(function (t) {
                        if (t.tileset.properties !== undefined &&
                            (t.tileset.properties.buildingProperty == 1 ||
                            t.tileset.properties.frameProperty == 1) && !t.active) {
                            t.active = true;
                        }
                    });
                }

                /**
                 * Get absolute position of given element
                 * @param element given DOM element
                 * @returns {{top: number, left: number}} left and top position
                 */
                function absolutePosition(element) {
                    var top = 0, left = 0;
                    do {
                        top += element.offsetTop || 0;
                        left += element.offsetLeft || 0;
                        element = element.offsetParent;
                    } while (element);

                    return {
                        top: top,
                        left: left
                    };
                }

                /**
                 * Clear all selections on the map
                 */
                function clearSelection(tiles) {
                    // Remove previous frame canvas and create a new one
                    $("#middleCanvas").remove();
                    middleCanvas = document.createElement('canvas');

                    // Set some attributes for the canvas
                    middleCanvas.setAttribute("id", "middleCanvas");
                    middleCanvas.setAttribute("style", "position: absolute; left: 0; top: 0; z-index: 1;");
                    middleCanvas.width = json.width * json.tilewidth * scale;
                    middleCanvas.height = json.height * json.tileheight + json.tileheight * 3 * scale;

                    container.appendChild(middleCanvas);

                    // Deactivate currently active tiles
                    tiles.forEach(function (t) {
                        t.active = false;
                        t.frame = false;
                    });

                    // Clear info box text
                    title.html("");
                    description.html("");
                }

                /**
                 * Tile object constructor
                 * @param imageIndex Index of the picture on the spreadsheet
                 * @param tileset Tiles tileset
                 * @param spreadsheet Tiles spreadsheet
                 * @param layerNo Index of the layer that the tile is drawn to
                 * @param dataIndex Index of the tile in layer data
                 * @param x Tiles x-coordinate on the map
                 * @param y Tiles y-coordinate on the map
                 */
                function Tile(imageIndex, tileset, spreadsheet, layerNo, dataIndex, x, y) {

                    // Index for the image on spreadsheet
                    this.imageIndex = imageIndex;

                    // Tileset where the tile is located
                    this.tileset = tileset;

                    // Spreadsheet where the tile is located
                    this.spreadsheet = spreadsheet;

                    // Layer that the tile is on
                    this.layerNo = layerNo;

                    // Index of the tile in layer data
                    this.dataIndex = dataIndex;

                    // Tiles x-coordinate on the map
                    this.x = x;

                    // Tiles y-coordinate on the map
                    this.y = y;

                    // Is tile active?
                    this.active = false;

                    // Is a frame drawn on the tile?
                    this.frame = false;

                    // Height correction for shorter tiles
                    this.heightCorr = 0;

                    if (this.tileset.tileheight < json.tileheight * 3) {
                        this.heightCorr = json.tileheight;
                    }

                    if (this.tileset.tileheight < json.tileheight * 2) {
                        this.heightCorr = json.tileheight * 3;
                    }

                    // Get possible offset of the tileset
                    this.offsetX = 0;
                    this.offsetY = 0;

                    if (this.tileset.tileoffset !== undefined) {
                        this.offsetX = this.tileset.tileoffset.x;
                        this.offsetY = -this.tileset.tileoffset.y;
                    }

                    // Current position on the tileset image
                    var currentY = 0;
                    var currentX = 0;

                    // Find the correct position on the tileset image
                    for (var j = this.tileset.firstgid; j < this.imageIndex + 1; j++) {
                        this.sourceX = currentX++ * this.tileset.tilewidth;
                        this.sourceY = currentY * this.tileset.tileheight;

                        if (currentX == this.tileset.columns) {
                            currentX = 0;
                            currentY++;
                        }
                    }

                    // Draw the tile
                    this.draw = function (canvas) {
                        var context = canvas.getContext("2d");

                        context.drawImage(this.spreadsheet, this.sourceX, this.sourceY,
                            this.tileset.tilewidth, this.tileset.tileheight, (this.x + this.offsetX) * scale, (this.y + this.offsetY + this.heightCorr) * scale,
                            this.tileset.tilewidth * scale, this.tileset.tileheight * scale);
                    };

                    // Check if point is inside the tile
                    this.isPointInside = function (x, y) {
                        return ( x >= (this.x + this.offsetX) * scale &&
                        x <= (this.x + this.tileset.tilewidth + this.offsetX) * scale &&
                        y >= (this.y + this.offsetY + this.heightCorr) * scale &&
                        y <= (this.y + this.tileset.tileheight + this.offsetY + this.heightCorr) * scale);
                    };

                }

                /**
                 * Draw tiles according to data dictionary
                 * @param dict Dictionary holding the data for the tiles
                 * @param tiles Array of tile objects
                 * @param canvas Canvas to draw on
                 * @param layer Layer that is being drawn
                 */
                function drawTilesFromDict(dict, tiles, canvas, layer) {
                    // Get all keys from the data dictionarty
                    var keys = Object.keys(dict);

                    // Spreadsheet and tileset that tile uses
                    var image;
                    var tileset;

                    // Go through every key and select appropriate spreadsheet for the tile
                    keys.forEach(function (key) {
                        for (var j = 0; j < json.tilesets.length; j++) {
                            if (json.tilesets[j].firstgid + json.tilesets[j].tilecount > dict[key]) {
                                tileset = json.tilesets[j];
                                image = images[j];
                                break;
                            }
                        }
                        // Calculate the position for the tile
                        var posIndex = parseInt(key);
                        var posX = posIndex % json.width * json.tilewidth;
                        var posY = Math.floor(posIndex / json.width) * json.tileheight;

                        // Create tile object and draw it on the map
                        var t = new Tile(dict[key], tileset, image, layer, posIndex, posX, posY);
                        t.draw(canvas);
                        tiles.push(t);
                    });
                }

                /**
                 * Function draws a map on canvases
                 * @returns {Array} All tile objects of the map
                 */
                function drawMap() {
                    // Array of tile objects on the map
                    var tiles = [];

                    // Current layer
                    var layer;

                    // Current canvas
                    var c;

                    // Go through every layer of the map in reverse order so that the draw order will be correct
                    for (var k = json.layers.length - 1; k >= 0; k--) {
                        layer = json.layers[k];

                        // Helper canvas that the layer is drawn on
                        c = canvases[k];

                        drawTilesFromDict(layer.data, tiles, c, k);
                    }

                    // Draw images in helper canvases on a single canvas
                    for (var n = 0; n < canvases.length; n++) {
                        var context = bottomCanvas.getContext("2d");
                        context.drawImage(canvases[n], 0, 0);
                    }


                    // View all frames if required
                    if (showAll) {
                        activateAll(tiles);
                        drawOnActive(tiles);
                    }

                    addClickListener();

                    return tiles;
                }

                // Positions for tiles used in the building frames
                var tileFrameSet;
                var tileImage;

                var roofFrameSet;
                var roofImage;

                // Find tilesets and spreadsheets needed for drawing a frame

                for (var j = 0; j < json.tilesets.length; j++) {
                    if (json.tilesets[j].properties !== undefined && json.tilesets[j].properties.frameProperty == 2) {
                        tileFrameSet = json.tilesets[j];
                        tileImage = images[j];
                    }
                    if (json.tilesets[j].properties !== undefined && json.tilesets[j].properties.frameProperty == 3) {
                        roofFrameSet = json.tilesets[j];
                        roofImage = images[j];
                    }
                }

                // Tile images used to draw the building frames
                var frameTile = findTileImagePos(tileFrameSet, 0);

                var grayTile = findTileImagePos(tileFrameSet, 1);

                var roofTile = findTileImagePos(roofFrameSet, 1);

                var roofFrameTile = findTileImagePos(roofFrameSet, 0);

                // Draw the map
                var tiles = drawMap();

                // Button click function for showing frames on all possible tiles
                $(".showFrames").click(function () {
                    showAll = !showAll;
                    clearSelection(tiles);

                    if (showAll) {
                        activateAll(tiles);
                        drawOnActive(tiles);
                    }
                });

                // On change event for the zoom input
                $(".mapZoom").on("change", function () {
                    scale = $(".mapZoom").val();
                    clearSelection(tiles);
                    createCanvases();
                    tiles = drawMap();
                });

                // On change event for the alpha input
                $(".alphaRange").on("change", function () {
                    alpha = $(".alphaRange").val();
                    clearSelection(tiles);
                    createCanvases();
                    tiles = drawMap();
                });
            });
        },
        error: function (jqxhr, textStatus, error) {
            console.log("Request Failed: " + textStatus + ', ' + error);
        }
    });
});
});
