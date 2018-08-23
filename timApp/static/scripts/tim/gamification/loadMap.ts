import $ from "jquery";
import {$http} from "../util/ngimport";
import {IController} from "angular";
import {SearchBoxCtrl} from "../search/searchBox";
import {timApp} from "../app";
import {Binding} from "../util/utils";

interface ITileSet {
    tileoffset: {x: number, y: number};
    columns: number;
    firstgid: number;
    image: string;
    imageheight: number;
    imagewidth: number;
    margin: number;
    name: string;
    properties: {buildingProperty: number, frameProperty: number};
    propertytypes: {buildingProperty: string, frameProperty: string};
    spacing: number;
    tilecount: number;
    tileheight: number;
    tilewidth: number;
    transparentcolor: string;
}

interface ITile {
    frame: boolean;
    tileset: ITileSet;
    y: number;
    x: number;
    dataIndex: number;
    layerNo: number;
    active: boolean;

    draw(canvas: HTMLCanvasElement): void;
    isPointInside(x: number, y: number): boolean;
}

interface ILayer {
    data: number[];
    height: number;
    name: string;
    opacity: number;
    type: string;
    visible: true;
    width: number;
    x: number;
    y: number;
}

interface LayerData {[index: string]: number; }

interface IMapResponse {
    width: number;
    height: number;
    tilewidth: number;
    tileheight: number;
    layers: Array<{
        properties: {title: string, maxpoints: number, studentpoints: number, site: string},
        name: string,
        data: LayerData,
    }>;
    tilesets: ITileSet[];
}

export class GamificationMapCtrl implements IController {
    private scale: number = 0.5; // Scale of the map
    private displayMap: boolean = false;
    private sources: string[] = [];

    // List of canvases that are used to help with drawing the full map
    private canvases: HTMLCanvasElement[] = [];
    // Used for event listener
    private topCanvas!: HTMLCanvasElement;
    // Used for dynamic drawing
    private middleCanvas!: HTMLCanvasElement;
    // Used for drawing the map
    private bottomCanvas!: HTMLCanvasElement;
    private json!: IMapResponse;
    private alpha: number = 0.2; // Alpha value for the building frames
    private showAll: boolean = false; // Show all building frames
    private tiles: ITile[] = []; // Array of tile objects on the map
    private images: {[index: number]: HTMLImageElement} = {};

    // Positions for tiles used in the building frames
    private tileFrameSet!: ITileSet;
    private tileImage!: HTMLImageElement;

    // Spreadsheet and tileset that tile uses
    private image!: HTMLImageElement;
    private tileset!: ITileSet;

    private grayTile!: {x: number, y: number};
    private roofTile!: {x: number, y: number};
    private frameTile!: {x: number, y: number};
    private roofFrameTile!: {x: number, y: number};

    private roofFrameSet!: ITileSet;
    private roofImage!: HTMLImageElement;
    private data!: Binding<string, "@">;

    async $onInit() {
        // Get the div that will hold canvases
        const $container = $(".mapContainer");

        // Get the JSON map file
        const response = await $http.post<IMapResponse>("/generateMap", JSON.parse(this.data));
        this.json = response.data;

        // don't try to do anything if there are no maps on the page
        if ($container.length === 0) {
            return;
        }

        // separate scope is intentional
        {
            const container = $container[0];
            $container.css({
                "z-index": 0,
                "display": "none",
                "position": "relative",
            });

            // Title and description elements for the info box
            const title = $("<p></p>");
            title.addClass("infoBoxTitle");
            title.css("position", "absolute");
            title.appendTo(container);

            const description = $("<p></p>");
            description.addClass("infoBoxDescription");
            description.css("position", "absolute");
            description.appendTo(container);
        }


        // Fill the sources array with tileset image sources
        for (let k = 0; k < this.json.tilesets.length; k++) {
            this.sources.push(this.json.tilesets[k].image);
        }

        this.createCanvases();

        // Preload the images and draw the map
        this.loadImages((p) => this.callback(p));
    }

    /**
     * Draws an info box
     * @param x X coordinate for the box
     * @param y Y coordinate for the box
     * @param tile Clicked tile
     */
    private drawInfo(x: number, y: number, tile: ITile) {
        let tscale = this.scale;
        if (tscale < 0.7) {
            tscale = 0.7;
        }
        // Size and position of the box
        const height = this.json.tileheight * 10 * tscale;
        const width = this.json.tilewidth * 8 * tscale;
        let rectX = x - this.json.tilewidth * 4 * this.scale;
        let rectY = y - this.json.tileheight * 6 * this.scale - height;

        if (rectY < this.json.tileheight * this.scale) {
            rectY = y + this.json.tileheight * this.scale;
        }

        if (rectX < this.json.tilewidth * this.scale) {
            rectX = this.json.tilewidth * this.scale;
        }

        if (rectX >= (this.json.tilewidth * this.json.width) * this.scale - width) {
            rectX = this.json.tilewidth * this.json.width * this.scale - this.json.tilewidth * this.scale - width;
        }

        // Set style settings for info box text elements
        const title = $(".mapContainer .infoBoxTitle");
        const description = $(".mapContainer .infoBoxDescription");
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
            "z-index": 4,
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
            "z-index": 4,
        });

        let totalMax = 0;
        let totalPoints = 0;
        const titles: string[] = [];

        for (let i = 0; i < this.json.layers.length; i++) {
            if (!titles.includes(this.json.layers[i].properties.title)) {
                totalMax += this.json.layers[i].properties.maxpoints;
                titles.push(this.json.layers[i].properties.title);
            }
            if (this.json.layers[i].name.length === 2) {
                totalPoints += this.json.layers[i].properties.studentpoints;
            }
        }

        // Set info box text

        title.html("<a href='" + this.json.layers[tile.layerNo].properties.site + "'>" +
            this.json.layers[tile.layerNo].properties.title + "</a>");
        if (this.json.layers[tile.layerNo].properties.maxpoints !== 0) {
            description.html("Dokumentin pisteet: " +
                this.rround(this.json.layers[tile.layerNo].properties.studentpoints, 2) + "/" +
                this.json.layers[tile.layerNo].properties.maxpoints + "<br>Kurssin pisteet yhteensä: " +
                this.rround(totalPoints, 2) + "/" + totalMax);
        } else {
            description.html("Kurssin pisteet yhteensä: " + this.rround(totalPoints, 2) + "/" + totalMax);
        }

        // Draw the box on the canvas
        const context = this.middleCanvas.getContext("2d");

        if (!context) {
            return;
        }

        context.beginPath();
        context.fillStyle = "rgba(132,225,225,0.8)";
        context.fillRect(rectX, rectY, width, height);
    }

    /**
     * Function for drawing a frame of a building on given position
     * @param tile Tile that the frame is drawn over
     */
    private drawFrame(tile: ITile) {
        const context = this.middleCanvas.getContext("2d");
        if (!context) {
            return;
        }

        // if clicked on a "top tile", draw a full frame
        if (tile.tileset.properties != null &&
            tile.tileset.properties.frameProperty === 1 && !this.hasOwnProperty(
                this.json, tile, 1, 0)) {
            /*
             && this.json.layers[tile.layerNo + 1] && this.json.layers[tile.layerNo + 1].data &&
             !json.layers[tile.layerNo + 1].data.hasOwnProperty((tile.dataIndex).toString()))  {
             */
            context.globalAlpha = this.alpha;
            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                tile.y * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight * 2) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.roofImage, this.roofTile.x, this.roofTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight * 3) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.globalAlpha = 1;

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                tile.y * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight * 2) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.roofImage, this.roofFrameTile.x, this.roofFrameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight * 3) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);
            // If clicked on a bulding with one floor, draw two frame tiles and a roof frame
        } else if (tile.tileset.properties != null &&
            tile.tileset.properties.buildingProperty === 1 && // && this.json.layers[tile.layerNo + 1] &&

            !this.hasOwnProperty(this.json, tile, 1, -this.json.width) &&
            this.hasOwnProperty(this.json, tile, -1, 0)) {
            /*
             !json.layers[tile.layerNo + 1].data.hasOwnProperty((tile.dataIndex - this.json.width).toString()) &&
             this.json.layers[tile.layerNo - 1].data.hasOwnProperty((tile.dataIndex).toString())) {
             */
            context.globalAlpha = this.alpha;

            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight * 2) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.roofImage, this.roofTile.x, this.roofTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight * 3) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.globalAlpha = 1;

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight * 2) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.roofImage, this.roofFrameTile.x, this.roofFrameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight * 3) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);
            // If clicked on a bulding with two floors, draw one frame tile and a roof frame
        } else if (tile.tileset.properties != null &&
            tile.tileset.properties.buildingProperty === 1 && // this.json.layers[tile.layerNo + 1] &&
            !this.hasOwnProperty(this.json, tile, 1, -this.json.width) &&
            this.hasOwnProperty(this.json, tile, -1, this.json.width) &&
            this.hasOwnProperty(this.json, tile, -2, this.json.width)) {
            /*
             !json.layers[tile.layerNo + 1].data.hasOwnProperty((tile.dataIndex - this.json.width).toString()) &&
             this.json.layers[tile.layerNo - 1].data.hasOwnProperty((tile.dataIndex + this.json.width).toString()) &&
             this.json.layers[tile.layerNo - 2].data.hasOwnProperty((tile.dataIndex + this.json.width).toString())) {
             */
            context.globalAlpha = this.alpha;

            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.roofImage, this.roofTile.x, this.roofTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight * 2) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.globalAlpha = 1;

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.drawImage(this.roofImage, this.roofFrameTile.x, this.roofFrameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight * 2) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);
            // If clicked on a building with three floors and no roof, draw a roof frame
        } else if (tile.tileset.properties != null &&
            tile.tileset.properties.buildingProperty == 1 && // this.json.layers[tile.layerNo + 1] &&
            !this.hasOwnProperty(this.json, tile, 1, -this.json.width) &&
            this.hasOwnProperty(this.json, tile, -1, this.json.width) &&
            this.hasOwnProperty(this.json, tile, -2, this.json.width * 2) &&
            this.hasOwnProperty(this.json, tile, -3, this.json.width * 2)) {

            context.globalAlpha = this.alpha;

            context.drawImage(this.roofImage, this.roofTile.x, this.roofTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);

            context.globalAlpha = 1;

            context.drawImage(this.roofImage, this.roofFrameTile.x, this.roofFrameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * this.scale,
                (tile.y - this.json.tileheight) * this.scale,
                this.tileFrameSet.tilewidth * this.scale, this.tileFrameSet.tileheight * this.scale);
        }

    }

    /**
     * Function for preloading the images
     * @param sources File paths to the images
     * @param callback Callback function
     */
    private loadImages(callback: (images: {[index: number]: HTMLImageElement}) => void) {
        const images: {[index: number]: HTMLImageElement} = {};
        let loadedImages = 0;
        const onload = () => {
            if (++loadedImages >= this.sources.length) {
                callback(images);
            }
        };
        for (let i = 0; i < this.sources.length; i++) {
            images[i] = new Image();
            images[i].onload = onload;
            images[i].src = this.sources[i].replace("..", "/static");
        }
    }

    /**
     * Draw tiles according to data dictionary
     * @param dict Dictionary holding the data for the tiles
     * @param canvas Canvas to draw on
     * @param layer Layer that is being drawn
     */
    private drawTilesFromDict(dict: LayerData, canvas: HTMLCanvasElement, layer: number) {
        // Get all keys from the data dictionarty
        const keys: string[] = Object.keys(dict);

        // Go through every key and select appropriate spreadsheet for the tile
        for (const key of keys) {
            for (let j = 0; j < this.json.tilesets.length; j++) {
                if (this.json.tilesets[j].firstgid + this.json.tilesets[j].tilecount > dict[key]) {
                    this.tileset = this.json.tilesets[j];
                    this.image = this.images[j];
                    break;
                }
            }
            // Calculate the position for the tile
            const posIndex = parseInt(key);
            const posX = posIndex % this.json.width * this.json.tilewidth;
            const posY = Math.floor(posIndex / this.json.width) * this.json.tileheight;

            // Create tile object and draw it on the map
            const t = new Tile(this.json, dict[key], this.tileset, this.image, layer, posIndex, posX, posY);
            t.draw(canvas);
            this.tiles.push(t);
        }
    }

    /**
     * Function draws a map on canvases
     * @returns {Array} All tile objects of the map
     */
    private drawMap() {
        // Current layer
        let layer;

        // Current canvas
        let c;

        // Go through every layer of the map in reverse order so that the draw order will be correct
        for (let k = this.json.layers.length - 1; k >= 0; k--) {
            layer = this.json.layers[k];

            // Helper canvas that the layer is drawn on
            c = this.canvases[k];

            this.drawTilesFromDict(layer.data, c, k);
        }

        // Draw images in helper canvases on a single canvas
        for (let n = 0; n < this.canvases.length; n++) {
            const context = this.bottomCanvas.getContext("2d");
            if (context) {
                context.drawImage(this.canvases[n], 0, 0);
            }
        }

        // View all frames if required
        if (this.showAll) {
            this.activateAll();
            this.drawOnActive();
        }
    }

    /**
     * Draws a frame on all active tiles
     */
    private drawOnActive() {
        for (const t of this.tiles) {
            if (t.active && !t.frame) {
                t.frame = true;
                this.drawFrame(t);
            }
        }
    }

    private rround(d: number, des: number) {
        const mult = Math.pow(10, des);
        return Math.round(d * mult) / mult;
    }

    /**
     * Add click-event listener to the top canvas
     */
    private canvasClicked(event: MouseEvent) {
        // Add event listener for `click` events on top canvas.
        // this.topCanvas.addEventListener("click", function(event) {
        const pos = this.absolutePosition(this.topCanvas);
        const x = event.pageX - pos.left;
        const y = event.pageY - pos.top;

        for (let i = 0; i < this.tiles.length; i++) {
            const tile = this.tiles[i];
            // Check if clicked tile
            if (tile.isPointInside(x, y)) {
                // Clear earlier selections
                this.clearSelection();

                // If clicked tile is a "top tile" or a building
                if (tile.tileset.properties != null &&
                    (tile.tileset.properties.frameProperty === 1 ||
                        tile.tileset.properties.buildingProperty === 1 ||
                        tile.tileset.properties.buildingProperty === 2)) {
                    if (!tile.active) {
                        if (!this.showAll) {
                            // Activate clicked tile cluster
                            this.activateCluster(tile);
                        } else {
                            this.activateAll();
                        }

                        // Draw a frame on activated tiles
                        this.drawOnActive();
                    }
                    // Draw info box over the clicked point
                    this.drawInfo(x, y, tile);
                } else if (tile.tileset.properties.buildingProperty === 0 &&
                    tile.tileset.properties.frameProperty === 0) {
                    if (this.showAll) {
                        this.activateAll();
                        this.drawOnActive();
                    }
                }
                break;
            }
        }
    }

    /**
     * Activate a cluster of "top tiles" and building tiles on the map
     * @param tile A tile that is part of the cluster
     */
    private activateCluster(tile: ITile) {
        tile.active = true;

        // Boolean for checking if iterated tile is next to the given tile
        let isNextTo;

        for (const t of this.tiles) {
            isNextTo = (t.x * this.scale <= (tile.x + this.json.tilewidth * 2) * this.scale &&
                t.x * this.scale >= (tile.x - this.json.tilewidth * 2) * this.scale &&
                (t.y * this.scale <= (tile.y + this.json.tileheight * 2) * this.scale &&
                    t.y * this.scale >= (tile.y - this.json.tileheight * 2) * this.scale));

            // If the iterated tile is a building or a "top tile" and it is next to
            // or above the given tile, call this function in  recursion.
            if (t.tileset.properties != null &&
                (t.tileset.properties.buildingProperty === 1 ||
                    t.tileset.properties.frameProperty === 1) && !t.active &&
                (isNextTo || ((t.layerNo === tile.layerNo + 1 ||
                    t.layerNo === tile.layerNo - 1) &&
                    t.x === tile.x && (t.y === tile.y - this.json.tileheight ||
                        t.y === tile.y + this.json.tileheight)))) {
                this.activateCluster(t);
            }
        }
    }

    /**
     * Activate all "top tiles" and building tiles on the map.
     * @param tiles All tiles on the map
     */
    private activateAll() {
        for (const t of this.tiles) {
            if (t.tileset.properties != null &&
                (t.tileset.properties.buildingProperty === 1 ||
                    t.tileset.properties.frameProperty === 1) && !t.active) {
                t.active = true;
            }
        }
    }

    /**
     * Get absolute position of given element
     * @param element given DOM element
     * @returns {{top: number, left: number}} left and top position
     */
    private absolutePosition(element: HTMLElement) {
        let top = 0;
        let left = 0;
        let e = element;
        do {
            top += e.offsetTop || 0;
            left += e.offsetLeft || 0;
            const p = e.offsetParent;
            if (!(p instanceof HTMLElement)) {
                break;
            }
            e = p;
        } while (e);

        return {
            top,
            left,
        };
    }

    /**
     * Clear all selections on the map
     */
    private clearSelection() {
        // Remove previous frame canvas and create a new one
        $("#middleCanvas").remove();
        this.middleCanvas = document.createElement("canvas");

        // Set some attributes for the canvas
        this.middleCanvas.setAttribute("id", "middleCanvas");
        this.middleCanvas.setAttribute("style", "position: absolute; left: 0; top: 0; z-index: 1;");
        this.middleCanvas.width = this.json.width * this.json.tilewidth * this.scale;
        this.middleCanvas.height = this.json.height * this.json.tileheight + this.json.tileheight * 3 * this.scale;

        $(".mapContainer").append(this.middleCanvas);
        const title = $(".mapContainer .infoBoxTitle");
        const description = $(".mapContainer .infoBoxDescription");

        // Deactivate currently active tiles
        for (const t of this.tiles) {
            t.active = false;
            t.frame = false;
        }

        // Clear info box text
        title.html("");
        description.html("");
    }

    /**
     * Create canvases for drawing the map, frames and event listeners
     */
    private createCanvases() {
        // If earlier canvases exist, remove them
        if (this.canvases.length > 0) {
            $("#topCanvas").remove();
            $("#middleCanvas").remove();
            $("#bottomCanvas").remove();
            this.canvases = [];
        }

        let canvas;

        // Canvases used to draw the final map
        for (let j = 0; j < 3; j++) {
            canvas = document.createElement("canvas");
            $(".mapContainer").append(canvas);

            // Set some attributes for the canvas
            if (j === 0) {
                canvas.setAttribute("id", "bottomCanvas");
                canvas.setAttribute("style", "padding-bottom: 1em; z-index: " + j + ";");
                this.bottomCanvas = canvas;
            } else if (j === 1) {
                canvas.setAttribute("id", "middleCanvas");
                canvas.setAttribute("style", "position: absolute; left: 0; top: 0; z-index: " + j + ";");
                this.middleCanvas = canvas;
            } else {
                canvas.setAttribute("id", "topCanvas");
                canvas.setAttribute("style", "position: absolute; left: 0; top: 0; z-index: " + j + ";");
                this.topCanvas = canvas;
            }
            canvas.width = this.json.width * this.json.tilewidth * this.scale;
            canvas.height = (this.json.height * this.json.tileheight + this.json.tileheight * 3) * this.scale;
        }

        // Canvases used to draw the layers
        for (let j = 0; j < this.json.layers.length; j++) {
            canvas = document.createElement("canvas");

            // Set some attributes for the canvas and get it's context
            canvas.setAttribute("id", "layer" + j);
            canvas.width = this.json.width * this.json.tilewidth * this.scale;
            canvas.height = (this.json.height * this.json.tileheight + this.json.tileheight * 3) * this.scale;
            this.canvases.push(canvas);
        }
    }

    /**
     * Find the position of a tile on spreadsheet
     * @param tileset Used tileset
     * @param offset Number of tiles that come after the searched tile on the spreadsheet
     * @returns {{x: *, y: *}} Position of the tile on spreadsheet
     */
    private findTileImagePos(tileset: ITileSet, offset: number): {x: number, y: number} {
        let currentX = 0;
        let currentY = 0;

        let x = 0;
        let y = 0;

        // Find the position of gray tile on a spreadsheet
        for (let j = 0; j < tileset.columns - offset; j++) {
            x = currentX++ * tileset.tilewidth;
            y = currentY * tileset.tileheight;

            if (currentX === tileset.columns) {
                currentX = 0;
                currentY++;
            }
        }

        return {
            x,
            y,
        };
    }

    private hasOwnProperty(json: IMapResponse, tile: ITile, layerdelta: number, datadelta: number) {
        if (!json.layers[tile.layerNo + layerdelta]) {
            return false;
        }
        if (!json.layers[tile.layerNo + layerdelta].data) {
            return false;
        }
        return (json.layers[tile.layerNo + layerdelta].data.hasOwnProperty((tile.dataIndex + datadelta).toString()));
    }

    private callback(images: {[index: number]: HTMLImageElement}) {
        this.images = images;
        // Find tilesets and spreadsheets needed for drawing a frame
        for (let j = 0; j < this.json.tilesets.length; j++) {
            if (this.json.tilesets[j].properties != null && this.json.tilesets[j].properties.frameProperty === 2) {
                this.tileFrameSet = this.json.tilesets[j];
                this.tileImage = this.images[j];
            }
            if (this.json.tilesets[j].properties != null && this.json.tilesets[j].properties.frameProperty === 3) {
                this.roofFrameSet = this.json.tilesets[j];
                this.roofImage = this.images[j];
            }
        }
        // Tile images used to draw the building frames
        const frameTile = this.findTileImagePos(this.tileFrameSet!, 0);
        this.grayTile = this.findTileImagePos(this.tileFrameSet!, 1);
        this.roofTile = this.findTileImagePos(this.roofFrameSet!, 1);
        const roofFrameTile = this.findTileImagePos(this.roofFrameSet!, 0);
        // Draw the map
        this.drawMap();
    }

    /**
     * Button click function for showing frames on all possible tiles.
     */
    private clickShowFrames() {
        this.showAll = !this.showAll;
        this.clearSelection();

        if (this.showAll) {
            this.activateAll();
            this.drawOnActive();
        }
    }

    private update() {
        console.log("scale: " + this.scale + ", alpha: " + this.alpha);
        this.clearSelection();
        this.createCanvases();
        this.drawMap();
    }

    private clickShowMap() {
        console.log("Click!");
        const map = $(".mapContainer")[0];
        const ui = $(".uiContainer")[0];
        console.log(map);
        console.log(ui);
        if (!this.displayMap) {
            this.displayMap = true;
            map.setAttribute("style", "z-index: 0; position: relative;");
            ui.setAttribute("style", "");

        } else {
            this.displayMap = false;
            map.setAttribute("style", "z-index: 0; display: none; position: relative;");
            ui.setAttribute("style", "display: none;");
        }
    }
}

timApp.component("gamificationMap", {
bindings: {
    data: "@",
},
controller: GamificationMapCtrl, template: `
<button class="btn showMap" ng-click="$ctrl.clickShowMap()">Show map</button>
<div class="uiContainer" ng-show="$ctrl.displayMap">
    <table>
        <tr><td>Goals <input type="checkbox" class="showFrames" ng-click="$ctrl.clickShowFrames()"></td></tr>
        <tr><td>Zoom </td><td><input type="range" max="1" min="0.3" step="0.05" class="mapZoom"
        ng-model="$ctrl.scale" ng-change="$ctrl.update()"></td></tr>
        <tr><td>Goal Transparency</td><td><input type="range" max="1" min="0.3" step="0.05" class="alphaRange"
        ng-model="$ctrl.alpha" ng-change="$ctrl.update()"></td></tr>
    </table>
</div>
<div class="mapContainer" ng-show="$ctrl.displayMap"></div>
`,
});

class Tile {
    public imageIndex: number;
    public tileset: ITileSet;
    public spreadsheet: HTMLImageElement;
    public layerNo: number;
    public dataIndex: number;
    public x: number;
    public y: number;
    public active: boolean;
    public frame: boolean;
    public heightCorr: number;
    public offsetX: number;
    public offsetY: number;
    public sourceX: number;
    public sourceY: number;
    private scale: number = 0.5;

    /**
     * Tile object constructor
     * @param json generateMap response
     * @param imageIndex Index of the picture on the spreadsheet
     * @param tileset Tiles tileset
     * @param spreadsheet Tiles spreadsheet
     * @param layerNo Index of the layer that the tile is drawn to
     * @param dataIndex Index of the tile in layer data
     * @param x Tiles x-coordinate on the map
     * @param y Tiles y-coordinate on the map
     */
    constructor(json: IMapResponse,
                imageIndex: number,
                tileset: ITileSet,
                spreadsheet: HTMLImageElement,
                layerNo: number,
                dataIndex: number,
                x: number,
                y: number) {

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

        if (this.tileset.tileoffset != null) {
            this.offsetX = this.tileset.tileoffset.x;
            this.offsetY = -this.tileset.tileoffset.y;
        }

        // Current position on the tileset image
        let currentY = 0;
        let currentX = 0;
        this.sourceX = 0;
        this.sourceY = 0;

        // Find the correct position on the tileset image
        for (let j = this.tileset.firstgid; j < this.imageIndex + 1; j++) {
            this.sourceX = currentX++ * this.tileset.tilewidth;
            this.sourceY = currentY * this.tileset.tileheight;

            if (currentX === this.tileset.columns) {
                currentX = 0;
                currentY++;
            }
        }
    }

    // Draw the tile
    public draw(canvas: HTMLCanvasElement) {
        const context = canvas.getContext("2d");
        if (!context) {
            return;
        }

        context.drawImage(this.spreadsheet,
            this.sourceX,
            this.sourceY,
            this.tileset.tilewidth,
            this.tileset.tileheight,
            (this.x + this.offsetX) * this.scale,
            (this.y + this.offsetY + this.heightCorr) * this.scale,
            this.tileset.tilewidth * this.scale,
            this.tileset.tileheight * this.scale);
    }

    // Check if point is inside the tile
    public isPointInside(x: number, y: number) {
        return (x >= (this.x + this.offsetX) * this.scale &&
            x <= (this.x + this.tileset.tilewidth + this.offsetX) * this.scale &&
            y >= (this.y + this.offsetY + this.heightCorr) * this.scale &&
            y <= (this.y + this.tileset.tileheight + this.offsetY + this.heightCorr) * this.scale);
    }
}