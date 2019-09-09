import {$http} from "../util/ngimport";
import {Binding, to} from "../util/utils";
import {Tile} from "./tile";

import {IController, IRootElementService} from "angular";
import {timApp} from "../app";

export interface ITileSet {
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

export interface ITile {
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

interface IParsedData {
    lectures: [{
        id: number,
        name: string,
        link: string,
    }];
    demos: [{
        id: number;
        name: string;
        link: string;
        maxPoints: number;
        gotPoints: number;
    }];
    buttonText: string;
}

export interface ILayerData {
    [index: string]: number;
}

export interface IMapResponse {
    width: number;
    height: number;
    tilewidth: number;
    tileheight: number;
    layers: Array<{
        properties: {title: string, maxpoints: number, studentpoints: number, site: string},
        name: string,
        data: ILayerData,
    }>;
    tilesets: ITileSet[];
}

// A global variable because Tile-class uses this too.
export let scale: number = 0.5;
const defaultButtonText: string = "Show map";

export class GamificationMapCtrl implements IController {
    // noinspection JSUnusedLocalSymbols
    static $inject = ["$element"];
    private displayMap: boolean = false;
    private sources: string[] = [];
    private scaleInput = scale;

    // List of canvases that are used to help with drawing the full map
    private canvases: HTMLCanvasElement[] = [];
    // Used for event listener
    private topCanvas!: HTMLCanvasElement;
    // Used for dynamic drawing
    private middleCanvas!: HTMLCanvasElement;
    // Used for drawing the map
    private bottomCanvas!: HTMLCanvasElement;
    private json!: IMapResponse;
    private alpha: number = 0.2; // Alpha value for the building frames.
    private showAll: boolean = false; // Show all building frames.
    private tiles: ITile[] = []; // Array of tile objects on the map.
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
    private parsedData!: IParsedData;
    private buttonText: string = "";
    private errorMessage: string | undefined;
    private loading = false;

    constructor(protected element: IRootElementService) {
    }

    $onInit() {
        this.parsedData = JSON.parse(this.data) as IParsedData;
        this.buttonText = this.parsedData.buttonText;
        if (!this.buttonText) {
            this.buttonText = defaultButtonText;
        }
    }

    async loadMap() {
        // Configure data and generate the JSON map file.
        this.loading = true;
        const r = await to($http.post<IMapResponse>("/generateMap", this.parsedData));
        this.loading = false;
        if (r.ok) {
            this.errorMessage = undefined;
            this.json = r.result.data;
            // Fill the sources array with tileset image sources.
            for (const tileset of this.json.tilesets) {
                this.sources.push(tileset.image);
            }
            this.createCanvases();
            // Preload the images and draw the map.
            this.loadImages((p) => this.callback(p));
        } else {
            this.errorMessage = r.result.data.error;
        }

    }

    /**
     * Draws an info box.
     * @param x X coordinate for the box.
     * @param y Y coordinate for the box.
     * @param tile Clicked tile.
     */
    private drawInfo(x: number, y: number, tile: ITile) {
        let tscale = scale;
        if (tscale < 0.7) {
            tscale = 0.7;
        }
        // Size and position of the box
        const height = this.json.tileheight * 10 * tscale;
        const width = this.json.tilewidth * 8 * tscale;
        let rectX = x - this.json.tilewidth * 4 * scale;
        let rectY = y - this.json.tileheight * 6 * scale - height;

        if (rectY < this.json.tileheight * scale) {
            rectY = y + this.json.tileheight * scale;
        }

        if (rectX < this.json.tilewidth * scale) {
            rectX = this.json.tilewidth * scale;
        }

        if (rectX >= (this.json.tilewidth * this.json.width) * scale - width) {
            rectX = this.json.tilewidth * this.json.width * scale - this.json.tilewidth * scale - width;
        }

        // Set style settings for info box text elements.
        const title = this.element.find(".mapContainer .infoBoxTitle");
        const description = this.element.find(".mapContainer .infoBoxDescription");
        // noinspection TsLint
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
        // noinspection TsLint
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

        for (const layer of this.json.layers) {
            if (!titles.includes(layer.properties.title)) {
                totalMax += layer.properties.maxpoints;
                titles.push(layer.properties.title);
            }
            if (layer.name.length === 2) {
                totalPoints += layer.properties.studentpoints;
            }
        }

        // Set info box text
        title.html("<a href='" + this.json.layers[tile.layerNo].properties.site + "'>" +
            this.json.layers[tile.layerNo].properties.title + "</a>");
        if (this.json.layers[tile.layerNo].properties.maxpoints !== 0) {
            description.html("Dokumentin pisteet: " +
                rround(this.json.layers[tile.layerNo].properties.studentpoints, 2) + "/" +
                this.json.layers[tile.layerNo].properties.maxpoints + "<br>Kurssin pisteet yhteensä: " +
                rround(totalPoints, 2) + "/" + totalMax);
        } else {
            description.html("Kurssin pisteet yhteensä: " + rround(totalPoints, 2) + "/" + totalMax);
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
     * Function for drawing a frame of a building on given position.
     * @param tile Tile that the frame is drawn over.
     */
    private drawFrame(tile: ITile) {
        const context = this.middleCanvas.getContext("2d");
        if (!context) {
            return;
        }

        // if clicked on a "top tile", draw a full frame
        if (tile.tileset.properties != null &&
            tile.tileset.properties.frameProperty === 1 && !hasOwnProperty(this.json, tile, 1, 0)) {
            /*
             && this.json.layers[tile.layerNo + 1] && this.json.layers[tile.layerNo + 1].data &&
             !json.layers[tile.layerNo + 1].data.hasOwnProperty((tile.dataIndex).toString()))  {
             */
            context.globalAlpha = this.alpha;
            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                tile.y * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight * 2) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.roofImage, this.roofTile.x, this.roofTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight * 3) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.globalAlpha = 1;

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                tile.y * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight * 2) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.roofImage, this.roofFrameTile.x, this.roofFrameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight * 3) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);
            // If clicked on a bulding with one floor, draw two frame tiles and a roof frame
        } else if (tile.tileset.properties != null &&
            tile.tileset.properties.buildingProperty === 1 && // && this.json.layers[tile.layerNo + 1] &&

            !hasOwnProperty(this.json, tile, 1, -this.json.width) &&
            hasOwnProperty(this.json, tile, -1, 0)) {
            /*
             !json.layers[tile.layerNo + 1].data.hasOwnProperty((tile.dataIndex - this.json.width).toString()) &&
             this.json.layers[tile.layerNo - 1].data.hasOwnProperty((tile.dataIndex).toString())) {
             */
            context.globalAlpha = this.alpha;

            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight * 2) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.roofImage, this.roofTile.x, this.roofTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight * 3) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.globalAlpha = 1;

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight * 2) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.roofImage, this.roofFrameTile.x, this.roofFrameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight * 3) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);
            // If clicked on a bulding with two floors, draw one frame tile and a roof frame
        } else if (tile.tileset.properties != null &&
            tile.tileset.properties.buildingProperty === 1 && // this.json.layers[tile.layerNo + 1] &&
            !hasOwnProperty(this.json, tile, 1, -this.json.width) &&
            hasOwnProperty(this.json, tile, -1, this.json.width) &&
            hasOwnProperty(this.json, tile, -2, this.json.width)) {
            /*
             !json.layers[tile.layerNo + 1].data.hasOwnProperty((tile.dataIndex - this.json.width).toString()) &&
             this.json.layers[tile.layerNo - 1].data.hasOwnProperty((tile.dataIndex + this.json.width).toString()) &&
             this.json.layers[tile.layerNo - 2].data.hasOwnProperty((tile.dataIndex + this.json.width).toString())) {
             */
            context.globalAlpha = this.alpha;

            context.drawImage(this.tileImage, this.grayTile.x, this.grayTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.roofImage, this.roofTile.x, this.roofTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight * 2) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.globalAlpha = 1;

            context.drawImage(this.tileImage, this.frameTile.x, this.frameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.drawImage(this.roofImage, this.roofFrameTile.x, this.roofFrameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight * 2) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);
            // If clicked on a building with three floors and no roof, draw a roof frame
        } else if (tile.tileset.properties != null &&
            tile.tileset.properties.buildingProperty === 1 && // this.json.layers[tile.layerNo + 1] &&
            !hasOwnProperty(this.json, tile, 1, -this.json.width) &&
            hasOwnProperty(this.json, tile, -1, this.json.width) &&
            hasOwnProperty(this.json, tile, -2, this.json.width * 2) &&
            hasOwnProperty(this.json, tile, -3, this.json.width * 2)) {

            context.globalAlpha = this.alpha;

            context.drawImage(this.roofImage, this.roofTile.x, this.roofTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);

            context.globalAlpha = 1;

            context.drawImage(this.roofImage, this.roofFrameTile.x, this.roofFrameTile.y,
                this.tileFrameSet.tilewidth, this.tileFrameSet.tileheight, tile.x * scale,
                (tile.y - this.json.tileheight) * scale,
                this.tileFrameSet.tilewidth * scale, this.tileFrameSet.tileheight * scale);
        }

    }

    /**
     * Function for preloading the images.
     * @param callback Callback function.
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
     * Draw tiles according to data dictionary.
     * @param dict Dictionary holding the data for the tiles.
     * @param canvas Canvas to draw on.
     * @param layer Layer that is being drawn.
     */
    private drawTilesFromDict(dict: ILayerData, canvas: HTMLCanvasElement, layer: number) {
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
            const posIndex = +key;
            const posX = posIndex % this.json.width * this.json.tilewidth;
            const posY = Math.floor(posIndex / this.json.width) * this.json.tileheight;

            // Create tile object and draw it on the map
            const t = new Tile(this.json, dict[key], this.tileset, this.image, layer, posIndex, posX, posY);
            t.draw(canvas);
            this.tiles.push(t);
        }
    }

    /**
     * Function draws a map on canvases.
     * @returns {Array} All tile objects of the map.
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
        for (const canvas of this.canvases) {
            const context = this.bottomCanvas.getContext("2d");
            if (context) {
                context.drawImage(canvas, 0, 0);
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

    // noinspection JSUnusedLocalSymbols
    /**
     * Add click-event listener to the top canvas
     */
    private clickCanvas(event: MouseEvent) {
        // Add event listener for `click` events on top canvas.
        // this.topCanvas.addEventListener("click", function(event) {
        const pos = absolutePosition(this.topCanvas);
        const x = event.pageX - pos.left;
        const y = event.pageY - pos.top;

        for (const tile of this.tiles) {
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
     * Activate a cluster of "top tiles" and building tiles on the map.
     * @param tile A tile that is part of the cluster.
     */
    private activateCluster(tile: ITile) {
        tile.active = true;

        // Boolean for checking if iterated tile is next to the given tile
        let isNextTo;

        for (const t of this.tiles) {
            isNextTo = (t.x * scale <= (tile.x + this.json.tilewidth * 2) * scale &&
                t.x * scale >= (tile.x - this.json.tilewidth * 2) * scale &&
                (t.y * scale <= (tile.y + this.json.tileheight * 2) * scale &&
                    t.y * scale >= (tile.y - this.json.tileheight * 2) * scale));

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
     * Clear all selections on the map
     */
    private clearSelection() {
        // Remove previous frame canvas and create a new one
        this.element.find(".middleCanvas").remove();
        this.middleCanvas = document.createElement("canvas");

        // Set some attributes for the canvas
        this.middleCanvas.setAttribute("class", "middleCanvas");
        this.middleCanvas.setAttribute("style", "position: absolute; left: 0; top: 0; z-index: 1;");
        this.middleCanvas.width = this.json.width * this.json.tilewidth * scale;
        this.middleCanvas.height = this.json.height * this.json.tileheight + this.json.tileheight * 3 * scale;

        this.element.find(".mapContainer").append(this.middleCanvas);
        const title = this.element.find(".mapContainer .infoBoxTitle");
        const description = this.element.find(".mapContainer .infoBoxDescription");

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
            this.element.find(".topCanvas").remove();
            this.element.find(".middleCanvas").remove();
            this.element.find(".bottomCanvas").remove();
            this.canvases = [];
        }
        let canvas;
        // Canvases used to draw the final map
        for (let j = 0; j < 3; j++) {
            canvas = document.createElement("canvas");
            this.element.find(".mapContainer").append(canvas);

            // Set some attributes for the canvas
            if (j === 0) {
                canvas.setAttribute("class", "bottomCanvas");
                canvas.setAttribute("style", "padding-bottom: 1em; z-index: " + j + ";");
                this.bottomCanvas = canvas;
            } else if (j === 1) {
                canvas.setAttribute("class", "middleCanvas");
                canvas.setAttribute("style", "position: absolute; left: 0; top: 0; z-index: " + j + ";");
                this.middleCanvas = canvas;
            } else {
                canvas.setAttribute("class", "topCanvas");
                canvas.setAttribute("style", "position: absolute; left: 0; top: 0; z-index: " + j + ";");
                this.topCanvas = canvas;
            }
            canvas.width = this.json.width * this.json.tilewidth * scale;
            canvas.height = (this.json.height * this.json.tileheight + this.json.tileheight * 3) * scale;
        }

        // Canvases used to draw the layers.
        for (const layer of this.json.layers) {
            canvas = document.createElement("canvas");

            canvas.width = this.json.width * this.json.tilewidth * scale;
            canvas.height = (this.json.height * this.json.tileheight + this.json.tileheight * 3) * scale;
            this.canvases.push(canvas);
        }
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
        this.frameTile = findTileImagePos(this.tileFrameSet, 0);
        this.grayTile = findTileImagePos(this.tileFrameSet, 1);
        this.roofTile = findTileImagePos(this.roofFrameSet, 1);
        this.roofFrameTile = findTileImagePos(this.roofFrameSet, 0);
        // Draw the map
        this.drawMap();
    }

    // noinspection JSUnusedLocalSymbols
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

    // noinspection JSUnusedLocalSymbols
    /**
     * Called when user changes settings.
     */
    private update() {
        // Change global variable so tiles get the new value.
        scale = this.scaleInput;
        // console.log(`showAll: ${this.showAll}, scale: ${scale}, alpha: ${this.alpha}`);
        this.clearSelection();
        this.createCanvases();
        this.drawMap();
    }

    // noinspection JSUnusedLocalSymbols
    /**
     * Toggle map and UI.
     */
    private async clickShowMap() {
        if (!this.json && !this.errorMessage) {
            await this.loadMap();
        }
        this.displayMap = !this.displayMap;
    }
}

/**
 * @param {number} d
 * @param {number} des
 * @returns {number}
 */
function rround(d: number, des: number) {
    const mult = Math.pow(10, des);
    return Math.round(d * mult) / mult;
}

/**
 * Get absolute position of given element
 * @param element given DOM element
 * @returns {{top: number, left: number}} left and top position
 */
function absolutePosition(element: HTMLElement) {
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
        left,
        top,
    };
}

/**
 * Find the position of a tile on spreadsheet
 * @param tileset Used tileset
 * @param offset Number of tiles that come after the searched tile on the spreadsheet
 * @returns {{x: *, y: *}} Position of the tile on spreadsheet
 */
function findTileImagePos(tileset: ITileSet, offset: number): {x: number, y: number} {
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

/**
 * @param {IMapResponse} json
 * @param {ITile} tile
 * @param {number} layerdelta
 * @param {number} datadelta
 * @returns {boolean}
 */
function hasOwnProperty(json: IMapResponse, tile: ITile, layerdelta: number, datadelta: number): boolean {
    if (!json.layers[tile.layerNo + layerdelta]) {
        return false;
    }
    if (!json.layers[tile.layerNo + layerdelta].data) {
        return false;
    }
    return (json.layers[tile.layerNo + layerdelta].data.hasOwnProperty((tile.dataIndex + datadelta).toString()));
}

timApp.component("gamificationMap", {
    bindings: {
        data: "@",
    },
    controller: GamificationMapCtrl, template: `
<div class="no-highlight hidden-print">
    <button class="timButton"
            ng-click="$ctrl.clickShowMap()"
            ng-disabled="$ctrl.loading">{{$ctrl.buttonText}}</button>
    <tim-loading ng-if="$ctrl.loading"></tim-loading>
    <tim-alert ng-if="$ctrl.errorMessage">
        {{$ctrl.errorMessage}}
    </tim-alert>
    <div class="uiContainer" ng-show="$ctrl.displayMap && !$ctrl.errorMessage">
        <table>
            <tr><td>Goals <input type="checkbox" class="showFrames" ng-click="$ctrl.clickShowFrames()"></td></tr>
            <tr><td>Zoom </td><td><input type="range" max="1" min="0.3" step="0.05" class="mapZoom"
            ng-model="$ctrl.scaleInput" ng-change="$ctrl.update()"></td></tr>
            <tr><td>Goal Transparency</td><td><input type="range" max="1" min="0" step="0.05" class="alphaRange"
            ng-model="$ctrl.alpha" ng-change="$ctrl.update()"></td></tr>
        </table>
    </div>
    <div class="mapContainer" ng-show="$ctrl.displayMap" ng-click="$ctrl.clickCanvas($event)"
        style="position:relative; z-index:0;">
        <p class="infoBoxTitle" style="position: absolute;"></p>
        <p class="infoBoxDescription" style="position: absolute;"></p>
    </div>
</div>
`,
});
