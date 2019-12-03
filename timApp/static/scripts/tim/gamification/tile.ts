import {IMapResponse} from "tim/gamification/IMapResponse";
import {ITileSet} from "tim/gamification/ITileSet";

export class Tile {
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
    private readonly scaleGetter: () => number;

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
     * @param scaleGetter Function for getting the current scale
     */
    constructor(json: IMapResponse,
                imageIndex: number,
                tileset: ITileSet,
                spreadsheet: HTMLImageElement,
                layerNo: number,
                dataIndex: number,
                x: number,
                y: number,
                scaleGetter: () => number) {
        this.scaleGetter = scaleGetter;
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
        const scale = this.scaleGetter();
        context.drawImage(this.spreadsheet,
            this.sourceX,
            this.sourceY,
            this.tileset.tilewidth,
            this.tileset.tileheight,
            (this.x + this.offsetX) * scale,
            (this.y + this.offsetY + this.heightCorr) * scale,
            this.tileset.tilewidth * scale,
            this.tileset.tileheight * scale);
    }

    // Check if point is inside the tile
    public isPointInside(x: number, y: number) {
        const scale = this.scaleGetter();
        return (x >= (this.x + this.offsetX) * scale &&
            x <= (this.x + this.tileset.tilewidth + this.offsetX) * scale &&
            y >= (this.y + this.offsetY + this.heightCorr) * scale &&
            y <= (this.y + this.tileset.tileheight + this.offsetY + this.heightCorr) * scale);
    }
}
