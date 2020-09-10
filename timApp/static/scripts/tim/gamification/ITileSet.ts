export interface ITileSet {
    tileoffset: {x: number; y: number};
    columns: number;
    firstgid: number;
    image: string;
    imageheight: number;
    imagewidth: number;
    margin: number;
    name: string;
    properties: {buildingProperty: number; frameProperty: number};
    propertytypes: {buildingProperty: string; frameProperty: string};
    spacing: number;
    tilecount: number;
    tileheight: number;
    tilewidth: number;
    transparentcolor: string;
}
