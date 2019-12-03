import {ILayerData} from "tim/gamification/ILayerData";
import {ITileSet} from "tim/gamification/ITileSet";

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
