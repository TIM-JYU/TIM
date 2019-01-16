import {Coords} from "../util/utils";
import {Paragraph} from "./parhelpers";

export type MenuFunction = ((e: JQueryEventObject, par: Paragraph, coords?: Coords) => any);

export interface MenuFunctionEntry {
    func: MenuFunction;
    desc: string;
    show: boolean;
    closeAfter?: boolean;
}

export type MenuFunctionList = MenuFunctionEntry[];

export function getEmptyCoords() {
    return {left: 0, top: 0};
}
