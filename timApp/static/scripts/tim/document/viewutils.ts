import {Coords} from "tim/util/utils";

export type MenuFunction = (e: MouseEvent, coords?: Coords) => unknown;

export interface IMenuFunctionEntry {
    func: MenuFunction;
    desc: string;
    show: boolean;
    closeAfter?: boolean;
    allowAsDefault?: boolean;
    spacingBefore?: number;
}

export type MenuFunctionList = IMenuFunctionEntry[];

export function getEmptyCoords() {
    return {left: 0, top: 0};
}

export interface ICtrlWithMenuFunctionEntry {
    getMenuEntry: () => IMenuFunctionEntry;
}
