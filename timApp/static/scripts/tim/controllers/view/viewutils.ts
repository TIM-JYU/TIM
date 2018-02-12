import {Coords, nameofFactoryCtrl, nameofFactoryCtrl2} from "../../utils";
import {Paragraph} from "./parhelpers";
import {ViewCtrl} from "./viewctrl";

export type MenuFunction = ((e: Event, $par: Paragraph, coords?: Coords) => any);
export type MenuFunctionEntry = {func: MenuFunction; desc: string; show: boolean};
export type MenuFunctionCollection = MenuFunctionEntry[];
export const viewCtrlDot = nameofFactoryCtrl<ViewCtrl>();
export const parMenuDot = nameofFactoryCtrl2<ViewCtrl, "parmenuHandler">("parmenuHandler");
export const editingDot = nameofFactoryCtrl2<ViewCtrl, "editingHandler">("editingHandler");
export const notesDot = nameofFactoryCtrl2<ViewCtrl, "notesHandler">("notesHandler");

export function getEmptyCoords() {
    return {left: 0, top: 0};
}
