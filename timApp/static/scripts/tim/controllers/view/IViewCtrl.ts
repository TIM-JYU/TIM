import {Coords} from "../../utils";

export type MenuFunction = ((e: Event, $par: JQuery, coords?: Coords) => any);
export type MenuFunctionEntry = {func: MenuFunction; desc: string; show: boolean};
export type MenuFunctionCollection = MenuFunctionEntry[];
