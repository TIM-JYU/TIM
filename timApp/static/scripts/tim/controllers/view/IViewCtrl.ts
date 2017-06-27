export type MenuFunction = ((e, $par: JQuery, coords?) => any);
type MenuFunctionEntry = {func: MenuFunction; desc: string; show: boolean};
export type MenuFunctionCollection = MenuFunctionEntry[];
