import {ViewCtrl} from "tim/document/viewctrl";

export let vctrlInstance: ViewCtrl | undefined;

export function setViewCtrl(v: ViewCtrl) {
    vctrlInstance = v;
}
