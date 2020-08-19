import {RootCtrl} from "./timRoot";

export let rootInstance: RootCtrl | undefined;

export function setRoot(v: RootCtrl) {
    rootInstance = v;
}
