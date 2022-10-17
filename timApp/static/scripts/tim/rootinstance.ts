import type {RootCtrl} from "tim/timRoot";

export let rootInstance: RootCtrl | undefined;

export function setRoot(v: RootCtrl) {
    rootInstance = v;
}
