import type {Type} from "@angular/core";
import {Directive, ViewContainerRef} from "@angular/core";

export interface OnTabSelect {
    onSelect: () => void;
}

// TODO: Figure out why lazy loading breaks AngularJS dialogs in production (or port the dialogs to Angular as well)
export interface TabEntry {
    id: string;
    icon: string;
    title: string;
    visible: () => boolean;
    component: Type<unknown>;
}

@Directive({
    selector: "[timMenuTab]",
})
export class MenuTabDirective {
    constructor(public vcr: ViewContainerRef) {}
}
