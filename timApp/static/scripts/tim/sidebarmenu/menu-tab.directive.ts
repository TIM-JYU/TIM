import type {Type} from "@angular/core";
import {Directive, ViewContainerRef} from "@angular/core";

export interface OnTabSelect {
    onSelect: () => void;
}

export interface TabEntry {
    id: string;
    icon: string;
    title: string;
    visible: () => boolean;
    component: Type<unknown>;
}

@Directive({
    selector: "[timMenuTab]",
    standalone: false,
})
export class MenuTabDirective {
    constructor(public vcr: ViewContainerRef) {}
}
