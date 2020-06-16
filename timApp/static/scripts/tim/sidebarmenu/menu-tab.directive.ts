import {Directive, Type, ViewContainerRef} from "@angular/core";

export interface IMenuTab {
    entry: TabEntry;
    onSelect?: () => void;
}

export interface TabEntry {
    id: string;
    icon: string;
    title: string;
    visible: () => boolean;
    importComponent: () => Promise<Type<IMenuTab>>;
}

@Directive({
    selector: "[timMenuTab]",
})
export class MenuTabDirective {
    constructor(public vcr: ViewContainerRef) {
    }
}
