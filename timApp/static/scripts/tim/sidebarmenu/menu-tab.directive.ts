import {Directive, Type, ViewContainerRef} from "@angular/core";

export interface IMenuTab {
    entry: TabEntry;
    onSelect?: () => void;
}

export interface TabEntry {
    tabType: Type<IMenuTab>;
    icon: string;
    title: string;
    visible: boolean;
}

@Directive({
    selector: "[timMenuTab]",
})
export class MenuTabDirective {
    constructor(public vcr: ViewContainerRef) {
    }
}
