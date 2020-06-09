import {Directive, Type, ViewContainerRef} from "@angular/core";

export interface IMenuTab {
  entry: TabEntry;
}

export interface TabEntry {
  tabType: Type<IMenuTab>;
  icon: string;
  title: string;
}

@Directive({
  selector: "[timMenuTab]",
})
export class MenuTabDirective {
  constructor(public vcr: ViewContainerRef) { }
}
