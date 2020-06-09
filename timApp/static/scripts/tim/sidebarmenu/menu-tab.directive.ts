import {Directive, Type, ViewContainerRef} from "@angular/core";

export interface IMenuTab {
  tabType: Type<unknown>;
  icon: string;
  title: string;
}

@Directive({
  selector: "[timMenuTab]",
})
export class MenuTabDirective {
  constructor(public vcr: ViewContainerRef) { }
}
