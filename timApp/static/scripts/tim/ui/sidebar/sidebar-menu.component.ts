import { Component, OnInit } from "@angular/core";
import {TabDirective} from "ngx-bootstrap/tabs";

@Component({
  selector: "app-sidebar-menu",
  templateUrl: "./sidebar-menu.component.html",
  styleUrls: ["./sidebar-menu.component.scss"],
})
export class SidebarMenuComponent implements OnInit {
  currentElement?: HTMLElement;
  constructor() { }

  ngOnInit(): void {
  }

  onTabSelect(tab: TabDirective) {
    this.currentElement = tab.elementRef.nativeElement as HTMLElement;
  }

  shouldRender(tab: HTMLElement) {
    return this.currentElement == tab;
  }

}
