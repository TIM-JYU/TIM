import {Component, Input, OnInit} from "@angular/core";
import {IMenuTab, TabEntry} from "tim/sidebarmenu/menu-tab.directive";

@Component({
  selector: "bookmarks-tab",
  template: `
    <p>
      bookmarks-tab works!
    </p>
  `,
})
export class BookmarksTabComponent implements OnInit, IMenuTab {
  @Input() entry!: TabEntry;

  constructor() {
  }

  ngOnInit(): void {
    console.log(this.entry);
  }
}
