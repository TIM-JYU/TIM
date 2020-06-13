import {Component, Input, OnInit} from "@angular/core";
import {IMenuTab, TabEntry} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "bookmarks-tab",
    template: `
        <h5 i18n>Bookmarks</h5>
        <bookmarks-list></bookmarks-list>
    `,
})
export class BookmarksTabComponent implements OnInit, IMenuTab {
    @Input() entry!: TabEntry;

    constructor() {
    }

    ngOnInit(): void {
    }
}
