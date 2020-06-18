import {Component, OnInit} from "@angular/core";

@Component({
    selector: "bookmarks-tab",
    template: `
        <ng-template i18n="@@bookmarksTabTitle">Bookmarks</ng-template>
        <h5 i18n>Bookmarks</h5>
        <bookmarks-list></bookmarks-list>
    `,
})
export class BookmarksTabComponent {
    constructor() {
    }
}
