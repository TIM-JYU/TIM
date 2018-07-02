/**
 * A component that shows contents of bookmark folder.
 **/

import {IController} from "angular";
import {timApp} from "../app";
import {IBookmarkGroup} from "../directives/bookmarks";
import {Binding} from "../utils";

class BookmarkFolderBoxCtrl implements IController {
    private bookmarkFolder: IBookmarkGroup | undefined;
    private bookmarkFolderName!: Binding<string, "@">;
    private bookmarks!: Binding<IBookmarkGroup[], "<">;

    async $onInit() {
        await this.getBookmarkFolder(this.bookmarkFolderName);
    }

    /**
     * Gets specified bookmark folder.
     * @param {string} folderName
     * @returns {Promise<void>}
     */
    private async getBookmarkFolder(folderName: string) {
        for (const folder of this.bookmarks) {
            if (folder.name === folderName) {
                this.bookmarkFolder = folder;
                return;
            }
        }
    }
}

timApp.component("bookmarkFolderBox", {
    bindings: {
        bookmarkFolderName: "@",
        bookmarks: "<",
    },
    controller: BookmarkFolderBoxCtrl,
    template: `
        <div ng-cloak ng-if="$ctrl.bookmarkFolder.items.length > 0">
        <h3>{{$ctrl.bookmarkFolder.name}}</h3>
        <ul class="list-unstyled">
            <li class="h5 list-unstyled" ng-repeat="bookmark in $ctrl.bookmarkFolder.items">
                <a href="{{bookmark.link}}">{{bookmark.name}}</a>
            </li>
        </ul>
        </div>
    `,
});
