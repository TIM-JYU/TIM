/**
 * A component that shows contents of bookmark folder.
 */

import {IController} from "angular";
import {timApp} from "../app";
import {IBookmarkGroup} from "../directives/bookmarks";
import {ITaggedItem, TagType} from "../IItem";
import {$http} from "../ngimport";
import {Binding} from "../utils";

class BookmarkFolderBoxCtrl implements IController {
    private bookmarkFolder: IBookmarkGroup | undefined;
    private bookmarkFolderName!: Binding<string, "@">;
    private bookmarks!: Binding<IBookmarkGroup[], "<">;
    private documents?: ITaggedItem[];

    async $onInit() {
        await this.getBookmarkFolder(this.bookmarkFolderName);
        await this.getDocumentData();
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

    private async getDocumentData() {
        const response = await $http<ITaggedItem[]>({
            method: "GET",
            url: `/courses/documents/${this.bookmarkFolderName}`,
        });
        if (response) {
            this.documents = response.data;
        }
    }

    /**
     * Returns course code if it exists for the item and and the code hasn't expired.
     * @param {ITaggedItem} d Document and its tags.
     * @returns {string} Course code or empty strring, if none were found.
     */
    private getCourseCode(d: ITaggedItem) {
        for (const tag of d.tags) {
            if (tag.type === TagType.CourseCode) {
                return tag.name;
            }
        }
        return "";
    }
}

timApp.component("bookmarkFolderBox", {
    bindings: {
        bookmarkFolderName: "@",
        bookmarks: "<",
    },
    controller: BookmarkFolderBoxCtrl,
    template: `
        <div ng-cloak ng-if="$ctrl.documents.length > 0">
        <h3>{{$ctrl.bookmarkFolder.name}}</h3>
        <ul class="list-unstyled">
            <li class="h5 list-unstyled" ng-repeat="d in $ctrl.documents">
                <span class="btn-xs btn-primary">{{$ctrl.getCourseCode(d)}}</span>
                <a href="/view/{{d.path}}">{{d.title}}</a>
            </li>
        </ul>
        </div>
    `,
});
