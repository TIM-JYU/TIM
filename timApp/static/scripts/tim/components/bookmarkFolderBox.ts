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
        if (!this.bookmarks) {
            return;
        }
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
     * Forms the link text with or without course code.
     * @param {ITaggedItem} d
     * @returns {string}
     */
    private getLinkText(d: ITaggedItem) {
        const cc = this.getCourseCode(d);
        if (!cc) {
            return d.title;
        } else {
            return `${cc} - ${d.title}`;
        }
    }

    /**
     * Returns course code if it exists for the item.
     * @param {ITaggedItem} d Document and its tags.
     * @returns {string} Course code or empty string, if none were found.
     */
    private getCourseCode(d: ITaggedItem) {
        for (const tag of d.tags) {
            if (tag.type === TagType.CourseCode) {
                return tag.name;
            }
        }
        return undefined;
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
                <li class="h5 list-unstyled" ng-repeat="d in $ctrl.documents | orderBy:$ctrl.getCourseCode">
                    <a href="/view/{{d.path}}">
                        <span>{{$ctrl.getLinkText(d)}}</span>
                    </a>
                </li>
            </ul>
        </div>
    `,
});
