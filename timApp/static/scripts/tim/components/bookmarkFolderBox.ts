/**
 * A component that shows contents of bookmark folder.
 */

import {IController} from "angular";
import {timApp} from "../app";
import {IBookmark, IBookmarkGroup, showBookmarkDialog} from "../directives/bookmarks";
import {ITaggedItem, TagType} from "../IItem";
import {$http, $timeout, $window} from "../ngimport";
import {Binding, to} from "../utils";

export interface ITaggedBookmarkedItem {
    doc: ITaggedItem;
    bookmark: IBookmark;
}

class BookmarkFolderBoxCtrl implements IController {
    private bookmarkFolder: IBookmarkGroup | undefined;
    private bookmarkFolderName!: Binding<string, "@">;
    private bookmarks!: Binding<IBookmarkGroup[], "<">;
    private documents?: ITaggedBookmarkedItem[];
    private editOn: boolean = false;

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

    /**
     * Gets entries of documents corresponding bookmark items and rejoins them with the bookmarks.
     * @returns {Promise<void>}
     */
    private async getDocumentData() {
        const response = await $http<ITaggedItem[]>({
            method: "GET",
            url: `/courses/documents/${this.bookmarkFolderName}`,
        });
        if (response && this.bookmarkFolder) {
            this.documents = [];
            for (const responseItem of response.data) {
                for (const b of this.bookmarkFolder.items) {
                    if ("/view/" + responseItem.path === b.link) {
                        this.documents.push({doc: responseItem, bookmark: b});
                    }
                }
            }
        }
    }

    /**
     * Forms the link text with or without course code.
     * @param {ITaggedItem} d
     * @returns {string}
     */
    private getLinkText(d: ITaggedBookmarkedItem) {
        const cc = this.getCourseCode(d);
        if (!cc) {
            return d.bookmark.name;
        } else {
            return `${cc} - ${d.bookmark.name}`;
        }
    }

    /**
     * Deletes the bookmark and updates list.
     * @param {ITaggedBookmarkedItem} d
     * @returns {Promise<void>}
     */
    private async removeFromList(d: ITaggedBookmarkedItem) {
        const response = await $http.post<IBookmarkGroup[]>("/bookmarks/delete", {
            group: this.bookmarkFolderName,
            name: d.bookmark.name,
        });
        if (response) {
            await this.updateBookmarks();
            await this.getBookmarkFolder(this.bookmarkFolderName);
            await this.getDocumentData();
        }
    }

    /**
     * Opens editing dialog for the bookmark and updates list if changes were made.
     * @param {ITaggedBookmarkedItem} d
     * @returns {Promise<void>}
     */
    private async renameFromList(d: ITaggedBookmarkedItem) {
        const [err, bookmark] = await to(showBookmarkDialog({
            group: this.bookmarkFolderName,
            link: d.bookmark.link,
            name: d.bookmark.name,
        }));
        if (!bookmark || !bookmark.name) {
            return;
        }
        const response = await $http.post<IBookmarkGroup[]>("/bookmarks/edit", {
            old: {
            group: this.bookmarkFolderName,
            link: d.bookmark.link,
            name: d.bookmark.name,
            }, new: bookmark,
        });
        if (response) {
            await this.updateBookmarks();
            await this.getBookmarkFolder(this.bookmarkFolderName);
            await this.getDocumentData();
        }
    }

    /**
     * Gets changed bookmarks from the database.
     * @returns {Promise<void>}
     */
    private async updateBookmarks() {
        const response = await $http.get<IBookmarkGroup[]>("/bookmarks/get");
        if (response) {
            this.bookmarks = response.data;
        }
    }

    /**
     * Returns course code if it exists for the item.
     * @param {ITaggedItem} d Document and its tags.
     * @returns {string} Course code or empty string, if none were found.
     */
    private getCourseCode(d: ITaggedBookmarkedItem) {
        for (const tag of d.doc.tags) {
            if (tag.type === TagType.CourseCode) {
                return tag.name;
            }
        }
        return undefined;
    }
}

// Note: the compicated structure with empty headers and elements inside elements is to display edit icon
// on the same line as the header without changing its size.

timApp.component("bookmarkFolderBox", {
    bindings: {
        bookmarkFolderName: "@",
        bookmarks: "<",
    },
    controller: BookmarkFolderBoxCtrl,
    template: `
        <div ng-cloak ng-if="$ctrl.documents.length > 0">
            <span><h3></h3><h3 class="display-inline">{{$ctrl.bookmarkFolder.name}}</h3>
            <a><i class="glyphicon glyphicon-pencil" title="Open editing {{$ctrl.bookmarkFolder.name}}"
            ng-click="$ctrl.editOn = !$ctrl.editOn"></i></a></span>
            <ul class="list-unstyled">
                <li class="h5 list-unstyled" ng-repeat="d in $ctrl.documents | orderBy:$ctrl.getCourseCode">
                    <a href="/view/{{d.doc.path}}">
                        <span>{{$ctrl.getLinkText(d)}}
                        <a ng-if="$ctrl.editOn"><i class="glyphicon glyphicon-pencil" title="Edit bookmark"
                        ng-click="$ctrl.renameFromList(d)"></i></a>
                         <a ng-if="$ctrl.editOn"><i class="glyphicon glyphicon-remove"
                        title="Remove bookmark" ng-click="$ctrl.removeFromList(d)"></i>
                        </a>
                        </span>
                    </a>
                </li>
            </ul>
        </div>
    `,
});
