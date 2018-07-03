/**
 * A component that shows contents of a bookmark folder. Shows course codes separately
 * and allows editing the bookmarks.
 */

import {IController} from "angular";
import {timApp} from "../app";
import {IBookmark, IBookmarkGroup, showBookmarkDialog} from "./bookmarks";
import {getCourseCode, ITaggedItem} from "../item/IItem";
import {Binding, to} from "../util/utils";
import {$http} from "../util/ngimport";

export interface ITaggedBookmarkedItem {
    doc: ITaggedItem;
    bookmark: IBookmark;
}

class BookmarkFolderBoxCtrl implements IController {
    private bookmarkFolder: IBookmarkGroup | undefined;
    private bookmarkFolderName!: Binding<string, "@">;
    private bookmarks!: Binding<IBookmarkGroup[], "<">;
    private documents?: ITaggedBookmarkedItem[]; // Documents of the bookmark folder.
    private editOn: boolean = false; // Show bookmark edit and removal icons.

    async $onInit() {
        await this.getBookmarkFolder(this.bookmarkFolderName);
        await this.getDocumentData();
    }

    /**
     * Gets the specified bookmark folder.
     * @param {string} folderName
     */
    private getBookmarkFolder(folderName: string) {
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
        // Returns a list of ITaggedItems because bookmarks aren't directly linked to
        // document items.
        const response = await $http<ITaggedItem[]>({
            method: "GET",
            url: `/courses/documents/${this.bookmarkFolderName}`,
        });
        // Bookmarks are added to their corresponding documents here.
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
     * For example: if there's a course code: "ACBD123 - Test document", if not: "Test document".
     * @param {ITaggedBookmarkedItem} d The document data including its bookmark.
     * @returns {string}
     */
    private getLinkText(d: ITaggedBookmarkedItem) {
        const cc = getCourseCode(d.doc.tags);
        if (!cc) {
            return d.bookmark.name;
        } else {
            return `${cc} - ${d.bookmark.name}`;
        }
    }

    /**
     * Deletes the bookmark and updates document list.
     * @param {ITaggedBookmarkedItem} d The document data including its bookmark.
     * @returns {Promise<void>}
     */
    private async removeFromList(d: ITaggedBookmarkedItem) {
        const response = await $http.post<IBookmarkGroup[]>("/bookmarks/delete", {
            group: this.bookmarkFolderName,
            name: d.bookmark.name,
        });
        if (response) {
            this.bookmarks = response.data;
            await this.getBookmarkFolder(this.bookmarkFolderName);
            await this.getDocumentData();
        }
    }

    /**
     * Opens editing dialog for the bookmark and updates list if changes were made.
     * @param {ITaggedBookmarkedItem} d The document data including its bookmark.
     * @returns {Promise<void>}
     */
    private async editFromList(d: ITaggedBookmarkedItem) {
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
            this.bookmarks = response.data;
            await this.getBookmarkFolder(this.bookmarkFolderName);
            await this.getDocumentData();
        }
    }

    /**
     * Returns course code if it exists. Doesn't take expiration into account.
     * @param {ITaggedBookmarkedItem} d The document data including its bookmark.
     * @returns {string} Coursecode or undefined.
     */
    private courseCode(d: ITaggedBookmarkedItem) {
        return getCourseCode(d.doc.tags);
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
            <h3>{{$ctrl.bookmarkFolder.name}}<a class="font-medium"><i class="glyphicon glyphicon-pencil"
            title="Toggle editing {{$ctrl.bookmarkFolder.name}}"
            ng-click="$ctrl.editOn = !$ctrl.editOn"></i></a></h3>
            <ul class="list-unstyled">
                <li class="h5 list-unstyled" ng-repeat="d in $ctrl.documents | orderBy:$ctrl.courseCode">
                    <a href="/view/{{d.doc.path}}">
                        <span>{{$ctrl.getLinkText(d)}}
                        <a ng-if="$ctrl.editOn"><i class="glyphicon glyphicon-pencil" title="Edit bookmark"
                        ng-click="$ctrl.editFromList(d)"></i></a>
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
