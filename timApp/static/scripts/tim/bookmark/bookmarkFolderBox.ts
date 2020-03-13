/**
 * A component that shows contents of a bookmark folder. Shows course codes separately
 * and allows editing the bookmarks.
 */

import {IController} from "angular";
import {showBookmarkDialog} from "tim/bookmark/bookmark-dialog.component";
import {timApp} from "../app";
import {getCourseCode, ITaggedItem} from "../item/IItem";
import {$http} from "../util/ngimport";
import {Binding, to} from "../util/utils";
import {IBookmark, IBookmarkGroup} from "./bookmarks";

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
    private orphanBookmarks: IBookmark[] = []; // Bookmarks that aren't pointing to any TIM-document.

    async $onInit() {
        this.getBookmarkFolder(this.bookmarkFolderName);
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
     */
    private async getDocumentData() {
        // Returns a list of ITaggedItems because bookmarks aren't directly linked to
        // document items.
        const response = await to($http<ITaggedItem[]>({
            method: "GET",
            url: `/courses/documents/${this.bookmarkFolderName}`,
        }));
        if (!response.ok) {
            return;
        }
        // Bookmarks are added to their corresponding documents here.
        if (response && this.bookmarkFolder) {
            this.documents = [];
            for (const responseItem of response.result.data) {
                for (const b of this.bookmarkFolder.items) {
                    if ("/view/" + responseItem.path === b.link) {
                        this.documents.push({doc: responseItem, bookmark: b});
                    }
                }
            }
        }
        // Make a list of bookmarks without documents.
        this.updateOrphanBookmarks();
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
            if (d.bookmark.name.length > 0) {
                return `${cc} - ${d.bookmark.name}`;
            } else {
                return `${cc} - ${d.doc.title}`;
            }
        }
    }

    /**
     * Deletes the bookmark and updates document list.
     * @param {IBookmark} d Bookmark to delete.
     */
    private async removeFromList(d: IBookmark) {
        const response = await to($http.post<IBookmarkGroup[]>("/bookmarks/delete", {
            group: this.bookmarkFolderName,
            name: d.name,
        }));
        if (response.ok) {
            this.bookmarks = response.result.data;
            await this.getBookmarkFolder(this.bookmarkFolderName);
            await this.getDocumentData();
        }
    }

    /**
     * Opens editing dialog for the bookmark and updates list if changes were made.
     * @param {IBookmark} b Bookmark to edit.
     */
    private async editFromList(b: IBookmark) {
        const r = await to(showBookmarkDialog({
            group: this.bookmarkFolderName,
            link: b.link,
            name: b.name,
        }));
        if (!r.ok || !r.result.name) {
            return;
        }
        const response = await to($http.post<IBookmarkGroup[]>("/bookmarks/edit", {
            old: {
            group: this.bookmarkFolderName,
            link: b.link,
            name: b.name,
            }, new: r.result,
        }));
        if (response.ok) {
            this.bookmarks = response.result.data;
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

    /**
     * Makes a list of bookmarks that aren't in the tagged-bookmarked documents list.
     */
    private updateOrphanBookmarks() {
        this.orphanBookmarks = [];
        if (this.bookmarkFolder) {
            for (const bookmark of this.bookmarkFolder.items) {
                if (this.bookmarkIndexOf(bookmark) < 0) {
                    this.orphanBookmarks.push(bookmark);
                }
            }
        }
    }

    /**
     * Returns the index of bookmark in the tagged-bookmarked documents list.
     * @param {IBookmark} bookmark
     * @returns {any}
     */
    private bookmarkIndexOf(bookmark: IBookmark) {
        if (this.documents) {
            for (const {item, index} of this.documents.map((it, ind) => ({ item: it, index: ind }))) {
                if (bookmark.link === item.bookmark.link) {
                    return index;
                }
            }
        }
        return -1;
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
    <h3>{{$ctrl.bookmarkFolder.name}}<a class="font-medium margin-4"><i class="glyphicon glyphicon-pencil"
    title="Toggle editing {{$ctrl.bookmarkFolder.name}}"
    ng-click="$ctrl.editOn = !$ctrl.editOn"></i></a></h3>
    <ul class="list-unstyled">
        <li class="h5" ng-repeat="d in $ctrl.documents | orderBy:$ctrl.courseCode">
            <a href="/view/{{d.doc.path}}">
                <span>{{$ctrl.getLinkText(d)}}
                <a ng-if="$ctrl.editOn"><i class="glyphicon glyphicon-pencil" title="Edit bookmark"
                ng-click="$ctrl.editFromList(d.bookmark)"></i></a>
                 <a ng-if="$ctrl.editOn"><i class="glyphicon glyphicon-remove"
                title="Remove bookmark" ng-click="$ctrl.removeFromList(d.bookmark)"></i>
                </a>
                </span>
            </a>
        </li>
        <li class="h5" ng-repeat="b in $ctrl.orphanBookmarks">
            <a href="{{b.link}}">
                <span>{{b.name}}
                <a ng-if="$ctrl.editOn"><i class="glyphicon glyphicon-pencil" title="Edit bookmark"
                ng-click="$ctrl.editFromList(b)"></i></a>
                 <a ng-if="$ctrl.editOn"><i class="glyphicon glyphicon-remove"
                title="Remove bookmark" ng-click="$ctrl.removeFromList(b)"></i>
                </a>
                </span>
            </a>
        </li>
    </ul>
</div>
    `,
});
