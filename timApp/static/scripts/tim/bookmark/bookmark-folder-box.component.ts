/**
 * A component that shows contents of a bookmark folder. Shows course codes separately
 * and allows editing the bookmarks.
 */

import {Component, Input, OnInit} from "@angular/core";
import {IBookmark, IBookmarkGroup} from "tim/bookmark/bookmark.service";
import {showBookmarkDialog} from "tim/bookmark/showBookmarkDialog";
import {HttpClient} from "@angular/common/http";
import {RootCtrl} from "tim/timRoot";
import {rootInstance} from "tim/rootinstance";
import {getCourseCode, ITaggedItem} from "../item/IItem";
import {to2, toPromise} from "../util/utils";

export interface ITaggedBookmarkedItem {
    doc: ITaggedItem;
    bookmark: IBookmark;
}

@Component({
    selector: "tim-bookmark-folder-box",
    template: `
        <div *ngIf="bookmarkFolder && bookmarkFolder.items.length > 0">
            <h3>{{displayName || bookmarkFolder.name}}<a class="font-medium margin-4">
                <i class="glyphicon glyphicon-pencil"
                   title="Toggle editing"
                   i18n-title
                   (click)="editOn = !editOn"></i></a></h3>
            <ul class="list-unstyled">
                <li class="h5" *ngFor="let d of documents">
                    <a href="/view/{{d.doc.path}}">
                        {{getLinkText(d)}}</a>&nbsp;
                    <ng-container *ngIf="editOn">
                        <a><i class="glyphicon glyphicon-pencil"
                              title="Edit bookmark"
                              i18n-title
                              (click)="editFromList(d.bookmark)"></i></a>
                        <a><i class="glyphicon"
                              [class.glyphicon-remove]="!hideMode"
                              [class.glyphicon-eye-close]="hideMode"
                              [title]="removeText"
                              (click)="removeFromList(d.bookmark)"></i>
                        </a>
                    </ng-container>
                </li>
                <li class="h5" *ngFor="let b of orphanBookmarks">
                    <a href="{{b.link}}">
                        {{b.name}}</a>&nbsp;
                    <ng-container *ngIf="editOn">
                        <a><i class="glyphicon glyphicon-pencil"
                              title="Edit bookmark"
                              i18n-title
                              (click)="editFromList(b)"></i></a>
                        <a><i class="glyphicon"
                              [class.glyphicon-remove]="!hideMode"
                              [class.glyphicon-eye-close]="hideMode"
                              [title]="removeText"
                              (click)="removeFromList(b)"></i>
                        </a>
                    </ng-container>
                </li>
            </ul>
        </div>
    `,
})
export class BookmarkFolderBoxComponent implements OnInit {
    bookmarkFolder: IBookmarkGroup | undefined;
    @Input() bookmarkFolderName!: string;
    @Input() displayName?: string;
    @Input() bookmarks!: IBookmarkGroup[];
    @Input() hideMode!: boolean;
    root!: RootCtrl;
    removeText: string = $localize`Remove bookmark`;
    documents?: ITaggedBookmarkedItem[]; // Documents of the bookmark folder.
    editOn: boolean = false; // Show bookmark edit and removal icons.
    orphanBookmarks: IBookmark[] = []; // Bookmarks that aren't pointing to any TIM document.

    constructor(private http: HttpClient) {}

    async ngOnInit() {
        this.root = rootInstance!;
        if (this.hideMode) {
            this.removeText = $localize`Hide bookmark`;
        }
        this.getBookmarkFolder(this.bookmarkFolderName);
        await this.getDocumentData();
    }

    /**
     * Gets the specified bookmark folder.
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
        const response = await toPromise(
            this.http.get<ITaggedItem[]>(
                `/courses/documents/${this.bookmarkFolderName}`
            )
        );
        if (!response.ok) {
            return;
        }
        const taggedItemByPath = new Map<string, ITaggedItem>(
            response.result.map((item) => [`/view/${item.path}`, item])
        );
        // Bookmarks are added to their corresponding documents here.
        if (taggedItemByPath.size > 0 && this.bookmarkFolder) {
            this.documents = [];
            for (const b of this.bookmarkFolder.items) {
                const taggedItem = taggedItemByPath.get(b.link);
                if (taggedItem) {
                    this.documents.push({doc: taggedItem, bookmark: b});
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
     */
    getLinkText(d: ITaggedBookmarkedItem) {
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
    async removeFromList(d: IBookmark) {
        const response = await toPromise(
            this.http.post<IBookmarkGroup[]>("/bookmarks/delete", {
                group: this.bookmarkFolderName,
                name: d.name,
            })
        );
        if (response.ok) {
            this.bookmarks = response.result;
            await this.getBookmarkFolder(this.bookmarkFolderName);
            await this.getDocumentData();
            await this.root.bookmarksCtrl?.refresh();
        }
    }

    /**
     * Opens editing dialog for the bookmark and updates list if changes were made.
     * @param {IBookmark} b Bookmark to edit.
     */
    async editFromList(b: IBookmark) {
        const r = await to2(
            showBookmarkDialog({
                group: this.bookmarkFolderName,
                link: b.link,
                name: b.name,
            })
        );
        if (!r.ok || !r.result.name) {
            return;
        }
        const response = await toPromise(
            this.http.post<IBookmarkGroup[]>("/bookmarks/edit", {
                old: {
                    group: this.bookmarkFolderName,
                    name: b.name,
                },
                new: r.result,
            })
        );
        if (response.ok) {
            this.bookmarks = response.result;
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
     */
    private bookmarkIndexOf(bookmark: IBookmark) {
        if (this.documents) {
            for (const {item, index} of this.documents.map((it, ind) => ({
                item: it,
                index: ind,
            }))) {
                if (bookmark.link === item.bookmark.link) {
                    return index;
                }
            }
        }
        return -1;
    }
}
