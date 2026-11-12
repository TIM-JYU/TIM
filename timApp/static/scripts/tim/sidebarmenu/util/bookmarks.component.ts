import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";
import {genericglobals} from "tim/util/globals";
import {to2, toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import type {IBookmark, IBookmarkGroup} from "tim/bookmark/bookmark.service";
import {BookmarkService} from "tim/bookmark/bookmark.service";
import type {RootCtrl} from "tim/timRoot";
import {rootInstance} from "tim/rootinstance";
import {showBookmarkDialog} from "tim/bookmark/showBookmarkDialog";
import {showMessageDialog} from "tim/ui/showMessageDialog";

@Component({
    selector: "bookmarks-list",
    template: `
        @for (group of groups; track group) {
          <div class="btn-group btn-group-sm margin-4"
            dropdown
            [isOpen]="group.isOpen"
            container="body">
            <button dropdownToggle type="button" class="btn btn-default">
              @if (group.name) {
                {{group.name | bookmarkName}}
              } @else {
                <ng-container i18n>Top level </ng-container>
              }
              <span class="caret"></span>
            </button>
            <ul *dropdownMenu class="dropdown-menu" role="menu" aria-labelledby="single-button">
              @for (item of group.items; track item) {
                <li role="menuitem">
                  <a [href]="item.link">{{item.name}}
                    @if (deleting) {
                      <i (click)="editItem($event, group, item)" class="glyphicon glyphicon-pencil"></i>
                      <i (click)="deleteItem($event, group, item)" class="glyphicon glyphicon-remove"></i>
                    }
                  </a>
                </li>
              }
              @if (group.items.length > 0) {
                <li class="divider"></li>
              }
              <li role="menuitem">
                <a (click)="newBookmark($event, group.name)" href="#" i18n>New bookmark...</a>
              </li>
              @if (group.editable) {
                <li class="divider"></li>
                <li role="menuitem">
                  <a (click)="toggleDelete($event)" href="#">
                    @if (deleting) {
                      <ng-container i18n>Done editing</ng-container>
                    } @else {
                      <ng-container i18n>Edit...</ng-container>
                    }
                  </a>
                </li>
              }
              @if (group.editable && deleting) {
                <li role="menuitem">
                  <a (click)="deleteGroup($event, group)" href="#" i18n>Delete this folder</a>
                </li>
              }
            </ul>
          </div>
        }
        @for (bookmark of getTopLevelBookmarks(); track bookmark) {
          <a
            [href]="bookmark.link"
            class="btn btn-sm btn-default space-right bookmark">{{bookmark.name}}
          </a>
        }
        <button (click)="newBookmark($event)" class="btn btn-sm btn-default" i18n>
            <i class="glyphicon glyphicon-plus"></i> New bookmark...
        </button>
        `,
    styleUrls: ["./bookmarks.component.scss"],
    standalone: false,
})
export class BookmarksComponent implements OnInit {
    groups?: IBookmarkGroup[];
    deleting: boolean = false;
    private rootCtrl?: RootCtrl = rootInstance;

    constructor(
        private http: HttpClient,
        private bookmarkSvc: BookmarkService
    ) {}

    ngOnInit(): void {
        this.groups = this.bookmarkSvc.getGroups();
        if (this.rootCtrl) {
            this.rootCtrl.registerBookmarks(this);
        }
    }

    getFromServer(
        response: IBookmarkGroup[],
        groupToKeepOpen?: IBookmarkGroup
    ) {
        this.groups = response;
        this.keepGroupOpen(groupToKeepOpen);
    }

    keepGroupOpen(groupToKeepOpen?: IBookmarkGroup) {
        if (!groupToKeepOpen || !this.groups) {
            return;
        }
        for (const group of this.groups) {
            if (group.name === groupToKeepOpen.name) {
                group.isOpen = true;
                return;
            }
        }
    }

    getTopLevelBookmarks() {
        if (!this.groups) {
            return [];
        }
        for (const group of this.groups) {
            if (group.name == "") {
                return group.items;
            }
        }
        return [];
    }

    async newBookmark(e: Event, group?: string) {
        e.preventDefault();
        const suggestedName =
            genericglobals().curr_item?.title ?? document.title;
        const dialogResult = await to2(
            showBookmarkDialog({
                group: group ?? "",
                name: suggestedName,
                link: "",
            })
        );
        if (!dialogResult.ok || !dialogResult.result.name) {
            return;
        }
        const bookmark = dialogResult.result;
        const resp = await toPromise(
            this.http.post<IBookmarkGroup[]>("/bookmarks/add", bookmark)
        );
        if (!resp.ok) {
            return;
        }
        this.getFromServer(resp.result);
    }

    async editItem(e: Event, group: IBookmarkGroup, item: IBookmark) {
        e.stopPropagation();
        e.preventDefault();
        const r = await to2(
            showBookmarkDialog({
                group: group.name,
                name: item.name,
                link: item.link,
            })
        );
        if (!r.ok) {
            setTimeout(() => {
                this.keepGroupOpen(group);
            });
            return;
        }
        if (!r.result.name) {
            return;
        }
        const response = await toPromise(
            this.http.post<IBookmarkGroup[]>("/bookmarks/edit", {
                old: {
                    group: group.name,
                    name: item.name,
                },
                new: r.result,
            })
        );
        if (!response.ok) {
            return;
        }
        this.getFromServer(response.result, group);
    }

    async deleteItem(e: Event, group: IBookmarkGroup, item: IBookmark) {
        e.stopPropagation();
        e.preventDefault();
        const r = await toPromise(
            this.http.post<IBookmarkGroup[]>("/bookmarks/delete", {
                group: group.name,
                name: item.name,
            })
        );
        if (!r.ok) {
            showMessageDialog("Could not delete bookmark.");
            return;
        }

        this.getFromServer(r.result, group);
    }

    async deleteGroup(e: Event, group: IBookmarkGroup) {
        e.stopPropagation();
        e.preventDefault();
        if (
            window.confirm(
                "Are you sure you want to delete this bookmark group?"
            )
        ) {
            const r = await toPromise(
                this.http.post<IBookmarkGroup[]>("/bookmarks/deleteGroup", {
                    group: group.name,
                })
            );
            if (!r.ok) {
                showMessageDialog("Could not delete bookmark group.");
                return;
            }

            this.getFromServer(r.result);
        }
    }

    toggleDelete(e: Event) {
        e.stopPropagation();
        e.preventDefault();
        this.deleting = !this.deleting;
    }

    /**
     * Updates bookmarks.
     */
    async refresh() {
        const response = await this.bookmarkSvc.fetchBookmarks(this.http);
        if (!response.ok) {
            return;
        }
        this.getFromServer(response.result);
    }
}
