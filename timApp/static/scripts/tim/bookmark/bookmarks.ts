import {IController} from "angular";
import {timApp} from "tim/app";
import * as focusMe from "tim/ui/focusMe";
import {Binding, clone, markAsUsed, to} from "tim/util/utils";
import {showBookmarkDialog} from "tim/bookmark/bookmark-dialog.component";
import {ViewCtrl} from "../document/viewctrl";
import {showMessageDialog} from "../ui/dialog";
import {genericglobals} from "../util/globals";
import {$http, $timeout} from "../util/ngimport";

markAsUsed(focusMe);

export interface IBookmarkGroup {
    name: string;
    isOpen: boolean;
    items: IBookmark[];
}

export interface IBookmark {
    group: string;
    link: string;
    name: string;
}

export class BookmarksController implements IController {
    private data?: Binding<IBookmarkGroup[], "<">;
    private deleting: boolean;
    private userId?: Binding<number, "<">;
    public viewctrl?: ViewCtrl;

    constructor() {
        this.deleting = false;
    }

    $onInit() {
        if (!this.data) {
            this.data = clone(genericglobals().bookmarks) ?? undefined;
        }
        if (this.userId && !this.data) {
            void this.refresh();
        }
        if (this.viewctrl) {
            this.viewctrl.registerBookmarks(this);
        }
    }

    getFromServer(response: IBookmarkGroup[], groupToKeepOpen?: IBookmarkGroup) {
        this.data = response;
        this.keepGroupOpen(groupToKeepOpen);
    }

    keepGroupOpen(groupToKeepOpen?: IBookmarkGroup) {
        if (!groupToKeepOpen || !this.data) {
            return;
        }
        for (const d of this.data) {
            if (d.name === groupToKeepOpen.name) {
                d.isOpen = true;
                return;
            }
        }
    }

    getTopLevelBookmarks() {
        if (!this.data) {
            return [];
        }
        for (const d of this.data) {
            if (d.name === "") {
                return d.items;
            }
        }
        return [];
    }

    isSaveablePage() {
        return true;
    }

    async newBookmark(group: string | undefined, e: Event) {
        e.preventDefault();
        const suggestedName = (genericglobals().curr_item ?? {title: undefined}).title ?? document.title;
        const bookmark = await showBookmarkDialog({
            group: group ?? "",
            name: suggestedName,
            link: "",
        });

        if (!bookmark.name) {
            return;
        }
        const resp = await to($http.post<IBookmarkGroup[]>("/bookmarks/add", bookmark));
        if (!resp.ok) {
            return;
        }
        this.getFromServer(resp.result.data);
    }

    async editItem(group: IBookmarkGroup, item: IBookmark, e: Event) {
        e.stopPropagation();
        e.preventDefault();
        const r = await to(showBookmarkDialog({
            group: group.name,
            name: item.name,
            link: item.link,
        }));
        if (!r.ok) {
            $timeout(() => {
                this.keepGroupOpen(group);
            }, 0);
            return;
        }
        if (!r.result.name) {
            return;
        }
        const response = await to($http.post<IBookmarkGroup[]>("/bookmarks/edit", {
            old: {
                group: group.name,
                name: item.name,
                link: item.link,
            }, new: r.result,
        }));
        if (!response.ok) {
            return;
        }
        this.getFromServer(response.result.data, group);
    }

    async deleteItem(group: IBookmarkGroup, item: IBookmark, e: Event) {
        e.stopPropagation();
        e.preventDefault();
        const r = await to($http.post<IBookmarkGroup[]>("/bookmarks/delete", {
            group: group.name,
            name: item.name,
        }));
        if (!r.ok) {
            void showMessageDialog("Could not delete bookmark.");
            return;
        }

        this.getFromServer(r.result.data, group);
    }

    async deleteGroup(group: IBookmarkGroup, e: Event) {
        e.stopPropagation();
        e.preventDefault();
        if (window.confirm("Are you sure you want to delete this bookmark group?")) {
            const r = await to($http.post<IBookmarkGroup[]>("/bookmarks/deleteGroup", {
                group: group.name,
            }));
            if (!r.ok) {
                void showMessageDialog("Could not delete bookmark group.");
                return;
            }

            this.getFromServer(r.result.data);
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
        const response = await to($http.get<IBookmarkGroup[]>("/bookmarks/get"));
        if (!response.ok) {
            return;
        }
        this.getFromServer(response.result.data);
    }
}

timApp.component("bookmarks", {
    bindings: {
        data: "<?",
        userId: "<?",
    },
    require: {
        viewctrl: "?^timView",
    },
    controller: BookmarksController,
    template: `
<div ng-repeat="group in $ctrl.data" class="btn-group btn-group-sm margin-4" uib-dropdown dropdown-append-to-body is-open="group.isOpen">
    <button type="button" class="btn btn-default" uib-dropdown-toggle>
        {{ group.name || 'Top level' }} <span class="caret"></span>
    </button>
    <ul class="dropdown-menu"
        uib-dropdown-menu
        role="menu"
        aria-labelledby="single-button">
        <li ng-repeat="item in group.items" role="menuitem">
            <a ng-href="{{ item.link }}">{{ item.name }}
                <i ng-click="$ctrl.editItem(group, item, $event)" ng-show="$ctrl.deleting"
                   class="glyphicon glyphicon-pencil"></i>
                <i ng-click="$ctrl.deleteItem(group, item, $event)" ng-show="$ctrl.deleting"
                   class="glyphicon glyphicon-remove"></i>
            </a>
        </li>
        <li ng-show="group.items.length > 0"
            class="divider"></li>
        <li role="menuitem">
            <a ng-click="$ctrl.newBookmark(group.name, $event)"
               href="#">New bookmark...</a>
        </li>
        <li ng-show="group.editable"
            class="divider"></li>
        <li ng-show="group.editable" role="menuitem">
            <a ng-click="$ctrl.toggleDelete($event)"
               href="#">{{ $ctrl.deleting ? 'Done editing' : 'Edit...' }}</a>
        </li>
        <li ng-show="group.editable && $ctrl.deleting" role="menuitem">
            <a ng-click="$ctrl.deleteGroup(group, $event)"
               href="#">Delete this folder</a>
        </li>
    </ul>
</div>
<a ng-repeat="bookmark in $ctrl.getTopLevelBookmarks()"
   ng-href="{{ bookmark.link }}"
   ng-bind="bookmark.name"
   class="btn btn-sm btn-default"></a>
<button ng-click="$ctrl.newBookmark(undefined, $event)" class="btn btn-sm btn-default">
    <i class="glyphicon glyphicon-plus"></i> New bookmark...
</button>
    `,
});
