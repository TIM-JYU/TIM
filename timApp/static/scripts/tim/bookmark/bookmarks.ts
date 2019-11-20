import {IController, IFormController, IRootElementService, IScope} from "angular";
import {timApp} from "tim/app";
import * as focusMe from "tim/ui/focusMe";
import {Binding, clone, markAsUsed, to} from "tim/util/utils";
import {ViewCtrl} from "../document/viewctrl";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../ui/dialog";
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
        if (genericglobals().bookmarks && !this.data) {
            this.data = clone(genericglobals().bookmarks);
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
        const suggestedName = (genericglobals().curr_item || {title: undefined}).title || document.title;
        const bookmark = await showBookmarkDialog({
            group: group || "",
            name: suggestedName,
            link: "",
        });

        if (!bookmark.name) {
            return;
        }
        const resp = await $http.post<IBookmarkGroup[]>("/bookmarks/add", bookmark);
        this.getFromServer(resp.data);
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
        const response = await $http.post<IBookmarkGroup[]>("/bookmarks/edit", {
            old: {
                group: group.name,
                name: item.name,
                link: item.link,
            }, new: r.result,
        });
        this.getFromServer(response.data, group);
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
        const response = await $http.get<IBookmarkGroup[]>("/bookmarks/get");
        this.getFromServer(response.data);
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
    templateUrl: "/static/templates/bookmarks.html",
});

class CreateBookmarkCtrl extends DialogController<{params: IBookmark}, IBookmark> {
    static component = "timBookmarksDialog";
    static $inject = ["$element", "$scope"] as const;
    private f!: IFormController; // initialized in the template
    private focusName?: boolean;
    private focusGroup?: boolean;
    private showParamsCheckbox?: boolean;
    private showHashCheckbox?: boolean;
    private bookmark!: IBookmark; // $onInit
    private includeParams?: boolean;
    private includeHash?: boolean;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    $onInit() {
        super.$onInit();
        this.bookmark = this.resolve.params;
        if (this.bookmark.group === "Last edited" || this.bookmark.group === "Last read") {
            this.bookmark.group = "";
        }
        this.focusGroup = false;
        this.focusName = true;
        this.showParamsCheckbox = window.location.search.length > 1;
        this.showHashCheckbox = window.location.hash.length > 1;
    }

    protected getTitle() {
        return "Bookmark";
    }

    public ok() {
        if (!this.bookmark.link) {
            this.bookmark.link = window.location.pathname;
            if (this.includeParams) {
                this.bookmark.link += window.location.search;
            }
            if (this.includeHash) {
                this.bookmark.link += window.location.hash;
            }
        }

        this.close(this.bookmark);
    }

    public cancel() {
        this.dismiss();
    }
}

registerDialogComponent(CreateBookmarkCtrl,
    {
        template: `
<tim-dialog>
    <dialog-header>
        Bookmark
    </dialog-header>
    <dialog-body>
        <form name="$ctrl.f" class="form-horizontal">
            <div class="form-group"
                 ng-class="{'has-error': !$ctrl.f.nameField.$pristine && $ctrl.f.nameField.$error.required}">
                <label for="name" class="col-sm-2 control-label">Name</label>
                <div class="col-sm-10">
                    <input required focus-me="$ctrl.focusName" ng-model="$ctrl.bookmark.name" name="nameField"
                           type="text"
                           class="form-control" id="name" placeholder="Bookmark name">
                </div>
            </div>
            <div class="form-group">
                <label for="group" class="col-sm-2 control-label">Folder</label>
                <div class="col-sm-10">
                    <input focus-me="$ctrl.focusGroup" ng-model="$ctrl.bookmark.group" name="groupField" type="text"
                           class="form-control" id="group"
                           placeholder="Folder name or blank to make a top-level bookmark">
                </div>
            </div>
            <div class="form-group">
                <label for="link" class="col-sm-2 control-label">Link</label>
                <div class="col-sm-10">
                    <input ng-model="$ctrl.bookmark.link" type="text" class="form-control" name="linkField" id="link"
                           placeholder="Leave blank to add current page">
                </div>
            </div>
            <div ng-show="$ctrl.showParamsCheckbox" class="form-group">
                <div class="col-sm-offset-2 col-sm-10">
                    <div class="checkbox">
                        <label>
                            <input ng-model="$ctrl.includeParams" ng-disabled="$ctrl.bookmark.link" type="checkbox">
                            Include URL parameters in link
                        </label>
                    </div>
                </div>
            </div>
            <div ng-show="$ctrl.showHashCheckbox" class="form-group">
                <div class="col-sm-offset-2 col-sm-10">
                    <div class="checkbox">
                        <label>
                            <input ng-model="$ctrl.includeHash" ng-disabled="$ctrl.bookmark.link" type="checkbox">
                            Include URL hash in link
                        </label>
                    </div>
                </div>
            </div>
        </form>
    </dialog-body>
    <dialog-footer>
        <button ng-disabled="!$ctrl.f.$valid" class="timButton" type="button" ng-click="$ctrl.ok()">Save
        </button>
        <button class="btn btn-default" type="button" ng-click="$ctrl.cancel()">Cancel</button>
    </dialog-footer>
</tim-dialog>
    `,
    });

export function showBookmarkDialog(bookmark: IBookmark) {
    return showDialog(CreateBookmarkCtrl, {params: () => bookmark}).result;
}
