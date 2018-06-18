import angular, {IController, IFormController, IPromise, IRootElementService, IScope} from "angular";
import {timApp} from "tim/app";
import * as focusMe from "tim/directives/focusMe";
import {markAsUsed, to} from "tim/utils";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../dialog";
import {IItem} from "../IItem";
import {$http, $timeout, $window} from "../ngimport";
import {ViewCtrl} from "../controllers/view/viewctrl";

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
    private data: IBookmarkGroup[];
    private deleting: boolean;
    private userId: number;
    public viewctrl?: ViewCtrl;

    constructor() {
        if ($window.bookmarks && !this.data) {
            this.data = angular.copy($window.bookmarks);
        }
        this.deleting = false;

        if (this.userId && !this.data) {
            void this.refresh();
        }
    }

    $onInit() {
        if (this.viewctrl) {
            this.viewctrl.registerBookmarks(this);
        }
    }

    getFromServer(response: IBookmarkGroup[], groupToKeepOpen?: IBookmarkGroup) {
        this.data = response;
        this.keepGroupOpen(groupToKeepOpen);
    }

    keepGroupOpen(groupToKeepOpen?: IBookmarkGroup) {
        if (!groupToKeepOpen) {
            return;
        }
        for (let i = 0; i < this.data.length; ++i) {
            if (this.data[i].name === groupToKeepOpen.name) {
                this.data[i].isOpen = true;
                return;
            }
        }
    }

    getTopLevelBookmarks() {
        if (!this.data) {
            return [];
        }
        for (let i = 0; i < this.data.length; ++i) {
            if (this.data[i].name === "") {
                return this.data[i].items;
            }
        }
        return [];
    }

    isSaveablePage() {
        return true;
    }

    async newBookmark(group: string | undefined, e: Event) {
        e.preventDefault();
        const suggestedName = ($window.item || {}).title || document.title;
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
        const [err, bookmark] = await to(showBookmarkDialog({
            group: group.name,
            name: item.name,
            link: item.link,
        }));
        if (!bookmark) {
            $timeout(() => {
                this.keepGroupOpen(group);
            }, 0);
            return;
        }
        if (!bookmark.name) {
            return;
        }
        const response = await $http.post<IBookmarkGroup[]>("/bookmarks/edit", {
            old: {
                group: group.name,
                name: item.name,
                link: item.link,
            }, new: bookmark,
        });
        this.getFromServer(response.data, group);
    }

    deleteItem(group: IBookmarkGroup, item: IBookmark, e: Event) {
        e.stopPropagation();
        e.preventDefault();
        return $http.post<IBookmarkGroup[]>("/bookmarks/delete", {
            group: group.name,
            name: item.name,
        })
            .then((response) => {
                this.getFromServer(response.data, group);
            }, (response) => {
                $window.alert("Could not delete bookmark.");
            });
    }

    deleteGroup(group: IBookmarkGroup, e: Event) {
        e.stopPropagation();
        e.preventDefault();
        if ($window.confirm("Are you sure you want to delete this bookmark group?")) {
            $http.post<IBookmarkGroup[]>("/bookmarks/deleteGroup", {group: group.name})
                .then((resp) => this.getFromServer(resp.data), response => {
                    $window.alert("Could not delete bookmark group.");
                });
        }
    }

    toggleDelete(e: Event) {
        e.stopPropagation();
        e.preventDefault();
        this.deleting = !this.deleting;
    }

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

class CreateBookmarkCtrl extends DialogController<{params: IBookmark}, IBookmark, "timBookmarksDialog"> {
    private static $inject = ["$element", "$scope"];
    private f: IFormController;
    private focusName: boolean;
    private focusGroup: boolean;
    private showParamsCheckbox: boolean;
    private showHashCheckbox: boolean;
    private bookmark: IBookmark;
    private includeParams: boolean;
    private includeHash: boolean;

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
        this.showParamsCheckbox = $window.location.search.length > 1;
        this.showHashCheckbox = $window.location.hash.length > 1;
    }

    protected getTitle() {
        return "Bookmark";
    }

    public ok() {
        if (!this.bookmark.link) {
            this.bookmark.link = $window.location.pathname;
            if (this.includeParams) {
                this.bookmark.link += $window.location.search;
            }
            if (this.includeHash) {
                this.bookmark.link += $window.location.hash;
            }
        }

        this.close(this.bookmark);
    }

    public cancel() {
        this.dismiss();
    }
}

registerDialogComponent("timBookmarksDialog", CreateBookmarkCtrl,
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

function showBookmarkDialog(bookmark: IBookmark): IPromise<IBookmark> {
    return showDialog<CreateBookmarkCtrl>("timBookmarksDialog", {params: () => bookmark}).result;
}
