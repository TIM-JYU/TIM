import { IFormController, IScope } from "angular";
import {DialogController} from "tim/ui/dialogController";
import {registerDialogComponent, showDialog} from "../ui/dialog";
import {IBookmark} from "./bookmarks";

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

    constructor(protected element: JQLite, protected scope: IScope) {
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
