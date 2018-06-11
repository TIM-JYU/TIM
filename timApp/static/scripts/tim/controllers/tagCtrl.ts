import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../dialog";
import {IItem} from "../IItem";
import {IParResponse} from "../edittypes";
import {IRootElementService, IScope} from "angular";
import {Moment} from "moment";
import {$http} from "../ngimport";
import {to} from "../utils";

/**
 * Controller and HTML template for tag dialog.
 */

export class ShowAddTagController extends DialogController<{params: IItem}, {}, "timAddTag"> {
    private static $inject = ["$element", "$scope"];
    private docUrl?: string;
    private loading: boolean;
    private document: IItem;
    private tagName: string;
    private expires: Moment;
    private added: boolean = false;
    private error: boolean = false;
    private errorMessage: string;
    private successMessage: string;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    public getTitle() {
        return "Edit document tags";
    }

    /*
     * Sends post method with tag name and optional expiration date, and
     * saves either success or error message to be used in the dialog.
     */
    private async addTagClicked() {
        const docTitle = this.resolve.params.name;
        const data = {tag: this.tagName, expires: this.expires};
        const [err, response] = await to($http.post<IParResponse>(`/tags/add/${docTitle}`, data));
        if (err) {
            this.error =  true;
            this.errorMessage = err.data.error;
            if (this.added) {
                this.added = false;
            }
            return;
        }
        if (response) {
            this.added = true;
            this.successMessage = "Tag '" + this.tagName + "' successfully added.";
            if (this.error) {
                this.error = false;
            }
            return;
        }
    }
}

registerDialogComponent("timAddTag",
    ShowAddTagController,
    {
        template:
            `<tim-dialog class="overflow-visible">
    <dialog-header ng-bind-html="$ctrl.getTitle()">
    </dialog-header>
    <dialog-body>
    <form name="$ctrl.f" class="form-horizontal">
        <div class="form-group" tim-error-state
             ng-class="{'has-error': !$ctrl.f.nameField.$pristine && $ctrl.f.nameField.$error.required}">
            <label for="name" class="col-sm-4 control-label">Tag name:</label>
            <div class="col-sm-8">
                <input required focus-me="$ctrl.focusName" tim-short-name ng-model="$ctrl.tagName" name="tagField"
                       type="text"
                       class="form-control" id="name" placeholder="Tag name">
            </div>
            <tim-error-message></tim-error-message>
        </div>
        <div class="form-group"
             ng-class="{'has-error': !$ctrl.f.nameField.$pristine && $ctrl.f.nameField.$error.required}">
            <label for="name" class="col-sm-4 control-label">Expiration date:</label>
            <div class="col-sm-8"><div class="input-group date" datetimepicker ng-model="$ctrl.expires"
                 data-options="datePickerOptionsFrom">
                <input type="text" class="form-control" placeholder="Leave blank for indefinite period of validity"/>
                <span class="input-group-addon">
                        <span class="glyphicon glyphicon-calendar"></span>
                </span>
            </div></div>
        </div>
    </form>
        <button class="btn timButton"  data-ng-disabled="$ctrl.f.$invalid" ng-click="$ctrl.addTagClicked()">Add</button>
        <button class="btn timButton" ng-click="$ctrl.dismiss()"><span>Cancel</span></button>
    </dialog-body>
    <dialog-footer>
        <div ng-show="$ctrl.added" class="alert alert-success">
            <span class="glyphicon glyphicon-ok"></span> {{$ctrl.successMessage}}
        </div>
        <div ng-show="$ctrl.error" class="alert alert-warning">
            <span class="glyphicon glyphicon-exclamation-sign"></span> {{$ctrl.errorMessage}}
        </div>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showAddTagDialog(d: IItem) {
    return await showDialog<ShowAddTagController>("timAddTag", {params: () => d}).result;
}