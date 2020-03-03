import {to} from "tim/util/utils";
import {DialogController} from "tim/ui/dialogController";
import {registerDialogComponent, showDialog, showMessageDialog} from "../../ui/dialog";
import {KEY_S} from "../../util/keycodes";
import {$http} from "../../util/ngimport";
import {Duplicate, IExtraData, IManageResponse, IParResponse} from "./edittypes";

export interface IManageRenameParams {
    duplicates: Duplicate[];
    original_par?: never;
    new_par_ids?: never;
    extraData?: never;
}

export interface IParRenameParams {
    duplicates: Duplicate[];
    original_par?: {md: string, attrs: unknown};
    new_par_ids: string[];
    extraData: IExtraData;
}

function isFromPar(p: IParRenameParams | IManageRenameParams): p is IParRenameParams {
    return (p as IParRenameParams).extraData != null;
}

export type IRenameParams = IManageRenameParams | IParRenameParams;

export type RenameResult = IParResponse | IManageResponse;

export function isManageResponse(r: RenameResult): r is IManageResponse {
    return (r as IManageResponse).fulltext != null;
}

class PluginRenameForm extends DialogController<{params: IRenameParams}, RenameResult> {
    static component = "timPluginRename";
    static $inject = ["$element", "$scope"] as const;
    private newNames: string[] = [];

    protected getTitle(): string {
        return "Rename duplicate task names";
    }

    $onInit() {
        super.$onInit();
    }

    getExtraData() {
        return isFromPar(this.resolve.params) ? this.resolve.params.extraData : {};
    }

    getDuplicates() {
        return this.resolve.params.duplicates;
    }

    /**
     * Called when user wants to cancel changes after entering duplicate task-ids
     */
    async cancelPluginRenameClicked() {
        // Cancels recent changes to paragraph/document
        if (isFromPar(this.resolve.params)) {
            await to($http.post("/cancelChanges/", {
                docId: this.resolve.params.extraData.docId,
                newPars: this.resolve.params.new_par_ids,
                originalPar: this.resolve.params.original_par,
                parId: this.resolve.params.extraData.par,
                ...this.getExtraData(),
            }));
        }
        this.dismiss();
    }

    /**
     * Function that handles different cases of user input in plugin rename form
     * after user has saved multiple plugins with the same taskname
     * @param renameAutomatically - Whether to rename the tasks automatically
     */
    async renameTaskNamesClicked(renameAutomatically: boolean) {
        const duplicateData = [];
        const duplicates = this.getDuplicates();

        if (renameAutomatically) {
            if (duplicates.length > 0) {
                for (const dupe of duplicates) {
                    duplicateData.push([dupe[0], "", dupe[1]]);
                }
            }
        } else {
            // use the given names
            for (let j = 0; j < duplicates.length; j++) {
                duplicateData.push([duplicates[j][0], this.newNames[j], duplicates[j][1]]);
            }
        }
        // Save the new task names for duplicates
        const response = await to($http.post<IParResponse | IManageResponse>("/postNewTaskNames/", {
            duplicates: duplicateData,
            ...this.getExtraData(),
        }));
        if (!response.ok) {
            return;
        }
        const data = response.result.data;
        // If no new duplicates were found
        if (!data.duplicates || data.duplicates.length <= 0) {
            this.close(data);
            return;
        }

        // If there still are duplicates, refresh the form
        if (data.duplicates.length > 0) {
            await showMessageDialog("Some duplicates are still remaining. Please rename them. Click OK to update the form.");
            this.resolve.params.duplicates = data.duplicates;
        }
    }

    keyHandler(e: KeyboardEvent) {
        if (e.ctrlKey) {
            if (e.keyCode === KEY_S) {
                this.renameTaskNamesClicked(false);
                e.preventDefault();
            }
        }
    }
}

registerDialogComponent(PluginRenameForm,
    {
        template: `<tim-dialog>
    <dialog-header>

    </dialog-header>
    <dialog-body>
        <p>There are multiple objects with the same task name in this document. Plugins with duplicate task names will
            not work properly. Rename the duplicates by writing new names in the field(s) below and click "Save", choose
            "Rename automatically" or "Ignore" to proceed without renaming.</p>
        <strong>Rename duplicates:</strong>
        <form name="$ctrl.form" ng-keydown="$ctrl.keyHandler($event)">
            <div ng-repeat="dupe in $ctrl.getDuplicates()" class="form-group form-horz-flex">
                <label for="field{{$id}}" ng-bind="dupe[0]"></label>
                <input type="text" class="form-control" name="field{{$id}}" id="field{{$id}}" required
                       ng-model="$ctrl.newNames[$index]">
                <i ng-show="dupe[2]" class="glyphicon glyphicon-warning-sign"
                   uib-tooltip="There are answers related to this task.
                   They might be lost upon renaming this task."></i>
            </div>
        </form>
    </dialog-body>
    <dialog-footer>
        <button ng-disabled="$ctrl.form.$invalid" class="timButton"
                title="Rename task names with given names (Ctrl + S)"
                ng-click="$ctrl.renameTaskNamesClicked(false)">Save
        </button>
        <button class="timButton" title="Rename duplicate task names automatically"
                ng-click="$ctrl.renameTaskNamesClicked(true)">Rename automatically
        </button>
        <button class="timButton" title="Proceed without renaming"
                ng-click="$ctrl.dismiss()">Ignore
        </button>
        <button class="timButton" title="Return to editor"
                ng-click="$ctrl.cancelPluginRenameClicked()">Cancel
        </button>
    </dialog-footer>
</tim-dialog>
    `,
    });

export function showRenameDialog(p: IRenameParams) {
    return showDialog(PluginRenameForm, {params: () => p}).result;
}
