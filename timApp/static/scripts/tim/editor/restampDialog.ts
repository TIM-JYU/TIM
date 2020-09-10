/**
 * Controller and HTML template for attachment restamping dialog.
 */

import {IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController} from "tim/ui/dialogController";
import {registerDialogComponent, showDialog} from "../ui/dialog";
import {$http} from "../util/ngimport";
import {markAsUsed, to} from "../util/utils";
import {IStampingData} from "./pareditor";

// The close states dialog can return.
export enum RestampDialogClose {
    RestampedReturnToEditor,
    NoRestampingReturnToEditor,
    RestampingFailedReturnToEditor,
    SaveAndExit,
}

markAsUsed(focusMe);

/*
 * Restamping dialog's controller.
 */
export class RestampDialogController extends DialogController<
    {params: IStampingData},
    RestampDialogClose
> {
    static component = "restampDialog";
    static $inject = ["$element", "$scope"] as const;
    private header = "";
    private stampingData?: IStampingData;
    private stamping: boolean = false;
    private stampingDone: boolean = false;
    private errorMessage?: string;
    private successMessage?: string;

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);
    }

    $onInit() {
        super.$onInit();
        this.stampingData = this.resolve.params;
    }

    /**
     * Dismiss is same as returning to editor.
     */
    protected dismiss() {
        this.returnToEditor();
    }

    /**
     * Restamps all attachments in the open editor.
     */
    async restamp() {
        if (this.stamping) {
            return;
        }
        this.stamping = true;
        if (this.stampingData && this.stampingData.attachments.length > 0) {
            const stampingParams = {
                attachments: this.stampingData.attachments,
                customStampModel: this.stampingData.customStampModel,
                meetingDate: this.stampingData.meetingDate,
                stampFormat: this.stampingData.stampFormat,
            };
            const r = await to($http.post(`/upload/restamp`, stampingParams));

            if (!r.ok) {
                this.errorMessage = r.result.data.error;
                this.successMessage = undefined;
                if (!this.errorMessage) {
                    this.errorMessage = "Unknown error while updating stamps";
                }
            } else {
                this.errorMessage = undefined;
                this.successMessage = `Stamps successfully updated.`;
            }
        } else {
            // This should never show up.
            this.errorMessage = "Unable to find attachments!";
        }
        this.stampingDone = true;
        this.stamping = false;
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Update stamps";
    }

    /**
     * Returns an appropriate close state when returning to editor
     * based on whether restamping was done and whether it was successful.
     */
    private returnToEditor() {
        if (this.stampingDone && this.errorMessage) {
            this.close(RestampDialogClose.RestampingFailedReturnToEditor);
            return;
        }
        if (this.stampingDone) {
            this.close(RestampDialogClose.RestampedReturnToEditor);
            return;
        }
        this.close(RestampDialogClose.NoRestampingReturnToEditor);
    }

    /**
     * When continuing exiting from editor.
     * Whether restamping was done or not doesn't matter here.
     */
    private saveAndExit() {
        this.close(RestampDialogClose.SaveAndExit);
    }
}

registerDialogComponent(RestampDialogController, {
    template: `<tim-dialog>
    <dialog-header>
    </dialog-header>
    <dialog-body>
        <tim-alert ng-if="!$ctrl.successMessage && !$ctrl.errorMessage" severity="info">There are changes in
             <ng-pluralize count="$ctrl.stampingData.attachments.length"
             when="{'1': ' the contents of the macro ', 'other': ' the macro contents '}"></ng-pluralize>
             that may not have been updated to the stamps yet!
        </tim-alert>
        <tim-alert severity="success" ng-if="$ctrl.successMessage">{{$ctrl.successMessage}}</tim-alert>
        <tim-alert severity="warning" ng-if="$ctrl.errorMessage">{{$ctrl.errorMessage}}</tim-alert>
        <div>
            <p>You can update the stamps by pressing the <ng-pluralize count="$ctrl.stampingData.attachments.length"
            when="{'1': 'Restamp', 'other': 'Restamp all'}"></ng-pluralize> button below. You can also return to the
            editor to manually check and reupload the attachments.</p>
            <div>
                <button class="timButton" ng-click="$ctrl.restamp()">
                <span ng-if="$ctrl.stamping" class="glyphicon glyphicon-refresh glyphicon-refresh-animate"></span>
                <span ng-if="$ctrl.stamping">Stamping...</span>
                <ng-pluralize ng-if="!$ctrl.stamping" count="$ctrl.stampingData.attachments.length"
                when="{'1': 'Restamp', 'other': 'Restamp all'}"></ng-pluralize>
                </button>
            </div>
        </div>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.returnToEditor()">Return to editor</button>
        <button class="timButton" ng-click="$ctrl.saveAndExit()">Save and exit</button>
    </dialog-footer>
</tim-dialog>
`,
});

export function showRestampDialog(stampingData: IStampingData) {
    return showDialog(RestampDialogController, {params: () => stampingData})
        .result;
}
