/**
 * Controller and HTML template for tag search dialog.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {markAsUsed, to} from "../util/utils";
import {IStampingData} from "./pareditor";
import {$http} from "../util/ngimport";

markAsUsed(focusMe);

/*
 * Tag search dialog's controller.
 */
export class ShowRestampDialogController extends DialogController<{params: IStampingData}, {}> {
    static component = "restampDialog";
    static $inject = ["$element", "$scope"] as const;
    private header = "";
    private stampingData?: IStampingData;
    private stamping: boolean = false;
    private stampingDone: boolean = false;
    private errorMessage?: string;
    private successMessage?: string;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    /*
     * Show tag list when dialog loads and focus on tag-field.
     */
    async $onInit() {
        super.$onInit();
        this.stampingData = this.resolve.params;
        console.log(this.stampingData);
        console.log(this.stampingData.attachments.length);
    }

    /**
     * Restamps all attachments in open editor.
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
                stampFormat: this.stampingData.stampFormat
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
            this.errorMessage = "Unable to find attachments!";
        }
        // this.stampingDone = true;
        this.stamping = false;
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Update stamps";
    }

    private returnToEditor() {
        //TODO:
    }

}

registerDialogComponent(ShowRestampDialogController,
    {
        template:
            `<tim-dialog>
    <dialog-header>
    </dialog-header>
    <dialog-body>
        <b>Attachment stamps may not be up-to-date</b>
        <div class="alert alert-warning">
            <span class="glyphicon glyphicon-exclamation-sign"></span> There are changes in
             <ng-pluralize count="$ctrl.stampingData.attachments.length"
             when="{'1': ' the contents of the macro ', 'other': ' the macro contents '}"></ng-pluralize>
             that may not have been updated to the stamps yet!
        </div>
        <div ng-if="!$ctrl.stampingDone">
            <p>You can update the stamps by pressing the <ng-pluralize count="$ctrl.stampingData.attachments.length"
            when="{'1': 'Restamp', 'other': 'Restamp all'}"></ng-pluralize> button below. Alternatively you can
            reupload the attachments manually in the editor.</p>
            <div>
                <button class="timButton" ng-click="$ctrl.restamp()">
                <span ng-if="$ctrl.stamping" class="glyphicon glyphicon-refresh glyphicon-refresh-animate"></span>
                <span ng-if="$ctrl.stamping">Stamping...</span>
                <ng-pluralize ng-if="!$ctrl.stamping" count="$ctrl.stampingData.attachments.length"
                when="{'1': 'Restamp', 'other': 'Restamp all'}"></ng-pluralize>
                </button>
            </div>
        </div>
        <div ng-show="$ctrl.successMessage" class="alert alert-success">
            <span class="glyphicon glyphicon-ok"></span> {{$ctrl.successMessage}}
        </div>
        <div ng-show="$ctrl.errorMessage" class="alert alert-warning">
            <span class="glyphicon glyphicon-exclamation-sign"></span> {{$ctrl.errorMessage}}
        </div>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.returnToEditor()">Return to editor</button>
        <button class="timButton" ng-click="$ctrl.dismiss()">Save and exit</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showRestampDialog(stampingData: IStampingData) {
    return await showDialog(ShowRestampDialogController, {params: () => stampingData}).result;
}
