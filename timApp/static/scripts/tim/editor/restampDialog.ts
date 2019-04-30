/**
 * Controller and HTML template for tag search dialog.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {markAsUsed} from "../util/utils";
import {IAttachmentData, IStampingData} from "./pareditor";

markAsUsed(focusMe);

/*
 * Tag search dialog's controller.
 */
export class ShowRestampDialogController extends DialogController<{params: IStampingData}, {}> {
    static component = "restampDialog";
    static $inject = ["$element", "$scope"] as const;
    private header = "";
    private stampingData?: IStampingData;
    private stampingDone: boolean = false;

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

    $onDestroy() {
    }

    restamp() {
        // TODO: Stamping & error handling.
        this.stampingDone = true;
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Update stamps";
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
            You can update the stamps by pressing the <ng-pluralize count="$ctrl.stampingData.attachments.length"
            when="{'1': 'Restamp', 'other': 'Restamp all'}"></ng-pluralize> button below. Alternatively you can
            reupload the attachments manually in the editor.
            <div>
                <button class="timButton" ng-click="$ctrl.restamp()">
                <ng-pluralize count="$ctrl.stampingData.attachments.length"
                when="{'1': 'Restamp', 'other': 'Restamp all'}"></ng-pluralize>
                </button>
            </div>
        </div>
        <div ng-if="$ctrl.stampingDone">
            <span>Stamps updated!</span>
        </div>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.dismiss()">Continue</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showRestampDialog(stampingData: IStampingData) {
    return await showDialog(ShowRestampDialogController, {params: () => stampingData}).result;
}
