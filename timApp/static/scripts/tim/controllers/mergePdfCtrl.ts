import {ngStorage} from "ngstorage";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {IItem} from "../IItem";

export interface ITemplate extends IItem {

}

export interface IMergeParams {

}

export class ShowMergePdfController extends DialogController<{ params: IMergeParams }, {}, "mergePdf"> {
    private storage: ngStorage.StorageService & { timPrintingTemplateId: null | number };


    constructor() {
        super();
    }

    public getTitle() {
        return "Merging attachments";
    }

    $onInit() {
        super.$onInit();
    }
}

registerDialogComponent("mergePdf",
    ShowMergePdfController,
    {
        template:
            `
            <tim-dialog>
            <dialog-header ng-bind-html="$ctrl.getTitle()">
            </dialog-header>
            <dialog-body>
                <p>
                    <label>Merging happening.</label>
                </p>
            </dialog-body>
            <dialog-footer>
                <button class="timButton" ng-click="$ctrl.close()">Close</button>
            </dialog-footer>
        </tim-dialog>
`,
    });

export async function showMergePdfDialog(p: IMergeParams) {
    return await showDialog<ShowMergePdfController>("mergePdf", {params: () => p}).result;
}
