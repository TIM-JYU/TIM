import {ngStorage} from "ngstorage";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {IItem} from "../IItem";
import {$http} from "../ngimport";

export interface IMergeParams {
    document: IItem;
}

export class ShowMergePdfController extends DialogController<{ params: IMergeParams }, {}, "timMergePdf"> {
    private storage: ngStorage.StorageService & { timPrintingTemplateId: null | number };


    constructor() {
        super();
    }

    public getTitle() {
        return "Merging attachments";
    }

    async mergeClicked() {
        console.log(this.resolve.params.document.id);
        console.log(this.resolve.params.document);

        const response = await $http.get<{ url: string }>(`/mergeAttachments/${this.resolve.params.document.location}/${this.resolve.params.document.name}`);

        //console.log(response.data);
        //console.log(response.data.url);

        //const response = await $http.get<{url:string}>('/processAttachments/${this.resolve.params.document.id');
        //console.log(response.data.url);
        //response.data.url;
    }

    $onInit() {
        super.$onInit();
    }
}

registerDialogComponent("timMergePdf",
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
                <button class = "timButton" ng-click ="$ctrl.mergeClicked()">Merge</button>
                <button class="timButton" ng-click="$ctrl.close()">Close</button>
            </dialog-footer>
        </tim-dialog>
`,
    });

export async function showMergePdfDialog(p: IMergeParams) {
    return await showDialog<ShowMergePdfController>("timMergePdf", {params: () => p}).result;
}
