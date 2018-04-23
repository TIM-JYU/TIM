import {ngStorage} from "ngstorage";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {IItem} from "../IItem";
import {$http} from "../ngimport";




/**
 * @module mergePdfCtrl
 * @author Matti Leinonen
 * @author Ronja Lindholm
 * @author Visa Naukkarinen
 * @author Rami Pasanen
 * @author Enni Stylman
 * @licence MIT
 * @copyright 2018 Titus project authors
 */

export interface IMergeParams {
    document: IItem;
}

export class ShowMergePdfController extends DialogController<{ params: IMergeParams }, {}, "timMergePdf"> {
    private storage: ngStorage.StorageService & { timPrintingTemplateId: null | number };
    private docUrl?: string;
    private loading: boolean;
    private document: IItem;

    constructor() {
        super();
    }

    public getTitle() {
        return "Merging attachments";
    }

    async mergeClicked() {

        //this.document.path = `/mergeAttachments/${this.resolve.params.document.location}/${this.resolve.params.document.name}`;
        this.loading = true;
        const postURL = "/merge" + this.resolve.params.document.path;

       const response = await $http.get<{url: string}>(`/mergeAttachments/${this.resolve.params.document.path}`, {});

        const response1 = await $http.post<{url: string}>(`/mergeAttachments/${this.resolve.params.document.path}`, {});

        //console.log(this.resolve.params.document.path);

        if (response) {
            this.loading = false;
        }

        this.docUrl = response1.data.url;
        //console.log(this.docUrl); undefined
    }

    $onInit() {
        super.$onInit();
        this.loading = false;
    }
}

registerDialogComponent("timMergePdf",
    ShowMergePdfController,
    {
        template:
            `<tim-dialog>
    <dialog-header ng-bind-html="$ctrl.getTitle()">
    </dialog-header>
    <dialog-body>
        <div>Merging all attachments from the current document.</div>
        <p id="link">
        </p>
        <button class="btn timButton" ng-click="$ctrl.mergeClicked()">
                    <span ng-show="$ctrl.loading"><i class="glyphicon glyphicon-refresh glyphicon-refresh-animate"></i>
                    Merging</span>
            <span ng-hide="$ctrl.loading">Merge</span>
        </button>
        <button class="btn timButton" ng-click="$ctrl.dismiss()"><span>Cancel</span>
        </button>
        <div ng-show="$ctrl.docUrl" class="alert alert-success">
            <span class="glyphicon glyphicon-ok"></span>Merging succeeded!
            <a href="{{$ctrl.docUrl}}"
               target="_blank">View the document.</a>
        </div>
    </dialog-body>
    <dialog-footer></dialog-footer>
</tim-dialog>
`,
    });

export async function showMergePdfDialog(p: IMergeParams) {
    return await showDialog<ShowMergePdfController>("timMergePdf", {params: () => p}).result;
}
