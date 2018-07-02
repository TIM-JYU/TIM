/**
Controller for merging attachments in TIM documents.
 */
import {IRootElementService, IScope} from "angular";
import {ngStorage} from "ngstorage";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../dialog";
import {IItem} from "../IItem";
import {$http} from "../ngimport";
import {to} from "../utils";

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
    private static $inject = ["$element", "$scope"];
    private docUrl?: string;
    private loading: boolean = false;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    public getTitle() {
        return "Merging attachments";
    }

    /**
 * Deals with clicking "Merge" timMergePdf dialog.
 * Show error messages for users.
     * */
    async mergeClicked() {

        this.loading = true;
        const postURL = "/merge" + this.resolve.params.document.path;

       const  [err, response] = await to($http.get<{url: string}>(`/minutes/mergeAttachments/${this.resolve.params.document.path}`, {}));

       //catch error message frin get route
        if (err) {
            showMessageDialog(err.data.error);
            this.loading = false;
            return;
        }

        if (response) {
            this.loading = false;

            const [err2, response2] = await to($http.post<{url: string}>(`/minutes/mergeAttachments/${this.resolve.params.document.path}`, {}));

            //catch error message from post route
            if(err2) {
                showMessageDialog (err2.data.error);
                this.loading = false;
                return;
            }

            else if (response2) {
                this.docUrl = response2.data.url;
            }
        }
    }

    $onInit() {
        super.$onInit();
        this.loading = false;
    }
}

/**
HTML Template for merge dialog.
 */

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
        <button class="timButton" ng-click="$ctrl.mergeClicked()">
                    <span ng-show="$ctrl.loading"><i class="glyphicon glyphicon-refresh glyphicon-refresh-animate"></i>
                    Merging</span>
            <span ng-hide="$ctrl.loading">Merge</span>
        </button>
        <button class="timButton" ng-click="$ctrl.dismiss()"><span>Cancel</span>
        </button>
        <div ng-show="$ctrl.docUrl" class="alert alert-success">
            <span class="glyphicon glyphicon-ok"></span> Merging succeeded!
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
