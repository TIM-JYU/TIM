/**
 * Controller for merging attachments in TIM documents.
 */
import {IRootElementService, IScope} from "angular";
import {IItem} from "../../item/IItem";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../../ui/dialog";
import {$http} from "../../util/ngimport";
import {to} from "../../util/utils";

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

export interface IAttachmentParams {
    pdf_paths: string[];
    incomplete_list: boolean;
}

export class MergePdfController extends DialogController<{ params: IMergeParams }, {}> {
    static component = "timMergePdf";
    static $inject = ["$element", "$scope"] as const;
    private docUrl?: string;
    private loading: boolean = false;
    private attachmentList: string[] = [];
    private warnIncompleteList: boolean = false;
    private errorMessage?: string;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    /**
     * Dialog title.
     * @returns {string}
     */
    public getTitle() {
        return "Merge attachments";
    }

    /**
     * Gets a list of attachments in the documents and notes whether invalid files were found.
     * @returns {Promise<void>}
     */
    async listAttachments() {
        const url = `/minutes/listAttachments/${this.resolve.params.document.path}`;
        const r = await to($http.get<IAttachmentParams>(url, {}));
        if (!r.ok) {
            this.errorMessage = r.result.data.error;
            return;
        }
        this.attachmentList = r.result.data.pdf_paths;
        this.warnIncompleteList = r.result.data.incomplete_list;
    }

    /**
     * Deals with clicking "Merge" timMergePdf dialog.
     * Show error messages for users.
     */
    async mergeClicked() {

        this.loading = true;
        const url = `/minutes/mergeAttachments/${this.resolve.params.document.path}`;
        const r = await to($http.get<{url: string}>(url, {}));
        if (!r.ok) {
            void showMessageDialog(r.result.data.error);
            this.loading = false;
            return;
        }

        this.loading = false;
        const r2 = await to($http.post<{url: string}>(url, {}));

        if (!r2.ok) {
            void showMessageDialog(r2.result.data.error);
            this.loading = false;
            return;
        }
        this.docUrl = r2.result.data.url;
    }

    async $onInit() {
        super.$onInit();
        await this.listAttachments();
        this.loading = false;
    }
}

/**
 * HTML Template for merge dialog.
 */
registerDialogComponent(MergePdfController,
    {
        template:
            `<tim-dialog>
    <dialog-header ng-bind-html="$ctrl.getTitle()">
    </dialog-header>
    <dialog-body>
        <p ng-show="$ctrl.attachmentList.length > 0">Following attachments were found from the current document</p>
        <div>
            <ul>
                <li ng-repeat="x in $ctrl.attachmentList track by $index">{{x}}</li>
            </ul>
            <p ng-if="$ctrl.attachmentList.length == 0">No attachments found</p>
        </div>
        <div ng-show="$ctrl.warnIncompleteList" class="alert alert-warning">
            <span class="glyphicon glyphicon-exclamation-sign"></span>
            One or more unlisted attachments may have been invalid and won't be part of the merged file.
            Check attachment macros for broken links and non-pdf files or continue by merging
            the listed attachments.
        </div>

        <p id="link">
        </p>
        <button class="timButton" ng-click="$ctrl.mergeClicked()" ng-disabled="$ctrl.attachmentList.length == 0">
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
    return await showDialog(MergePdfController, {params: () => p}).result;
}
