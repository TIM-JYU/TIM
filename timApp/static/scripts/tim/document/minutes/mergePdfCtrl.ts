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

export interface IAttachment {
    path: string;
    macro: string;
    error: string;
}

export class MergePdfController extends DialogController<{ params: IMergeParams }, {}> {
    static component = "timMergePdf";
    static $inject = ["$element", "$scope"] as const;
    private docUrl?: string;
    private loading: boolean = false;
    private attachmentList: IAttachment[] = [];
    private checking: boolean = true;
    private errorMessage?: string;

    // TODO: Clean reduntant code and add loading icon when checking.
    // TODO: Clean prints from server side.

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
     */
    async listAttachments() {
        this.checking = true;
        const url = `/minutes/checkAttachments/${this.resolve.params.document.path}`;
        const r = await to($http.get<IAttachment[]>(url, {}));
        if (!r.ok) {
            this.errorMessage = r.result.data.error;
            return;
        }
        console.log(r.result.data);
        this.attachmentList = r.result.data;
        this.checking = false;
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

    /**
     * Pick different colors for macros.
     * @param macro Macro name.
     */
    private macroStyle(macro: string) {
        if (macro == "liite") {
            return "color:green;";
        }
        if (macro == "perusliite") {
            return "color:#999900;";
        }
        // Error case.
        if (macro == "unknown") {
            return "color:red;";
        }
        return "";
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
                <li ng-repeat="x in $ctrl.attachmentList track by $index">{{x.path}}
                     <span style="{{::$ctrl.macroStyle(x.macro)}}">{{x.macro}}</span>
                     <span ng-if="x.error" style="color:red;" class="glyphicon glyphicon-warning-sign"
                        uib-tooltip="{{x.error}}" tooltip-placement="left"></span>
                </li>
            </ul>
            <p ng-if="$ctrl.attachmentList.length == 0 && !$ctrl.checking">No attachments found</p>
        </div>
        <div ng-if="$ctrl.checking" class="alert alert-warning">
            <span class="glyphicon glyphicon-exclamation-sign"></span>
            Checking validity of the attachments, please wait...
        </div>
        <div ng-if="!$ctrl.checking && $ctrl.attachmentList.length > 0" class="alert alert-warning">
            <span class="glyphicon glyphicon-exclamation-sign"></span>
            Note: Missing and "perusliite" attachments will not be merged.
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
