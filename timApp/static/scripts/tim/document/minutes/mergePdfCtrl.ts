/**
 * Controller for checking PDF-attachment validity and merging them.
 */
import {IScope} from "angular";
import {DialogController} from "tim/ui/dialogController";
import {IItem} from "../../item/IItem";
import {registerDialogComponent, showDialog, showMessageDialog} from "../../ui/dialog";
import {$http} from "../../util/ngimport";
import {to} from "../../util/utils";

/**
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
    url: string;
    macro: string;
    error: string;
    selected: boolean;
}

export class MergePdfController extends DialogController<{ params: IMergeParams }, {}> {
    static component = "timMergePdf";
    static $inject = ["$element", "$scope"] as const;
    private attachmentList: IAttachment[] = [];
    private loading: boolean = false; // Whether the merging process is underway.
    private checking: boolean = true; // Whether the attachment check is underway.
    private errorMessage?: string;
    private mergedUrl?: string; // Link to file opening route with merge data as params.

    constructor(protected element: JQLite, protected scope: IScope) {
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
            this.checking = false;
            return;
        }
        this.attachmentList = r.result.data;
        this.checking = false;
    }

    /**
     * Deals with clicking "Merge" timMergePdf dialog.
     * Show error messages for users.
     */
    async mergeClicked() {
        this.loading = true;
        const url = `/minutes/mergeAttachments`;
        const data = this.getSelectedAttachmentData();
        const r1 = await to($http.post<{url: string}>(url, data));
        if (!r1.ok) {
            void showMessageDialog(r1.result.data.error);
            this.loading = false;
            return;
        }
        this.loading = false;
        this.mergedUrl = r1.result.data.url;
    }

    $onInit() {
        super.$onInit();
        (async () => {
            await this.listAttachments();
        })();
    }

    /**
     * Pick different colors for macros.
     * @param macro Macro name.
     */
    private macroStyle(macro: string) {
        if (macro == "liite") {
            return {color: "green"};
        }
        if (macro == "perusliite") {
            return {color: "#999900"};
        }
        // Error case.
        return {color: "red"};
    }

    /**
     * Format the attachment list as route data.
     */
    private getSelectedAttachmentData() {
        const urls: string[] = [];
        for (const attachment of this.attachmentList) {
            if (attachment.selected) {
                urls.push(attachment.url);
            }
        }
        return {doc_id: this.resolve.params.document.id, urls: urls};
    }

    /**
     * Show only the last part of a file URL or path (the file name).
     * @param url URL or path to shorten with either \ or /, doesn't need to be complete.
     */
    private getFileName(url: string) {
        return url.replace(/^.*[\\\/]/, "");
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
    <form class="form-horizontal">
        <p ng-if="$ctrl.attachmentList.length > 0">Following attachments were found in the current document:</p>
        <div class="form-group">
            <div class="col-sm-12">
                <ul class="list-unstyled">
                    <li ng-repeat="x in $ctrl.attachmentList track by $index">
                        <label>
                            <input type="checkbox" ng-model="x.selected" ng-disabled="x.error">
                            <a href="{{::x.url}}" target="_blank">{{::$ctrl.getFileName(x.url)}}</a>
                        </label>
                        <span ng-style="::$ctrl.macroStyle(x.macro)">{{::x.macro}}</span>
                        <span ng-if="::x.error" style="color:red;" class="glyphicon glyphicon-warning-sign"
                           uib-tooltip="{{::x.error}}" tooltip-placement="auto"></span>
                    </li>
                </ul>
                <p ng-if="!$ctrl.errorMessage && $ctrl.attachmentList.length == 0 && !$ctrl.checking">No attachments found</p>
                <div ng-if="$ctrl.errorMessage && !$ctrl.checking" class="alert alert-warning">
                    <span class="glyphicon glyphicon-exclamation-sign"></span>{{$ctrl.errorMessage}}
                </div>
            </div>
        </div>
        <div ng-if="$ctrl.checking" class="alert alert-warning">
            <i class="glyphicon glyphicon-refresh glyphicon-refresh-animate"></i>
            Checking validity of the attachments, please wait...
        </div>
    </form>
    </dialog-body>
    <dialog-footer>
        <div style="float: left;">
            <button class="timButton" ng-click="$ctrl.mergeClicked()" ng-disabled="$ctrl.attachmentList.length == 0"
                        title="Merge selected files">
                        <span ng-if="$ctrl.loading"><i class="glyphicon glyphicon-refresh glyphicon-refresh-animate"></i>
                        Merging</span>
                <span ng-hide="$ctrl.loading">Merge selected</span>
            </button>
            <button class="timButton" ng-click="$ctrl.dismiss()"><span>Cancel</span></button>
        </div>
        <div style="float: right;" ng-if="$ctrl.mergedUrl" class="alert alert-success">
            <span class="glyphicon glyphicon-ok"></span> Merging succeeded!
            <a href="{{$ctrl.mergedUrl}}" target="_blank">View the document.</a>
        </div>
</dialog-footer>
</tim-dialog>
`,
    });

export async function showMergePdfDialog(p: IMergeParams) {
    return await showDialog(MergePdfController, {params: () => p}).result;
}
