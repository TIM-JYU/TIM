/**
 * Dialog showing view range menu.
 */

import {IRootElementService, IScope} from "angular";
import {ngStorage} from "ngstorage";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {$http, $localStorage} from "../util/ngimport";
import {markAsUsed, to} from "../util/utils";
import {IItem} from "./IItem";

markAsUsed(focusMe);

/**
 * Partition document by reloading.
 * @param b First shown par index.
 * @param e Last shown par incex.
 * @param doc Document.
 */
export function partitionDocument(b: number, e: number, doc: Document) {
    doc.location.search = `?b=${b}&e=${e}`;
}

/**
 * Disable document partitioning by reloading.
 * @param doc Document.
 */
export function unpartitionDocument(doc: Document) {
    if (doc.location.search.toString().includes("&e=")) {
        doc.location.search = "";
    }
}

export async function unsetPieceSize() {
    const r = await to($http.get<number>(`/viewrange/unset/piecesize`));
    if (!r.ok) {
        console.log("Failed to remove view range cookie: " + r);
    } else {
        console.log("View range cookie set as -1");
    }
}

export async function setPieceSize(pieceSize: number) {
    const data = {pieceSize: pieceSize};
    const r = await to($http.post(`/viewrange/set/piecesize`, data));
    if (!r.ok) {
        console.log("Failed to set view range cookie: " + r);
    } else {
        console.log("View range cookie created with value: " + pieceSize);
    }
}

export async function getPieceSize(): Promise<number | undefined> {
    const r = await to($http.get<number>(`/viewrange/get/piecesize`));
    if (!r.ok) {
        console.log("View range cookie not found: " + r.result.data);
    } else {
        console.log("View range cookie found with value: " + r.result.data);
        return r.result.data;
    }
}

export interface IViewRange {
    b: number;
    e: number;
}

export async function getViewRange(doc: Document, index: number, forwards: boolean): Promise<IViewRange | undefined> {
    let forwardsInt = 1;
    if (!forwards) {
        forwardsInt = 0;
    }
    const r = await to($http.get<IViewRange>(`/viewrange/get/${doc.location.pathname}/${index}/${forwardsInt}`));
    if (!r.ok) {
        console.log(r.result.data);
    } else {
        console.log(r.result.data);
        return r.result.data;
    }
}

/*
 * Dialog displaying view range options.
 */
export class ViewRangeEditController extends DialogController<{ params: IItem }, {}> {
    static component = "viewRangeEditDialog";
    static $inject = ["$element", "$scope"] as const;
    private item!: IItem;
    private partitionDocumentsSetting: boolean = false;
    private viewRangeSetting: number = 20;
    private storage: ngStorage.StorageService & {
    viewRange: null | string,
    partitionDocuments: null | boolean};

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
        this.storage = $localStorage.$default({
            viewRange: null,
            partitionDocuments: null,
        });
    }

    async $onInit() {
        super.$onInit();
        this.item = this.resolve.params;
        this.loadValues();
    }

    /**
     * Fetches options from local storage, if existent.
     */
    public loadValues() {
        if (this.storage.partitionDocuments != null) {
            this.partitionDocumentsSetting = this.storage.partitionDocuments;
        }
        if (this.storage.viewRange != null) {
            this.viewRangeSetting = +this.storage.viewRange;
        }
    }

    /**
     * Save new values to local storage.
     */
    private saveValues() {
        this.storage.viewRange = this.viewRangeSetting.toString();
        this.storage.partitionDocuments = this.partitionDocumentsSetting;
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Edit view range";
    }

    /**
     * Saves view range settings and quits.
     */
    private ok() {
        this.saveValues();
        if (this.partitionDocumentsSetting) {
            setPieceSize(this.viewRangeSetting);
            // partitionDocument(0, this.viewRangeSetting, document);
        } else {
            unpartitionDocument(document);
        }
        this.dismiss();
    }

    /**
     * Saves view range settings.
     */
    private returnDefaults() {
        this.viewRangeSetting = 20;
        this.partitionDocumentsSetting = false;
    }
}

registerDialogComponent(ViewRangeEditController,
    {
        template:
            `<tim-dialog class="overflow-visible">
    <dialog-header>
    </dialog-header>
    <dialog-body>
        <div>
            <h4>{{$ctrl.header}}</h4>
            <p>Toggle showing documents in smaller parts and edit the step size.</p>
            <div>
                <label class="footerLabel">Enable partitioning documents
                    <input type="checkbox" ng-model="$ctrl.partitionDocumentsSetting">
                </label>
            </div>
            <br>
            <label>Number of paragraphs shown per part:
            <input ng-model="$ctrl.viewRangeSetting" type="number" min="1"
                        title="Enter how many paragraphs are shown at a time">
            </label>
        </div>
        <br>
        <div ng-cloak class="alert alert-warning">
            <span class="glyphicon glyphicon-exclamation-sign"></span> Note: the page may reload if changes are saved.
        </div>
    </dialog-body>
    <dialog-footer>
        <button style="float: left;" title="Return default settings" class="timButton" ng-click="$ctrl.returnDefaults()">Return defaults</button>
        <button class="timButton" title="Quit and reload page with chosen settings" ng-click="$ctrl.ok()">Ok</button>
        <button class="timButton" title="Quit and discard changes" ng-click="$ctrl.dismiss()">Cancel</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showViewRangeEditDialog(d: IItem) {
    return await showDialog(ViewRangeEditController, {params: () => d}).result;
}
