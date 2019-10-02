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

// TODO: Are b and e always in the same order?
export const viewRangeRegExp = new RegExp("\&b=\\d+\&e=\\d+");
export const viewRangeCookieRegExp = new RegExp("r=\\d+;");

/**
 * Disable document partitioning by reloading. Preserves other params.
 */
export function unpartitionDocument() {
    document.location.search = document.location.search.replace(viewRangeRegExp, "");
}

/**
 * Partition document by reloading. If there's existing partitioning, replace it,
 * otherwise add to url parameters.
 * @param b First shown par index.
 * @param e Last shown par index.
 */
export function partitionDocument(b: number, e: number) {
    const allParams = document.location.search;
    const newParams = `&b=${b}&e=${e}`;
    if (viewRangeRegExp.test(allParams)) {
        document.location.search = document.location.search.replace(viewRangeRegExp, newParams);
    } else {
        document.location.search += `${newParams}`;
    }
}

export async function unsetPieceSize() {
    const r = await to($http.get<number>(`/viewrange/unset/piecesize`));
    if (!r.ok) {
        console.log("Failed to remove view range cookie: " + r);
    }
}

export async function setPieceSize(pieceSize: number) {
    const data = {pieceSize: pieceSize};
    const r = await to($http.post(`/viewrange/set/piecesize`, data));
    if (!r.ok) {
        console.log("Failed to set view range cookie: " + r);
    }
}

export async function getPieceSize() {
    const cookie = document.cookie.match(viewRangeCookieRegExp);
    let value = 0;
    if (cookie != null) {
        value = +cookie[0].replace(/[^\d-]/g, "");
    }
    // Convert to null since +string is 0 if string to integer conversion fails.
    if (cookie == null || value == 0) {
        return null;
    } else {
        return value;
    }
}

export interface IViewRange {
    b: number;
    e: number;
}

/**
 * Get view range from the document. Depends on starting index, direction, and document's size, areas and
 * @param docId
 * @param index
 * @param forwards
 */
export async function getViewRange(docId: number, index: number, forwards: boolean) {
    let forwardsInt = 1;
    if (!forwards) {
        forwardsInt = 0;
    }
    const r = await to($http.get<IViewRange>(`/viewrange/get/${docId}/${index}/${forwardsInt}`));
    if (!r.ok) {
        console.log(r.result.data);
    } else {
        return r.result.data;
    }
}

/**
 * Toggle document partitioning with part starting from index 0.
 * @param docId
 * @param pieceSize Size of the document part.
 */
export async function toggleViewRange(docId: number, pieceSize: number) {
    const currentViewRange = await getPieceSize();
    if (currentViewRange && currentViewRange > 0) {
        void unsetPieceSize(); // Waiting for this isn't necessary?
        unpartitionDocument();
    } else {
        await setPieceSize(pieceSize);
        const range = await getViewRange(docId, 0, true);
        if (range) {
            partitionDocument(range.b, range.e);
        }
    }
}

export function getCurrentViewRange() {
    const b = new URL(document.location.href).searchParams.get("b");
    const e = new URL(document.location.href).searchParams.get("e");
    if (b != null && e != null) {
        return {b: +b, e: +e};
    } else {
        return undefined;
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
        pieceSize: null | string,
    };

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
        this.storage = $localStorage.$default({
            pieceSize: null,
        });
    }

    async $onInit() {
        super.$onInit();
        this.item = this.resolve.params;
        this.loadValues();
        const cookie = await getPieceSize();
        if (cookie) {
            this.partitionDocumentsSetting = true;
        }
    }

    /**
     * Fetches options from local storage, if existent.
     */
    public loadValues() {
        if (this.storage.pieceSize != null) {
            this.viewRangeSetting = +this.storage.pieceSize;
        }
    }

    /**
     * Save new values to local storage.
     */
    private saveValues() {
        this.storage.pieceSize = this.viewRangeSetting.toString();
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
    private async ok() {
        this.saveValues();
        if (this.partitionDocumentsSetting) {
            await setPieceSize(this.viewRangeSetting);
            const b = new URL(document.location.href).searchParams.get("b");
            let beginIndex = 0;
            console.log(b);
            if (b)  {
                // TODO: Handle case when b is not a number.
                beginIndex = +b;
            }
            const range = await getViewRange(this.resolve.params.id, beginIndex, true);
            if (range) {
                partitionDocument(range.b, range.e);
            }
        } else {
            await unsetPieceSize();
            unpartitionDocument();
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
