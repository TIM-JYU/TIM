/**
 * Dialog showing view range menu.
 */

import {IRootElementService, IScope} from "angular";
import {ngStorage} from "ngstorage";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {documentglobals} from "../util/globals";
import {$http, $localStorage} from "../util/ngimport";
import {markAsUsed, to} from "../util/utils";
import {IItem} from "./IItem";

markAsUsed(focusMe);

// TODO: Are b and e always in the same order?
export const viewRangeRegExp = new RegExp("b=\\d+\&e=\\d+\(&preamble=(true|false)|)");
export const viewRangeCookieRegExp = new RegExp("r=\\d+;");

export interface IViewRange {
    b: number;
    e: number;
}

/**
 * Disable document partitioning by reloading.
 */
export async function unpartitionDocument() {
    await unsetPieceSize();
    if (!viewRangeRegExp.test(document.location.search)) {
        location.reload();
    } else {
        document.location.search = document.location.search.replace(viewRangeRegExp, "");
    }
}

/**
 * Get document par count. Requires view rights.
 * @param docId Document id.
 */
export async function getParCount(docId: number) {
    const r = await to($http.get<{parCount: number}>(`/viewrange/parCount/${docId}`));
    if (!r.ok) {
        return undefined;
    } else {
        return r.result.data.parCount;
    }
}

/**
 * Partition document by reloading. If there's existing partitioning, replace it,
 * otherwise add to url parameters.
 * @param b First shown par index.
 * @param e Last shown par index.
 * @param loadPreamble Load preamble at the beginning of each part.
 */
export function partitionDocument(b: number, e: number, loadPreamble: boolean) {
    const allParams = document.location.search;
    const newParams = `b=${b}&e=${e}&preamble=${loadPreamble}`;
    if (viewRangeRegExp.test(allParams)) {
        document.location.search = document.location.search.replace(viewRangeRegExp, newParams);
    } else {
        document.location.search += `${newParams}`;
    }
}

export async function unsetPieceSize() {
    const r = await to($http.get<number>(`/viewrange/unset/piecesize`));
    if (!r.ok) {
        console.error("Failed to remove view range cookie!");
    }
}

export async function setPieceSize(pieceSize: number) {
    const data = {pieceSize: pieceSize};
    const r = await to($http.post(`/viewrange/set/piecesize`, data));
    if (!r.ok) {
        console.error("Failed to set view range cookie!");
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
        return undefined;
    } else {
        return value;
    }
}

/**
 * Get next or previous view range indices for the document.
 * These depend on starting index, direction, and document size.
 * @param docId Document id.
 * @param index Begin (or end, if fetching previous range) index.
 * @param forwards True if next piece, false if previous.
 */
export async function getViewRange(docId: number, index: number, forwards: boolean) {
    if (!index) {
        index = 0;
    }
    let forwardsInt = 1;
    if (!forwards) {
        forwardsInt = 0;
    }
    const r = await to($http.get<IViewRange>(`/viewrange/get/${docId}/${index}/${forwardsInt}`));
    if (!r.ok) {
        return undefined;
    } else {
        return r.result.data;
    }
}

/**
 *
 * @param docId
 * @param headerId
 */
export async function getViewRangeWithHeaderId(docId: number, headerId: string) {
    const r = await to($http.get<IViewRange>(`/viewrange/getWithHeaderId/${docId}/${headerId}`));
    if (!r.ok) {
        return undefined;
    } else {
        return r.result.data;
    }
}

/**
 * Toggle document partitioning with part starting from index 0.
 * @param docId Document id.
 * @param pieceSize Size of the document part.
 */
export async function toggleViewRange(docId: number, pieceSize: number) {
    const currentViewRange = await getCurrentViewRange();
    if (currentViewRange) {
        await unpartitionDocument();
    } else {
        await setPieceSize(pieceSize);
        const range = await getViewRange(docId, 0, true);
        if (range) {
            partitionDocument(range.b, range.e, true);
        } else {
            await unpartitionDocument();
        }
    }
}

/**
 * Get currently active view range (if it exists) from document variables.
 */
export function getCurrentViewRange() {
    const viewRange = documentglobals().current_view_range;
    const pieceSize = getPieceSize();
    if (viewRange && pieceSize) {
        return viewRange;
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
        pieceSize: null | number,
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
        this.storage.pieceSize = this.viewRangeSetting;
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
            if (b)  {
                beginIndex = +b;
            }
            const range = await getViewRange(this.resolve.params.id, beginIndex, true);
            if (range) {
                partitionDocument(range.b, range.e, true);
            }
        } else {
            await unpartitionDocument();
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
            <span>Toggle showing documents in smaller parts and edit the number of paragraphs shown per part.</span>
            <br>
            <br>
            <label title="Enable partitioning TIM documents">Enable partitioning documents: <input type="checkbox"
                ng-model="$ctrl.partitionDocumentsSetting"></label>
            <br>
            <label title="Enter how many paragraphs are shown at a time">Piece size: <input
                ng-model="$ctrl.viewRangeSetting" type="number" min="1"></label>
        </div>
        <br>
        <tim-alert severity="info">Note: the page may reload when the changes are saved.</tim-alert>
    </dialog-body>
    <dialog-footer>
        <button style="float: left;" title="Return default settings" class="timButton"
            ng-click="$ctrl.returnDefaults()">Return defaults</button>
        <button class="timButton" title="Quit and save changes" ng-click="$ctrl.ok()">Ok</button>
        <button class="timButton" title="Quit and discard changes" ng-click="$ctrl.dismiss()">Cancel</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showViewRangeEditDialog(d: IItem) {
    return await showDialog(ViewRangeEditController, {params: () => d}).result;
}
