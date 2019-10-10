/**
 * Dialog showing view range menu.
 */

import {IRootElementService, IScope} from "angular";
import {
    getPieceSize,
    getViewRange,
    partitionDocument,
    setPieceSize,
    unpartitionDocument,
} from "../document/viewRangeInfo";
import {IItem} from "./IItem";

import {ngStorage} from "ngstorage";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {$localStorage} from "../util/ngimport";
import {markAsUsed} from "../util/utils";

markAsUsed(focusMe);

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
