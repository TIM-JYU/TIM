/**
 * Dialog showing view range menu.
 */

import {IRootElementService, IScope} from "angular";
import {IItem} from "../item/IItem";
import {
    getCurrentPartitionURLParams,
    getPieceSize,
    getViewRange,
    partitionDocument,
    setPieceSize,
    unpartitionDocument,
} from "./viewRangeInfo";

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
    private errorMessage?: string;
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
        this.errorMessage = undefined;
        if (!this.viewRangeSetting || this.viewRangeSetting < 1) {
            this.errorMessage = "Piece size needs to be an integer greater than zero.";
            return;
        }
        this.saveValues();
        if (this.partitionDocumentsSetting) {
            await setPieceSize(this.viewRangeSetting);
            const params = getCurrentPartitionURLParams();
            const b = params ? params.get("b") : undefined;
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
        this.close({});
    }

    /**
     * Resets view range settings to defaults.
     */
    private resetDefaults() {
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
            <p>Toggle showing documents in smaller parts and edit the number of paragraphs shown per part.</p>
            <div class="checkbox">
                <label><input type="checkbox" ng-model="$ctrl.partitionDocumentsSetting">Enable partitioning documents</label>
            </div>
            <label title="Enter how many paragraphs are shown at a time">Piece size: <input class="form-control"
                ng-model="$ctrl.viewRangeSetting" type="number" min="1"></label>
        </div>
        <br>
        <tim-alert ng-if="$ctrl.errorMessage" severity="warning">{{$ctrl.errorMessage}}</tim-alert>
        <tim-alert severity="info">The page may reload when the changes are saved.</tim-alert>
    </dialog-body>
    <dialog-footer>
        <button title="Return default settings" class="timButton pull-left"
            ng-click="$ctrl.resetDefaults()">Reset defaults</button>
        <button class="timButton" title="Quit and save changes" ng-click="$ctrl.ok()">OK</button>
        <button class="timButton" title="Quit and discard changes" ng-click="$ctrl.dismiss()">Cancel</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showViewRangeEditDialog(d: IItem) {
    return showDialog(ViewRangeEditController, {params: () => d}).result;
}
