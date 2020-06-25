/**
 * Dialog showing relevance editing component.
 */

import {IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController} from "tim/ui/dialogController";
import {registerDialogComponent, showDialog} from "../ui/dialog";
import {markAsUsed} from "../util/utils";
import {IItem} from "./IItem";

markAsUsed(focusMe);

/*
 * Dialog displaying relevance editing component.
 */
export class RelevanceEditController extends DialogController<{ params: IItem }, void> {
    static component = "relevanceEditDialog";
    static $inject = ["$element", "$scope"] as const;
    private item!: IItem;

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);
    }

    $onInit() {
        super.$onInit();
        this.item = this.resolve.params;
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Edit relevance";
    }

}

registerDialogComponent(RelevanceEditController,
    {
        template:
            `<tim-dialog class="overflow-visible">
    <dialog-header>
    </dialog-header>
    <dialog-body>
        <h4>{{$ctrl.header}}</h4>
        <relevance-edit item="$ctrl.item"></relevance-edit>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showRelevanceEditDialog(d: IItem) {
    return await showDialog(RelevanceEditController, {params: () => d}).result;
}
