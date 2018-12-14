/**
 * Dialog showing relevance editing component.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {markAsUsed} from "../util/utils";
import {IItem} from "./IItem";

markAsUsed(focusMe);

/*
 * Dialog displaying relevance editing component.
 */
export class ShowRelevanceEditDialog extends DialogController<{ params: IItem }, {}, "relevanceEditDialog"> {
    private static $inject = ["$element", "$scope"];

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    async $onInit() {
        super.$onInit();
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Edit relevance";
    }

}

registerDialogComponent("relevanceEditDialog",
    ShowRelevanceEditDialog,
    {
        template:
            `<tim-dialog class="overflow-visible">
    <dialog-header>
    </dialog-header>
    <dialog-body>
        <h4>{{$ctrl.header}}</h4>
        <relevance-edit></relevance-edit>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showRelevanceEditDialog(d: IItem) {
    return await showDialog<ShowRelevanceEditDialog>("relevanceEditDialog", {params: () => d}).result;
}
