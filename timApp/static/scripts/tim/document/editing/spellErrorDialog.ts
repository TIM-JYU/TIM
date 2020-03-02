/**
 * Dialog for showing suggestions for spelling errors.
 */

import {IRootElementService, IScope} from "angular";
import * as focusMe from "tim/ui/focusMe";
import {PareditorController} from "../../editor/pareditor";
import {DialogController, registerDialogComponent, showDialog} from "../../ui/dialog";
import {copyToClipboard, markAsUsed, Require} from "../../util/utils";
import {ViewCtrl} from "../viewctrl";

export interface ISpellErrorParams {
    suggestions: string[];
    position: number;
    length: number;
    dialogX: number;
    dialogY: number;
    vc: Require<ViewCtrl>;
}

export class SpellErrorDialogController extends DialogController<{params: ISpellErrorParams}, {}> {
    static component = "spellErrorDialogController";
    static $inject = ["$element", "$scope"] as const;
    private vctrl!: Require<ViewCtrl>;
    private suggestions: string[] = [];
    private pare: PareditorController;
    private options: ISpellErrorParams;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    async $onInit() {
        super.$onInit();
        this.vctrl = this.resolve.params.vc;
        this.pare = this.vctrl.editingHandler.getParEditor() as PareditorController;
        this.suggestions = this.resolve.params.suggestions;
        this.options = this.resolve.params;
        this.selectWord();
        this.getDraggable().makeHeightAutomatic();
        this.moveTo({X: this.options.dialogX, Y: this.options.dialogY});
    }

    public getTitle() {
        return "Suggestions";
    }

    async ignoreWord() {
        this.selectWord();
        this.pare.getEditor()!.surroundClicked("[", "]{.nospell}");
        this.pare.editorChanged();
        this.close({});
    }

    async selectWord() {
        this.pare.getEditor()!.focus();
        this.pare.getEditor()!.setPosition([this.options.position, this.options.position + this.options.length]);
    }

    async selectSuggestion(s: string) {
        copyToClipboard(s);
        this.selectWord();
    }

    async replaceWord(s: string) {
        this.pare.getEditor()!.focus();
        this.pare.getEditor()!.setPosition([this.options.position, this.options.position + this.options.length]);
        this.pare.getEditor()!.editor!.replaceSelectedText(s);
        this.pare.editorChanged();
        this.close({});
    }
}

registerDialogComponent(SpellErrorDialogController,
    {
        template:
            `<tim-dialog class="overflow-visible">
    <dialog-body>
            <div ng-repeat="sug in $ctrl.suggestions">
                    <div ng-click="$ctrl.selectSuggestion(sug); $event.stopPropagation()" title="Korvaa sana">
                        <button class="timButton" ng-click="$ctrl.replaceWord(sug); $event.stopPropagation()">K</button> {{sug}}
                    </div>
             </div>
             <button class="timButton" ng-click="$ctrl.ignoreWord(); $event.stopPropagation()" title="Älä huomioi sanaa jatkossa">Ohita</button>
            <div ng-show="!$ctrl.suggestions.length" class="red">Ei ehdotuksia.</div>
    </dialog-body>
</tim-dialog>
`,
    });

export async function showSpellErrorDialog(s: ISpellErrorParams) {
    return await showDialog(SpellErrorDialogController, {params: () => s}).result;
}
