/**
 * Dialog for showing suggestions for spelling errors.
 */

import {ISpellWordInfo, PareditorController} from "tim/editor/pareditor";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "tim/ui/dialog";
import {copyToClipboard} from "tim/util/utils";
import {vctrlInstance} from "tim/document/viewctrlinstance";

export interface ISpellErrorParams {
    info: ISpellWordInfo;
    dialogX: number;
    dialogY: number;
}

enum SourceType {
    Comment,
    Paragraph,
}

export function computeSourcePosition(editorText: string, info: Readonly<ISpellWordInfo>, type: SourceType) {

    // If the word starts or ends with a non-ascii character, word boundary regex does not work.
    // See https://stackoverflow.com/questions/10590098/javascript-regexp-word-boundaries-unicode-characters
    const nonAsciiRe = /[åäöÅÄÖ]$|^[åäöÅÄÖ]/g;
    let word = info.word;
    const needsAsciiHack = nonAsciiRe.test(word);
    if (needsAsciiHack) {
        // We'll replace the non-ascii characters with uncommon ascii characters.
        const asciiChar = "X";
        const asciiRe = /[åäöÅÄÖ]/g;
        editorText = editorText.replace(asciiRe, asciiChar);
        word = word.replace(asciiRe, asciiChar);
    }

    let blocks = type == SourceType.Paragraph ? editorText.split(/(?:\n|^)(?:#-|(?:#+|`{3,}) *{.+})/) : [editorText];
    if (blocks[0].trim() == "") {
        blocks = blocks.slice(1);
    }
    const block = blocks[info.blockIndex];
    if (block == undefined) {
        return undefined;
    }
    const re = new RegExp(`\\b${word}\\b`, "g");
    let match;
    let index;
    let occurrence = 0;
    while ((match = re.exec(block)) != null) {
        occurrence++;
        if (occurrence == info.occurrence) {
            index = match.index;
            break;
        }
    }
    if (index == undefined) {
        return undefined;
    }
    return index + editorText.indexOf(block);
}

export class SpellErrorDialogController extends DialogController<{ params: ISpellErrorParams }, {}> {
    static component = "spellErrorDialogController";
    static $inject = ["$element", "$scope"] as const;
    private suggestions: string[] = [];
    private pare!: PareditorController;
    private type!: SourceType;

    $onInit() {
        super.$onInit();
        const vctrl = vctrlInstance!;
        if (vctrl.notesHandler.editor) {
            this.pare = vctrl.notesHandler.editor;
            this.type = SourceType.Comment;
        } else {
            this.pare = vctrl.editingHandler.getParEditor()!;
            this.type = SourceType.Paragraph;
        }
        this.suggestions = this.resolve.params.info.suggestions;
        (async () => {
            await this.getDraggable().makeHeightAutomatic();
            await this.moveTo({X: this.resolve.params.dialogX, Y: this.resolve.params.dialogY});
            this.selectWord();
        })();
    }

    get options() {
        return this.resolve.params.info;
    }

    public getTitle() {
        return "Oikeinkirjoitus";
    }

    ignoreWord() {
        this.selectWord();
        this.pare.getEditor()!.surroundClicked("[", "]{.nospell}");
        this.pare.editorChanged();
        this.close({});
    }

    selectWord() {
        this.pare.getEditor()!.focus();
        const pos = computeSourcePosition(this.pare.getEditor()!.getEditorText(), this.options, this.type);
        if (pos == undefined) {
            void showMessageDialog(`Cannot find the word '${this.options.word}' from editor text.`);
            return;
        }
        this.pare.getEditor()!.setPosition([pos, pos + this.options.word.length]);
    }

    selectSuggestion(s: string) {
        copyToClipboard(s);
        this.selectWord();
    }

    replaceWord(s: string) {
        this.selectWord();
        this.pare.getEditor()!.replaceSelectedText(s);
        this.pare.editorChanged();
        this.close({});
    }
}

registerDialogComponent(SpellErrorDialogController,
    {
        template:
            `
<tim-dialog class="overflow-visible">
    <dialog-body>
        <div ng-if="$ctrl.suggestions.length > 0">
            Ehdotukset:
            <ul ng-repeat="sug in $ctrl.suggestions">
                <li ng-click="$ctrl.selectSuggestion(sug); $event.stopPropagation()"
                    title="Korvaa '{{$ctrl.options.word}}' sanalla '{{sug}}'">
                    <a ng-click="$ctrl.replaceWord(sug); $event.stopPropagation()">{{sug}}</a>
                </li>
            </ul>
        </div>
        <div ng-if="$ctrl.suggestions.length == 0">Ei ehdotuksia.</div>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" ng-click="$ctrl.ignoreWord(); $event.stopPropagation()"
                title="Älä huomioi sanaa jatkossa">
            Ohita sana
        </button>
        <button class="timButton" ng-click="$ctrl.dismiss()">Peruuta</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export function showSpellErrorDialog(s: ISpellErrorParams) {
    return showDialog(SpellErrorDialogController, {params: () => s});
}
