/**
 * Dialog for showing suggestions for spelling errors.
 */

import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {ISpellWordInfo, PareditorController} from "tim/editor/pareditor";
import {copyToClipboard} from "tim/util/utils";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {$rootScope} from "tim/util/ngimport";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {BrowserModule} from "@angular/platform-browser";
import {getCurrentEditor} from "tim/editor/editorScope";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

export interface ISpellErrorParams {
    info: ISpellWordInfo;
    dialogX: number;
    dialogY: number;
}

enum SourceType {
    Comment,
    Paragraph,
    Other,
}

function computeSourcePosition(
    editorText: string,
    info: Readonly<ISpellWordInfo>,
    type: SourceType
) {
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

    let blocks =
        type == SourceType.Paragraph
            ? editorText.split(/(?:\n|^)(?:#-|(?:#+|`{3,}) *{.+})/)
            : [editorText];
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

@Component({
    selector: "tim-spell-error-dialog",
    template: `
        <tim-dialog-frame class="overflow-visible">
            <ng-container header>
                Sanan {{options.word}} oikeinkirjoitus
            </ng-container>
            <ng-container body>
                <div *ngIf="suggestions.length > 0">
                    Ehdotukset:
                    <ul *ngFor="let sug of suggestions">
                        <li title="Korvaa '{{options.word}}' sanalla '{{sug}}'">
                            <a (click)="replaceWord(sug); $event.stopPropagation()">{{sug}}</a>
                        </li>
                    </ul>
                </div>
                <div *ngIf="suggestions.length == 0">Ei ehdotuksia.</div>
                <tim-alert *ngIf="wordFindError">
                    Sanaa '{{options.word}}' ei löydy markdown-tekstistä.
                </tim-alert>
                <tim-alert *ngIf="copied" severity="info">
                    Sana '{{copied}}' kopioitu leikepöydälle.
                </tim-alert>
            </ng-container>
            <ng-container footer>
                <button class="timButton"
                        (click)="ignoreWord(); $event.stopPropagation()"
                        [disabled]="wordFindError"
                        title="Älä huomioi sanaa jatkossa">
                    Ohita sana
                </button>
                <button class="timButton" (click)="dismiss()">Peruuta</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class SpellErrorDialogComponent extends AngularDialogComponent<
    ISpellErrorParams,
    void
> {
    protected dialogName = "spellError";
    suggestions: string[] = [];
    private pare!: PareditorController;
    private type!: SourceType;
    wordFindError = false;
    copied?: string;

    ngOnInit() {
        const editor = getCurrentEditor();
        if (!editor) {
            throw Error("No editor was open");
        }
        this.pare = editor;
        const vctrl = vctrlInstance!;
        if (vctrl.notesHandler.editor) {
            this.type = SourceType.Comment;
        } else if (vctrl.editingHandler.getParEditor()) {
            this.type = SourceType.Paragraph;
        } else {
            this.type = SourceType.Other; // Can be e.g. timTable cell editor.
        }
        this.suggestions = this.data.info.suggestions;
        this.selectWord();
        $rootScope.$applyAsync();
    }

    async ngAfterViewInit() {
        super.ngAfterViewInit();
        this.frame.setPos({
            x: this.data.dialogX - this.xOrigin!,
            y: this.data.dialogY,
        });
        await this.setHeightAutomatic();
    }

    get options() {
        return this.data.info;
    }

    ignoreWord() {
        this.selectWord();
        this.pare.getEditor()!.surroundClicked("[", "]{.nospell}");
        this.close();
    }

    selectWord() {
        this.pare.getEditor()!.focus();
        const pos = computeSourcePosition(
            this.pare.getEditor()!.getEditorText(),
            this.options,
            this.type
        );
        if (pos == undefined) {
            this.wordFindError = true;
            return;
        }
        this.pare
            .getEditor()!
            .setPosition([pos, pos + this.options.word.length]);
    }

    replaceWord(s: string) {
        if (this.wordFindError) {
            copyToClipboard(s);
            this.copied = s;
            return;
        }
        this.selectWord();
        this.pare.getEditor()!.replaceSelectedText(s);
        this.close();
    }
}

@NgModule({
    declarations: [SpellErrorDialogComponent],
    imports: [BrowserModule, DialogModule, TimUtilityModule],
})
export class SpellErrorDialogModule {}
