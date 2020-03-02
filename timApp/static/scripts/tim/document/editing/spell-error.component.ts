import {PareditorController} from "tim/editor/pareditor";
import {Component, Input, OnInit, ElementRef, OnDestroy} from "@angular/core";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {to} from "tim/util/utils";
import {showSpellErrorDialog, SpellErrorDialogController} from "./spellErrorDialog";

@Component({
    selector: "tim-spell-error",
    template: `
        <span class="dropdown-error" (click)="selectWord($event)"><ng-content></ng-content></span>
    `,
    styleUrls: ["./spell-error.component.scss"],
})
export class SpellErrorComponent implements OnInit, OnDestroy {
    @Input() private sugg!: string[];
    @Input() private count?: number;
    private pare!: PareditorController;
    private dlg?: SpellErrorDialogController;

    constructor(private e: ElementRef<HTMLElement>) {
    }

    ngOnDestroy() {
        if (this.dlg) {
            this.dlg.close({});
        }
    }

    ngOnInit() {
        const vctrl = vctrlInstance!;
        this.pare = vctrl.editingHandler.getParEditor()!;
    }

    async selectWord(e: MouseEvent) {
        if (this.dlg) {
            return;
        }
        const par = $(this.e.nativeElement).parents(".par");
        const d = showSpellErrorDialog({
            info: {
                word: this.e.nativeElement.textContent!,
                occurrence: this.count ?? 1,
                blockIndex: par.index(),
                suggestions: this.sugg,
            },
            dialogX: e.clientX,
            dialogY: e.clientY,
        });
        this.dlg = await d.dialogInstance.promise;
        await to(d.result);
        this.dlg = undefined;
    }
}
