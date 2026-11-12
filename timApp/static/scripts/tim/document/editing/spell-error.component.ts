import type {OnDestroy} from "@angular/core";
import {
    Component,
    ElementRef,
    Input,
    ChangeDetectionStrategy,
} from "@angular/core";
import {to2} from "tim/util/utils";
import $ from "jquery";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {
    ISpellErrorParams,
    SpellErrorDialogComponent as SpellErrorDialogComponentType,
} from "tim/document/editing/spell-error-dialog.component";

@Component({
    selector: "tim-spell-error",
    template: `
        <span class="dropdown-error" (click)="selectWord($event)"><ng-content></ng-content></span>
    `,
    styleUrls: ["./spell-error.component.scss"],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class SpellErrorComponent implements OnDestroy {
    // The element is a custom element (see registerElement), so the inputs come from HTML attributes as strings.
    @Input() sugg: string[] | string = [];
    @Input() count?: number | string;
    private dlg?: SpellErrorDialogComponentType;

    constructor(private e: ElementRef<HTMLElement>) {}

    ngOnDestroy() {
        if (this.dlg) {
            this.dlg.close();
        }
    }

    private getSuggestions(): string[] {
        if (typeof this.sugg !== "string") {
            return this.sugg;
        }
        try {
            const parsed: unknown = JSON.parse(this.sugg);
            return Array.isArray(parsed) ? (parsed as string[]) : [];
        } catch {
            return [];
        }
    }

    async selectWord(e: MouseEvent) {
        if (this.dlg) {
            return;
        }
        const par = $(this.e.nativeElement).parents(".par");
        this.dlg = await showSpellErrorDialog({
            info: {
                word: this.e.nativeElement.textContent,
                occurrence: Number(this.count ?? 1) || 1,
                blockIndex: par.length > 0 ? par.index() : 0, // In case we're editing a comment, there is no par class.
                suggestions: this.getSuggestions(),
            },
            dialogX: e.clientX,
            dialogY: e.clientY,
        });
        await to2(this.dlg.result);
        this.dlg = undefined;
    }
}

async function showSpellErrorDialog(s: ISpellErrorParams) {
    const {SpellErrorDialogComponent} = await import(
        "./spell-error-dialog.component"
    );
    return angularDialog.open<
        ISpellErrorParams,
        void,
        SpellErrorDialogComponentType
    >(SpellErrorDialogComponent, s);
}
