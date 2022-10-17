import type {
    ApplicationRef,
    DoBootstrap,
    OnDestroy,
    OnInit,
} from "@angular/core";
import {Component, ElementRef, Input, NgModule} from "@angular/core";
import {to2} from "tim/util/utils";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import $ from "jquery";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {
    ISpellErrorParams,
    SpellErrorDialogComponent as SpellErrorDialogComponentType,
} from "tim/document/editing/spell-error-dialog.component";
import {CommonModule} from "@angular/common";

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
    private dlg?: SpellErrorDialogComponentType;

    constructor(private e: ElementRef<HTMLElement>) {}

    ngOnDestroy() {
        if (this.dlg) {
            this.dlg.close();
        }
    }

    ngOnInit() {}

    async selectWord(e: MouseEvent) {
        if (this.dlg) {
            return;
        }
        const par = $(this.e.nativeElement).parents(".par");
        this.dlg = await showSpellErrorDialog({
            info: {
                word: this.e.nativeElement.textContent!,
                occurrence: this.count ?? 1,
                blockIndex: par.length > 0 ? par.index() : 0, // In case we're editing a comment, there is no par class.
                suggestions: this.sugg,
            },
            dialogX: e.clientX,
            dialogY: e.clientY,
        });
        await to2(this.dlg.result);
        this.dlg = undefined;
    }
}

@NgModule({
    declarations: [SpellErrorComponent],
    imports: [CommonModule, TimUtilityModule],
})
export class SpellModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
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

export const spellModule = createDowngradedModule((extraProviders) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(SpellModule);
});

doDowngrade(spellModule, "timSpellError", SpellErrorComponent);
