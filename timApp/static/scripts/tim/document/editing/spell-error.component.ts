import {
    ApplicationRef,
    Component,
    DoBootstrap,
    ElementRef,
    Input,
    NgModule,
    OnDestroy,
    OnInit,
} from "@angular/core";
import {to} from "tim/util/utils";
import {BrowserModule} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import $ from "jquery";
import {angularDialog} from "../../ui/angulardialog/dialog.service";
import type {
    ISpellErrorParams,
    SpellErrorDialogComponent as SpellErrorDialogComponentType,
} from "./spell-error-dialog.component";

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
        const d = await showSpellErrorDialog({
            info: {
                word: this.e.nativeElement.textContent!,
                occurrence: this.count ?? 1,
                blockIndex: par.length > 0 ? par.index() : 0, // In case we're editing a comment, there is no par class.
                suggestions: this.sugg,
            },
            dialogX: e.clientX,
            dialogY: e.clientY,
        });
        this.dlg = await d;
        await to(d.result);
        this.dlg = undefined;
    }
}

@NgModule({
    declarations: [SpellErrorComponent],
    imports: [BrowserModule],
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
