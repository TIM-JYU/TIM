import {ApplicationRef, Component, DoBootstrap, ElementRef, Input, NgModule, OnDestroy, OnInit} from "@angular/core";
import {to} from "tim/util/utils";
import {BrowserModule} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import $ from "jquery";
import {showDialog} from "../../ui/dialog";
import {$injector} from "../../util/ngimport";
import {ISpellErrorParams, SpellErrorDialogController} from "./spellErrorDialog";

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
    private dlg?: SpellErrorDialogController;

    constructor(private e: ElementRef<HTMLElement>) {
    }

    ngOnDestroy() {
        if (this.dlg) {
            this.dlg.close();
        }
    }

    ngOnInit() {
    }

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
        this.dlg = await d.dialogInstance.promise;
        await to(d.result);
        this.dlg = undefined;
    }
}

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        SpellErrorComponent,
    ],
    imports: [
        BrowserModule,
    ],
})
export class SpellModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
    }
}

async function showSpellErrorDialog(s: ISpellErrorParams) {
    const file = await import("./spellErrorDialog");
    $injector.loadNewModules([file.dialogModule.name]);
    return showDialog(file.SpellErrorDialogController, {params: () => s});
}

export const spellModule = createDowngradedModule((extraProviders) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(SpellModule);
});

doDowngrade(spellModule, "timSpellError", SpellErrorComponent);
