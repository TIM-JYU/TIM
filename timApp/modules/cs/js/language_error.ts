import {
        Component,
        DoBootstrap,
        NgModule,
        StaticProvider,
        ApplicationRef,
    } from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {BrowserModule} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {FormsModule} from "@angular/forms";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CsBase} from "./csPlugin";

@Component({
    selector: "cs-error",
    template: `
    <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
    <div *ngIf="!markupError" class="error">
        <p>Error(s) initializing csPlugin:</p>
        <pre *ngIf="error">{{error}}</pre>
        <pre *ngIf="own_error">{{own_error}}</pre>
    </div>`,
})
class CSErrorComponent extends CsBase {

    error?: string;
    own_error?: string;

    ngOnInit() {
        super.ngOnInit();

        this.error = this.attrsall.error ?? "";
        this.own_error = this.attrsall.own_error ?? "";
    }
}

@NgModule({
    declarations: [
        CSErrorComponent,
    ],
    exports: [
        CSErrorComponent,
    ],
    imports: [
        BrowserModule,
        TimUtilityModule,
        FormsModule,
        HttpClientModule,
    ],
})
export class CsErrorModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
    }
}

const bootstrapFn = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(CsErrorModule);
};

const angularJsModule = createDowngradedModule(bootstrapFn);
doDowngrade(angularJsModule, "csError", CSErrorComponent);
export const moduleDefs = [angularJsModule];
