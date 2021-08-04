/**
 * Defines the client-side implementation of the review canvas.
 */

import * as t from "io-ts";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {valueDefu} from "tim/util/utils";
import {
    ApplicationRef,
    Component,
    DoBootstrap,
    NgModule,
    OnDestroy,
    OnInit,
} from "@angular/core";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {Subject, Subscription} from "rxjs";
import {debounceTime, distinctUntilChanged} from "rxjs/operators";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";

const PluginMarkupFields = t.intersection([
    t.partial({
        inputplaceholder: nullable(t.string),
        inputstem: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
    }),
]);

const PluginFields = t.intersection([
    getTopLevelFields(PluginMarkupFields),
    t.type({
        state: nullable(t.type({})),
    }),
]);

@Component({
    selector: "reviewcanvas-runner",
    template: `
        <tim-plugin-frame [markupError]="markupError">
            <tim-plugin-header *ngIf="header" header>
                <span [innerHTML]="header"></span>
            </tim-plugin-header>
            <p stem *ngIf="stem" [innerHTML]="stem"></p>
            <ng-container body>
                <div class="form-inline">
                    <label>{{inputstem}}
                        <input type="submit"
                               class="form-control"
                               [ngModel]="file"
                               (ngModelChange)="modelChanged.next($event)"
                               [readonly]="readonly"
                               [placeholder]="inputplaceholder"
                               [size]="cols">
                    </label>
                    <ng-container>
                        <i title="There is nothing in here"></i>
                    </ng-container>
                </div>
                <div class="buttons">
                    <button class="timButton"
                            *ngIf="buttonText()"
                            [disabled]="isRunning || !userword || readonly"
                            (click)="uploadFile()">
                        {{buttonText()}}
                    </button>
                </div>
                <tim-loading *ngIf="isRunning"></tim-loading>
                <div *ngIf="error" [innerHTML]="error"></div>
                <pre *ngIf="result">{{result}}</pre>
            </ng-container>
            <p footer *ngIf="footer" [textContent]="footer"></p>
        </tim-plugin-frame>
    `,
    styleUrls: ["./reviewcanvas.scss"],
})
export class ReviewCanvasComponent
    extends AngularPluginBase<
        t.TypeOf<typeof PluginMarkupFields>,
        t.TypeOf<typeof PluginFields>,
        typeof PluginFields
    >
    implements OnInit, OnDestroy {
    result?: string;
    error?: string;
    isRunning = false;
    file = undefined;
    modelChanged: Subject<object> = new Subject<object>();
    private modelChangeSub!: Subscription;

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() ?? "Save";
    }

    ngOnInit() {
        super.ngOnInit();

        this.modelChangeSub = this.modelChanged
            .pipe(debounceTime(this.autoupdate), distinctUntilChanged())
            .subscribe((newValue) => {
                this.file = newValue;
            });
    }

    ngOnDestroy() {
        this.modelChangeSub.unsubscribe();
    }

    get correct() {
        return true;
    }

    get autoupdate(): number {
        return this.markup.autoupdate;
    }

    get inputplaceholder() {
        return this.markup.inputplaceholder ?? "";
    }

    get inputstem() {
        return this.markup.inputstem ?? null;
    }

    reset(e: MouseEvent) {
        this.error = undefined;
        this.result = undefined;
    }

    async uploadFile() {
        return this.doUploadFile();
    }

    async doUploadFile() {
        this.isRunning = true;
        this.result = undefined;
        const params = {
            input: {
                filedata: this.file,
            },
        };

        const r = await this.postAnswer<{
            web: {result: string; error?: string};
        }>(params);
        this.isRunning = false;
        if (r.ok) {
            const data = r.result;
            this.error = data.web.error;
            this.result = data.web.result;
        } else {
            this.error = r.result.error.error;
        }
    }

    getAttributeType() {
        return PluginFields;
    }
}

@NgModule({
    declarations: [ReviewCanvasComponent],
    imports: [BrowserModule, HttpClientModule, FormsModule, TimUtilityModule],
})
export class ReviewCanvasModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                ReviewCanvasModule
            )
        ),
        "reviewCanvasRunner",
        ReviewCanvasComponent
    ),
];
