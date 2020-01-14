/**
 * Defines the client-side implementation of an example plugin (a palindrome checker).
 */
import * as t from "io-ts";
import {GenericPluginMarkup, getTopLevelFields, nullable, withDefault} from "tim/plugin/attributes";
import {valueDefu} from "tim/util/utils";
import {ApplicationRef, Component, DoBootstrap, NgModule, OnDestroy, OnInit, StaticProvider} from "@angular/core";
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
        initword: t.string,
        inputplaceholder: nullable(t.string),
        inputstem: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const PluginFields = t.intersection([
    getTopLevelFields(PluginMarkupFields),
    t.type({
        state: nullable(t.type({userword: t.string})),
    }),
]);

function isPalindrome(s: string) {
    const sc = s.toLowerCase().replace(/[^a-zåöä]/g, "");
    return sc === sc.split("").reverse().join("");
}

@Component({
    selector: "pali-runner",
    template: `
        <tim-plugin-frame [markupError]="markupError">
            <tim-plugin-header *ngIf="header" header>
                <span [innerHTML]="header"></span>
            </tim-plugin-header>
            <p stem *ngIf="stem" [innerHTML]="stem"></p>
            <ng-container body>
                <div class="form-inline">
                    <label>{{inputstem}}
                        <input type="text"
                               class="form-control"
                               [ngModel]="userword"
                               (ngModelChange)="modelChanged.next($event)"
                               [readonly]="readonly"
                               [placeholder]="inputplaceholder"
                               [size]="cols">
                    </label>
                    <ng-container *ngIf="userword">
                        <i title="This word is a palindrome" class="glyphicon glyphicon-ok" *ngIf="correct"></i>
                        <i title="This word is not a palindrome" class="glyphicon glyphicon-remove"
                           *ngIf="!correct"></i>
                    </ng-container>
                </div>
                <div class="buttons">
                    <button class="timButton"
                            *ngIf="buttonText()"
                            [disabled]="isRunning || !userword || readonly"
                            (click)="saveText()">
                        {{buttonText()}}
                    </button>
                    <button class="btn btn-default btn-xs"
                            *ngIf="edited"
                            (click)="reset($event)">{{resetText}}</button>
                </div>
                <tim-loading *ngIf="isRunning"></tim-loading>
                <div *ngIf="error" [innerHTML]="error"></div>
                <pre *ngIf="result">{{result}}</pre>
            </ng-container>
            <p footer *ngIf="footer" [textContent]="footer"></p>
        </tim-plugin-frame>
    `,
    styleUrls: ["./pali.scss"],
})
class PaliComponent extends AngularPluginBase<t.TypeOf<typeof PluginMarkupFields>, t.TypeOf<typeof PluginFields>, typeof PluginFields>
    implements OnInit, OnDestroy {
    result?: string;
    error?: string;
    isRunning = false;
    userword = "";
    modelChanged: Subject<string> = new Subject<string>();
    private modelChangeSub!: Subscription;

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || "Save";
    }

    ngOnInit() {
        super.ngOnInit();
        this.userword = (this.attrsall.state && this.attrsall.state.userword) || this.getInitWord();
        this.modelChangeSub = this.modelChanged
            .pipe(
                debounceTime(this.autoupdate),
                distinctUntilChanged()
            )
            .subscribe((newValue) => {
                this.userword = newValue;
            });
    }

    ngOnDestroy() {
        this.modelChangeSub.unsubscribe();
    }

    get correct() {
        return this.checkPalindrome();
    }

    get edited() {
        return this.getInitWord() !== this.userword;
    }

    get autoupdate(): number {
        return this.markup.autoupdate;
    }

    get inputplaceholder() {
        return this.markup.inputplaceholder || "";
    }

    get inputstem() {
        return this.markup.inputstem || null;
    }

    get cols() {
        return this.markup.cols;
    }

    get resetText() {
        return valueDefu(this.markup.resetText, "Reset");
    }

    checkPalindrome() {
        return isPalindrome(this.userword);
    }

    reset(e: MouseEvent) {
        this.userword = this.getInitWord();
        this.error = undefined;
        this.result = undefined;
    }

    private getInitWord() {
        return this.markup.initword || "";
    }

    async saveText() {
        return this.doSaveText(false);
    }

    async doSaveText(nosave: boolean) {
        this.isRunning = true;
        this.result = undefined;
        const params = {
            input: {
                nosave: nosave,
                paliOK: this.checkPalindrome(),
                userword: this.userword,
            },
        };

        const r = await this.postAnswer<{ web: { result: string, error?: string } }>(params);
        this.isRunning = false;
        if (r.ok) {
            const data = r.result;
            this.error = data.web.error;
            this.result = data.web.result;
        } else {
            this.error = r.result.data.error;
        }
    }

    getAttributeType() {
        return PluginFields;
    }
}

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        PaliComponent,
    ],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
    ],
})
export class PaliModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
    }
}

const bootstrapFn = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(PaliModule);
};

const angularJsModule = createDowngradedModule(bootstrapFn);
doDowngrade(angularJsModule, "paliRunner", PaliComponent);
export const moduleDefs = [angularJsModule];
