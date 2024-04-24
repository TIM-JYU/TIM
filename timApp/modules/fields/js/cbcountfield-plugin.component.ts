/**
 * Defines the client-side implementation of cbcountfield/label plugin.
 */
import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, ElementRef, NgModule, NgZone} from "@angular/core";
import type {ISetAnswerResult, ITimComponent} from "tim/document/viewctrl";
import {ChangeType, FormModeOption} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {getFormBehavior} from "tim/plugin/util";
import {valueOr} from "tim/util/utils";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {DomSanitizer} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {FieldBasicData} from "./textfield-plugin.component";

const CbcountfieldMarkup = t.intersection([
    t.partial({
        tag: nullable(t.string),
        inputplaceholder: nullable(t.string),
        inputstem: nullable(t.string),
        initword: nullable(t.string),
        validinput: nullable(t.string),
        errormessage: nullable(t.string),
        readOnlyStyle: nullable(t.string),
        showname: nullable(t.number),
        autosave: t.boolean,
        limit: nullable(t.number),
    }),
    GenericPluginMarkup,
    t.type({
        cols: withDefault(t.number, 0),
    }),
]);
const CbcountfieldAll = t.intersection([
    t.partial({
        count: t.number,
    }),
    t.type({
        info: Info,
        markup: CbcountfieldMarkup,
        preview: t.boolean,
        state: nullable(FieldBasicData),
    }),
]);

@Component({
    selector: "tim-cbcountfield-runner",
    template: `
<div class="textfieldNoSaveDiv" [ngStyle]="cols">
    <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
    <h4 *ngIf="header" [innerHtml]="header | purify"></h4>
    <p class="stem" *ngIf="stem">{{stem}}</p>
     <span style="width: 100%">
      <span class="inputstem" *ngIf="inputstem" [innerHtml]="inputstem | purify"></span>
      <span  *ngIf="!isPlainText()" [ngClass]="{warnFrame: (isUnSaved() ), alertFrame: saveFailed}">
        <!-- <span *ngIf="isUnSaved()"  [ngClass]="{warnFrame: (isUnSaved() )  }">&nbsp;</span> -->
        <input type="checkbox"
               *ngIf="!isPlainText()"
               [ngStyle]="cbStyle"
               class="form-control"
               [(ngModel)]="userword"
               (ngModelChange)="autoSave()"
               [disabled]="disabled || readonly || this.attrsall['preview']"
               [tooltip]="errormessage"
               [isOpen]="errormessage !== undefined"
               >
               <button class="timButton"
                        *ngIf="!hasButton() && saveFailed"
                        (click)="autoSave()">
                    {{buttonText()}}
               </button>
          <span class="cbfieldcount">{{count}}</span>
      </span>
<span *ngIf="isPlainText()" style="">{{userword}}</span>
         </span>
        <button class="timButton"
            *ngIf="!isPlainText() && hasButton()"
            [disabled]="(disableUnchanged && !isUnSaved()) || isRunning || readonly || attrsall['preview']"
            (click)="saveText()">
        {{buttonText()}}
        </button>
    <!-- <p class="savedtext" *ngIf="!hideSavedText && buttonText()">Saved!</p> -->
    <p *ngIf="footer" [innerHtml]="footer | purify" class="plgfooter"></p>
</div>
`,
    styleUrls: ["./cbcountfield-plugin.component.scss"],
})
export class CbcountfieldPluginComponent
    extends AngularPluginBase<
        t.TypeOf<typeof CbcountfieldMarkup>,
        t.TypeOf<typeof CbcountfieldAll>,
        typeof CbcountfieldAll
    >
    implements ITimComponent
{
    private result?: string;
    isRunning = false;
    userword: boolean = false;

    private initialValue: boolean = false;
    errormessage?: string;
    hideSavedText = true;
    private saveResponse: {saved: boolean; message: string | undefined} = {
        saved: false,
        message: undefined,
    };
    private preventedAutosave = false;
    count = 0;
    disabled = false;
    saveFailed = false;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        private zone: NgZone
    ) {
        super(el, http, domSanitizer);
    }

    getDefaultMarkup() {
        return {};
    }

    hasButton() {
        const buttonText = super.buttonText();
        return buttonText != "" && buttonText != null;
    }

    /**
     * Returns (user) defined text for the button.
     */
    buttonText() {
        return super.buttonText() ?? $localize`Save`;
    }

    static makeBoolean(s: string): boolean {
        if (s == "") {
            return false;
        }
        if (s == "0") {
            return false;
        }
        if (s == "false") {
            return false;
        }
        if (s == "1") {
            return true;
        }
        return true;
    }

    ngOnInit() {
        super.ngOnInit();

        const uw = valueOr(
            this.attrsall.state?.c,
            this.markup.initword ?? ""
        ).toString();
        this.userword = CbcountfieldPluginComponent.makeBoolean(uw);
        this.count = this.attrsall.count ?? 0;

        if (!this.attrsall.preview) {
            this.vctrl.addTimComponent(this);
        }
        this.initialValue = this.userword;
        if (this.markup.showname) {
            this.initCode();
        }
        this.checkDisabled();
    }

    ngOnDestroy() {
        if (!this.attrsall.preview) {
            this.vctrl.removeTimComponent(this);
        }
    }

    checkDisabled() {
        // Note: changes the disabled value
        if (this.isReadOnly()) {
            return (this.disabled = true);
        }
        if (this.attrsall.markup.limit == null) {
            return (this.disabled = false);
        }
        if (this.userword) {
            return (this.disabled = false);
        }
        if (this.count >= this.attrsall.markup.limit) {
            return (this.disabled = true);
        }
        return (this.disabled = false);
    }

    /**
     * Returns (user) content in string form.
     */
    getContent(): string {
        return this.userword ? "1" : "0";
    }

    /**
     * Save method for other plugins, needed by e.g. multisave plugin.
     */
    async save() {
        return this.zone.run(() => this.saveText());
    }

    resetField(): undefined {
        return this.zone.run(() => {
            this.initCode();
            this.errormessage = undefined;
            return undefined;
        });
    }

    formBehavior(): FormModeOption {
        return getFormBehavior(this.markup.form, FormModeOption.IsForm);
    }

    resetChanges(): void {
        this.zone.run(() => {
            this.userword = this.initialValue;
            this.saveFailed = false;
            this.updateListeners(ChangeType.Saved);
        });
    }

    // TODO: Use answer content as arg or entire IAnswer?
    setAnswer(content: Record<string, unknown>): ISetAnswerResult {
        return this.zone.run(() => {
            this.errormessage = undefined;
            let message;
            let ok = true;
            // TODO: should receiving empty answer reset to defaultnumber or clear field?
            if (Object.keys(content).length == 0) {
                this.resetField();
            } else {
                try {
                    this.userword = CbcountfieldPluginComponent.makeBoolean(
                        content.c as string
                    );
                } catch (e) {
                    this.userword = false;
                    ok = false;
                    message = `Couldn't find related content ("c") from ${JSON.stringify(
                        content
                    )}`;
                    this.errormessage = message;
                }
            }
            this.saveFailed = false;
            this.initialValue = this.userword;
            this.checkDisabled();
            return {ok: ok, message: message};
        });
    }

    /**
     * Returns (user) set inputstem (textfeed before userinput box).
     */
    get inputstem() {
        return this.markup.inputstem ?? "";
    }

    /**
     * Returns (user) set col size (size of the field).
     */
    get cols() {
        if (!this.markup.cols) {
            return {};
        }
        return {width: this.markup.cols + "em", display: "inline-block"};
    }

    get cbStyle() {
        if (!this.inputstem && (this.stem || this.header)) {
            return {};
        }
        return {
            // otherwise input stem and cb are vertical
            width: "auto",
        };
    }

    /**
     * Initialize content.
     */
    initCode() {
        this.userword = CbcountfieldPluginComponent.makeBoolean(
            this.markup.initword ?? ""
        );
        this.checkDisabled();
        this.initialValue = this.userword;
        this.saveFailed = false;
        this.result = undefined;
    }

    /**
     * Redirects save request to actual save method.
     * Used as e.g. timButton (click) event.
     */
    async saveText() {
        if (this.isUnSaved()) {
            return this.doSaveText(false);
        } else {
            // return {saved: false, message:undefined};
            this.saveResponse.saved = false;
            this.saveResponse.message = undefined;
            return this.saveResponse;
        }
    }

    /**
     * Returns true value, if label is set to plaintext.
     * Used to define readOnlyStyle in angular, either input or span.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isPlainText() {
        return (
            this.markup.readOnlyStyle == "plaintext" &&
            window.location.pathname.startsWith("/view/")
        );
    }

    isReadOnly() {
        return this.markup.readOnlyStyle == "box" &&
            window.location.pathname.startsWith("/view/")
            ? "disable"
            : "";
    }

    /**
     * Checking if input has been changed since the last Save or initialization.
     * Displays a red thick marker at the right side of the inputfield to notify users
     * about unsaved changes.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isUnSaved() {
        if (this.initialValue != this.userword) {
            this.hideSavedText = true;
        }
        return this.initialValue != this.userword;
    }

    /**
     * Autosaver used by ng-blur in cbcountfieldApp component.
     * Needed to separate from other save methods because of the if-structure.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    autoSave() {
        if (this.preventedAutosave) {
            this.preventedAutosave = false;
            return;
        }
        this.updateListeners(
            this.isUnSaved() ? ChangeType.Modified : ChangeType.Saved
        );
        if (this.markup.autosave || this.markup.autosave === undefined) {
            this.saveText();
        }
    }

    /**
     * Actual save method, called by different save alternatives implemented above.
     * @param nosave true/false parameter boolean checker for the need to save
     */
    async doSaveText(nosave: boolean) {
        if (!this.isUnSaved()) {
            this.saveResponse.saved = false;
            this.saveResponse.message = "No changes";
            return this.saveResponse;
        }
        this.errormessage = undefined;
        this.isRunning = true;
        let c = "0";
        if (this.userword) {
            c = "1";
        }
        this.result = undefined;
        const params = {
            input: {
                nosave: false,
                c: c,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }

        const r = await this.postAnswer<{
            web: {
                count: number;
                new: number;
                result: string;
                error?: string;
            };
        }>(params);
        this.isRunning = false;
        if (r.ok) {
            this.saveFailed = false;
            const data = r.result;
            // TODO: Make angular to show tooltip even without user having to move cursor out and back into the input
            // (Use premade bootstrap method / add listener for enter?)
            this.errormessage = data.web.error;
            this.count = data.web.count;
            this.result = data.web.result;
            if (this.userword && data.web.new == 0) {
                // value was changed, so disable
                this.userword = false;
                this.disabled = true;
            }
            this.initialValue = this.userword;
            this.updateListeners(ChangeType.Saved);
            this.hideSavedText = false;
            this.saveResponse.saved = true;
            this.saveResponse.message = this.errormessage;
        } else {
            this.errormessage =
                r.result.error.error ||
                "Syntax error, infinite loop or some other error?";
            this.saveFailed = true;
        }
        return this.saveResponse;
    }

    getAttributeType() {
        return CbcountfieldAll;
    }
}

@NgModule({
    declarations: [CbcountfieldPluginComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        TimUtilityModule,
        FormsModule,
        TooltipModule.forRoot(),
        PurifyModule,
    ],
})
export class CbcountfieldModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin(
    "cbcountfield-runner",
    CbcountfieldModule,
    CbcountfieldPluginComponent
);
