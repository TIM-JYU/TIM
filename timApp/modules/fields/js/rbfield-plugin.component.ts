/**
 * Defines the client-side implementation of rbfield/label plugin.
 */
import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap, OnDestroy} from "@angular/core";
import {Component, ElementRef, NgModule, NgZone} from "@angular/core";
import type {ISetAnswerResult, ITimComponent} from "tim/document/viewctrl";
import {ChangeType, FormModeOption, RegexOption} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {getFormBehavior} from "tim/plugin/util";
import {valueOr} from "tim/util/utils";
import {DomSanitizer} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {FieldBasicData} from "./textfield-plugin.component";

const RbfieldMarkup = t.intersection([
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
        isRb: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        cols: withDefault(t.number, 0),
    }),
]);
const RbfieldAll = t.intersection([
    t.partial({}),
    t.type({
        info: Info,
        markup: RbfieldMarkup,
        preview: t.boolean,
        state: nullable(FieldBasicData),
    }),
]);

@Component({
    selector: "tim-rbfield-runner",
    template: `
<div class="textfieldNoSaveDiv" [ngStyle]="cols">
    <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
    <h4 *ngIf="header" [innerHtml]="header | purify"></h4>
    <p class="stem" *ngIf="stem">{{stem}}</p>
     <span style="width: 100%">
      <span *ngIf="inputstem" class="inputstem" [innerHtml]="inputstem | purify"></span>
      <span *ngIf="!isPlainText()" [ngClass]="{warnFrame: (isUnSaved() ), alertFrame: saveFailed}">
        <input type="radio"
               *ngIf="!isPlainText()"
               name="{{getName()}}"
               id="{{getName()}}"
               value="1"
               [ngStyle]="cbStyle"
               class="form-control"
               [(ngModel)]="userword"
               (ngModelChange)="autoSave()"
               [disabled]="readonly || attrsall['preview']"
               [readonly]="readonly"
               [tooltip]="errormessage"
               [isOpen]="errormessage !== undefined"
               >
               <button class="timButton"
                        *ngIf="!hasButton() && saveFailed"
                        (click)="autoSave()">
                    {{buttonText()}}
               </button>
         </span>
         <span *ngIf="isPlainText()" style="">{{userword}}</span>
      </span>
        <button class="timButton"
            *ngIf="!isPlainText() && hasButton()"
            [disabled]="(disableUnchanged && !isUnSaved()) || isRunning || readonly || attrsall['preview']"
            (click)="autoSave()">
        {{buttonText()}}
        </button>
    <p *ngIf="footer" [innerText]="footer | purify" class="plgfooter"></p>
</div>
`,
    styleUrls: ["./rbfield-plugin.component.scss"],
})
export class RbfieldPluginComponent
    extends AngularPluginBase<
        t.TypeOf<typeof RbfieldMarkup>,
        t.TypeOf<typeof RbfieldAll>,
        typeof RbfieldAll
    >
    implements ITimComponent, OnDestroy
{
    private result?: string;
    isRunning = false;
    userword: string = "0";

    private initialValue: string = "0";
    errormessage?: string;
    private hideSavedText = true;
    private redAlert = false;
    private saveResponse: {saved: boolean; message: string | undefined} = {
        saved: false,
        message: undefined,
    };
    private preventedAutosave = false; // looks depracated???
    private rbName: string = "";
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

    /*
    makeBoolean(s: string): boolean {
        if ( s == "" ) { return false; }
        if ( s == "0" ) { return false; }
        if ( s == "false" ) { return false; }
        if ( s == "1" ) { return true; }
        return true;
    }
*/
    ngOnInit() {
        super.ngOnInit();
        this.rbName = this.rbname;

        const uw = valueOr(
            this.attrsall.state?.c,
            this.markup.initword ?? "0"
        ).toString();
        this.userword = uw; // this.makeBoolean(uw);

        if (!this.attrsall.preview) {
            if (this.markup.tag) {
                this.vctrl.addTimComponent(this, this.markup.tag);
            } else {
                this.vctrl.addTimComponent(this);
            }
        }
        this.initialValue = this.userword;
        if (this.markup.showname) {
            this.initCode();
        }
    }

    get inputtype(): string {
        return "radio";
    }

    get rbname(): string {
        if (this.rbName) {
            return this.rbName;
        }
        let n: string = this.getName() ?? "rb";
        n = n.replace(/[0-9]+/, "");
        this.rbName = n;
        return n;
    }

    /**
     * Returns (user) content in string form.
     */
    getContent(): string {
        return this.userword; //  ? "1" : "0";
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

    resetChanges() {
        this.zone.run(() => {
            this.userword = this.initialValue;
            this.saveFailed = false;
            this.updateListeners(ChangeType.Saved);
        });
    }

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
                    this.userword = content.c as string;
                } catch (e) {
                    this.userword = "";
                    ok = false;
                    message = `Couldn't find related content ("c") from ${JSON.stringify(
                        content
                    )}`;
                    this.errormessage = message;
                }
            }
            this.saveFailed = false;
            this.initialValue = this.userword;
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
        this.userword = this.markup.initword ?? "";
        this.initialValue = this.userword;
        this.result = undefined;
        this.saveFailed = false;
        this.updateListeners(ChangeType.Saved);
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

    setChecked(b: boolean) {
        this.userword = b ? "1" : "0";
        this.updateListeners(
            this.isUnSaved() ? ChangeType.Modified : ChangeType.Saved
        );
        if (this.markup.autosave || this.markup.autosave === undefined) {
            // We want to save the plugin regardless of unSaved status to prevent two radio buttons
            // from being checked at the same time.
            this.doSaveText(false);
        }
    }

    /**
     * Autosaver used by ng-blur in rbfieldApp component.
     * Needed to seperate from other save methods because of the if-structure.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    autoSave() {
        const tid = this.getTaskId();
        if (!tid?.docId) {
            return;
        }
        const comps = this.vctrl.getTimComponentsByRegex(
            `${tid.docId}\.${this.rbname}.*`,
            RegexOption.DontPrependCurrentDocId
        );
        const n = this.getName();
        for (const c of comps) {
            if (c.getName() == n) {
                continue;
            }
            if (!(c instanceof RbfieldPluginComponent)) {
                continue;
            }
            c.setChecked(false);
        }
        this.updateListeners(
            this.isUnSaved() ? ChangeType.Modified : ChangeType.Saved
        );
        if (this.preventedAutosave) {
            this.preventedAutosave = false;
            return;
        }
        if (this.markup.autosave || this.markup.autosave === undefined) {
            this.saveText();
        }
    }

    /**
     * Actual save method, called by different save alternatives implemented above.
     * @param nosave true/false parameter boolean checker for the need to save
     */
    async doSaveText(nosave: boolean) {
        this.errormessage = undefined;
        this.isRunning = true;
        const c = this.userword;
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
            web: {result: string; error?: string};
        }>(params);
        this.isRunning = false;
        if (r.ok) {
            this.saveFailed = false;
            const data = r.result;
            this.errormessage = data.web.error;
            this.result = data.web.result;
            this.initialValue = this.userword;
            this.updateListeners(ChangeType.Saved);
            this.hideSavedText = false;
            this.saveResponse.saved = true;
            this.saveResponse.message = this.errormessage;
        } else {
            this.errormessage =
                r.result.error.error ||
                "Syntax error, infinite loop or some other error?";
            this.saveFailed = this.userword == "1";
        }
        return this.saveResponse;
    }

    getAttributeType() {
        return RbfieldAll;
    }

    updateListeners(state: ChangeType) {
        if (!this.vctrl) {
            return;
        }
        const taskId = this.pluginMeta.getTaskId();
        if (!taskId) {
            return;
        }
        this.vctrl.informChangeListeners(
            taskId,
            state,
            this.markup.tag ? this.markup.tag : undefined
        );
    }

    ngOnDestroy(): void {
        if (!this.attrsall.preview) {
            if (this.markup.tag) {
                this.vctrl.removeTimComponent(this, this.markup.tag);
            } else {
                this.vctrl.removeTimComponent(this);
            }
        }
    }
}

@NgModule({
    declarations: [RbfieldPluginComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        TimUtilityModule,
        FormsModule,
        TooltipModule.forRoot(),
        PurifyModule,
    ],
})
export class RbfieldModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("rbfield-runner", RbfieldModule, RbfieldPluginComponent);
