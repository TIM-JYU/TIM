/**
 * Defines the client-side implementation of numericfield/label plugin.
 */
import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {
    Component,
    ElementRef,
    NgModule,
    NgZone,
    ViewChild,
} from "@angular/core";
import type {ISetAnswerResult, ITimComponent} from "tim/document/viewctrl";
import {ChangeType, FormModeOption} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {getFormBehavior, parseStyles} from "tim/plugin/util";
import {defaultErrorMessage, valueOr} from "tim/util/utils";
import {DomSanitizer} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import type {TFieldContent} from "./textfield-plugin.component";
import {
    FieldDataWithStyles,
    jumpToNextField,
} from "./textfield-plugin.component";

const REDOUBLE = /[^0-9,.e\-+]+/g;

const NumericfieldMarkup = t.intersection([
    t.partial({
        tag: nullable(t.string),
        inputplaceholder: nullable(t.number),
        inputstem: nullable(t.string),
        initnumber: nullable(t.number),
        validinput: nullable(t.string),
        errormessage: nullable(t.string),
        readOnlyStyle: nullable(t.string),
        step: nullable(t.number),
        arrows: t.boolean,
        wheel: t.boolean,
        verticalkeys: t.boolean,
        autosave: t.boolean,
        nosave: t.boolean,
        ignorestyles: t.boolean,
        clearstyles: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        autoupdate: withDefault(t.number, 500),
        autoUpdateTables: withDefault(t.boolean, true),
        cols: withDefault(t.number, 6),
    }),
]);
const NumericfieldAll = t.intersection([
    t.partial({}),
    t.type({
        info: Info,
        markup: NumericfieldMarkup,
        preview: t.boolean,
        state: nullable(FieldDataWithStyles),
    }),
]);

/**
 * Introducing numericfieldRunner as HTML component.
 * Attribute style used to force user given cols to determine size.
 * Developers note: attribute step="0.01" can determine lower step size between scroll-up/down.
 */
@Component({
    selector: "tim-numericfield-runner",
    template: `
<div class="numericfieldNoSaveDiv inline-form">
    <tim-markup-error *ngIf="markupError" [data]="markupError!"></tim-markup-error>
    <h4 *ngIf="header" [innerHtml]="header | purify"></h4>
    <p class="stem" *ngIf="stem" [innerHtml]="stem | purify"></p>
    <div class="form-inline">
     <label><span>
      <span *ngIf="inputstem" class="inputstem" [innerHtml]="inputstem | purify"></span>
      <span *ngIf="!isPlainText()" [class.noarrows]="!arrows">
        <input #inputField type="number"
               [style.width.em]="cols"
               step="{{ stepCheck() }}"
               class="form-control"
               [(ngModel)]="numericvalue"
               (blur)="autoSave()"
               (keydown.enter)="saveAndRefocus()"
               (keydown.control.s)="saveAndPreventDefault($event)"
               (keydown.meta.s)="saveAndPreventDefault($event)"
               (ngModelChange)="updateInput()"
               [readonly]="readonly"
               [tooltip]="errormessage"
               [isOpen]="errormessage !== undefined"
               [disabled]="attrsall['preview']"
               placeholder="{{inputplaceholder}}"
               [ngClass]="{warnFrame: isUnSaved(), alertFrame: redAlert}"
               [ngStyle]="styles">
               <button class="timButton"
                        *ngIf="!hasButton() && saveFailed"
                        (click)="saveText()">
                    {{buttonText()}}
               </button>
      </span>
      <span *ngIf="isPlainText()" class="plaintext" [style.width.em]="cols" [ngStyle]="styles">{{numericvalue}} </span>
     </span></label>
    </div>
    <button class="timButton"
            *ngIf="!isPlainText() && hasButton()"
            [disabled]="(disableUnchanged && !isUnSaved()) || isRunning || readonly || attrsall['preview']"
            (click)="saveText()">
        {{buttonText()}}
    </button>
    <a href="" *ngIf="undoButton && isUnSaved()" title="{{undoTitle}}" (click)="tryResetChanges($event);">{{undoButton}}</a>
    <p class="savedtext" *ngIf="!hideSavedText && hasButton()">Saved!</p>
    <p *ngIf="footer" [innerHtml]="footer | purify" class="plgfooter"></p>
</div> `,
    styleUrls: ["./numericfield-plugin.component.scss"],
})
export class NumericfieldPluginComponent
    extends AngularPluginBase<
        t.TypeOf<typeof NumericfieldMarkup>,
        t.TypeOf<typeof NumericfieldAll>,
        typeof NumericfieldAll
    >
    implements ITimComponent
{
    @ViewChild("inputField") inputField!: ElementRef<HTMLInputElement>;
    private changes = false;
    private result?: string;
    isRunning = false;
    numericvalue?: number;

    private initialValue?: number;
    errormessage?: string;
    hideSavedText = true;
    redAlert = false;
    private saveResponse: {saved: boolean; message: string | undefined} = {
        saved: false,
        message: undefined,
    };
    private preventedAutosave = false;
    styles: Record<string, string> = {};
    private saveCalledExternally = false;
    saveFailed = false;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        private zone: NgZone
    ) {
        super(el, http, domSanitizer);
    }

    get arrows() {
        return this.markup.arrows;
    }

    get inputplaceholder() {
        return this.markup.inputplaceholder;
    }

    getDouble(s: string | number): number {
        if (typeof s === "number") {
            return s;
        }
        s = s.replace(REDOUBLE, "");
        s = s.replace(",", ".");
        if (s.startsWith("e")) {
            s = "1" + s;
        }
        return parseFloat(s);
    }

    /**
     * Check if numericfield content is an accepted type of non-number value
     * typing empty input will result in ""-value
     * null is used in jsrunner scripts to specify missing value
     * @param c input to inspect
     */
    isAllowedNull(c: string | null | number): c is null | "" {
        return c === null || c === "";
    }

    saveAndRefocus() {
        this.saveText();
        this.changeFocus();
    }

    saveAndPreventDefault(e: Event) {
        e.preventDefault();
        this.saveText();
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

    get valueOrEmpty(): string {
        return valueOr<string | number, string>(
            this.numericvalue,
            ""
        ).toString();
    }

    private updateFieldValue(val: TFieldContent): boolean {
        if (typeof val === "number") {
            this.numericvalue = val;
            return true;
        }
        if (this.isAllowedNull(val)) {
            this.numericvalue = undefined;
            return true;
        }
        // TODO: parseFloat accepts too much like "6hello", should have a more accurate float check.
        const numericvalue = this.getDouble(val);
        if (isNaN(numericvalue)) {
            this.numericvalue = undefined;
            if (val !== "") {
                this.errormessage = $localize`Content is not a number (${val}); showing empty value.`;
                this.redAlert = true;
            }
            return false;
        }
        this.numericvalue = numericvalue;
        return true;
    }

    ngOnInit() {
        super.ngOnInit();

        const state = this.attrsall.state?.c;
        if (state === undefined || state === null) {
            this.initCode();
        } else {
            this.updateFieldValue(state);
        }
        if (!this.markup.wheel) {
            this.element.on(
                "mousewheel DOMMouseScroll",
                (e) => (e.bubbles = false)
            );
        }
        if (!this.markup.verticalkeys) {
            this.element.on("keydown", (e) => {
                e.bubbles = false;
                if (e.key == "ArrowUp" || e.key == "ArrowDown") {
                    e.preventDefault();
                }
            });
        }
        if (!this.attrsall.preview) {
            this.vctrl.addTimComponent(this);
        }
        this.initialValue = this.numericvalue;
        if (this.attrsall.state?.styles && !this.markup.ignorestyles) {
            this.styles = parseStyles(this.attrsall.state.styles);
        }
    }

    ngOnDestroy() {
        if (!this.attrsall.preview) {
            this.vctrl.removeTimComponent(this);
        }
    }

    /**
     * Returns (user) content in string form.
     * Not used in numericfield plugin, but promised to be implemented in ITimComponent.
     */
    getContent(): string {
        return this.valueOrEmpty;
    }

    /**
     * Save method for other plguins, needed by e.g. multisave plugin.
     */
    async save() {
        return this.zone.run(() => {
            this.saveCalledExternally = true;
            return this.saveText();
        });
    }

    resetField(): undefined {
        return this.zone.run(() => {
            this.initCode();
            this.styles = {};
            this.errormessage = undefined;
            this.redAlert = false;
            return undefined;
        });
    }

    resetChanges(): void {
        this.zone.run(() => {
            this.numericvalue = this.initialValue;
            this.changes = false;
            this.saveFailed = false;
            this.redAlert = false;
            this.updateListeners(ChangeType.Saved);
        });
    }

    setAnswer(content: unknown): ISetAnswerResult {
        return this.zone.run(() => {
            this.errormessage = undefined;
            let message;
            let ok = true;
            // TODO: should receiving empty answer reset to defaultnumber or clear field?
            if (!FieldDataWithStyles.is(content)) {
                this.resetField();
            } else {
                if (!this.updateFieldValue(content.c)) {
                    ok = false;
                    message = 'Value at "c" was not a valid number';
                }

                if (!this.markup.ignorestyles) {
                    this.styles = content.styles
                        ? parseStyles(content.styles)
                        : {};
                }
            }
            this.initialValue = this.numericvalue;
            this.changes = false;
            this.saveFailed = false;
            this.updateListeners(ChangeType.Saved);
            return {ok: ok, message: message};
        });
    }

    /**
     * Method for autoupdating.
     */
    get autoupdate(): number {
        return this.markup.autoupdate;
    }

    /**
     * Returns (user) set inputstem (textfeed before userinput box).
     */
    get inputstem() {
        return this.markup.inputstem ?? null;
    }

    /**
     * Returns (user) set col size (size of the field).
     */
    get cols() {
        return this.markup.cols;
    }

    /**
     * Initialize content.
     */
    initCode() {
        if (this.markup.initnumber == undefined) {
            this.numericvalue = undefined;
        } else {
            this.numericvalue = this.markup.initnumber;
        }
        this.initialValue = this.numericvalue;
        this.result = undefined;
        this.changes = false;
        this.saveFailed = false;
        this.updateListeners(ChangeType.Saved);
    }

    /**
     * Redirects save request to actual save method.
     * Used as e.g. timButton (click) event.
     */
    saveText() {
        if (this.isUnSaved()) {
            return this.doSaveText(false);
        } else {
            this.saveResponse.saved = false;
            this.saveResponse.message = undefined;
            return this.saveResponse;
        }
    }

    /**
     * Autosaver used by ng-blur in textfieldApp component.
     * Needed to separate from other save methods because of the if-structure.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    autoSave() {
        this.saveCalledExternally = false;
        if (this.preventedAutosave) {
            this.preventedAutosave = false;
            return;
        }
        if (this.markup.autosave || this.markup.autosave === undefined) {
            this.saveText();
        }
    }

    /**
     * Stepper used by step in numericfield-Runner component.
     * Used to define range of each numeric step for scroll up/down, e.g. 0.25 or 1.0.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    stepCheck() {
        return this.markup.step;
    }

    /**
     * Returns true value, if label is set to plaintext.
     * Used to define readOnlyStyle in angular, either input or span.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isPlainText() {
        const ros = this.markup.readOnlyStyle;
        if (ros === "htmlalways") {
            return true;
        }
        return (
            ros === "plaintext" && window.location.pathname.startsWith("/view/")
        );
    }

    /**
     * Method to check numeric input type for stringified numericfield.
     * Used as e.g. to define negative or positive numeric input [0-9]+.
     * @param re validinput defined by given attribute.
     */
    validityCheck(re: string) {
        if (this.numericvalue === null) {
            return true;
        }
        const regExpChecker = new RegExp(re);
        return regExpChecker.test(this.valueOrEmpty);
    }

    /**
     * Returns focus on next HTML field.
     * Used by keydown (Enter) in angular.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    changeFocus() {
        const inputfields = document.querySelectorAll(
            "tim-numericfield-runner input, tim-textfield-runner input"
        );
        return jumpToNextField(
            inputfields,
            () => (this.preventedAutosave = true)
        );
    }

    /**
     * Checking if input has been changed since the last Save or initialization.
     * Displays a red thick marker at the right side of the inputfield to notify users
     * about unsaved changes.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isUnSaved() {
        return !this.markup.nosave && this.changes;
    }

    /**
     * Actual saver, called by different save alternatives implemented above.
     * @param nosave true/false parameter boolean checker for the need to save
     */
    async doSaveText(nosave: boolean) {
        const rejected = this.inputField.nativeElement.validity.badInput;
        this.errormessage = undefined;
        this.redAlert = false;
        if (this.markup.validinput) {
            if (!this.validityCheck(this.markup.validinput)) {
                this.errormessage =
                    this.markup.errormessage ??
                    "Input does not pass the RegEx: " + this.markup.validinput;
                this.redAlert = true;
                this.saveResponse.message = this.errormessage;
                return this.saveResponse;
            }
        }
        /* No visible text
        this.error = "... saving ..."; */
        this.isRunning = true;
        this.result = undefined;
        const params = {
            input: {
                nosave: false,
                c: this.numericvalue?.toString() ?? "",
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const r = await this.postAnswer<{
            web: {
                result: string;
                error?: string;
                value: TFieldContent;
            };
        }>(params);
        this.isRunning = false;
        if (r.ok) {
            const data = r.result;
            if (data.web.error) {
                this.errormessage = data.web.error;
                this.redAlert = true;
            }
            this.result = data.web.result;
            if (this.result === "saved") {
                if (rejected) {
                    this.errormessage = $localize`Content is not a number; saving empty value.`;
                    this.redAlert = true;
                } else {
                    this.redAlert = false;
                }
                this.updateFieldValue(data.web.value);
                this.initialValue = this.numericvalue;
                this.changes = false;
                this.saveFailed = false;
                this.updateListeners(ChangeType.Saved);
                this.hideSavedText = false;
                this.saveResponse.saved = true;
            }
            if (this.markup.clearstyles) {
                this.styles = {};
            }
            this.saveResponse.message = this.errormessage;
            if (this.vctrl && !this.saveCalledExternally) {
                const taskId = this.getTaskId();
                if (taskId) {
                    const tid = taskId.docTask().toString();
                    if (this.markup.autoUpdateTables) {
                        this.vctrl.updateAllTables([tid]);
                    }
                    if (this.vctrl.docSettings.form_mode) {
                        const duplicates = this.vctrl.getTimComponentArray(tid);
                        if (duplicates && duplicates.length > 1) {
                            await this.vctrl.updateFields([tid]);
                            // for (const dup of duplicates) {
                            //     dup.setAnswer({"c": this.numericvalue, "styles": this.styles})
                            // }
                        }
                    }
                }
            }
        } else {
            this.errormessage =
                r.result.error?.error ??
                this.markup.connectionErrorMessage ??
                defaultErrorMessage;
            this.redAlert = true;
            this.saveFailed = true;
        }
        return this.saveResponse;
    }

    formBehavior(): FormModeOption {
        return getFormBehavior(this.markup.form, FormModeOption.IsForm);
    }

    getAttributeType() {
        return NumericfieldAll;
    }

    updateInput() {
        if (!this.changes) {
            this.changes = true;
            this.hideSavedText = true;
            this.updateListeners(ChangeType.Modified);
        }
    }
}

@NgModule({
    declarations: [NumericfieldPluginComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        TimUtilityModule,
        FormsModule,
        PurifyModule,
        TooltipModule.forRoot(),
    ],
})
export class NumericfieldModule implements DoBootstrap {
    ngDoBootstrap(_appRef: ApplicationRef) {}
}

registerPlugin(
    "numericfield-runner",
    NumericfieldModule,
    NumericfieldPluginComponent
);
