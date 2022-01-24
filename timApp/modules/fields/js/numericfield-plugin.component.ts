/**
 * Defines the client-side implementation of numericfield/label plugin.
 */
import * as t from "io-ts";
import {ApplicationRef, Component, DoBootstrap, NgModule} from "@angular/core";
import {
    ChangeType,
    FormModeOption,
    ISetAnswerResult,
    ITimComponent,
    ViewCtrl,
} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {getFormBehavior} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {defaultErrorMessage, defaultTimeout, to, valueOr} from "tim/util/utils";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {TimUtilityModule} from "../../../static/scripts/tim/ui/tim-utility.module";
import {PurifyModule} from "../../../static/scripts/tim/util/purify.module";
import {
    createDowngradedModule,
    doDowngrade,
} from "../../../static/scripts/tim/downgrade";
import {vctrlInstance} from "../../../static/scripts/tim/document/viewctrlinstance";
import {AngularPluginBase} from "../../../static/scripts/tim/plugin/angular-plugin-base.directive";
import {FieldDataWithStyles} from "./textfield";

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
<div class="numericfieldNoSaveDiv">
    <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
    <h4 *ngIf="header" [innerHtml]="header"></h4>
    <p class="stem" *ngIf="stem" [innerHtml]="stem"></p>
    <div class="form-inline">
     <label><span>
      <span [innerHtml]="inputstem"></span>
      <span *ngIf="!isPlainText()" [class.noarrows]="!arrows">
        <input type="number"
               [style.width.em]="cols"
               step="{{ stepCheck() }}"
               class="form-control"
               [(ngModel)]="numericvalue"
               (blur)="autoSave()"
               (keydown.enter)="saveAndRefocus()"
               (ngModelChange)="updateInput()"
               [readonly]="readonly"
               [tooltip]="errormessage"
               [isOpen]="errormessage !== undefined"
               triggers="mouseenter"
               placeholder="{{inputplaceholder}}"
               [ngClass]="{warnFrame: (isUnSaved() && !redAlert), alertFrame: redAlert}"
               [ngStyle]="styles">
      </span>
      <span *ngIf="isPlainText()" class="plaintext" [style.width.em]="cols">{{numericvalue}}</span>
     </span></label>
    </div>
    <div *ngIf="errormessage"  class="error" style="font-size: 12px" [innerHtml]="errormessage"></div>
    <button class="timButton"
            *ngIf="!isPlainText() && buttonText()"
            [disabled]="!isUnSaved() || isRunning || readonly"
            (click)="saveText()">
        {{buttonText()}}
    </button>
    <a href="" *ngIf="undoButton && isUnSaved()" title="{{undoTitle}}" (click)="tryResetChanges($event);">{{undoButton}}</a>
    <p class="savedtext" *ngIf="!hideSavedText && buttonText()">Saved!</p>
    <p *ngIf="footer" [innerText]="footer" class="plgfooter"></p>
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
    private changes = false;
    private result?: string;
    isRunning = false;
    numericvalue?: number;
    private vctrl!: ViewCtrl;
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
        const d = parseFloat(s);
        return d;
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
        this.autoSave();
        this.changeFocus();
    }

    getDefaultMarkup() {
        return {};
    }

    /**
     * Returns (user) defined text for the button.
     */
    buttonText() {
        return super.buttonText() ?? null;
    }

    get valueOrEmpty(): string {
        return valueOr<string | number, string>(
            this.numericvalue,
            ""
        ).toString();
    }

    ngOnInit() {
        super.ngOnInit();
        this.vctrl = vctrlInstance!;
        const state = this.attrsall.state?.c;
        if (state === undefined || state === null) {
            this.initCode();
        } else {
            if (typeof state === "number") {
                this.numericvalue = state;
            } else {
                if (this.isAllowedNull(state)) {
                    this.numericvalue = undefined;
                } else {
                    // TODO: parseFloat accepts too much like "6hello", should have a more accurate float check.
                    const numericvalue = this.getDouble(state);
                    this.numericvalue = numericvalue;
                    if (isNaN(numericvalue)) {
                        this.numericvalue = undefined;
                        if (state !== "") {
                            this.errormessage = `Content is not a number (${state}); showing empty value.`;
                        }
                    }
                }
            }
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
        this.vctrl.addTimComponent(this, this.markup.tag);
        this.initialValue = this.numericvalue;
        if (this.attrsall.state?.styles && !this.markup.ignorestyles) {
            this.applyStyling(this.attrsall.state.styles);
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
        this.saveCalledExternally = true;
        return this.saveText();
    }

    resetField(): undefined {
        this.initCode();
        this.applyStyling({});
        this.errormessage = undefined;
        return undefined;
    }

    tryResetChanges(e?: Event): void {
        if (e) {
            e.preventDefault();
        }
        if (this.undoConfirmation && !window.confirm(this.undoConfirmation)) {
            return;
        }
        this.resetChanges();
    }

    resetChanges(): void {
        this.numericvalue = this.initialValue;
        this.changes = false;
        this.updateListeners(ChangeType.Saved);
        this.scope.$digest();
    }

    setAnswer(content: unknown): ISetAnswerResult {
        this.errormessage = undefined;
        let message;
        let ok = true;
        // TODO: should receiving empty answer reset to defaultnumber or clear field?
        if (!FieldDataWithStyles.is(content)) {
            this.resetField();
        } else {
            if (this.isAllowedNull(content.c)) {
                this.numericvalue = undefined;
            } else {
                const parsed = this.getDouble(content.c);
                if (isNaN(parsed)) {
                    this.numericvalue = undefined;
                    ok = false;
                    message = 'Value at "c" was not a valid number';
                    this.errormessage = `Content is not a number (${content.c}); showing empty value.`;
                } else {
                    this.numericvalue = parsed;
                }
            }

            if (!this.markup.ignorestyles && content.styles) {
                this.applyStyling(content.styles);
            }
        }
        this.initialValue = this.numericvalue;
        this.changes = false;
        this.updateListeners(ChangeType.Saved);
        return {ok: ok, message: message};
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
            this.numericvalue = 0;
        } else {
            this.numericvalue = this.markup.initnumber;
        }
        this.initialValue = this.numericvalue;
        this.result = undefined;
        this.changes = false;
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
     * Needed to seperate from other save methods because of the if-structure.
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
     * Parses "styles" from the plugin answer that were saved by tableForm
     * For now only backgroundColor is supported
     * See TODOs at textfield
     */
    applyStyling(styles: Record<string, string>) {
        if (Object.keys(styles).length == 0) {
            this.styles = {};
            return;
        }
        if (styles.backgroundColor) {
            this.styles.backgroundColor = styles.backgroundColor;
        }
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
            "numericfield-runner input, textfield-runner input"
        );
        for (let i = 0; i < inputfields.length; ++i) {
            const selectedfield = inputfields[i] as HTMLInputElement;
            if (
                selectedfield === document.activeElement &&
                inputfields[i + 1]
            ) {
                const nextfield = inputfields[i + 1] as HTMLInputElement;
                return nextfield.focus();
            }
        }
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
        this.errormessage = undefined;
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
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to(
            $http.put<{
                web: {
                    result: string;
                    error?: string;
                    clear?: boolean;
                    value: string;
                };
            }>(url, params, {timeout: defaultTimeout})
        );
        this.isRunning = false;
        if (r.ok) {
            const data = r.result.data;
            if (data.web.error) {
                this.errormessage = data.web.error;
            }
            this.result = data.web.result;
            if (this.result === "saved") {
                this.numericvalue = +data.web.value.toString();
                this.initialValue = this.numericvalue;
                this.changes = false;
                this.updateListeners(ChangeType.Saved);
                this.hideSavedText = false;
                this.redAlert = false;
                this.saveResponse.saved = true;
            }
            if (data.web.clear) {
                this.applyStyling({});
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
                            this.vctrl.updateFields([tid]);
                            // for (const dup of duplicates) {
                            //     dup.setAnswer({"c": this.numericvalue, "styles": this.styles})
                            // }
                        }
                    }
                }
            }
        } else {
            this.errormessage =
                r.result.data?.error ??
                this.markup.connectionErrorMessage ??
                defaultErrorMessage;
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
}

@NgModule({
    declarations: [NumericfieldPluginComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        TimUtilityModule,
        FormsModule,
        PurifyModule,
        TooltipModule.forRoot(),
    ],
})
export class NumericfieldModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                NumericfieldModule
            )
        ),
        "numericfieldRunner",
        NumericfieldPluginComponent
    ),
];
