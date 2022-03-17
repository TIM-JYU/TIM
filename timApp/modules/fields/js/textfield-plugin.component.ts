/**
 * Defines the client-side implementation of textfield/label plugin.
 */
import * as t from "io-ts";
import {
    ApplicationRef,
    Component,
    DoBootstrap,
    ElementRef,
    NgModule,
    NgZone,
} from "@angular/core";
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
import {defaultErrorMessage, timeout, valueOr} from "tim/util/utils";
import {BrowserModule, DomSanitizer} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {AngularPluginBase} from "../../../static/scripts/tim/plugin/angular-plugin-base.directive";
import {vctrlInstance} from "../../../static/scripts/tim/document/viewctrlinstance";
import {TimUtilityModule} from "../../../static/scripts/tim/ui/tim-utility.module";
import {PurifyModule} from "../../../static/scripts/tim/util/purify.module";
import {
    createDowngradedModule,
    doDowngrade,
} from "../../../static/scripts/tim/downgrade";

const TextfieldMarkup = t.intersection([
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
        nosave: t.boolean,
        ignorestyles: t.boolean,
        clearstyles: t.boolean,
        textarea: t.boolean,
        autogrow: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        autoupdate: withDefault(t.number, 500),
        autoUpdateTables: withDefault(t.boolean, true),
        cols: withDefault(t.number, 6),
        rows: withDefault(t.number, 1),
    }),
]);
export const FieldContent = t.union([t.string, t.number, t.null]);
export const FieldBasicData = t.type({
    c: FieldContent,
});
export const FieldDataWithStyles = t.intersection([
    FieldBasicData,
    t.partial({
        styles: nullable(t.record(t.string, t.string)),
    }),
]);
const TextfieldAll = t.intersection([
    t.partial({}),
    t.type({
        info: Info,
        markup: TextfieldMarkup,
        preview: t.boolean,
        state: nullable(FieldDataWithStyles),
    }),
]);
export type TFieldContent = t.TypeOf<typeof FieldContent>;

@Component({
    selector: "tim-textfield-runner",
    template: `
<div class="textfieldNoSaveDiv inline-form">
    <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
    <h4 *ngIf="header" [innerHtml]="header"></h4>
    <p class="stem" *ngIf="stem">{{stem}}</p>
    <form #f class="form-inline">
     <label><span>
      <span class="inputstem" [innerHtml]="inputstem"></span>
      <span *ngIf="!isPlainText()" >
        <input type="text"
               *ngIf="!isTextArea()"
               class="form-control"
               [(ngModel)]="userword"
               (blur)="autoSave()"
               (keydown.enter)="saveAndRefocus()"
               (ngModelChange)="updateInput()"
               [ngModelOptions]="{standalone: true}"
               [pattern]="getPattern()"
               [readonly]="readonly"
               [tooltip]="errormessage"
               [placeholder]="placeholder"
               [class.warnFrame]="isUnSaved() && !redAlert"
               [class.alertFrame]="redAlert"
               [ngStyle]="styles"
               [style.width.em]="cols"
               >
       <textarea
               [rows]="rows"
               *ngIf="isTextArea()"
               class="form-control textarea"
               [(ngModel)]="userword"
               [ngModelOptions]="{standalone: true}"
               (blur)="autoSave()"
               (keydown)="tryAutoGrow()"
               (keyup)="tryAutoGrow()"
               (ngModelChange)="updateInput()"
               [readonly]="readonly"
               [tooltip]="errormessage"
               [placeholder]="placeholder"
               [class.warnFrame]="isUnSaved() && !redAlert"
               [class.alertFrame]="redAlert"
               [ngStyle]="styles"
               [style.width.em]="cols">
               </textarea>
         </span>
         <span *ngIf="isPlainText()" [innerHtml]="userword" class="plaintext" [style.width.em]="cols" style="max-width: 100%"></span>
         </span></label>
    </form>
    <div *ngIf="errormessage" class="error" style="font-size: 12px" [innerHtml]="errormessage"></div>
    <button class="timButton"
            *ngIf="!isPlainText() && buttonText()"
            [disabled]="(disableUnchanged && !isUnSaved()) || isRunning || readonly"
            (click)="saveText()">
        {{buttonText()}}
    </button>
    <a href="" *ngIf="undoButton && isUnSaved() && undoButton" title="{{undoTitle}}"
            (click)="tryResetChanges($event);">{{undoButton}}</a>    
    <p class="savedtext" *ngIf="!hideSavedText && buttonText()">Saved!</p>
    <p *ngIf="footer" [innerText]="footer" class="plgfooter"></p>
</div>
`,
    styleUrls: ["textfield-plugin.component.scss"],
})
export class TextfieldPluginComponent
    extends AngularPluginBase<
        t.TypeOf<typeof TextfieldMarkup>,
        t.TypeOf<typeof TextfieldAll>,
        typeof TextfieldAll
    >
    implements ITimComponent
{
    private changes = false;
    private result?: string;
    isRunning = false;
    userword = "";
    private vctrl!: ViewCtrl;
    private initialValue = "";
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

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        private zone: NgZone
    ) {
        super(el, http, domSanitizer);
    }

    get disableUnchanged() {
        return this.markup.disableUnchanged;
    }

    get placeholder() {
        return this.markup.inputplaceholder ?? "";
    }

    getDefaultMarkup() {
        return {};
    }

    tryAutoGrow() {
        if (this.markup.autogrow) {
            this.autoGrow();
        }
    }

    /**
     * Returns (user) defined text for the button.
     */
    buttonText() {
        return super.buttonText() ?? null;
    }

    ngOnInit() {
        super.ngOnInit();
        this.vctrl = vctrlInstance!;
        this.userword = valueOr(
            this.attrsall.state?.c,
            this.markup.initword ?? ""
        ).toString();
        this.vctrl.addTimComponent(this, this.markup.tag);
        this.initialValue = this.userword;
        if (this.markup.showname) {
            this.initCode();
        }
        if (this.attrsall.state?.styles && !this.markup.ignorestyles) {
            this.applyStyling(this.attrsall.state.styles);
        }
        if (this.markup.textarea && this.markup.autogrow) {
            this.autoGrow();
        }
    }

    saveAndRefocus() {
        this.autoSave();
        this.changeFocus();
    }

    /**
     * Returns (user) content in string form.
     */
    getContent(): string {
        return this.userword;
    }

    /**
     * Save method for other plugins, needed by e.g. multisave plugin.
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
            this.applyStyling({});
            this.errormessage = undefined;
            return undefined;
        });
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
        this.zone.run(() => {
            this.userword = this.initialValue;
            this.changes = false;
            this.updateListeners(ChangeType.Saved);
        });
    }

    // TODO: Use answer content as arg or entire IAnswer?
    // TODO: get rid of any (styles can arrive as object)
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    setAnswer(content: Record<string, any>): ISetAnswerResult {
        return this.zone.run(() => {
            this.errormessage = undefined;
            let message;
            let ok = true;
            // TODO: should receiving empty answer reset to defaultnumber or clear field?
            if (Object.keys(content).length == 0) {
                this.resetField();
            } else {
                try {
                    this.userword = content.c;
                } catch (e) {
                    this.userword = "";
                    ok = false;
                    message = `Couldn't find related content ("c") from ${JSON.stringify(
                        content
                    )}`;
                    this.errormessage = message;
                }
                if (!this.markup.ignorestyles) {
                    this.applyStyling(content.styles);
                }
            }
            this.changes = false;
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
        return this.markup.inputstem ?? "";
    }

    /**
     * Returns (user) set col size (size of the field).
     */
    get cols() {
        return this.markup.cols;
    }

    get rows() {
        return this.markup.rows;
    }

    /**
     * Initialize content.
     */
    initCode() {
        if (this.markup.showname) {
            const u = this.vctrl.selectedUser;
            if (this.markup.showname == 1) {
                this.userword = u.real_name ?? "";
            }
            if (this.markup.showname == 2) {
                this.userword = u.name;
            }
        } else {
            this.userword = this.markup.initword ?? "";
        }
        this.initialValue = this.userword;
        this.result = undefined;
        this.changes = false;
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
     * Returns validinput attribute, if one is defined.
     * Used by pattern checker in angular.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    getPattern() {
        return this.markup.validinput ?? "";
    }

    /**
     * Returns focus on next HTML field.
     * Used by keydown (Enter) in angular.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    changeFocus() {
        const inputfields = document.querySelectorAll(
            "textfield-runner input, numericfield-runner input"
        );
        for (let i = 0; i < inputfields.length; ++i) {
            const selectedfield = inputfields[i] as HTMLInputElement;
            if (
                selectedfield === document.activeElement &&
                inputfields[i + 1]
            ) {
                const nextfield = inputfields[i + 1] as HTMLInputElement;
                this.preventedAutosave = true;
                return nextfield.focus();
            }
        }
    }

    /**
     * Returns true value, if label is set to plaintext.
     * Used to define readOnlyStyle in angular, either input or span.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isPlainText() {
        if (this.markup.showname) {
            return true;
        }
        const ros = this.markup.readOnlyStyle;
        if (ros === "htmlalways") {
            return true;
        }
        return (
            ros === "plaintext" && window.location.pathname.startsWith("/view/")
        );
    }

    isTextArea() {
        if (this.markup.textarea) {
            return true;
        }
        return false;
    }

    /**
     * Parses "styles" from the plugin answer that were saved by tableForm
     * For now only backgroundColor is supported
     * TODO: Extend styling for all attributes in timTable's cellStyles?
     *  For now tableForm is only able to define backgroundColor or textAlign
     *  Could also define (and import) generic tim-wide inputstyles
     * TODO: Could also just apply given styles as they are
     */
    applyStyling(styles: Record<string, string>) {
        if (!styles || Object.keys(styles).length == 0) {
            this.styles = {};
            return;
        }
        if (styles.backgroundColor) {
            this.styles.backgroundColor = styles.backgroundColor;
        }
    }

    /**
     * Method to check grading input type for textfield.
     * Used as e.g. grading checker for hyv | hyl | 1 | 2 | 3 | 4 | 5.
     * @param re validinput defined by given attribute.
     */
    validityCheck(re: string) {
        if (this.userword === "") {
            return new RegExp("").test(this.userword);
        }
        const regExpChecker = new RegExp(re);
        return regExpChecker.test(this.userword);
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
            // noinspection JSIgnoredPromiseFromCall
            this.saveText();
        }
    }

    /**
     * Actual save method, called by different save alternatives implemented above.
     * @param nosave: true/false parameter boolean checker for the need to save
     */
    async doSaveText(nosave: boolean) {
        if (!this.isUnSaved()) {
            this.saveResponse.saved = false;
            this.saveResponse.message = "No changes";
            return this.saveResponse;
        }
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
        this.isRunning = true;
        this.result = undefined;
        const params = {
            input: {
                nosave: false,
                c: this.userword.trim(),
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const r = await this.postAnswer<{
            web: {result: string; error?: string; clear?: boolean};
        }>(params);
        this.isRunning = false;
        if (r.ok) {
            const data = r.result;
            this.errormessage = data.web.error ?? "";
            this.result = data.web.result;
            this.initialValue = this.userword;
            this.changes = false;
            this.updateListeners(ChangeType.Saved);
            this.hideSavedText = false;
            this.redAlert = false;
            this.saveResponse.saved = true;
            this.saveResponse.message = this.errormessage;
            if (data.web.clear) {
                this.applyStyling({});
            }

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
                            //     dup.setAnswer({"c": this.userword, "styles": this.styles})
                            // }
                        }
                    }
                }
            }
        } else {
            this.errormessage =
                r.result.error.error ??
                this.markup.connectionErrorMessage ??
                defaultErrorMessage;
        }
        return this.saveResponse;
    }

    getAttributeType() {
        return TextfieldAll;
    }

    async autoGrow() {
        await timeout();
        const ele = this.element.find(".textarea").first();
        const scrollHeight = ele[0].scrollHeight;
        const prevHeight = parseFloat(ele.css("height"));
        if (scrollHeight < prevHeight) {
            return;
        }
        ele.css("height", ele[0].scrollHeight + "px");
    }

    formBehavior(): FormModeOption {
        return getFormBehavior(this.markup.form, FormModeOption.IsForm);
    }

    updateInput() {
        if (!this.changes) {
            this.changes = true;
            this.hideSavedText = true;
            this.updateListeners(ChangeType.Modified);
        }
    }

    // TODO: Generic, move
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
    declarations: [TextfieldPluginComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        TimUtilityModule,
        FormsModule,
        PurifyModule,
        TooltipModule.forRoot(),
    ],
})
export class TextfieldModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                TextfieldModule
            )
        ),
        "textfieldRunner",
        TextfieldPluginComponent
    ),
];
