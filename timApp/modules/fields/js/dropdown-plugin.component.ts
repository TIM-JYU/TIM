/**
 * Defines the client-side implementation of a dropdown plugin.
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
import {getFormBehavior, shuffleStrings} from "tim/plugin/util";
import {defaultErrorMessage} from "tim/util/utils";
import {BrowserModule, DomSanitizer} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {TimUtilityModule} from "../../../static/scripts/tim/ui/tim-utility.module";
import {PurifyModule} from "../../../static/scripts/tim/util/purify.module";
import {
    createDowngradedModule,
    doDowngrade,
} from "../../../static/scripts/tim/downgrade";
import {AngularPluginBase} from "../../../static/scripts/tim/plugin/angular-plugin-base.directive";
import {vctrlInstance} from "../../../static/scripts/tim/document/viewctrlinstance";

const DropdownMarkup = t.intersection([
    t.partial({
        tag: t.string,
        words: t.array(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        // All withDefaults should come here, NOT in t.partial.
        answers: withDefault(t.boolean, false),
        autosave: withDefault(t.boolean, false),
        instruction: withDefault(t.boolean, false),
        radio: withDefault(t.boolean, false),
        shuffle: withDefault(t.boolean, false),
    }),
]);
const DropdownAll = t.intersection([
    t.partial({}),
    t.type({
        info: Info,
        markup: DropdownMarkup,
        preview: t.boolean,
        state: nullable(t.type({c: t.string})),
    }),
]);

@Component({
    selector: "tim-dropdown-runner",
    template: `
<div>
    <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
    <h4 *ngIf="header" [innerHtml]="header | purify"></h4>
    <p class="stem" *ngIf="stem">{{stem}}</p>
    <div class="form-inline">
        <div class="dropdown-radio" *ngIf="radio">
            <label *ngFor="let item of wordList">
                <input type="radio"
                          name="selection"
                          value="{{item}}"
                          [(ngModel)]="selectedWord"
                          (ngModelChange)="updateSelection()">
                {{item}}
            </label>        
        </div>
        <select *ngIf="!radio"
                [(ngModel)]="selectedWord"
                (ngModelChange)="updateSelection()"
                [ngClass]="{warnFrame: isUnSaved()}">
            <option *ngFor="let item of wordList" [value]="item">{{item}}</option>
        </select>
    </div>
    <div *ngIf="connectionErrorMessage" class="error" style="font-size: 12px" [innerHtml]="connectionErrorMessage"></div>
    <div class="error" *ngIf="error" [innerHtml]="error"></div>
    <p *ngIf="footer" [innerText]="footer | purify" class="plgfooter"></p>
</div>`,
    styleUrls: ["./dropdown-plugin.component.scss"],
})
export class DropdownPluginComponent
    extends AngularPluginBase<
        t.TypeOf<typeof DropdownMarkup>,
        t.TypeOf<typeof DropdownAll>,
        typeof DropdownAll
    >
    implements ITimComponent
{
    error?: string;
    wordList?: string[];
    selectedWord?: string;
    private initialWord?: string;
    private vctrl!: ViewCtrl;
    private forceSave = false;
    radio?: boolean;
    private shuffle?: boolean;
    private changes = false;
    connectionErrorMessage?: string;

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

    ngOnInit() {
        super.ngOnInit();
        this.vctrl = vctrlInstance!;
        this.selectedWord = this.attrsall.state?.c ?? undefined;
        this.shuffle = this.markup.shuffle;
        if (this.shuffle && this.markup.words) {
            this.wordList = shuffleStrings(this.markup.words);
        } else {
            this.wordList = this.markup.words ?? [];
        }
        if (!this.attrsall.preview) {
            this.addToCtrl();
        }
        this.radio = this.markup.radio;
        if (this.markup.answers) {
            // TODO: Implement showing and hiding the answer browser if it is deemed useful.
        }
    }

    /**
     * Adds this plugin to ViewCtrl so other plugins can get information about the plugin though it.
     */
    addToCtrl() {
        this.vctrl.addTimComponent(this, this.markup.tag);
    }

    /**
     * Returns the selected choice from the dropdown-list.
     * @returns {string} The selected choice.
     */
    getContent() {
        return this.selectedWord;
    }

    /**
     * Saves the selection that in the plugin.
     */
    async save() {
        return this.zone.run(() => this.doSave(this.markup.instruction));
    }

    async doSave(nosave: boolean) {
        this.connectionErrorMessage = undefined;
        // TODO: Check whether to skip undefined inputs or save empty strings
        if (this.selectedWord == undefined) {
            this.selectedWord = "";
        }
        const params = {
            input: {
                nosave: false,
                selectedWord: this.selectedWord,
            },
            options: {
                forceSave: this.forceSave,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }

        const r = await this.postAnswer<{
            web: {result: string; error?: string};
        }>(params);

        if (r.ok) {
            this.changes = false;
            this.updateListeners(ChangeType.Saved);
            const data = r.result;
            this.error = data.web.error;
        } else {
            this.error = r.result.error.error;
            this.connectionErrorMessage =
                this.error ??
                this.markup.connectionErrorMessage ??
                defaultErrorMessage;
        }
        this.initialWord = this.selectedWord;
        return {saved: r.ok, message: this.error};
    }

    updateSelection() {
        if (!this.changes) {
            this.changes = true;
            this.updateListeners(ChangeType.Modified);
        }
        if (this.markup.autosave || this.markup.autosave === undefined) {
            this.save();
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
        this.vctrl.informChangeListeners(taskId, state, this.markup.tag);
    }

    /**
     * Force the plugin to save its information.
     *
     * @param force Whether to force the plugin to always save itself when the answer route is called.
     */
    setForceAnswerSave(force: boolean) {
        this.zone.run(() => {
            this.forceSave = force;
        });
    }

    /**
     * Sets the words visible in the plugin and randomizes their order if desired.
     *
     * @param words List of words to be shown in the plugin.
     */
    setPluginWords(words: string[]) {
        this.zone.run(() => {
            if (this.shuffle) {
                this.wordList = shuffleStrings(words);
            } else {
                this.wordList = words;
            }

            this.selectedWord = undefined;
            this.initialWord = this.selectedWord;
        });
    }

    getAttributeType() {
        return DropdownAll;
    }

    isUnSaved() {
        return this.changes;
    }

    formBehavior(): FormModeOption {
        return getFormBehavior(this.markup.form, FormModeOption.IsForm);
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    setAnswer(content: Record<string, any>): ISetAnswerResult {
        return this.zone.run(() => {
            this.error = undefined;
            let message;
            let ok = true;
            if (Object.keys(content).length == 0) {
                this.resetField();
            } else {
                try {
                    this.selectedWord = content.c;
                } catch (e) {
                    this.selectedWord = "";
                    ok = false;
                    message = `Couldn't find related content ("c") from ${JSON.stringify(
                        content
                    )}`;
                    this.error = message;
                }
            }
            this.changes = false;
            this.updateListeners(ChangeType.Saved);
            this.initialWord = this.selectedWord;
            return {ok: ok, message: message};
        });
    }

    resetField(): undefined {
        return this.zone.run(() => {
            this.selectedWord = "";
            this.initialWord = this.selectedWord;
            this.changes = false;
            this.error = undefined;
            this.updateListeners(ChangeType.Saved);
            return undefined;
        });
    }

    resetChanges(): void {
        this.zone.run(() => {
            this.selectedWord = this.initialWord;
            this.changes = false;
            this.updateListeners(ChangeType.Saved);
        });
    }
}

@NgModule({
    declarations: [DropdownPluginComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        TimUtilityModule,
        FormsModule,
        TooltipModule.forRoot(),
        PurifyModule,
    ],
})
export class DropdownModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                DropdownModule
            )
        ),
        "dropdownRunner",
        DropdownPluginComponent
    ),
];
