import deepEqual from "deep-equal";
import {
    ApplicationRef,
    ChangeDetectorRef,
    Component,
    DoBootstrap,
    ElementRef,
    NgModule,
} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BrowserModule, DomSanitizer} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import * as t from "io-ts";
import {
    AnswerSheetModule,
    IPreviewParams,
    makePreview,
} from "../document/question/answer-sheet.component";
import {ChangeType, ITimComponent, ViewCtrl} from "../document/viewctrl";
import {
    AnswerTable,
    AskedJsonJsonCodec,
    IQuestionMarkup,
} from "../lecture/lecturetypes";
import {defaultErrorMessage, to} from "../util/utils";
import {TimUtilityModule} from "../ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "../downgrade";
import {vctrlInstance} from "../document/viewctrlinstance";
import {PurifyModule} from "../util/purify.module";
import {showQuestionAskDialog} from "../lecture/showLectureDialogs";
import {GenericPluginMarkup, getTopLevelFields, nullable} from "./attributes";
import {AngularPluginBase} from "./angular-plugin-base.directive";

const PluginMarkupFields = t.intersection([
    GenericPluginMarkup,
    AskedJsonJsonCodec,
    t.partial({invalid: t.boolean, savedText: t.string}),
    t.type({
        isTask: t.boolean,
    }),
]);
const PluginFields = t.intersection([
    getTopLevelFields(PluginMarkupFields),
    t.type({
        state: nullable(t.array(t.array(t.string))),
    }),
    t.partial({
        show_result: t.boolean,
    }),
]);

@Component({
    selector: "tim-qst",
    template: `
        <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
        <div class="csRunDiv qst no-popup-menu" *ngIf="isTask()">
            <h4 *ngIf="getHeader()" [innerHtml]="getHeader() | purify"></h4>
            <p *ngIf="stem" class="stem" [innerHtml]="stem | purify"></p>
            <tim-answer-sheet
                    [questiondata]="preview"
                    (onAnswerChange)="updateAnswer($event)">
            </tim-answer-sheet>
            <button class="timButton" [innerHtml]="button" *ngIf="button"
                    [disabled]="isRunning || isInvalid() || (disableUnchanged && !isUnSaved())"
                    (click)="saveText()"></button>
            <a href="" *ngIf="undoButton && isUnSaved()" title="{{undoTitle}}" (click)="tryResetChanges($event)">
                &nbsp;{{undoButton}}
            </a>

            &nbsp;&nbsp;
            <a class="questionAddedNew" *ngIf="hasTeacherRight() && !isInvalid()" (click)="questionClicked()">
                <span class="glyphicon glyphicon-question-sign" title="Ask question"></span>
            </a>
            &ngsp;
            <span *ngIf="result">{{result}}</span>
            <p class="plgfooter" [innerHtml]="getFooter() | purify"></p>
            <div *ngIf="error" class="error" style="font-size: 12px" [innerHtml]="error"></div>
        </div>
        <div *ngIf="!isTask()">
            <a class="questionAddedNew" (click)="questionClicked()">
                <span class="glyphicon glyphicon-question-sign" title="{{getQuestionTitle()}}"></span>
            </a>
            <p class="questionNumber" [innerText]="getQuestionTitleShort()"></p>
        </div>
        <div *ngIf="log" class="qstLog" [innerHtml]="log"></div>
    `,
})
export class QstComponent
    extends AngularPluginBase<
        t.TypeOf<typeof PluginMarkupFields>,
        t.TypeOf<typeof PluginFields>,
        typeof PluginFields
    >
    implements ITimComponent {
    error?: string;
    log?: string;
    isRunning: boolean = false;
    result?: string;
    private vctrl!: ViewCtrl;
    preview!: IPreviewParams; // ngOnInit
    button: string = "";
    private savedAnswer?: AnswerTable;
    private newAnswer?: AnswerTable;
    private changes = false;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        private cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer);
    }

    getContent() {
        return JSON.stringify(this.newAnswer);
    }

    get disableUnchanged() {
        return this.attrsall.markup.disableUnchanged;
    }

    get undoButton() {
        return this.attrsall.markup.undo?.button;
    }

    get undoTitle() {
        return this.attrsall.markup.undo?.title;
    }

    get undoConfirmation() {
        return this.attrsall.markup.undo?.confirmation;
    }

    isUnSaved(userChange?: boolean | undefined): boolean {
        return this.changes;
    }

    async save(): Promise<{saved: boolean; message: string | undefined}> {
        if (this.isUnSaved()) {
            return this.doSaveText(false);
        } else {
            return {saved: false, message: undefined};
        }
    }

    ngOnInit() {
        super.ngOnInit();
        this.vctrl = vctrlInstance!;
        if (!this.pluginMeta.isPreview()) {
            this.vctrl.addTimComponent(this);
        }
        this.preview = makePreview(this.attrsall.markup, {
            answerTable: this.attrsall.state ?? [],
            showCorrectChoices: this.attrsall.show_result,
            showExplanations: this.attrsall.show_result,
            enabled: !this.attrsall.markup.invalid,
        });
        this.button = this.buttonText() ?? "Save";
    }

    getHeader() {
        return this.attrsall.markup.header;
    }

    getFooter() {
        return this.attrsall.markup.footer;
    }

    isTask() {
        return this.attrsall.markup.isTask;
    }

    private checkChanges() {
        const oldVal = this.changes;
        this.changes = !deepEqual(this.savedAnswer, this.newAnswer);
        if (oldVal != this.changes) {
            this.updateListeners(
                this.changes ? ChangeType.Modified : ChangeType.Saved
            );
        }
        this.cdr.detectChanges();
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
            this.attrsall.markup.tag ? this.attrsall.markup.tag : undefined
        );
    }

    updateAnswer(at: AnswerTable) {
        // updateAnswer is called always at least once from dynamicAnswerSheet (see the ngOnChanges in that file).
        // Upon first call, we record the currently saved answer.
        if (this.newAnswer === undefined) {
            this.savedAnswer = at;
        }
        this.newAnswer = at;
        this.checkChanges();
        if (this.changes) {
            this.result = undefined;
        }
        this.cdr.detectChanges();
    }

    getQuestionTitle() {
        return this.attrsall.markup.questionTitle;
    }

    getQuestionTitleShort() {
        return this.attrsall.markup.questionTitle;
    }

    questionClicked() {
        this.showQuestionNew(this.getPar().originalPar.id);
    }

    private async showQuestionNew(parId: string) {
        const result = await to(
            showQuestionAskDialog({
                docId: this.vctrl.docId,
                parId: parId,
                showAsk: this.vctrl.lectureCtrl.lectureSettings.inLecture,
            })
        );
        if (result.ok) {
            this.vctrl.lectureCtrl.lastQuestion = result.result;
        }
    }

    private initCode() {
        this.error = undefined;
        this.result = undefined;
    }

    saveText() {
        this.doSaveText(false);
    }

    hasTeacherRight() {
        return this.vctrl.item.rights.teacher;
    }

    isInvalid() {
        return this.attrsall.markup.invalid;
    }

    private async doSaveText(nosave: boolean) {
        this.log = undefined;
        this.error = undefined;
        this.isRunning = true;

        this.result = undefined;

        const answers = this.newAnswer;

        const params = {
            input: {
                answers,
                nosave: false,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const r = await this.postAnswer<{
            web: {
                result: string;
                show_result: boolean;
                state: AnswerTable;
                markup?: IQuestionMarkup;
                error?: string;
            };
        }>(params);
        this.isRunning = false;
        if (!r.ok) {
            this.error =
                r.result.error.error ??
                this.attrsall.markup.connectionErrorMessage ??
                defaultErrorMessage;
            return {
                saved: false,
                message:
                    r.result.error.error ??
                    this.attrsall.markup.connectionErrorMessage,
            };
        }
        const data = r.result;
        let result = data.web.result;
        if (result == "Saved" && this.attrsall.markup.savedText) {
            result = this.attrsall.markup.savedText;
        }
        this.result = result;
        this.log = data.web.error;
        if (data.web.markup && data.web.show_result) {
            this.preview = makePreview(data.web.markup, {
                answerTable: data.web.state,
                enabled: true,
            });
            this.preview.showExplanations = true;
            this.preview.showCorrectChoices = true;
        }
        this.savedAnswer = this.newAnswer;
        this.checkChanges();
        return {saved: true, message: undefined};
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
        this.newAnswer = this.savedAnswer;
        this.preview = makePreview(this.attrsall.markup, {
            answerTable: this.savedAnswer,
            showCorrectChoices: this.attrsall.show_result,
            showExplanations: this.attrsall.show_result,
            enabled: !this.attrsall.markup.invalid,
        });
        this.checkChanges();
    }

    getAttributeType() {
        return PluginFields;
    }

    getDefaultMarkup() {
        return {
            headers: [],
            rows: [],
            answerFieldType: "radio" as const,
            questionText: "",
            questionTitle: "",
            questionType: "matrix" as const,
            isTask: true,
            invalid: true,
        };
    }
}

@NgModule({
    declarations: [QstComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        AnswerSheetModule,
        PurifyModule,
    ],
})
export class QstModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(QstModule)
        ),
        "timQst",
        QstComponent
    ),
];
