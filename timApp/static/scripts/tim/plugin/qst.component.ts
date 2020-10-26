import deepEqual from "deep-equal";
import {
    ApplicationRef,
    Component,
    DoBootstrap,
    ElementRef,
    Input,
    NgModule,
    StaticProvider,
} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BrowserModule} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {getParId} from "../document/parhelpers";
import {
    AnswerSheetModule,
    IPreviewParams,
    makePreview,
} from "../document/question/answer-sheet.component";
import {ChangeType, ITimComponent, ViewCtrl} from "../document/viewctrl";
import {AnswerTable, IQuestionMarkup} from "../lecture/lecturetypes";
import {defaultErrorMessage, to, to2} from "../util/utils";
import {TimUtilityModule} from "../ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "../downgrade";
import {vctrlInstance} from "../document/viewctrlinstance";
import {handleAnswerResponse} from "../document/interceptor";
import {PurifyModule} from "../util/purify.module";
import {showQuestionAskDialog} from "../lecture/showLectureDialogs";
import {showMessageDialog} from "../ui/showMessageDialog";
import {IGenericPluginTopLevelFields} from "./attributes";
import {PluginBaseCommon, PluginMeta} from "./util";

// Represents fields that are not actually stored in plugin markup but that are added by TIM alongside markup
// in view route so that extra information can be passed to qst component. TODO: they should not be inside markup.
interface IQstExtraInfo {
    invalid?: boolean;
    isTask: boolean;
    savedText?: string;
}

interface IQstAttributes
    extends IGenericPluginTopLevelFields<IQuestionMarkup & IQstExtraInfo> {
    state: AnswerTable | null;
    show_result: boolean;
    savedText: string;
}

@Component({
    selector: "tim-qst",
    template: `
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
            <a href="" *ngIf="undoButton && isUnSaved()" title="{{undoTitle}}" (click)="tryResetChanges()">
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
export class QstComponent extends PluginBaseCommon implements ITimComponent {
    error?: string;
    log?: string;
    isRunning: boolean = false;
    result?: string;
    errors: string[];
    private vctrl!: ViewCtrl;
    @Input() json!: string;
    attrsall!: IQstAttributes; // ngOnInit
    preview!: IPreviewParams; // ngOnInit
    button: string = "";
    stem: string = "";
    private savedAnswer?: AnswerTable;
    private newAnswer?: AnswerTable;
    private changes = false;
    protected pluginMeta: PluginMeta;
    element: JQuery;

    constructor(element: ElementRef, private http: HttpClient) {
        super();
        const el = $(element.nativeElement);
        this.element = el;
        this.pluginMeta = new PluginMeta(el);
        this.errors = [];
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
        this.vctrl = vctrlInstance!;
        if (!this.pluginMeta.isPreview()) {
            this.vctrl.addTimComponent(this);
        }
        this.attrsall = JSON.parse(this.json) as IQstAttributes;
        this.preview = makePreview(this.attrsall.markup, {
            answerTable: this.attrsall.state ?? [],
            showCorrectChoices: this.attrsall.show_result,
            showExplanations: this.attrsall.show_result,
            enabled: !this.attrsall.markup.invalid,
        });
        this.result = "";
        this.button =
            this.attrsall.markup.button ??
            this.attrsall.markup.buttonText ??
            "Save";
        this.stem = this.attrsall.markup.stem ?? "";
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
    }

    getQuestionTitle() {
        return this.attrsall.markup.questionTitle;
    }

    getQuestionTitleShort() {
        return this.attrsall.markup.questionTitle;
    }

    questionClicked() {
        const par = this.element.parents(".par");
        const parId = getParId(par);
        if (!parId) {
            showMessageDialog("Not a valid paragraph.");
            return;
        }
        this.showQuestionNew(parId);
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
        this.error = "";
        this.result = "";
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

        this.result = "";

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
        const url = this.pluginMeta.getAnswerUrl();

        const r = await to2(
            this.http
                .put<{
                    web: {
                        result: string;
                        show_result: boolean;
                        state: AnswerTable;
                        markup?: IQuestionMarkup;
                        error?: string;
                    };
                    savedNew: number | false;
                }>(url, params, {})
                .toPromise()
        );
        this.isRunning = false;
        if (!r.ok) {
            this.errors.push(r.result.error.error);
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
        handleAnswerResponse(
            this.pluginMeta.getTaskId()!.docTask().toString(),
            data
        );
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

    tryResetChanges(): void {
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

const bootstrapFn = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(QstModule);
};

const angularJsModule = createDowngradedModule(bootstrapFn);
doDowngrade(angularJsModule, "timQst", QstComponent);
export const moduleDefs = [angularJsModule];
