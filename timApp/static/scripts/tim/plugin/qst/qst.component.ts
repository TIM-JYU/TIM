import deepEqual from "deep-equal";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {
    ChangeDetectorRef,
    Component,
    ElementRef,
    NgModule,
    NgZone,
} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {DomSanitizer} from "@angular/platform-browser";
import * as t from "io-ts";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {defaultErrorMessage, to2} from "tim/util/utils";
import type {IPreviewParams} from "tim/document/question/answer-sheet.component";
import {
    AnswerSheetModule,
    makePreview,
} from "tim/document/question/answer-sheet.component";
import type {ITimComponent} from "tim/document/viewctrl";
import {ChangeType} from "tim/document/viewctrl";
import type {AnswerTable, IQuestionMarkup} from "tim/lecture/lecturetypes";
import {AskedJsonJsonCodec} from "tim/lecture/lecturetypes";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {showQuestionAskDialog} from "tim/lecture/showLectureDialogs";
import type {ParContext} from "tim/document/structure/parContext";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
} from "tim/plugin/attributes";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {CommonModule} from "@angular/common";
import {TooltipModule} from "ngx-bootstrap/tooltip";

const PluginMarkupFields = t.intersection([
    GenericPluginMarkup,
    AskedJsonJsonCodec,
    t.partial({
        invalid: t.boolean,
        savedText: t.string,
    }),
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
        <div class="csRunDiv qst no-popup-menu" [class.cs-has-header]="getHeader()" *ngIf="isTask()">
            <h4 *ngIf="getHeader()" [innerHtml]="getHeader() | purify"></h4>
            <p *ngIf="stem" class="stem" [innerHtml]="stem | purify"></p>
            <tim-answer-sheet
                [questiondata]="preview"
                [customHeader]="markup['customHeader']"
                (onAnswerChange)="updateAnswer($event)">
            </tim-answer-sheet>
            <div class="csRunMenuArea">
                <div class="csRunMenu">
                    <button class="timButton"
                            [innerHtml]="button"
                            *ngIf="(button && !isAutosave) || saveFailed"
                            [disabled]="isRunning || isInvalid() || (disableUnchanged && !isUnSaved())"
                            (click)="saveText()"></button>
                    <a href="" *ngIf="undoButton && isUnSaved()" title="{{undoTitle}}"
                       (click)="tryResetChanges($event)">
                        &nbsp;{{ undoButton }}
                    </a>
                    <ng-container *ngIf="invalidMarker">&nbsp;&nbsp;&nbsp;&nbsp;</ng-container>
                    <span *ngIf="invalidMarker" [tooltip]="poptemplate" placement="bottom"
                          class="glyphicon glyphicon-lock text-warning"></span>
                    <ng-template #poptemplate>
                        <div *ngFor="let ttip of invalidMarkerTooltip">
                            <span [innerText]="ttip"></span>
                        </div>
                    </ng-template>
                    &nbsp;&nbsp;
                    <a class="questionAddedNew" *ngIf="hasTeacherRight() && !isInvalid()" (click)="questionClicked()">
                        <span class="glyphicon glyphicon-question-sign" i18n-title title="Ask question"></span>
                    </a>
                    &ngsp;
                    <span class="qstResult" *ngIf="result">{{ result }}</span>
                </div>
            </div>
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
    implements ITimComponent
{
    error?: string;
    log?: string;
    isRunning: boolean = false;
    result?: string;

    preview!: IPreviewParams; // ngOnInit
    button: string = "";
    private savedAnswer?: AnswerTable;
    private newAnswer?: AnswerTable;
    private changes = false;
    saveFailed = false;
    private enabled = true;
    invalidMarkerTooltip?: string[];
    invalidMarker = false;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        private zone: NgZone,
        private cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer);
    }

    getContent() {
        return JSON.stringify(this.newAnswer);
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

    get isAutosave() {
        // TODO: Autosave should be enabled for all types, but it requires proper slowdown to prevent too many saves
        const autoSaveType =
            this.markup.matrixType !== "textArea" &&
            this.markup.matrixType !== "inputText" &&
            this.markup.answerFieldType !== "inputText" &&
            this.markup.answerFieldType !== "text" &&
            this.markup.questionType !== "textarea";

        return (this.markup.autosave ?? false) && autoSaveType;
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

        if (!this.pluginMeta.isPreview()) {
            this.vctrl.addTimComponent(this);
        }
        if (this.attrsall.markup.invalid || this.readonly) {
            this.enabled = false;
        }
        this.preview = makePreview(this.attrsall.markup, {
            answerTable: this.attrsall.state ?? [],
            showCorrectChoices: this.attrsall.show_result,
            showExplanations: this.attrsall.show_result,
            enabled: this.enabled,
        });
        this.button = this.buttonText() ?? $localize`Save`;
        this.getInvalidMarkerTooltip();
    }

    ngOnDestroy() {
        if (!this.pluginMeta.isPreview()) {
            this.vctrl.removeTimComponent(this);
        }
    }

    async updateInvalidMarkerState() {
        await this.getInvalidMarkerTooltip();
    }

    private async getInvalidMarkerTooltip() {
        const result = await this.getInvalidMarkerState();
        this.invalidMarkerTooltip = result?.toolTipTexts ?? [];
        this.invalidMarker = result?.invalidMarkerSet ?? false;
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

    async updateAnswer(at: AnswerTable) {
        // updateAnswer is called always at least once from dynamicAnswerSheet (see the ngOnChanges in that file).
        // Upon first call, we record the currently saved answer.
        if (this.newAnswer === undefined) {
            this.savedAnswer = at;
        }
        this.newAnswer = at;
        this.checkChanges();
        if (this.changes) {
            this.result = undefined;
            if (this.isAutosave) {
                await this.doSaveText(false);
            }
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
        this.showQuestionNew(this.getPar()!);
    }

    private async showQuestionNew(par: ParContext) {
        const result = await to2(
            showQuestionAskDialog({
                docId: this.vctrl.docId,
                parId: par.originalPar.id,
                showAsk: this.vctrl.lectureCtrl.lectureSettings.inLecture,
            })
        );
        if (result.ok) {
            if (result.result.question) {
                this.vctrl.lectureCtrl.lastQuestion = result.result.question;
            }
            if (result.result.editResult) {
                this.zone.runOutsideAngular(() => {
                    this.vctrl.questionHandler.handleQstEditResult(
                        result.result.editResult!,
                        par
                    );
                });
            }
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
            this.saveFailed = true;
            return {
                saved: false,
                message:
                    r.result.error.error ??
                    this.attrsall.markup.connectionErrorMessage,
            };
        }
        const data = r.result;
        let result = data.web.result;
        if (result == "Saved") {
            if (this.attrsall.markup.savedText != undefined) {
                result = this.attrsall.markup.savedText;
            } else {
                result = $localize`Saved`;
            }
        }
        this.result = result;
        this.log = data.web.error;
        if (data.web.markup && data.web.show_result) {
            this.preview = makePreview(data.web.markup, {
                answerTable: data.web.state,
                enabled: this.enabled ?? true,
            });
            this.preview.showExplanations = true;
            this.preview.showCorrectChoices = true;
        }
        this.savedAnswer = this.newAnswer;
        this.saveFailed = false;
        this.checkChanges();
        return {saved: true, message: undefined};
    }

    resetChanges(): void {
        this.newAnswer = this.savedAnswer;
        this.preview = makePreview(this.attrsall.markup, {
            answerTable: this.savedAnswer,
            showCorrectChoices: this.attrsall.show_result,
            showExplanations: this.attrsall.show_result,
            enabled: this.enabled,
        });
        this.checkChanges();
        this.saveFailed = false;
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
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        AnswerSheetModule,
        PurifyModule,
        TooltipModule,
    ],
})
export class QstModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("tim-qst", QstModule, QstComponent);
