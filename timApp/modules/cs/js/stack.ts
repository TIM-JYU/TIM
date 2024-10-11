import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {ParCompiler} from "tim/editor/parCompiler";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {windowAsAny} from "tim/util/utils";
import type {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {ScriptedInnerHTMLModule} from "tim/util/scripted-inner-html.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";

const STACK_VARIABLE_PREFIX = "stackapi_";

const StackMarkup = t.intersection([
    t.partial({
        beforeOpen: t.string,
        buttonBottom: t.boolean,
        by: t.string,
        correctresponse: t.boolean,
        generalfeedback: t.boolean,
        open: t.boolean,
        timWay: t.boolean,
        inputplaceholder: nullable(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        autopeek: withDefault(t.boolean, true),
        lang: withDefault(t.string, "fi"),
        inputrows: withDefault(t.Integer, 1),
    }),
]);
const StackAll = t.intersection([
    t.partial({
        by: t.string,
        timWay: t.boolean,
        usercode: t.string,
    }),
    t.type({
        info: Info,
        markup: StackMarkup,
        preview: t.boolean,
    }),
]);

type StackResult =
    | string
    | {
          answernotes: string;
          api_time: number;
          error: false;
          formatcorrectresponse: string;
          generalfeedback: string;
          questiontext: string;
          request_time: number;
          score: number;
          summariseresponse: unknown;
      }
    | {
          error: true;
          message: string;
      };

interface IStackData {
    answer: Record<string, string>;
    prefix: string;
    seed?: number;
    verifyvar: string;
}

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "tim-stack-runner",
    template: `
        <div class="csRunDiv math que stack no-popup-menu" [class.cs-has-header]="header">
            <h4 *ngIf="header" [innerHtml]="header"></h4>
            <p *ngIf="stem" class="stem" [innerHtml]="stem"></p>
            <p *ngIf="!isOpen" class="stem" [innerHtml]="markup.beforeOpen | purify"></p>

            <div class="no-popup-menu stackOutput" *ngIf="timWay">
                <div class="csRunCode"><textarea class="csRunArea csInputArea"
                                                 name="stackapi_ans1" id="stackapi_ans1"
                                                 [rows]="inputrows"
                                                 [(ngModel)]="userCode"
                                                 [placeholder]="inputplaceholder"></textarea></div>
            </div>
            <div id="output" *ngIf="!timWay && stackOutput" class="stackOutput" [scriptedInnerHTML]="stackOutput"></div>
            <p class="csRunMenu">
                <button *ngIf="!isOpen"
                        class="timButton btn-sm"
                        (click)="runGetTask()">Show task
                </button>
                <button *ngIf="isOpen"
                        [disabled]="isRunning"
                        title="(Ctrl-S)"
                        class="timButton btn-sm"
                        (click)="runSave()"
                        [innerHtml]="button | purify"></button>
                <button *ngIf="!markup.autopeek"
                        class="timButton btn-sm"
                        [disabled]="isRunning"
                        (click)="runPeek()">Peek
                </button>
            </p>
            <div *ngIf="stackPeek" class="peekdiv" id="peek" style="min-height: 10em;">
                <div></div>
            </div>
            <div *ngIf="stackInputFeedback" id="stackinputfeedback"
                 class="stackinputfeedback1"
                 [scriptedInnerHTML]="stackInputFeedback"></div>
            <span class="csRunError"
                  *ngIf="error"
                  [innerHtml]="error | purify"></span>

            <div *ngIf="stackFeedback">
                <div *ngIf="markup.generalfeedback">
                    <h5>General feedback:</h5>
                    <div id="generalfeedback" [innerHtml]="stackFeedback | purify"></div>
                </div>
                <div *ngIf="markup.correctresponse">
                    <h5>Format correct response:</h5>
                    <div id="formatcorrectresponse" [innerHtml]="stackFormatCorrectResponse | purify"></div>
                    <div style="font-size: 0.7em;">
                        <p>Score: <span id="score" [innerHtml]="stackScore | purify"></span></p>
                        <p>Summarise response: <span id="summariseresponse"
                                                     [innerHtml]="stackSummariseResponse | purify"></span></p>
                        <p>Answer notes: <span id="answernotes" [innerHtml]="stackAnswerNotes | purify"></span></p>
                        <p>Time: <span id="time" [innerHtml]="stackTime | purify"></span></p>
                    </div>
                </div>
            </div>

            <p class="plgfooter" *ngIf="footer" [innerHtml]="footer | purify"></p>
        </div>
    `,
})
export class StackPluginComponent
    extends AngularPluginBase<
        t.TypeOf<typeof StackMarkup>,
        t.TypeOf<typeof StackAll>,
        typeof StackAll
    >
    implements ITimComponent
{
    vctrl!: ViewCtrl;

    getContent(): string {
        return this.userCode;
    }

    getContentArray?: () => string[] | undefined;
    isUnSaved(userChange?: boolean | undefined): boolean {
        return this.userCode !== this.originalUserCode;
    }

    async save() {
        await this.runSave();
        return {saved: true, message: undefined};
    }

    setPluginWords?: (words: string[]) => void;
    setForceAnswerSave?: (force: boolean) => void;

    get english() {
        return this.markup.lang === "en";
    }

    buttonText() {
        const txt = super.buttonText();
        if (txt) {
            return txt;
        }
        return this.english ? "Send" : "Lähetä";
    }

    private span: string = "";
    error: string = "";
    userCode: string = "";
    private originalUserCode: string = "";
    stackOutput?: string;
    stackInputFeedback?: string;
    stackPeek: boolean = false;
    stackFeedback: string = "";
    stackFormatCorrectResponse: string = "";
    stackScore: string = "";
    stackSummariseResponse: string = "";
    stackAnswerNotes: string = "";
    stackTime: string = "";
    isRunning: boolean = false;
    inputrows: number = 1;
    timWay: boolean = false; // if answer is given to TIM TextArea-field
    isOpen: boolean = false;
    private lastInputFieldId: string = "";
    private lastInputFieldValue: string = "";
    private lastInputFieldElement?: HTMLInputElement;
    button: string = "";
    inputplaceholder!: string;

    private timer?: number;

    ngOnInit() {
        super.ngOnInit();
        this.button = this.buttonText();
        const aa = this.attrsall;
        this.userCode = aa.usercode ?? this.markup.by ?? "";
        this.originalUserCode = this.userCode;
        this.timWay = aa.timWay ?? this.markup.timWay ?? false;
        this.inputrows = this.markup.inputrows;
        this.inputplaceholder = this.markup.inputplaceholder ?? "";

        this.element.on("keydown", (event) => {
            if (event.ctrlKey || event.metaKey) {
                switch (event.key) {
                    case "s":
                        event.preventDefault();
                        this.runSend(false);
                        break;
                }
            }
        });

        if (this.markup.open) {
            this.runGetTask();
        }

        if (!this.attrsall.preview) {
            this.vctrl.addTimComponent(this);
        }
    }

    ngOnDestroy() {
        if (!this.attrsall.preview) {
            this.vctrl.removeTimComponent(this);
        }
    }

    processNodes(
        res: Record<string, string>,
        nodes:
            | HTMLCollectionOf<HTMLInputElement>
            | HTMLCollectionOf<HTMLTextAreaElement>
            | HTMLCollectionOf<HTMLSelectElement>,
        id: string
    ): Record<string, string> {
        for (const element of nodes) {
            if (
                element.name.startsWith(STACK_VARIABLE_PREFIX) &&
                !element.name.includes("_val") &&
                element.name.includes(id)
            ) {
                if (
                    element instanceof HTMLInputElement &&
                    (element.type === "checkbox" || element.type === "radio")
                ) {
                    if (element.checked) {
                        res[element.name] = element.value;
                    }
                } else {
                    res[element.name] = element.value;
                }
            }
        }
        return res;
    }

    collectAnswer(id: string) {
        const parent = this.element[0];
        const inputs = parent.getElementsByTagName("input");
        const textareas = parent.getElementsByTagName("textarea");
        const selects = parent.getElementsByTagName("select");
        let res: Record<string, string> = {};
        if (!this.timWay) {
            res = this.processNodes(res, inputs, id);
            res = this.processNodes(res, textareas, id);
            res = this.processNodes(res, selects, id);
            if (Object.keys(res).length && this.userCode) {
                this.userCode = JSON.stringify(res);
            } else {
                try {
                    res = JSON.parse(this.userCode) as Record<string, string>;
                } catch {
                    // this.timWay = true;
                }
            } // note: cannot be else because timWay may change during try
        }
        if (this.timWay) {
            res[`${STACK_VARIABLE_PREFIX}ans1`] = this.userCode;
        }
        return res;
    }

    collectData() {
        return {
            answer: this.collectAnswer(""),
            prefix: STACK_VARIABLE_PREFIX,
        };
    }

    replace(s: string): string {
        // s = s.replace('https://stack-api-server/plots/', '/stackserver/plots/');
        return s;
    }

    async handleServerResult(r: StackResult, getTask: boolean) {
        if (typeof r === "string") {
            this.error = r.toString();
            return;
        }
        if (r.error) {
            this.error = r.message;
            return;
        }

        const qt = this.replace(r.questiontext);
        const i = qt.indexOf('<div class="stackinputfeedback"');
        const helper = await import("../stack/ServerSyncValues");
        windowAsAny().ServerSyncValues = helper.ServerSyncValues;
        windowAsAny().findParentElementFromScript =
            helper.findParentElementFromScript;
        if (this.markup.buttonBottom || i < 0) {
            this.stackOutput = qt;
            this.stackInputFeedback = "";
        } else {
            this.stackOutput = qt.substr(0, i) + "\n";
            this.stackInputFeedback = qt.substr(i);
        }

        if (!getTask) {
            this.stackFeedback = this.replace(r.generalfeedback);
            this.stackFormatCorrectResponse = this.replace(
                r.formatcorrectresponse
            );
            this.stackSummariseResponse = this.replace(
                JSON.stringify(r.summariseresponse)
            );
            this.stackAnswerNotes = this.replace(JSON.stringify(r.answernotes));
        }
        this.stackScore = r.score.toString();
        this.stackTime = `Request Time: ${r.request_time.toFixed(
            2
        )} Api Time: ${r.api_time.toFixed(2)}`;

        ParCompiler.processAllMathDelayed(this.element, 1);
        const html = this.element.find(".stackOutput");
        const inputs = html.find("input");
        const inputse = html.find("textarea");
        $(inputs).on("keyup", (e) => this.inputHandler(e));
        $(inputse).on("keyup", (e) => this.inputHandler(e));
        if (getTask) {
            // remove input validation texts
            const divinput = this.element.find(".stackinputfeedback");
            divinput.remove();
        }
    }

    inputHandler(e: JQuery.TriggeredEvent) {
        const target = e.currentTarget as HTMLInputElement;
        this.lastInputFieldElement = target;
        const id: string = target.id;
        if (
            this.lastInputFieldId === id &&
            this.lastInputFieldValue === target.value
        ) {
            return;
        }
        this.lastInputFieldId = id;
        this.lastInputFieldValue = target.value;
        this.autoPeekInput(id);
    }

    async handleServerPeekResult(r: StackResult) {
        if (typeof r === "string") {
            this.error = r.toString();
            return;
        }
        if (r.error) {
            this.error = r.message;
            return;
        }
        const peekDiv = this.element.find(".peekdiv");
        const peekDivC = peekDiv.children();
        // editorDiv.empty();
        const pdiv = $(`<div><div class="math">${r.questiontext}</div></div>`);
        await ParCompiler.processAllMath(pdiv);
        peekDivC.replaceWith(pdiv); // TODO: still flashes
    }

    autoPeekInput(id: string) {
        this.stopTimer();
        this.timer = window.setTimeout(() => this.timedAutoPeek(id), 500);
    }

    async timedAutoPeek(id: string) {
        this.stopTimer();
        if (!this.markup.autopeek) {
            return;
        }
        await this.doPeek(id);
    }

    async doPeek(id: string) {
        id = id.substr(STACK_VARIABLE_PREFIX.length);
        const isub = id.indexOf("_sub_");
        if (isub > 0) {
            id = id.substr(0, isub); // f.ex in matrix case stackapi_ans1_sub_0_1
        }
        const answ = this.collectAnswer(id);
        const data = {
            answer: answ,
            prefix: STACK_VARIABLE_PREFIX,
            verifyvar: id,
        };

        await this.runValidationPeek(data);
    }

    async runPeek() {
        if (this.lastInputFieldId) {
            await this.doPeek(this.lastInputFieldId);
        }
    }

    async runValidationPeek(data: IStackData) {
        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            return;
        }
        this.isRunning = true;
        if (!this.stackPeek) {
            // remove extra fields from screen
            let divinput = this.element.find(".stackinputfeedback");
            divinput.remove();
            divinput = this.element.find(".stackprtfeedback");
            divinput.remove();
            divinput = this.element.find(".stackpartmark");
            divinput.remove();
        }
        this.stackPeek = true;
        data.seed = 1;
        this.error = "";
        const r = await this.postAnswer<{web: {stackResult: StackResult}}>({
            input: {
                nosave: true,
                type: "stack",
                usercode: "",
                stackData: {...data},
            },
        });

        this.isRunning = false;
        if (!r.ok) {
            this.error = r.result.error.error;
            return;
        }
        await this.handleServerPeekResult(r.result.web.stackResult);
    }

    async runSave() {
        await this.runSend(false);
    }

    async runGetTask() {
        this.isOpen = true;
        await this.runSend(true);
    }

    async runSend(getTask = false) {
        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            return;
        }
        this.stackPeek = false;
        this.error = "";
        this.isRunning = true;
        const stackData = this.collectData();

        const r = await this.postAnswer<{
            web: {stackResult: StackResult; error?: string};
        }>({
            input: {
                getTask: getTask,
                stackData: stackData,
                type: "stack",
                usercode: this.timWay
                    ? this.userCode
                    : JSON.stringify(stackData.answer),
            },
        });
        this.isRunning = false;
        if (!r.ok) {
            this.error = r.result.error.error;
            return;
        }
        if (!r.result.web) {
            this.error = "No web reply from csPlugin!";
            return;
        }
        if (r.result.web.error) {
            this.error = r.result.web.error;
            return;
        }
        const stackResult = r.result.web.stackResult;
        this.originalUserCode = this.userCode;
        await this.handleServerResult(stackResult, getTask);
        if (this.lastInputFieldId) {
            this.lastInputFieldElement = this.element.find(
                `#${this.lastInputFieldId}`
            )[0] as HTMLInputElement;
            if (this.lastInputFieldElement) {
                this.lastInputFieldElement.focus();
                this.lastInputFieldElement.selectionStart = 0;
                this.lastInputFieldElement.selectionEnd = 1000;
            }
        }
    }

    getDefaultMarkup() {
        return {};
    }

    getAttributeType() {
        return StackAll;
    }

    private stopTimer(): boolean {
        if (!this.timer) {
            return false;
        }
        clearTimeout(this.timer);
        this.timer = undefined;
        return true;
    }
}

@NgModule({
    declarations: [StackPluginComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        TimUtilityModule,
        PurifyModule,
        FormsModule,
        ScriptedInnerHTMLModule,
    ],
})
export class StackModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("stack-runner", StackModule, StackPluginComponent);
