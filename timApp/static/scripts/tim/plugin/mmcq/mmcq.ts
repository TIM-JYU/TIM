import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, Directive, ElementRef, Input, NgModule} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {toPromise} from "tim/util/utils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {AnswerSheetModule} from "tim/document/question/answer-sheet.component";
import {PurifyModule} from "tim/util/purify.module";
import {handleAnswerResponse} from "tim/document/interceptor";
import type {IAnswerSaveEvent} from "tim/answer/answer-browser.component";
import {TaskId} from "tim/plugin/taskid";
import type {PluginJson} from "tim/plugin/angular-plugin-base.directive";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";

interface MMCQContent<State> {
    state?: State;
    question: {
        headerText: string | null;
        buttonText: string | null;
        falseText: string | null;
        trueText: string | null;
        wrongText: string | null;
        correctText: string | null;
        stem: string | null;
        choices: Array<{
            text: string;
            reason?: string;
            correct?: boolean;
        }>;
    };
}

@Directive()
export class MCQBase<State> implements PluginJson {
    public headerText: string = "Check your understanding";
    public buttonText: string = "Submit";
    public content!: MMCQContent<State>;
    protected element: JQuery<HTMLElement>;
    @Input() json!: string;

    constructor(
        protected hostElement: ElementRef<HTMLElement>,
        protected http: HttpClient
    ) {
        this.element = $(hostElement.nativeElement);
    }

    ngOnInit() {
        if (!this.json) {
            this.content = JSON.parse(
                this.element.attr("data-content")!
            ) as MMCQContent<State>;
        } else {
            this.content = JSON.parse(this.json) as MMCQContent<State>;
        }
    }

    protected getId() {
        return this.element.parent().attr("id");
    }

    protected getTaskId() {
        const r = TaskId.tryParse(this.getId() ?? "");
        if (r.ok) {
            return r.result;
        } else {
            throw new Error("Invalid task id");
        }
    }
}

@Component({
    selector: "tim-mmcq",
    template: `
<div class="mcq">
    <p class="header" style="font-weight:bold" [innerHtml]="headerText | purify"></p>
    <p class="stem" [innerHtml]="content.question.stem | purify"></p>
    <form>
        <table>
            <tbody>
                <tr>
                    <th></th>
                    <th [innerHtml]="trueText | purify"></th>
                    <th [innerHtml]="falseText | purify"></th>
                    <th *ngIf="checked"></th>
                    <th *ngIf="checked"></th>
                </tr>
                <tr *ngFor="let choice of content.question.choices; let i = index">
                    <td><span class="MCQItem" [innerHtml]="choice.text | purify"></span></td>
                    <td class="text-center">
                        <input [name]="'answerCb'+i" type="checkbox" [(ngModel)]="answer[i]"/>
                    </td>
                    <td class="text-center">
                        <input [name]="'answerCbOther'+i" type="checkbox" [ngModel]="answer[i] === undefined ? undefined : !answer[i]" (ngModelChange)="answer[i] = !$event"/>
                    </td>
                    <td *ngIf="checked">
                        <span [innerHtml]="correctText | purify"
                              *ngIf="!(answer[i] == null) && !(choice.correct == null) && (''+answer[i] == ''+choice.correct)"
                              class="correct"></span>
                        <span [innerHtml]="wrongText | purify"
                              *ngIf="!(answer[i] == null) && !(choice.correct == null) && (''+answer[i] !==''+choice.correct)"
                              class="wrong"></span>
                    </td>
                    <td *ngIf="choice.reason">
                        <span class="MCQExpl" [innerHtml]="choice.reason | purify"></span></td>
                </tr>
            </tbody>
        </table>
    </form>
    <div class="text-center">
        <button (click)="submit()" [innerHtml]="buttonText | purify"></button>
    </div>
</div>
`,
})
export class MMCQ extends MCQBase<null | boolean[]> {
    private active: boolean = false;
    checked: boolean = true;
    trueText: string = "True";
    falseText: string = "False";
    correctText: string = "Correct!";
    wrongText: string = "Wrong!";
    answer!: Array<boolean | "false" | "true">;

    ngOnInit() {
        super.ngOnInit();
        const fields = [
            "headerText",
            "buttonText",
            "falseText",
            "trueText",
            "correctText",
            "wrongText",
        ] as const;
        fields.forEach((opt) => {
            const o = this.content.question[opt];
            if (o !== null) {
                this[opt] = o;
            }
        });
        const s = this.content.state;
        if (s == null) {
            this.checked = false;
            this.active = true;
            this.answer = new Array(this.content.question.choices.length);
        } else {
            this.answer = s;
        }
    }

    extract() {
        for (let j = 0; j < this.answer.length; j++) {
            if (this.answer[j] == "false") {
                this.answer[j] = false;
            }
            if (this.answer[j] == "true") {
                this.answer[j] = true;
            }
        }
        this.active = false;
        return this.answer;
    }

    async submit() {
        const message = {
            input: this.extract(),
        };
        const ident = this.getId();
        if (!ident) {
            return;
        }

        const r = await toPromise(
            this.http.put<{web: MMCQ["content"]} & IAnswerSaveEvent>(
                `/mmcq/${ident}/answer`,
                message
            )
        );
        if (r.ok) {
            this.content = r.result.web;
            this.checked = true;
            handleAnswerResponse(ident, {
                savedNew: r.result.savedNew,
                errors: r.result.errors,
                feedback: r.result.feedback,
                topfeedback: r.result.topfeedback,
                valid: r.result.valid,
                refresh: r.result.refresh,
                refreshPoints: r.result.refreshPoints,
            });
        } else {
            handleAnswerResponse(ident, {
                savedNew: false,
                valid: false,
                errors: [r.result.error.error],
            });
            await showMessageDialog(r.result.error.error);
        }
    }
}

@Component({
    selector: "tim-mcq",
    template: `
<div class="mcq">
    <p class="header" style="font-weight:bold" [innerHtml]="headerText | purify"></p>
    <p class="stem" [innerHtml]="content.question.stem | purify"></p>
    <form>
        <table>
            <tbody>
                <tr *ngFor="let choice of content.question.choices; let i = index">
                    <td>
                        <input name="answerRB" type="radio" [(ngModel)]="userSelection" [value]="i"/>
                        <span *ngIf="content.state==i&&choice.correct">✓</span>
                        <span *ngIf="content.state==i&&!choice.correct">✗</span>
                    </td>
                    <td><span class="MCQItem" [innerHtml]="choice.text | purify"></span>
                        <span *ngIf="choice.reason" class="MCQExpl" [innerHtml]="choice.reason | purify"></span></td>
                </tr>
            </tbody>
        </table>
    </form>
    <div class="text-center">
        <button (click)="submit()" [innerHtml]="buttonText | purify"></button>
    </div>
</div>
`,
})
export class MCQ extends MCQBase<number | null> {
    userSelection: number | undefined;

    ngOnInit() {
        super.ngOnInit();
        this.userSelection = this.content.state ?? undefined;
        const fields = ["headerText", "buttonText"] as const;
        fields.forEach((opt) => {
            const o = this.content.question[opt];
            if (o !== null) {
                this[opt] = o;
            }
        });
    }

    async submit() {
        const message = {
            input: this.userSelection,
        };
        const ident = this.getId();
        if (!ident) {
            return;
        }

        const r = await toPromise(
            this.http.put<{web: MCQ["content"]} & IAnswerSaveEvent>(
                `/mcq/${ident}/answer`,
                message
            )
        );
        if (r.ok) {
            this.content = r.result.web;
            handleAnswerResponse(ident, {
                savedNew: r.result.savedNew,
                errors: r.result.errors,
                feedback: r.result.feedback,
                topfeedback: r.result.topfeedback,
                valid: r.result.valid,
                refresh: r.result.refresh,
                refreshPoints: r.result.refreshPoints,
            });
        } else {
            handleAnswerResponse(ident, {
                savedNew: false,
                valid: false,
                errors: [r.result.error.error],
            });
            await showMessageDialog(r.result.error.error);
        }
    }
}

@NgModule({
    declarations: [MCQ, MMCQ],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        AnswerSheetModule,
        PurifyModule,
    ],
})
export class MCQModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("mcq", MCQModule, MCQ);
registerPlugin("mmcq", MCQModule, MMCQ);
