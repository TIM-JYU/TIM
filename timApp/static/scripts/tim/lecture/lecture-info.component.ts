import {
    HTTP_INTERCEPTORS,
    HttpClient,
    HttpClientModule,
} from "@angular/common/http";
import {
    ApplicationRef,
    Component,
    DoBootstrap,
    NgModule,
    NgZone,
} from "@angular/core";
import moment from "moment";
import {to2} from "tim/util/utils";
import {BrowserModule} from "@angular/platform-browser";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {AnswerChartModule} from "tim/lecture/answer-chart.component";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {FormsModule} from "@angular/forms";
import {AngularDraggableModule} from "angular2-draggable";
import {IUser} from "../user/IUser";
import {isAdmin, Users} from "../user/userService";
import {lectureinfoglobals} from "../util/globals";
import {createDowngradedModule, doDowngrade} from "../downgrade";
import {TimeStampToMomentConverter} from "../util/time-stamp-to-moment-converter.service";
import {showQuestionEditDialog} from "../document/question/showQuestionEditDialog";
import {showLectureDialog} from "./showLectureDialogs";
import {
    IAskedQuestion,
    ILecture,
    ILectureMessage,
    IQuestionAnswerPlain,
} from "./lecturetypes";
import {LectureWallContentModule} from "./lecture-wall-content.component";

/**
 * Created by hajoviin on 11.5.2015.
 * Handles the controls of lecture info page.
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @author Veli-Pekka Oksanen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

@Component({
    selector: "tim-lecture-info",
    template: `
        <div class="panel panel-default">
            <div class="panel-heading">Lecture info</div>
            <div class="panel-body">
                <strong>Lecture code:</strong>
                <p [innerText]="lecture.lecture_code"></p>
                <strong>Start time:</strong>
                <p [innerText]="lecture.start_time | timdate"></p>
                <strong>End time:</strong>
                <p [innerText]="lecture.end_time | timdate"></p>
                <div *ngIf="canSeeAllData()">
                    <strong>Questions asked:</strong>
                    <ul>
                        <li *ngFor="let question of questions; let index = index">
                            {{ index + 1 }}. <span [innerText]="question.asked_time | timdate"></span>&ngsp;
                            <a (click)="editPoints(question.asked_id)">{{ question.json.json.questionTitle }}</a>
                        </li>
                    </ul>
                </div>
            </div>
        </div>

        <div class="panel panel-default">
            <div class="panel-heading">Lecture wall</div>
            <div class="panel-body">
                <tim-lecture-wall-content
                        [messages]="messages">
                </tim-lecture-wall-content>
            </div>
        </div>

        <div class="panel panel-default">
            <div class="panel-heading">Find answers
            </div>
            <div class="panel-body">
                <label *ngIf="canSeeAllData()">User name
                    <select
                            [disabled]="showAll"
                            class="form-control"
                            [(ngModel)]="selectedUser"
                            (ngModelChange)="updateAnswerMap()">
                        <option *ngFor="let u of answerers" [ngValue]="u">{{u.name}}</option>
                    </select>
                </label>
                <div class="checkbox" *ngIf="canSeeAllData()">
                    <label>
                        <input
                                type="checkbox"
                                [(ngModel)]="showAll"
                                (ngModelChange)="updateAnswerMap()">Show all users
                    </label>
                </div>
                <a class="timButton"
                   href="/getLectureAnswerTotals/{{ lecture.lecture_id }}?sum_field_name=sum&count_field_name=cnt">
                    Download as plain text</a>
                <div *ngFor="let question of questions; let index = index">
                    <p>{{ index + 1 }}.
                        <strong [innerHtml]="question.json.json.questionTitle"></strong>
                        /
                        <span [innerText]="question.asked_time | timdate"></span>
                    </p>
                    <div class="flex cl" [ngResizable]="true"
                         style="width: 400px; height: 300px; position: relative">
                        <span [innerHTML]="question.json.json.questionText"></span>
                        <tim-answer-chart
                                [question]="question"
                                [answers]="answerMap[question.asked_id]">
                        </tim-answer-chart>
                    </div>
                </div>

                <p *ngIf="answers.length === 0">
                    No answers from this lecture
                </p>
            </div>
        </div>

        <div *ngIf="canSeeAllData()" class="panel panel-default">
            <div class="panel-heading">
                Actions
            </div>
            <div class="panel-body">
                <button class="timButton" (click)="editLecture()">Edit lecture</button>&ngsp;
                <button class="btn btn-danger" (click)="deleteLecture()">Delete lecture</button>
            </div>
        </div>
    `,
    styleUrls: [
        "../../../../node_modules/angular2-draggable/css/resizable.min.css",
    ],
})
export class LectureInfoComponent {
    lecture: ILecture;
    private inLecture: boolean;
    private isLecturer: boolean;
    answerers: IUser[];
    answers: IQuestionAnswerPlain[] = [];
    questions: IAskedQuestion[] = [];
    selectedUser: IUser | undefined;
    showAll = false;
    answerMap: Record<number, IQuestionAnswerPlain[]> = {};
    messages: ILectureMessage[] = [];

    constructor(private http: HttpClient, private zone: NgZone) {
        const g = lectureinfoglobals();
        this.inLecture = g.inLecture;
        this.lecture = {
            doc_id: g.lecture.doc_id,
            end_time: moment(g.lecture.end_time),
            is_access_code: g.lecture.is_access_code,
            is_full: g.lecture.is_full,
            lecture_code: g.lecture.lecture_code,
            lecture_id: g.lecture.lecture_id,
            options: g.lecture.options,
            password: g.lecture.password,
            start_time: moment(g.lecture.start_time),
        };
        this.isLecturer = false;
        this.answerers = [];
    }

    ngOnInit() {
        this.getLectureInfo();
    }

    /**
     * Sends http request to get info about the specific lecture.
     */
    private async getLectureInfo() {
        const response = await to2(
            this.http
                .get<{
                    messages: ILectureMessage[];
                    answers: IQuestionAnswerPlain[];
                    questions: IAskedQuestion[];
                    isLecturer: boolean;
                    answerers: IUser[];
                }>("/getLectureInfo", {
                    params: {lecture_id: this.lecture.lecture_id.toString()},
                })
                .toPromise()
        );
        if (!response.ok) {
            return;
        }
        const data = response.result;
        this.messages = data.messages;
        this.answers = data.answers;
        this.questions = data.questions;
        this.isLecturer = data.isLecturer;
        this.answerers = data.answerers;
        this.selectedUser = Users.getCurrent();
        this.updateAnswerMap();
    }

    canSeeAllData() {
        return this.isLecturer || isAdmin();
    }

    updateAnswerMap() {
        this.answerMap = {};
        this.questions.forEach(
            (q) => (this.answerMap[q.asked_id] = this.getAnswers(q))
        );
        this.checkChanges();
    }

    private checkChanges() {
        // Not clear why this is needed.
        this.zone.run(() => {});
    }

    async editPoints(askedId: number) {
        const response = await to2(
            this.http
                .get<IAskedQuestion>("/getAskedQuestionById", {
                    params: {asked_id: askedId.toString()},
                })
                .toPromise()
        );
        if (!response.ok) {
            return;
        }
        await showQuestionEditDialog(response.result);
    }

    private getAnswers(question: IAskedQuestion) {
        return this.answers.filter(
            (q) =>
                q.asked_id === question.asked_id &&
                (this.showAll ||
                    !this.selectedUser ||
                    this.selectedUser.id === q.user_id)
        );
    }

    /**
     * Sends http request to delete the lecture.
     */
    async deleteLecture() {
        const confirmAnswer = window.confirm(
            "Do you really want to delete this lecture?"
        );
        if (confirmAnswer) {
            const response = await to2(
                this.http
                    .post("/deleteLecture", {
                        lecture_id: this.lecture.lecture_id,
                    })
                    .toPromise()
            );
            if (!response.ok) {
                return;
            }
            window.history.back();
        }
    }

    async editLecture() {
        const response = await to2(
            this.http
                .get<ILecture>("/showLectureInfoGivenName", {
                    params: {lecture_id: this.lecture.lecture_id.toString()},
                })
                .toPromise()
        );
        if (!response.ok) {
            return;
        }
        const lecture = response.result;
        this.lecture = await showLectureDialog(lecture);
    }
}

@NgModule({
    declarations: [LectureInfoComponent],
    imports: [
        BrowserModule,
        FormsModule,
        HttpClientModule,
        TimUtilityModule,
        AnswerChartModule,
        LectureWallContentModule,
        AngularDraggableModule,
    ],
    providers: [
        {
            provide: HTTP_INTERCEPTORS,
            useClass: TimeStampToMomentConverter,
            multi: true,
        },
    ],
})
export class LectureInfoModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                LectureInfoModule
            )
        ),
        "timLectureInfo",
        LectureInfoComponent
    ),
];
