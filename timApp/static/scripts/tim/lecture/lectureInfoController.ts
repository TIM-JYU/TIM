import {IController} from "angular";
import {timApp} from "tim/app";
import * as showChart from "tim/lecture/showChartDirective";
import {markAsUsed, to} from "tim/util/utils";
import {showQuestionEditDialog} from "../document/question/question-edit-dialog.component";
import {IUser} from "../user/IUser";
import {isAdmin, Users} from "../user/userService";
import {lectureinfoglobals} from "../util/globals";
import {$http} from "../util/ngimport";
import {showLectureDialog} from "./lecture-dialog.component";
import {
    IAskedQuestion,
    ILecture,
    ILectureMessage,
    IQuestionAnswerPlain,
} from "./lecturetypes";

markAsUsed(showChart);

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

export class LectureInfoController implements IController {
    static $inject = ["$element"];
    private element: JQLite;
    private lecture: ILecture;
    private inLecture: boolean;
    private isLecturer: boolean;
    private answerers: IUser[];
    private answers: IQuestionAnswerPlain[] = [];
    private questions: IAskedQuestion[] = [];
    private selectedUser: IUser | undefined;
    private showAll = false;
    private answerMap: {[index: number]: IQuestionAnswerPlain[]} = {};
    private messages: ILectureMessage[] = [];

    constructor(element: JQLite) {
        const g = lectureinfoglobals();
        this.inLecture = g.inLecture;
        this.lecture = g.lecture;
        this.isLecturer = false;
        this.answerers = [];
        this.element = element;
    }

    public $onInit() {
        this.getLectureInfo();
    }

    /**
     * Sends http request to get info about the specific lecture.
     */
    private async getLectureInfo() {
        const response = await to(
            $http<{
                messages: ILectureMessage[];
                answers: IQuestionAnswerPlain[];
                questions: IAskedQuestion[];
                isLecturer: boolean;
                answerers: IUser[];
            }>({
                url: "/getLectureInfo",
                method: "GET",
                params: {lecture_id: this.lecture.lecture_id},
            })
        );
        if (!response.ok) {
            return;
        }
        const data = response.result.data;
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

    private updateAnswerMap() {
        this.answerMap = {};
        this.questions.forEach(
            (q) => (this.answerMap[q.asked_id] = this.getAnswers(q))
        );
    }

    private async editPoints(askedId: number) {
        const response = await to(
            $http<IAskedQuestion>({
                url: "/getAskedQuestionById",
                method: "GET",
                params: {asked_id: askedId},
            })
        );
        if (!response.ok) {
            return;
        }
        await showQuestionEditDialog(response.result.data);
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
    private async deleteLecture() {
        const confirmAnswer = window.confirm(
            "Do you really want to delete this lecture?"
        );
        if (confirmAnswer) {
            const response = await to(
                $http({
                    url: "/deleteLecture",
                    method: "POST",
                    params: {lecture_id: this.lecture.lecture_id},
                })
            );
            if (!response.ok) {
                return;
            }
            window.history.back();
        }
    }

    private async editLecture() {
        const response = await to(
            $http<ILecture>({
                url: "/showLectureInfoGivenName",
                method: "GET",
                params: {lecture_id: this.lecture.lecture_id},
            })
        );
        if (!response.ok) {
            return;
        }
        const lecture = response.result.data;
        this.lecture = await showLectureDialog(lecture);
    }
}

timApp.component("timLectureInfo", {
    controller: LectureInfoController,
    template: `
<div class="panel panel-default">
    <div class="panel-heading">Lecture info</div>
    <div class="panel-body">
        <strong>Lecture code:</strong>

        <p ng-bind="$ctrl.lecture.lecture_code"></p>

        <strong>Start time:</strong>

        <p ng-bind="$ctrl.lecture.start_time | timdate"></p>

        <strong>End time:</strong>

        <p ng-bind="$ctrl.lecture.end_time | timdate"></p>

        <div ng-show="$ctrl.canSeeAllData()">
            <strong>Questions asked:</strong>
            <ul>
                <li ng-repeat="question in $ctrl.questions">
                    {{ $index + 1 }}. <span ng-bind="question.asked_time | timtim"></span>
                    <a ng-click="$ctrl.editPoints(question.asked_id)">{{ question.json.json.questionTitle }}</a>
                </li>
            </ul>
        </div>
    </div>
</div>

<div class="panel panel-default">
    <div class="panel-heading">Lecture wall</div>
    <div class="panel-body">
        <tim-lecture-wall-content
                messages="$ctrl.messages"
                show-name="true"
                show-time="true">
        </tim-lecture-wall-content>
    </div>
</div>

<div class="panel panel-default">
    <div class="panel-heading">Find answers
    </div>
    <div class="panel-body">
        <label ng-show="$ctrl.canSeeAllData()">User name
            <select
                    ng-disabled="$ctrl.showAll"
                    class="form-control"
                    ng-model="$ctrl.selectedUser"
                    ng-options="user.name for user in $ctrl.answerers track by user.id"
                    ng-change="$ctrl.updateAnswerMap()">
            </select>
        </label>
        <div class="checkbox" ng-show="$ctrl.canSeeAllData()">
            <label>
                <input
                        type="checkbox"
                        ng-model="$ctrl.showAll"
                        ng-change="$ctrl.updateAnswerMap()">Show all users
            </label>
        </div>
        <a class="timButton"
           href="/getLectureAnswerTotals/{{ $ctrl.lecture.lecture_id }}?sum_field_name=sum&count_field_name=cnt">
            Download as plain text</a>
        <div ng-repeat="question in $ctrl.questions">
            <p>{{ $index + 1 }}.
                <strong ng-bind-html="question.json.json.questionTitle"></strong>
                /
                <span ng-bind="question.asked_time | timtim"></span>
            </p>
            <div tim-draggable-fixed
                 caption="{{question.json.json.questionText}}"
                 resize="true"
                 detachable="false"
                 draggable="false"
                 class="flex cl"
                 style="width: 400px; height: 300px; position: relative">
                <show-chart-directive
                        question="question"
                        answers="$ctrl.answerMap[question.asked_id]">
                </show-chart-directive>
            </div>
        </div>

        <p ng-show="$ctrl.answers.length === 0">
            No answers from this lecture
        </p>
    </div>
</div>

<div ng-show="$ctrl.canSeeAllData()" class="panel panel-default">
    <div class="panel-heading">
        Actions
    </div>
    <div class="panel-body">
        <button class="timButton" ng-click="$ctrl.editLecture()">Edit lecture</button>
        <button class="btn btn-danger" ng-click="$ctrl.deleteLecture()">Delete lecture</button>
    </div>
</div>
    `,
});
