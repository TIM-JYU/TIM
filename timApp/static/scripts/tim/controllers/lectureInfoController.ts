import angular from "angular";
import {IController} from "angular";
import $ from "jquery";
import moment from "moment";
import {timApp} from "tim/app";
import {fixQuestionJson} from "tim/directives/dynamicAnswerSheet";
import * as showChart from "tim/directives/showChartDirective";
import {markAsUsed} from "tim/utils";
import {$http, $log, $rootScope, $window} from "../ngimport";
import {IItem} from "../IItem";
import {IAskedQuestion, ILecture, ILectureMessage, IQuestionAnswer} from "../lecturetypes";
import {IUser} from "../IUser";
import {Users} from "../services/userService";
import {showLectureDialog} from "./createLectureCtrl";

markAsUsed(showChart);

/**
 * Created by hajoviin on 11.5.2015.
 * Handles the controls of lecture info page.
 * @module lectureInfoController
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
    private static $inject = ["$element"];
    private element: angular.IRootElementService;
    private item: IItem;
    private lecture: ILecture;
    private inLecture: boolean;
    private msg: string;
    private isLecturer: boolean;
    private answerers: IUser[];
    private showPoints: boolean;
    private points: number[];
    private answers: IQuestionAnswer[];
    private questions: IAskedQuestion[];
    private questionAnswerData: Array<{question: any, answers: any}>;
    private selectedUser: IUser;

    constructor(element: angular.IRootElementService) {
        this.item = $window.item;
        this.inLecture = $window.inLecture;
        this.lecture = {
            max_students: null,
            lecture_code: $window.lectureCode,
            doc_id: this.item.id,
            end_time: moment($window.lectureEndTime),
            start_time: moment($window.lectureStartTime),
            lecture_id: $window.lectureId,
            password: "",
        };
        this.msg = "";
        this.isLecturer = false;
        this.answerers = [];
        this.showPoints = false;
        this.points = [];
        this.element = element;

        this.getLectureInfo();
    }

    /**
     * Sends http request to get info about the specific lecture.
     * @memberof module:lectureInfoController
     */
    async getLectureInfo() {
        const response = await $http<{
            messages: ILectureMessage[],
            answers: IQuestionAnswer[],
            questions: IAskedQuestion[],
            isLecturer: boolean,
            answerers: IUser[],
        }>({
            url: "/getLectureInfo",
            method: "GET",
            params: {lecture_id: this.lecture.lecture_id},
        });
        const answer = response.data;
        /*Update the header links to correct urls*/
        this.updateHeaderLinks();
        /*Add a return link icon to lecture header if user is in lecture*/
        if (this.inLecture) {
            this.addReturnLinkToHeader();
        }

        angular.forEach(answer.messages, (msg) => {
            this.msg += msg.sender + " <" + msg.time + ">: " + msg.message + "\n";
        });
        this.answers = answer.answers;
        for (let i = 0; i < answer.questions.length; i++) {
            this.points.push(0);
            let markup = JSON.parse(answer.questions[i].json);
            if (!markup.json) {
                markup = {json: markup}; // compatibility for old
            }
            const json = markup.json;
            fixQuestionJson(json);
            answer.questions[i].json = json;
        }
        this.questions = answer.questions;
        this.isLecturer = answer.isLecturer;
        this.answerers = answer.answerers;
        this.selectedUser = Users.getCurrent();
    }

    async editPoints(askedId: number) {
        const response = await $http<{json: string, points}>({
            url: "/getAskedQuestionById",
            method: "GET",
            params: {asked_id: askedId},
        });
        const data = response.data;
        let markup = JSON.parse(data.json);
        if (!markup.json) {
            markup = {json: markup}; // compatibility for old
        }
        markup.points = data.points;
        $rootScope.$broadcast("changeQuestionTitle", {questionTitle: markup.json.questionTitle});
        $rootScope.$broadcast("editQuestion", {
            asked_id: askedId,
            markup,
        });
    }

    /*
     Updates the header links in base.html
     */
    updateHeaderLinks() {
        if (document.getElementById("headerView")) {
            document.getElementById("headerView").setAttribute("href", "/view/" + this.item.path);
        }
        if (document.getElementById("headerManage")) {
            document.getElementById("headerManage").setAttribute("href", "/manage/" + this.item.path);
        }
        if (document.getElementById("headerTeacher")) {
            document.getElementById("headerTeacher").setAttribute("href", "/teacher/" + this.item.path);
        }
        if (document.getElementById("headerAnswers")) {
            document.getElementById("headerAnswers").setAttribute("href", "/answers/" + this.item.path);
        }
        if (document.getElementById("headerLecture")) {
            document.getElementById("headerLecture").setAttribute("href", "/lecture/" + this.item.path);
        }
        if (document.getElementById("headerSlide")) {
            document.getElementById("headerSlide").setAttribute("href", "/slide/" + this.item.path);
        }
    }

    /**
     * Adds a header to lectureMenu that allows user to return to lecture
     * if user is currently on the lecture.
     */
    addReturnLinkToHeader() {
        const menu = document.getElementById("inLectureIconSection");
        const linkToLecture = document.createElement("a");
        linkToLecture.setAttribute("href", /* "https://" + location.host + */ "/lecture/" + this.item.path + "?Lecture=" + this.lecture.lecture_code);
        linkToLecture.setAttribute("title", "Return to lecture");
        const returnImg = document.createElement("img");
        returnImg.setAttribute("src", "/static/images/join-icon3.png");
        returnImg.setAttribute("class", "icon");
        returnImg.setAttribute("title", "Return to lecture");
        linkToLecture.appendChild(returnImg);
        menu.appendChild(linkToLecture);
    }

    /**
     * Sends http request to delete the lecture.
     * @memberof module:lectureInfoController
     */
    async deleteLecture() {
        const confirmAnswer = $window.confirm("Do you really want to delete this lecture?");
        if (confirmAnswer) {
            await $http({
                url: "/deleteLecture",
                method: "POST",
                params: {doc_id: this.item.id, lecture_id: this.lecture.lecture_id},
            });
            $window.history.back();
        }
    }

    async editLecture() {
        const response = await $http<{lectureId, lectureCode, lectureStartTime, lectureEndTime, password}>({
            url: "/showLectureInfoGivenName",
            method: "GET",
            params: {lecture_code: this.lecture.lecture_code, doc_id: this.item.id},
        });
        const lecture = response.data;
        const lectureNew = await showLectureDialog(this.item, {
            doc_id: this.item.id,
            end_time: moment(lecture.lectureEndTime),
            lecture_code: lecture.lectureCode,
            lecture_id: lecture.lectureId,
            max_students: null,
            password: lecture.password || "",
            start_time: moment(lecture.lectureStartTime),
        });
        this.lecture = lectureNew;
    }

    /**
     * Draws charts from the answer of the current lecture.
     * @param userToShow Which users answers to shows. If undefined shows from every user.
     * @memberof module:lectureInfoController
     */
    async drawCharts(userToShow: IUser = null) {
        for (let p = 0; p < this.points.length; p++) {
            this.points[p] = 0;
        }
        this.showPoints = true;
        let userId: number;
        if (userToShow === null) {
            userId = null;
        } else {
            userId = userToShow.id;
        }
        const questionIds = [];
        for (let i = 0; i < this.questions.length; i++) {
            await this.dynamicAnswerShowControls[i].createChart(this.questions[i].json);
            questionIds.push(this.questions[i].asked_id);
        }

        for (let j = 0; j < this.answers.length; j++) {
            if ((this.isLecturer && userId === null) || this.answers[j].user_id === userId) {
                const index = questionIds.indexOf(this.answers[j].question_id);
                this.dynamicAnswerShowControls[index]
                    .addAnswer([{answer: this.answers[j].answer}]);
                this.points[index] += this.answers[j].points;
            }
        }
    }
}

timApp.component("timLectureInfo", {
    controller: LectureInfoController,
    controllerAs: "lictrl",
    // language=HTML
    template: `
        <div class="panel panel-default">
            <div class="panel-heading">Lecture info</div>
            <div class="panel-body">
                <strong>Lecture code:</strong>

                <p ng-bind="lictrl.lecture.lecture_code"></p>

                <strong>Start time:</strong>

                <p ng-bind="lictrl.lecture.start_time | timdate"></p>

                <strong>End time:</strong>

                <p ng-bind="lictrl.lecture.end_time | timdate"></p>

                <div ng-show="lictrl.isLecturer">
                    <!--TODO: Preview questions. Needs directive from the question preview and then just use it.-->
                    <strong>Questions asked:</strong>
                    <ul>
                        <li ng-repeat="question in lictrl.questions">
                            {{ $index + 1 }}. <span ng-bind="question.asked_time | timtim"></span>
                            <a ng-click="lictrl.editPoints(question.asked_id)">{{ question.json.questionTitle }}</a>
                        </li>
                    </ul>
                </div>
            </div>
        </div>

        <div class="panel panel-default">
            <div class="panel-heading">Lecture wall</div>
            <div class="panel-body">
                <textarea class="form-control" data-ng-model="lictrl.msg" readonly id="lectureInfoWall"></textarea>
            </div>
        </div>

        <div class="panel panel-default">
            <div class="panel-heading">Find answers <span ng-show="!lictrl.isLecturer"
                                                          ng-bind="lictrl.selectedUser.name"></span>
            </div>
            <div class="panel-body">
                <label ng-show="isLecturer">User name
                    <select class="form-control" ng-model="lictrl.selectedUser"
                            ng-options="user.name for user in lictrl.answerers track by user.id">
                    </select>
                </label>
                <button class="timButton" ng-click="lictrl.drawCharts(lictrl.selectedUser)">Show answers</button>
                <button class="timButton" ng-show="lictrl.isLecturer" ng-click="lictrl.drawCharts()">Show all</button>
                <a class="timButton"
                   href="/getLectureAnswerTotals/{{ lictrl.lectureId }}?sum_field_name=sum&count_field_name=cnt">
                    Download as plain text</a>
                <div>
                    <div ng-repeat="question in lictrl.questions" style="margin-bottom: 2em">
                        <div ng-show="lictrl.showPoints">
                            <p>{{ $index + 1 }}. <span class="bold" ng-bind-html="question.json.questionTitle"></span>
                                /
                                <span ng-bind="question.asked_time | timtim"></span>
                            </p>
                            <p ng-bind-html="question.json.questionText"
                               ng-click="lictrl.dynamicAnswerShowControls[$index].toggle()"></p>
                        </div>
                        <show-chart-directive data="lictrl.questionAnswerData">
                        </show-chart-directive>
                        <p ng-show="lictrl.showPoints">Points: <span ng-bind="lictrl.points[$index]"> </span></p>
                    </div>
                </div>

                <p ng-show="lictrl.answers.length">
                    No answers from this lecture
                </p>
            </div>
        </div>

        <div ng-show="lictrl.isLecturer" class="panel panel-default">
            <div class="panel-heading">
                Actions
            </div>
            <div class="panel-body">
                <button class="timButton" ng-click="lictrl.editLecture(lectureName)">Edit lecture</button>
                <button class="btn btn-danger" ng-click="lictrl.deleteLecture()">Delete lecture</button>
            </div>
        </div>
    `,
});
