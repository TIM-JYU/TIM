import {IController, IRootElementService} from "angular";
import {timApp} from "tim/app";
import {fixQuestionJson} from "tim/directives/dynamicAnswerSheet";
import * as showChart from "tim/directives/showChartDirective";
import {markAsUsed} from "tim/utils";
import {IItem} from "../IItem";
import {IUser} from "../IUser";
import {IAskedQuestion, ILecture, ILectureMessage, IQuestionAnswer} from "../lecturetypes";
import {$http, $window} from "../ngimport";
import {Users} from "../services/userService";
import {showLectureDialog} from "./createLectureCtrl";
import {showQuestionEditDialog} from "./questionController";

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
    private element: IRootElementService;
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

    constructor(element: IRootElementService) {
        this.item = $window.item;
        this.inLecture = $window.inLecture;
        this.lecture = $window.lecture;
        this.msg = "";
        this.isLecturer = false;
        this.answerers = [];
        this.showPoints = false;
        this.points = [];
        this.element = element;
    }

    public $onInit() {
        this.getLectureInfo();
    }

    /**
     * Sends http request to get info about the specific lecture.
     */
    private async getLectureInfo() {
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
        const data = response.data;
        /*Update the header links to correct urls*/
        this.updateHeaderLinks();
        /*Add a return link icon to lecture header if user is in lecture*/
        if (this.inLecture) {
            this.addReturnLinkToHeader();
        }

        data.messages.forEach((msg) => {
            this.msg += msg.user.name + " <" + msg.timestamp + ">: " + msg.message + "\n";
        });
        this.answers = data.answers;
        this.questions = data.questions;
        this.isLecturer = data.isLecturer;
        this.answerers = data.answerers;
        this.selectedUser = Users.getCurrent();
    }

    private async editPoints(askedId: number) {
        const response = await $http<IAskedQuestion>({
            url: "/getAskedQuestionById",
            method: "GET",
            params: {asked_id: askedId},
        });
        await showQuestionEditDialog(response.data);
    }

    /*
     Updates the header links in base.html
     */
    private updateHeaderLinks() {
        const viewNames = ["View", "Manage", "Teacher", "Answers", "Lecture", "Slide"];
        for (const name of viewNames) {
            const elem = document.getElementById(`header${name}`);
            if (elem) {
                elem.setAttribute("href", `/${name.toLowerCase()}/${this.item.path}`);
            }
        }
    }

    /**
     * Adds a header to lectureMenu that allows user to return to lecture
     * if user is currently on the lecture.
     */
    private addReturnLinkToHeader() {
        const menu = document.getElementById("inLectureIconSection");
        if (!menu) {
            return;
        }
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
     */
    private async deleteLecture() {
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

    private async editLecture() {
        const response = await $http<ILecture>({
            url: "/showLectureInfoGivenName",
            method: "GET",
            params: {lecture_id: this.lecture.lecture_id},
        });
        const lecture = response.data;
        this.lecture = await showLectureDialog(this.item, lecture);
    }

    /**
     * Draws charts from the answer of the current lecture.
     * @param userToShow Which users answers to shows. If undefined shows from every user.
     */
    private async drawCharts(userToShow?: IUser) {
        for (let p = 0; p < this.points.length; p++) {
            this.points[p] = 0;
        }
        this.showPoints = true;
        // TODO
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
                <button class="timButton" ng-click="lictrl.editLecture()">Edit lecture</button>
                <button class="btn btn-danger" ng-click="lictrl.deleteLecture()">Delete lecture</button>
            </div>
        </div>
    `,
});
