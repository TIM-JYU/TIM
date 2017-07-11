import angular from "angular";
import {IController} from "angular";
import {timApp} from "tim/app";
import {$http, $window} from "../ngimport";
import {ILecture} from "../lecturetypes";
import {LectureController} from "./lectureController";
import {ViewCtrl} from "./view/viewctrl";

/**
 * FILL WITH SUITABLE TEXT
 * @module smallMenuCtrl
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

export class SmallMenuCtrl implements IController {
    private currentLecturesList: ILecture[];
    private futureLecturesList: ILecture[];
    private showCurrentList: boolean;
    private showFutureList: boolean;
    private readonly lctrl: LectureController;
    private readonly viewctrl: ViewCtrl;

    constructor() {
        this.currentLecturesList = [];
        this.futureLecturesList = [];
        this.showCurrentList = false;
        this.showFutureList = false;
    }

    $postLink() {
        const w = angular.element($window);
        w.bind("resize", () => {
            // TODO might need $evalAsync
            this.showCurrentList = false;
            this.showFutureList = false;
        });
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:smallMenuCtrl
     */
    ready() {
        this.showCurrentList = false;
        this.showFutureList = false;
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:smallMenuCtrl
     */
    async openCurrentLectureMenu() {
        this.showCurrentList = true;
        this.showFutureList = false;
        await this.getLectures();
    }

    private async getLectures() {
        const response = await $http<{currentLectures, futureLectures}>({
            url: "/getAllLecturesFromDocument",
            method: "GET",
            params: {doc_id: this.viewctrl.docId},
        });
        this.currentLecturesList = response.data.currentLectures;
        this.futureLecturesList = response.data.futureLectures;
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:smallMenuCtrl
     */
    async openFutureLectureMenu() {
        this.showCurrentList = false;
        this.showFutureList = true;
        await this.getLectures();
    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:smallMenuCtrl
     */
    selectCurrentLecture() {

    }
}

timApp.component("timSmallMenu", {
    controller: SmallMenuCtrl,
    require: {
        lctrl: "^timLecture",
        viewctrl: "^timView",
    },
    template: `<div class="smallMenu" ng-show="!$ctrl.lctrl.lectureSettings.inLecture">
    <div class="current">
        <h1 ng-click="$ctrl.openCurrentLectureMenu()">Show current
            <br>lectures</h1>
    </div>
    <div class="future">
        <h1 ng-click="$ctrl.openFutureLectureMenu()">Show future
            <br>lectures</h1>
    </div>
</div>
<div class="smallMenu" ng-show="$ctrl.lctrl.lectureSettings.inLecture">
    <div class="currently">
        <h2 ng-bind="$ctrl.lctrl.lectureName"></h2>

        <div class="smallIconSection">
            <button class="timButton btn-lg"
                    ng-click="$ctrl.lctrl.leaveLecture()"
                    title="Leave this lecture"
                    ng-show="$ctrl.lctrl.lectureSettings.inLecture">
                <span class="glyphicon glyphicon-log-out"
                      aria-hidden="true"></span>
            </button>
            <button class="timButton btn-lg"
                    ng-click="$ctrl.lctrl.endLecture()"
                    title="End this lecture"
                    ng-show="$ctrl.lctrl.canStop">
                <span class="glyphicon glyphicon-stop"
                      aria-hidden="true"></span>
            </button>
            <button class="timButton btn-lg"
                    ng-click="$ctrl.lctrl.editLecture(lectureName)"
                    title="Edit this lecture"
                    ng-show="$ctrl.lctrl.canStop">
                <span class="glyphicon glyphicon-edit"
                      aria-hidden="true"></span>
            </button>
        </div>
    </div>
</div>
<div class="lecturesList">
    <div ng-show="$ctrl.showCurrentList">
        <ul>
            <li ng-show="$ctrl.currentLecturesList.length > 0" ng-repeat="lecture in $ctrl.currentLecturesList">
                <p ng-click="$ctrl.selectCurrentLecture()">{{ lecture.lecture_code }}
                    <button class="timButton" value="Join"
                            ng-click="$ctrl.lctrl.joinLecture(lecture.lecture_code,lecture.is_access_code)">Join
                    </button>
                </p>
            </li>
            <li ng-show="$ctrl.currentLecturesList.length == 0">
                <p>No lectures</p>
            </li>
            <li ng-show="$ctrl.lctrl.isLecturer"><p ng-click="$ctrl.lctrl.toggleLecture()" id="addLecture1">
            Add new lecture </p></li>
        </ul>
    </div>
    <div ng-show="$ctrl.showFutureList">
        <ul>
            <li ng-repeat="lecture in $ctrl.futureLecturesList">
                <p> {{ lecture.lecture_code }}
                    <button class="timButton" ng-show="$ctrl.lctrl.isLecturer" value="Start">Start</button>
                </p>
            </li>
            <li ng-show="$ctrl.futureLecturesList.length == 0">
                <p>No lectures</p>
            </li>
            <li ng-show="isLecturer">
            <p ng-click="$ctrl.lctrl.toggleLecture()" id="addLecture2">
            Add new lecture</p>
            </li>
        </ul>
    </div>
</div>
`,
});
