/**
 * Created by hajoviin on 24.2.2015.
 * Contoller that handles lectures.
 * @module lectureController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

import angular, {IScope} from "angular";
import {IController} from "angular";
import $ from "jquery";
import moment from "moment";
import {timApp} from "tim/app";
import sessionsettings from "tim/session";
import {GetURLParameter, markAsUsed, setsetting} from "tim/utils";
import {showMessageDialog} from "../dialog";
import {$http, $log, $rootScope, $timeout, $window} from "../ngimport";
import {ViewCtrl} from "./view/viewctrl";
import {showLectureDialog} from "./createLectureCtrl";
import * as wall from "../components/lectureWall";
import {
    hasLectureEnded,
    ILecture, ILectureListResponse, ILecturePerson, ILectureResponse, ILectureSettings,
    IMessage, isLectureListResponse,
} from "../lecturetypes";
import {Users} from "../services/userService";
import {showLectureEnding} from "../components/lectureEnding";

markAsUsed(wall);

enum LectureEndingDialogState {
    NotAnswered,
    Open,
    Answered,
}

// TODO: Painike, josta voisi hakea kysymyksiä.
// TODO: Button, to get questions and wall.
export class LectureController implements IController {
    private static $inject = ["$scope"];
    public isLecturer: boolean;
    public lecture: ILecture | null;
    public lectures: ILecture[];
    public lectureSettings: ILectureSettings;
    private lectureEndingDialogState: LectureEndingDialogState;
    private canStart: boolean;
    private canStop: boolean;
    private chosenLecture: ILecture | null;
    private clockOffset: number;
    private currentPointsId: number | null;
    private currentQuestionId: number | null;
    private futureLecture: ILecture | null;
    private futureLectures: ILecture[];
    private gettingAnswers: boolean;
    private lastID: number;
    private lectureAnswer: ILectureResponse;
    private lectureEnded: boolean;
    private lecturerTable: ILecturePerson[];
    private newMessagesAmount: number;
    private newMessagesAmountText: string;
    private passwordQuess: string | null;
    private polling: boolean;
    private pollingLectures: number[];
    private requestOnTheWay: boolean;
    private scope: IScope;
    private settings: any;
    private showPoll: boolean;
    private studentTable: ILecturePerson[];
    private timeout: any;
    private viewctrl: ViewCtrl | undefined;
    private wallMessages: IMessage[];
    private wallName: string;
    private pollTimeout: number;
    private pollingStopped: boolean;

    constructor(scope: IScope) {
        this.scope = scope;
        this.wallName = "Wall";
        this.newMessagesAmount = 0;
        this.newMessagesAmountText = "";
        this.showPoll = true;
        this.polling = true;
        this.requestOnTheWay = false;
        this.canStart = true;
        this.canStop = false;
        this.lectures = [];
        this.futureLectures = [];
        this.futureLecture = null;
        this.chosenLecture = null;
        this.passwordQuess = "";
        this.pollingLectures = [];
        this.isLecturer = false;
        this.studentTable = [];
        this.lecturerTable = [];
        this.gettingAnswers = false;
        this.lectureEndingDialogState = LectureEndingDialogState.NotAnswered;
        this.lectureEnded = false;
        this.wallMessages = [];
        this.clockOffset = 0;
        this.currentQuestionId = null;
        this.currentPointsId = null;
        this.settings = sessionsettings;
        this.lecture = null;

        this.lectureSettings = {
            inLecture: false,
            lectureMode: $window.lectureMode || false,
            useAnswers: true,
            useQuestions: true,
            useWall: true,
        };
    }

    $onInit() {
        this.checkIfInLecture();
        this.getClockOffset();
    }

    $postLink() {
        $(document).on("visibilitychange", () => {
            if (document.visibilityState === "hidden") {
                $log.info("hidden");
                this.timeout = $window.setTimeout(() => {
                    this.lostFocus();
                }, 1000);
            } else {
                this.gotFocus();
                $log.info("visible");
            }
        });

        angular.element($window).on("blur", () => {
            angular.element($window).on("focus.gotfocus", () => {
                this.gotFocus();
            });
        });

        this.focusOn();
    }

    showRightView(answer) {
        if (answer.isInLecture) {
            this.showLectureView(answer);
        } else {
            this.showBasicView(answer);
        }
    }

    getDocIdOrNull(): number | null {
        if (this.viewctrl) {
            return this.viewctrl.docId;
        }
        return null;
    }

    /**
     * Makes http request to check if the current user is in lecture.
     * @memberof module:lectureController
     */
    async checkIfInLecture() {
        const response = await $http<ILectureResponse | ILectureListResponse>({
            url: "/checkLecture",
            method: "GET",
            params: {doc_id: this.getDocIdOrNull(), buster: new Date().getTime()},
        });
        const answer = response.data;
        let lectureCode = GetURLParameter("lecture");

        // Check if the lecture parameter is autojoin.
        // If it is and there is only one lecture going on in the document, join it automatically.
        // Otherwise proceed as usual.
        const tryToJoin = lectureCode !== null;
        let tryToAutoJoin = lectureCode === "autojoin";
        if (isLectureListResponse(answer)) {
            if (tryToAutoJoin && answer.lectures.length === 1) {
                this.chosenLecture = answer.lectures[0];
                lectureCode = answer.lectures[0].lecture_code;
            } else {
                if (tryToAutoJoin && answer.lectures.length > 1) {
                    showMessageDialog("Cannot autojoin a lecture because there are more than one lecture in this document going on.");
                } else if (tryToAutoJoin && answer.lectures.length <= 0) {
                    showMessageDialog("There are no ongoing lectures for this document.");
                }
                return this.showRightView(answer);
            }
        } else {
            tryToAutoJoin = false;
        }
        if (tryToJoin) {
            $http<boolean>({
                url: "/lectureNeedsPassword",
                method: "GET",
                params: {
                    doc_id: this.viewctrl!.docId,
                    lecture_code: lectureCode,
                    buster: new Date().getTime(),
                },
            }).then((resp) => {
                const needsPassword = resp.data;
                if (lectureCode === null) {
                    throw new Error("lectureCode was unexpectedly null");
                }
                const changeLecture = this.joinLecture(lectureCode, needsPassword);
                if (!changeLecture) {
                    this.showRightView(answer);
                }
            }, () => {
                if (tryToAutoJoin) {
                    showMessageDialog("Could not find a lecture for this document.");
                } else {
                    showMessageDialog(`Lecture ${lectureCode} not found.`);
                }
                this.showRightView(answer);
            });
        } else {
            this.showRightView(answer);
        }
    }

    /**
     * Method to join selected lecture. Checks that lecture is chosen and sends http request to server.
     * @memberof module:lectureController
     * @param lectureCode Name of the lecture to be joined.
     * @param codeRequired True if lecture needs password, else false
     * @return {boolean} true, if successfully joined lecture, else false
     */
    async joinLecture(lectureCode: string, codeRequired: boolean) {
        let changeLecture = true;
        if (this.lecture !== null) {
            const inLecture = this.lectureSettings.inLecture;
            if (inLecture) {
                if (this.lecture.lecture_code === lectureCode) {
                    changeLecture = false;
                } else {
                    changeLecture = $window.confirm(`You are already in lecture ${this.lecture.lecture_code}. Do you want to switch to lecture ${lectureCode}?`);
                }
            }
        }
        if (!changeLecture) {
            return false;
        }

        if (codeRequired) {
            this.passwordQuess = $window.prompt("Please enter a password:", "");
        }
        if (this.chosenLecture === null && lectureCode === "") {
            $window.alert("Choose lecture to join");
            return false;
        }

        const response = await $http<ILectureResponse>({
            url: "/joinLecture",
            method: "POST",
            params: {
                doc_id: this.viewctrl!.docId,
                lecture_code: lectureCode,
                password_quess: this.passwordQuess,
                buster: new Date().getTime(),
            },
        });
        const answer = response.data;
        this.passwordQuess = "";
        const input = $("#passwordInput");
        if (hasLectureEnded(answer.lecture)) {
            showMessageDialog(`Lecture '${lectureCode}' has ended`);
            return false;
        } else if (answer.lecture.is_full) {
            showMessageDialog(`Lecture '${lectureCode}' is full`);
            return false;
        } else if (!answer.correctPassword) {
            showMessageDialog("Wrong access code!");
            return false;
        } else if (!Users.isLoggedIn()) {
            // if the user was logged in as a temporary user, we must refresh
            // to update the plugins (they may require login)
            $window.location.reload();
        } else {
            $("#currentList").slideUp();
            this.focusOn();
            this.studentTable = [];
            this.lecturerTable = [];
            input.removeClass("errorBorder");
            input.attr("placeholder", "Access code");
            if (this.isLecturer) {
                this.showLectureView(answer);
                this.lectureSettings.useWall = true;
                this.lectureSettings.useQuestions = true;
            } else {
                this.lectureAnswer = answer;
                this.showLectureView(this.lectureAnswer);
                this.lectureSettings.useWall = true;
                this.lectureSettings.useQuestions = true;
            }
            return true;
        }
    }

    /**
     * Checks time offset between client server. Used to set question end time right.
     */
    getClockOffset() {
        $http<{t1, t2, t3}>({
            url: "/getServerTime",
            method: "GET",
            params: {t1: new Date().valueOf()},
        }).then((response) => {
            const data = response.data;
            const t4 = new Date().valueOf();
            const t3 = parseInt(data.t3);
            const t2 = parseInt(data.t2);
            const t1 = parseInt(data.t1);
            this.clockOffset = ((t2 - t1) + (t3 - t4)) / 2;
            window.setTimeout(() => {
                setsetting("clock_offset", this.clockOffset.toString());
            }, 1000);
        }, () => {
            if (this.settings.clock_offset) {
                this.clockOffset = this.settings.clock_offset;
            } else {
                this.clockOffset = 0;
            }
        });
    }

    lectureOrThrow(): ILecture {
        if (this.lecture === null) {
            throw new Error("this.lecture was null");
        }
        return this.lecture;
    }

    async initEventHandlers() {
        this.scope.$on("joinLecture", (event, lecture) => {
            this.joinLecture(lecture.lecture_code, lecture.is_access_code);
        });

        /*
         Event listener for closeQuestion. Closes question pop-up.
         */
        this.scope.$on("closeQuestion", () => {
            this.currentQuestionId = null;
        });

        this.scope.$on("questionStopped", () => {
            this.currentQuestionId = null;
        });

        /*
         Event listener for answerToQuestion. Closes the answer pop-up and sends answer to server.
         */
        this.scope.$on("answerToQuestion", (event, answer) => {
            this.currentQuestionId = null;
            if (!this.isLecturer) {
                this.showAnswerWindow = false;
            }
            $http<{questionLate?}>({
                url: "/answerToQuestion",
                method: "PUT",
                params: {
                    asked_id: answer.askedId,
                    lecture_id: this.lectureOrThrow().lecture_id,
                    input: {answers: answer.answer},        // 'answer': answerString,
                    buster: new Date().getTime(),
                },
            })
                .then((response) => {
                    const answer = response.data;
                    if (angular.isDefined(answer.questionLate)) {
                        showMessageDialog(answer.questionLate);
                    }
                }, () => {
                    $log.info("Failed to answer to question");
                });
        });

        this.scope.$on("pointsClosed", (event, askedId) => {
            this.currentPointsId = null;
            $http({
                url: "/closePoints",
                method: "PUT",
                params: {
                    asked_id: askedId,
                    lecture_id: this.lectureOrThrow().lecture_id,
                    buster: new Date().getTime(),
                },
            }).then(() => {
            }, () => {
                $log.info("Failed to answer to question");
            });
        });
    }

    /**
     * Clears the password input when changing the lecture from current lectures list.
     * @memberof module:lectureController
     */
    clearChange() {
        this.passwordQuess = "";
    }

    /**
     * Puts chosen lecture options to use.
     * @memberof module:lectureController
     * @param useQuestions Whether or not to display questions?
     * @param useWall Whether or not to display the wall?
     */
    // TODO: Change showLectureView so that it doesn't set useWall and useQuestions if they are set in
    // lectureOptions dialog
    useOptions(useQuestions, useWall) {
        this.showLectureView(this.lectureAnswer);
        this.lectureSettings.useWall = useWall;
        this.lectureSettings.useQuestions = useQuestions;
    }

    /**
     * Toggle the lecture creation form.
     * @memberof module:lectureController
     */
    async toggleLecture() {
        const result = await showLectureDialog(this.viewctrl!.item);
        if (result !== null) {
            await this.checkIfInLecture();
        }
    }

    /**
     * Starts lecture that is in future lecture list.
     * @memberof module:lectureController
     */
    async startFutureLecture() {
        if (this.futureLecture === null) {
            throw new Error("futureLecture was null");
        }
        const response = await $http<ILectureResponse>({
            url: "/startFutureLecture",
            method: "POST",
            params: {doc_id: this.viewctrl!.docId, lecture_code: this.futureLecture.lecture_code},
        });
        const answer = response.data;
        this.showLectureView(answer);
    }

    /**
     * Change the usage of wall. Used as lectureWall close callback function.
     * @param wallUsage Whether wall should be displayed or not.
     * @memberof module:lectureController
     */
    changeUsingWall(wallUsage: boolean) {
        this.lectureSettings.useWall = wallUsage;
        this.scope.$apply();
    }

    /**
     * Initializes the window to be lecture view a.k.a. the current user in in lecture.
     * @param response The lecture to be shown.
     * @memberof module:lectureController
     */
    async showLectureView(response: ILectureResponse) {
        this.lecture = response.lecture;
        this.isLecturer = response.isLecturer;

        this.wallName = "Wall - " + response.lecture.lecture_code;
        if (this.wallName.length > 30) {
            this.wallName = this.wallName.substring(0, 30) + "...";
        }
        this.lectureSettings.inLecture = true;
        this.polling = true;
        this.lectureSettings.useWall = response.useWall;
        this.lectureSettings.useQuestions = response.useQuestions;

        await this.getAllMessages();

        if (this.isLecturer) {
            this.canStop = true;
            this.addPeopleToList(response.students, this.studentTable);
            this.addPeopleToList(response.lecturers, this.lecturerTable);
        }
    }

    /**
     * Adds people entities to give list.
     * @param people name: String, active:String
     * @param peopleList List of people in the lecture.
     * @memberof module:lectureController
     */
    addPeopleToList(people: ILecturePerson[], peopleList: ILecturePerson[]) {
        for (let i = 0; i < people.length; i++) {
            let oldUser = false;
            for (let index = 0; index < peopleList.length; index++) {
                if (peopleList[index].user_id === people[i].user_id) {
                    oldUser = true;
                    peopleList[index].active = people[i].active;
                    break;
                }
            }

            if (!oldUser) {
                const student = {
                    active: people[i].active,
                    name: people[i].name,
                    user_id: people[i].user_id,
                };
                peopleList.push(student);
            }
        }
    }

    /**
     * Initializes the window to be basic view a.k.a. view where user is not in lecture.
     * @param answer The lecture to be processed.
     * @memberof module:lectureController
     */
    showBasicView(answer: ILectureListResponse) {
        this.isLecturer = answer.isLecturer;
        if (this.isLecturer) {
            this.canStart = true;
            this.canStop = false;
        }
        this.lectureSettings.inLecture = false;
        this.wallMessages = [];
        this.polling = false;
        this.lecture = null;
        this.lecturerTable = [];
        this.studentTable = [];
        this.lectures = [];
        this.futureLectures = [];

        for (let i = 0; i < answer.lectures.length; i++) {
            this.lectures.push(answer.lectures[i]);
        }

        for (let i = 0; i < answer.futureLectures.length; i++) {
            this.futureLectures.push(answer.futureLectures[i]);
        }

        if (this.lectures.length > 0) {
            this.chosenLecture = this.lectures[0];
        }

        if (this.futureLectures.length > 0) {
            this.futureLecture = this.futureLectures[0];
        }
    }

    /**
     * Extends the lecture based on the time selected in pop-up to extend lecture.
     * Currently extends to the old lecture ending time. Other option is to extend
     * from the current moment(needs to be implemented).
     * @memberof module:lectureController
     */
    async extendLecture(minutes: number) {
        const lecture = this.lectureOrThrow();
        const endTimeDate = moment(lecture.end_time).add(minutes, "minutes");
        $log.info("extending lecture");
        $log.info(endTimeDate);
        await $http({
            url: "/extendLecture",
            method: "POST",
            params: {lecture_id: lecture.lecture_id, new_end_time: endTimeDate},
        });
        lecture.end_time = endTimeDate;
        this.lectureEnded = false;
        this.lectureEndingDialogState = LectureEndingDialogState.NotAnswered;
        $log.info("Lecture extended");
    }

    /**
     * Gets lectureInfo and shows editLecture dialog
     * @memberof module:lectureController
     */
    async editLecture(lectureId: string) {
        let params = {lecture_id: lectureId};
        const response = await $http<ILecture>({
            url: "/showLectureInfoGivenName",
            method: "GET",
            params,
        });
        const lecture = response.data;
        await showLectureDialog(this.viewctrl!.item, lecture);
    }

    /**
     * Sends http request to end the lecture.
     * @memberof module:lectureController
     */
    async endLecture() {
        // TODO: Change to some better confirm dialog.
        const confirmAnswer = $window.confirm("Do you really want to end this lecture?");
        if (confirmAnswer) {
            const response = await $http<ILectureListResponse>({
                url: "/endLecture",
                method: "POST",
                params: {lecture_id: this.lectureOrThrow().lecture_id},
            });
            const answer = response.data;
            this.showBasicView(answer);
            this.chosenLecture = null;
            $log.info("Lecture ended, not deleted");
        }
    }

    /**
     * Sends http request to leave the lecture.
     * @memberof module:lectureController
     */
    async leaveLecture() {
        const response = await $http<ILectureListResponse>({
            url: "/leaveLecture",
            method: "POST",
            params: {
                buster: new Date().getTime(),
                lecture_id: this.lectureOrThrow().lecture_id,
            },
        });
        await this.checkIfInLecture();
    }

    /**
     * Sends http request to get all the messages from the current lecture.
     * @memberof module:lectureController
     */
    async getAllMessages() {
        const response = await $http<{lastid, lectureId, data: IMessage[]}>({
            url: "/getAllMessages",
            method: "GET",
            params: {lecture_id: this.lectureOrThrow().lecture_id, buster: new Date().getTime()},
        });
        const answer = response.data;
        answer.data.forEach((msg) => {
            this.wallMessages.push(msg);
        });

        // TODO: Fix this to scroll bottom without cheating.
        const wallArea = $("#wallArea");
        wallArea.animate({scrollTop: wallArea[0].scrollHeight * 10}, 1000);

        this.lastID = answer.lastid;

        if (this.pollingLectures.indexOf(answer.lectureId) === -1) {
            this.startLongPolling(this.lastID);
            this.pollingLectures.push(answer.lectureId);
        }
    }

    /**
     * Gets answers from the current lecture to current question.
     * @param answer Lecture to get the answers from.
     * @memberof module:lectureController
     */
    async getLectureAnswers(answer) {
        let timeoutLectureAnswers;
        this.gettingAnswers = true;
        const response = await $http.get<{answers, noAnswer}>("/getLectureAnswers", {
            params: {
                asked_id: answer.askedId,
                buster: new Date().getTime(),
            },
        });
        const respData = response.data;
        $rootScope.$broadcast("putAnswers", {answers: respData.answers});
        if (this.gettingAnswers && !angular.isDefined(respData.noAnswer)) {
            $window.clearTimeout(timeoutLectureAnswers);

            // Odottaa sekunnin ennen kuin pollaa uudestaan.
            timeoutLectureAnswers = setTimeout(() => {
                this.getLectureAnswers(respData);
            }, 1000);
        }
    }

    /**
     * Starts long polling for the updates.(messages, questions, lecture ending)
     * @param lastID Last id which was received.
     * @memberof module:lectureController
     */
    startLongPolling(lastID) {
        this.message_longPolling(lastID);
    }

    message_longPolling(lastID) {
        $window.clearTimeout(this.pollTimeout);
        if (!lastID) {
            lastID = -1;
        }
        if (this.requestOnTheWay) {
            $log.info("Poll multiplication prevented");
            return;
        }
        if (this.lecture === null) {
            return;
        }
        this.requestOnTheWay = true;
        var buster = (""+new Date().getTime());
        buster =buster.substring(buster.length-4);
        $http<{
            e, lectureId, lectureEnding, students,
            lecturers, new_end_time, points_closed, question, result, askedId, status, data: IMessage[], lastid, ms,
        }>({
            url: "/getUpdates",
            method: "GET",
            params: {
                c: lastID,   //  client_message_id
                l: this.lecture.lecture_id, // lecture_id
                d: this.getDocIdOrNull(), // doc_id
                t: this.isLecturer ? 't' : null, // is_lecturer, Tarkista mielummin serverin päässä
                m: this.lectureSettings.useWall ? 't' : null, // get_messages
                q: this.lectureSettings.useQuestions ? 't' : null, // get_questions
                i: this.currentQuestionId || null, // current_question_id
                p: this.currentPointsId || null, // current_points_id
                b: buster,
            },
        })
            .then(async (response) => {
                $window.clearTimeout(this.pollTimeout);
                const answer = response.data;
                this.requestOnTheWay = false;
                var isLecture = answer.e;
                var poll_interval_ms = answer.ms;
                if (isLectureListResponse(answer)) {
                    this.showBasicView(answer);
                    return;
                } else if (isLecture !== null) {
                    this.pollingLectures.splice(this.pollingLectures.indexOf(answer.lectureId), 1);
                    if (this.lecture === null) {
                        return;
                    }
                    if (answer.lectureId !== this.lecture.lecture_id) {
                        return;
                    }

                    if (answer.lectureEnding !== 100) {
                        if (answer.lectureEnding === 1 && !this.lectureEnded) {
                            this.lectureEnded = true;
                            if (this.lectureEndingDialogState !== LectureEndingDialogState.Open) {
                                this.lectureEndingDialogState = LectureEndingDialogState.NotAnswered;
                            }
                        }
                        if (this.lectureEndingDialogState === LectureEndingDialogState.NotAnswered) {
                            this.lectureEndingDialogState = LectureEndingDialogState.Open;
                            const result = await showLectureEnding(this.lecture);
                            this.lectureEndingDialogState = LectureEndingDialogState.Answered;
                            switch (result.result) {
                                case "dontextend":
                                    break;
                                case "extend":
                                    this.extendLecture(result.extendTime);
                                    break;
                                case "end":
                                    this.endLecture();
                                    break;
                            }
                        }
                    }

                    this.addPeopleToList(answer.students, this.studentTable);
                    this.addPeopleToList(answer.lecturers, this.lecturerTable);

                    // If 'new_end_time' is not undefined, extend or end question according to new_end_time
                    if (typeof answer.new_end_time !== "undefined" && !this.isLecturer) {
                        $rootScope.$broadcast("update_end_time", answer.new_end_time);
                        // If 'question' or 'result' is in answer, show question/explanation accordingly
                    } else if (typeof answer.points_closed !== "undefined") {
                        this.currentPointsId = null;
                        $rootScope.$broadcast("update_end_time", null);
                    } else if ((answer.question || answer.result)) {
                        if (this.isLecturer) {
                            if (answer.result) {
                                this.currentQuestionId = null;
                                this.currentPointsId = answer.askedId;
                            } else {
                                this.currentQuestionId = answer.askedId;
                            }
                        } else {
                            this.showQuestion(answer);
                        }
                    }
                } else { // now new updates
                    answer.lastid = lastID;
                }

                $window.clearTimeout(this.pollTimeout);
                if (this.polling) {

                    if (this.pollingLectures.indexOf(this.lecture.lecture_id) < 0)
                        this.pollingLectures.push(this.lecture.lecture_id);

                    if (answer.status === "results") {
                        let newMessages = 0;
                        answer.data.forEach((msg) => {
                            newMessages++;
                            this.wallMessages.push(msg);
                        });
                        this.newMessagesAmount += newMessages;
                        this.newMessagesAmountText = " (" + this.newMessagesAmount + ")";
                        this.wallName = "Wall - " + this.lecture.lecture_code + this.newMessagesAmountText;
                        this.lastID = answer.lastid;
                        const wallArea = $("#wallArea");
                        wallArea.scrollTop(wallArea[0].scrollHeight);
                    } else {
                        $window.console.log("Sending new poll.");
                    }
                    // Odottaa pyydetyn ajan ennen kuin pollaa uudestaan.
                    var pollInterval = parseInt(poll_interval_ms);
                    if (isNaN(pollInterval) || pollInterval < 1000) pollInterval = 4000;
                    this.pollTimeout = setTimeout(() => {
                        this.message_longPolling(answer.lastid);
                    }, pollInterval);

                } else {
                    this.pollingStopped = true;
                    $window.console.log("Got answer but not polling anymore.");
                }
            }, () => {
                this.requestOnTheWay = false;
                $window.clearTimeout(this.pollTimeout);
                // Odottaa 30s ennen kuin yrittää uudelleen errorin jälkeen.
                this.pollTimeout = setTimeout(() => {
                    this.message_longPolling(this.lastID);
                }, 30000);
            });
    }

    showQuestion(answer) {
        $log.info("Showing question");
        let showPoints = "";
        if (answer.result) {
            showPoints = "Show points: ";
        }
        let markup = JSON.parse(answer.questionjson);
        if (!markup.json) {
            markup = {json: markup}; // compatibility for old
        }
        markup.expl = {};

        if (answer.expl) {
            markup.expl = JSON.parse(answer.expl);
        }
        if (answer.points) {
            markup.points = answer.points;
        }
        markup.userpoints = answer.userpoints;

        this.questionTitle = showPoints + markup.json.questionTitle;
        if (answer.result) {
            this.currentQuestionId = null;
            this.currentPointsId = answer.askedId;
        } else {
            this.currentQuestionId = answer.askedId;
        }
        $rootScope.$broadcast("setQuestionJson", {
            result: answer.result,
            answer: answer.answer,
            markup,
            askedId: answer.askedId,
            isLecturer: this.isLecturer,
            askedTime: answer.asked,
            clockOffset: this.clockOffset,
        });
    }

    async getQuestionManually(event) {
        const response = await $http<{already_answered}>({
            url: "/getQuestionManually",
            method: "GET",
            params: {
                lecture_id: this.lectureOrThrow().lecture_id,
                buster: new Date().getTime(),
            },
        });
        const answer = response.data;
        if (!answer) {
            showMessageDialog("No running questions.");
        } else if (answer.already_answered) {
            showMessageDialog("You have already answered to the current question.");
        } else {
            this.showQuestion(answer);
        }
    }

    gotFocus() {
        if (!this.lectureSettings.inLecture) {
            return;
        }
        $log.info("Got focus");
        if (typeof this.timeout !== "undefined") {
            $window.clearTimeout(this.timeout);
        }
        this.polling = true;
        this.scope.$apply();
        if (!this.requestOnTheWay) {
            this.startLongPolling(this.lastID);
        }
        this.focusOff();
    }

    lostFocus() {
        $log.info("Lost focus");
        this.polling = false;
        angular.element($window).on("focus.gotfocus", () => {
            this.gotFocus();
        });
    }

    focusOn() {
        angular.element($window).on("focus.gotfocus", () => {
            this.gotFocus();
        });
    }

    focusOff() {
        angular.element($window).off("focus.gotfocus");
    }
}

timApp.component("timLecture", {
    controller: LectureController,
    require: {
        viewctrl: "?^timView",
    },
    template: "<div ng-transclude></div>",
    transclude: true,
});
