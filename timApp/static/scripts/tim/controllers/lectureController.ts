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

import angular, {IController, IScope} from "angular";
import $ from "jquery";
import moment from "moment";
import {timApp} from "tim/app";
import sessionsettings from "tim/session";
import {clone, GetURLParameter, markAsUsed, setsetting, to} from "tim/utils";
import {showLectureEnding} from "../components/lectureEnding";
import * as wall from "../components/lectureWall";
import {showMessageDialog} from "../dialog";
import {
    alreadyAnswered,
    endTimeChanged,
    hasLectureEnded,
    hasUpdates,
    IAlreadyAnswered,
    IAskedQuestion,
    ILecture,
    ILectureListResponse,
    ILectureMessage,
    ILecturePerson,
    ILectureResponse,
    ILectureSettings,
    IQuestionAsked,
    IQuestionHasAnswer,
    IQuestionResult,
    isAskedQuestion,
    isLectureListResponse,
    IUpdateResponse,
    pointsClosed,
    questionAnswerReceived,
    questionAsked,
    questionHasAnswer,
} from "../lecturetypes";
import {$http, $log, $timeout, $window} from "../ngimport";
import {Users} from "../services/userService";
import {currentQuestion, showQuestionAnswerDialog} from "./answerToQuestionController";
import {showLectureDialog} from "./createLectureCtrl";
import {askQuestion} from "./questionAskController";
import {ViewCtrl} from "./view/viewctrl";

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
    public lecture: ILecture | undefined;
    public lectures: ILecture[];
    public lectureSettings: ILectureSettings;
    private lectureEndingDialogState: LectureEndingDialogState;
    private canStart: boolean;
    private canStop: boolean;
    private chosenLecture: ILecture | undefined;
    private clockOffset: number;
    private currentPointsId: number | undefined;
    private currentQuestionId: number | undefined;
    private futureLecture: ILecture | undefined;
    private futureLectures: ILecture[];
    private gettingAnswers: boolean;
    private lectureAnswer: ILectureResponse;
    private lectureEnded: boolean;
    private lecturerTable: ILecturePerson[];
    private newMessagesAmount: number;
    private newMessagesAmountText: string;
    private passwordQuess: string | undefined;
    private polling: boolean;
    private scope: IScope;
    private settings: any;
    private showPoll: boolean;
    private studentTable: ILecturePerson[];
    private timeout: any;
    viewctrl: ViewCtrl | undefined;
    private wallMessages: ILectureMessage[];
    private wallName: string;

    constructor(scope: IScope) {
        this.scope = scope;
        this.wallName = "Wall";
        this.newMessagesAmount = 0;
        this.newMessagesAmountText = "";
        this.showPoll = true;
        this.polling = true;
        this.canStart = true;
        this.canStop = false;
        this.lectures = [];
        this.futureLectures = [];
        this.passwordQuess = "";
        this.isLecturer = false;
        this.studentTable = [];
        this.lecturerTable = [];
        this.gettingAnswers = false;
        this.lectureEndingDialogState = LectureEndingDialogState.NotAnswered;
        this.lectureEnded = false;
        this.wallMessages = [];
        this.clockOffset = 0;
        this.settings = sessionsettings;

        this.lectureSettings = {
            inLecture: false,
            lectureMode: $window.lectureMode || false,
            useAnswers: true,
            useQuestions: true,
            useWall: true,
        };
    }

    async $onInit() {
        if (this.viewctrl) {
            this.viewctrl.lectureCtrl = this;
        }
        await this.checkIfInLecture();
        void this.startLongPolling();
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

    showRightView(answer: ILectureListResponse | ILectureResponse) {
        if (!isLectureListResponse(answer)) {
            this.showLectureView(answer);
        } else {
            this.showBasicView(answer);
        }
    }

    getDocIdOrNull(): number | undefined {
        if (this.viewctrl) {
            return this.viewctrl.docId;
        }
        return undefined;
    }

    /**
     * Makes http request to check if the current user is in lecture.
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
        const tryToJoin = lectureCode != null;
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
            const [err, resp] = await to($http<ILecture>({
                url: "/getLectureByCode",
                method: "GET",
                params: {
                    doc_id: this.viewctrl!.docId,
                    lecture_code: lectureCode,
                    buster: new Date().getTime(),
                },
            }));
            if (!resp) {
                if (tryToAutoJoin) {
                    await showMessageDialog("Could not find a lecture for this document.");
                } else {
                    await showMessageDialog(`Lecture ${lectureCode} not found.`);
                }
                this.showRightView(answer);
                return;
            }
            const changeLecture = this.joinLecture(resp.data);
            if (!changeLecture) {
                this.showRightView(answer);
            }
        } else {
            this.showRightView(answer);
        }
    }

    /**
     * Method to join selected lecture. Checks that lecture is chosen and sends http request to server.
     * @return {boolean} true, if successfully joined lecture, else false
     */
    async joinLecture(lecture: ILecture) {
        let changeLecture = true;
        const lectureCode = lecture.lecture_code;
        const codeRequired = lecture.is_access_code;
        if (this.lecture) {
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
            this.passwordQuess = $window.prompt("Please enter a password:", "") || undefined;
        }
        if (this.chosenLecture == null && lectureCode === "") {
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
        $http<{t1: number, t2: number, t3: number}>({
            url: "/getServerTime",
            method: "GET",
            params: {t1: new Date().valueOf()},
        }).then((response) => {
            const data = response.data;
            const t4 = new Date().valueOf();
            // const t3 = parseInt(data.t3);
            const t2 = data.t2;
            const t1 = data.t1;
            this.clockOffset = ((t2 - t1) + (t2 - t4)) / 2;
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
        if (this.lecture == null) {
            throw new Error("this.lecture was null");
        }
        return this.lecture;
    }

    /**
     * Clears the password input when changing the lecture from current lectures list.
     */
    clearChange() {
        this.passwordQuess = "";
    }

    /**
     * Puts chosen lecture options to use.
     * @param useQuestions Whether or not to display questions?
     * @param useWall Whether or not to display the wall?
     */
    // TODO: Change showLectureView so that it doesn't set useWall and useQuestions if they are set in
    // lectureOptions dialog
    useOptions(useQuestions: boolean, useWall: boolean) {
        this.showLectureView(this.lectureAnswer);
        this.lectureSettings.useWall = useWall;
        this.lectureSettings.useQuestions = useQuestions;
    }

    /**
     * Toggle the lecture creation form.
     */
    async toggleLecture() {
        const result = await showLectureDialog(this.viewctrl!.item);
        if (result != null) {
            await this.checkIfInLecture();
        }
    }

    /**
     * Starts lecture that is in future lecture list.
     */
    async startFutureLecture() {
        if (!this.futureLecture) {
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
     */
    changeUsingWall(wallUsage: boolean) {
        this.lectureSettings.useWall = wallUsage;
        this.scope.$apply();
    }

    /**
     * Initializes the window to be lecture view a.k.a. the current user in in lecture.
     * @param response The lecture to be shown.
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

        // TODO: Fix this to scroll bottom without cheating.
        const wallArea = $("#wallArea");
        wallArea.animate({scrollTop: wallArea[0].scrollHeight * 10}, 1000);

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
     */
    addPeopleToList(people: ILecturePerson[], peopleList: ILecturePerson[]) {
        for (let i = 0; i < people.length; i++) {
            let oldUser = false;
            for (let index = 0; index < peopleList.length; index++) {
                if (peopleList[index].user.id === people[i].user.id) {
                    oldUser = true;
                    peopleList[index].active = people[i].active;
                    break;
                }
            }

            if (!oldUser) {
                const student = clone(people[i]);
                peopleList.push(student);
            }
        }
    }

    /**
     * Initializes the window to be basic view a.k.a. view where user is not in lecture.
     * @param answer The lecture to be processed.
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
        this.lecture = undefined;
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
            this.chosenLecture = undefined;
            $log.info("Lecture ended, not deleted");
        }
    }

    /**
     * Sends http request to leave the lecture.
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
     * Starts long polling for the updates.(messages, questions, lecture ending)
     * @param lastID Last id which was received.
     */
    async startLongPolling() {
        let lastID = -1;
        while (true) {
            if (this.lecture == null) {
                await $timeout(5000);
                continue;
            }
            const [timeout, last] = await this.pollOnce(lastID);
            lastID = last;
            await $timeout(Math.max(timeout, 1000));
        }
    }

    async pollOnce(lastID: number): Promise<[number, number]> {
        let buster = "" + new Date().getTime();
        buster = buster.substring(buster.length - 4);
        const response = await $http<IUpdateResponse>({
            url: "/getUpdates",
            method: "GET",
            params: {
                c: lastID,   //  client_message_id
                d: this.getDocIdOrNull(), // doc_id
                m: this.lectureSettings.useWall ? "t" : null, // get_messages
                q: this.lectureSettings.useQuestions ? "t" : null, // get_questions
                i: this.currentQuestionId || null, // current_question_id
                p: this.currentPointsId || null, // current_points_id
                b: buster,
            },
        });
        const answer = response.data;
        if (isLectureListResponse(answer)) {
            this.showBasicView(answer);
        } else if (hasUpdates(answer)) {
            if (!this.lecture) {
                return [0, lastID];
            }
            if (answer.lectureId !== this.lecture.lecture_id) {
                return [0, lastID];
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
            if (answer.extra) {
                if (endTimeChanged(answer.extra)) {
                    // extend or end question according to new_end_time
                    if (!this.isLecturer) {
                        if (currentQuestion) {
                            currentQuestion.updateEndTime(answer.extra.new_end_time);
                        } else {
                            $log.error("currentQuestion was undefined when trying to update end time");
                        }
                    }
                    // If 'question' or 'result' is in answer, show question/explanation accordingly
                } else if (pointsClosed(answer.extra)) {
                    this.currentPointsId = undefined;
                    if (currentQuestion) {
                        currentQuestion.updateEndTime(null);
                    } else {
                        $log.error("currentQuestion was undefined when set end time to null");
                    }
                } else if (!alreadyAnswered(answer.extra) && !questionHasAnswer(answer.extra)) {
                    if (this.isLecturer) {
                        if (questionAnswerReceived(answer.extra)) {
                            this.currentQuestionId = undefined;
                            this.currentPointsId = answer.extra.data.asked_question.asked_id;
                        } else {
                            this.currentQuestionId = answer.extra.data.asked_id;
                        }
                    } else {
                        this.showQuestion(answer.extra);
                    }
                }
            }

            answer.msgs.forEach((msg) => {
                this.wallMessages.push(msg);
            });
            this.newMessagesAmount += answer.msgs.length;
            this.newMessagesAmountText = " (" + this.newMessagesAmount + ")";
            this.wallName = "Wall - " + this.lecture.lecture_code + this.newMessagesAmountText;
            const wallArea = $("#wallArea");
            wallArea.scrollTop(wallArea[0].scrollHeight);

            let pollInterval = answer.ms;
            if (isNaN(pollInterval) || pollInterval < 1000) {
                pollInterval = 4000;
            }
            let newLastId = lastID;
            if (answer.msgs.length > 0) {
                newLastId = answer.msgs[answer.msgs.length - 1].msg_id;
            }
            return [pollInterval, newLastId];
        }
        return [0, lastID];
        // }, () => {
        //     this.requestOnTheWay = false;
        //     $window.clearTimeout(this.pollTimeout);
        //     // Odottaa 30s ennen kuin yrittää uudelleen errorin jälkeen.
        //     this.pollTimeout = setTimeout(() => {
        //         this.pollOnce(this.lastID);
        //     }, 30000);
        // });
    }

    async showQuestion(answer: IQuestionAsked | IQuestionResult) {
        let question: IAskedQuestion;
        if (isAskedQuestion(answer.data)) {
            this.currentQuestionId = answer.data.asked_id;
            question = answer.data;
        } else {
            this.currentQuestionId = undefined;
            this.currentPointsId = answer.data.asked_question.asked_id;
            question = answer.data.asked_question;
        }
        const result = await showQuestionAnswerDialog({qa: answer.data, isLecturer: this.isLecturer});
        if (result.type === "pointsclosed") {
            this.currentPointsId = undefined;
            await $http({
                url: "/closePoints",
                method: "PUT",
                params: {
                    asked_id: result.askedId,
                    buster: new Date().getTime(),
                },
            });
        } else if (result.type === "closed") {
            this.currentQuestionId = undefined;
        } else if (result.type === "reask") {
            await askQuestion({askedId: question.asked_id});
        } else {
            // reask as new
            await askQuestion({parId: question.par_id, docId: question.doc_id});
        }
    }

    async getQuestionManually(event: Event) {
        const response = await $http<IAlreadyAnswered | IQuestionAsked | IQuestionHasAnswer | IQuestionResult | null>({
            url: "/getQuestionManually",
            method: "GET",
            params: {
                buster: new Date().getTime(),
            },
        });
        const answer = response.data;
        if (!answer) {
            await showMessageDialog("No running questions.");
        } else if (alreadyAnswered(answer)) {
            await showMessageDialog("You have already answered to the current question.");
        } else if (questionAsked(answer) || questionAnswerReceived(answer)) {
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
