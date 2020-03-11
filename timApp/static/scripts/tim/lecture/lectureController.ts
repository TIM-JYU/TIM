/**
 * Created by hajoviin on 24.2.2015.
 * Contoller that handles lectures.
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

import ifvisible from "ifvisible.js";
import moment from "moment";
import {clone, getURLParameter, markAsUsed, setStorage, to} from "tim/util/utils";
import {ViewCtrl} from "../document/viewctrl";
import {IModalInstance, showMessageDialog} from "../ui/dialog";
import {Users} from "../user/userService";
import {someglobals} from "../util/globals";
import {$http, $log, $rootScope, $timeout} from "../util/ngimport";
import {
    currentQuestion,
    getAskedQuestionFromQA,
    IAnswerQuestionResult,
    isOpenInAnotherTab,
    QUESTION_STORAGE,
    showQuestionAnswerDialog,
} from "./answerToQuestionController";
import {showLectureDialog} from "./createLectureCtrl";
import {showLectureEnding} from "./lectureEnding";
import {
    alreadyAnswered,
    hasLectureEnded,
    hasUpdates,
    IAlreadyAnswered,
    IAskedQuestion,
    IEmptyResponse,
    ILecture,
    ILectureListResponse,
    ILectureMessage,
    ILecturePerson,
    ILectureResponse,
    ILectureSettings, INoUpdatesResponse,
    IQuestionAsked,
    IQuestionHasAnswer,
    IQuestionResult,
    isAskedQuestion,
    isEmptyResponse,
    isLectureListResponse,
    isNoUpdatesResponse,
    IUpdateResponse,
    pointsClosed,
    questionAnswerReceived,
    questionAsked,
    questionHasAnswer,
} from "./lecturetypes";
import * as wall from "./lectureWall";
import {LectureWallController, showLectureWall} from "./lectureWall";
import {askQuestion} from "./questionAskController";

markAsUsed(wall);

enum LectureEndingDialogState {
    NotAnswered,
    Open,
    Answered,
}

const AUTOJOIN_CODE = "autojoin";

let lectureControllerInstance: LectureController | undefined;

export class LectureController {
    public isLecturer: boolean;
    public lecture: ILecture | undefined;
    public lectures: ILecture[];
    public lectureSettings: ILectureSettings;
    private lectureEndingDialogState: LectureEndingDialogState;
    private futureLectures: ILecture[];
    private lectureEnded: boolean;
    private lecturerTable: ILecturePerson[];
    private newMessagesAmount: number;
    private newMessagesAmountText: string;
    private showPoll: boolean;
    private studentTable: ILecturePerson[];
    viewctrl?: ViewCtrl;
    private wallMessages: ILectureMessage[];
    private wallName: string;
    private wallInstance: IModalInstance<LectureWallController> | undefined;
    // The last asked question that was initiated *from this tab* by the lecturer.
    // So this field should NOT be updated in the poll method.
    lastQuestion: IAskedQuestion | undefined;

    constructor(vctrl: ViewCtrl | undefined) {
        this.viewctrl = vctrl;
        this.wallName = "Wall";
        this.newMessagesAmount = 0;
        this.newMessagesAmountText = "";
        this.showPoll = true;
        this.lectures = [];
        this.futureLectures = [];
        this.isLecturer = false;
        this.studentTable = [];
        this.lecturerTable = [];
        this.lectureEndingDialogState = LectureEndingDialogState.NotAnswered;
        this.lectureEnded = false;
        this.wallMessages = [];

        const g = someglobals();
        this.lectureSettings = {
            inLecture: "in_lecture" in g ? g.in_lecture : false,
            lectureMode: "lectureMode" in g ? g.lectureMode : false,
            useAnswers: true,
            useQuestions: true,
            useWall: true,
        };
    }

    lectureViewOrInLecture() {
        return this.lectureSettings.lectureMode || this.lectureSettings.inLecture;
    }

    static createAndInit(vctrl: ViewCtrl | undefined) {
        if (lectureControllerInstance) {
            return lectureControllerInstance;
        }
        const l = new LectureController(vctrl);
        l.init();
        lectureControllerInstance = l;
        return l;
    }

    async init() {
        await this.checkIfInLecture();
        void this.startLongPolling();
    }

    async refreshWall() {
        if (!this.lecture) {
            return;
        }
        if (this.lectureSettings.useWall && !this.wallInstance) {
            this.wallInstance = showLectureWall(this.wallMessages);
            await to(this.wallInstance.result);
            this.lectureSettings.useWall = false;
        } else if (!this.lectureSettings.useWall) {
            this.closeLectureWallIfOpen();
        }
    }

    private closeLectureWallIfOpen() {
        if (this.wallInstance) {
            this.wallInstance.close({});
            this.wallInstance = undefined;
        }
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
        const response = await to($http<ILectureResponse | ILectureListResponse | IEmptyResponse>({
            url: "/checkLecture",
            method: "GET",
            params: {doc_id: this.getDocIdOrNull(), buster: new Date().getTime()},
        }));
        if (!response.ok) {
            return;
        }
        const answer = response.result.data;
        if (isEmptyResponse(answer)) {
            return;
        }
        let lectureCode = getURLParameter("lecture");

        // Check if the lecture parameter is autojoin.
        // If it is and there is only one lecture going on in the document, join it automatically.
        // Otherwise proceed as usual.
        let tryToAutoJoin = lectureCode === AUTOJOIN_CODE;
        if (isLectureListResponse(answer)) {
            if (tryToAutoJoin && answer.lectures.length === 1) {
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
        const tryToJoin = lectureCode != null && lectureCode !== AUTOJOIN_CODE;
        if (tryToJoin) {
            const r = await to($http<ILecture>({
                url: "/getLectureByCode",
                method: "GET",
                params: {
                    doc_id: this.viewctrl!.docId,
                    lecture_code: lectureCode,
                    buster: new Date().getTime(),
                },
            }));
            if (!r.ok) {
                if (tryToAutoJoin) {
                    await showMessageDialog("Could not find a lecture for this document.");
                } else {
                    await showMessageDialog(`Lecture ${lectureCode} not found.`);
                }
                this.showRightView(answer);
                return;
            }
            const joined = await this.joinLecture(r.result.data);
            if (!joined) {
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
    async joinLecture(lecture: ILecture): Promise<boolean> {
        let changeLecture = true;
        const lectureCode = lecture.lecture_code;
        const codeRequired = lecture.is_access_code;
        if (this.lecture) {
            const inLecture = this.lectureSettings.inLecture;
            if (inLecture) {
                if (this.lecture.lecture_code === lectureCode) {
                    changeLecture = false;
                } else {
                    changeLecture = window.confirm(`You are already in lecture ${this.lecture.lecture_code}. Do you want to switch to lecture ${lectureCode}?`);
                }
            }
        }
        if (!changeLecture) {
            return false;
        }

        let passwordGuess = null;
        if (codeRequired) {
            passwordGuess = window.prompt(`Please enter a password to join the lecture '${lecture.lecture_code}':`, "") ?? undefined;
            if (passwordGuess == null) {
                return false;
            }
        }
        const response = await to($http<ILectureResponse>({
            url: "/joinLecture",
            method: "POST",
            params: {
                doc_id: this.viewctrl!.docId,
                lecture_code: lectureCode,
                password_quess: passwordGuess,
                buster: new Date().getTime(),
            },
        }));
        if (!response.ok) {
            return false;
        }
        const answer = response.result.data;
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
            window.location.reload();
            return true;
        } else {
            this.studentTable = [];
            this.lecturerTable = [];
            this.showLectureView(answer);
            this.lectureSettings.useWall = true;
            this.lectureSettings.useQuestions = true;
            return true;
        }
    }

    lectureOrThrow(): ILecture {
        if (this.lecture == null) {
            throw new Error("this.lecture was null");
        }
        return this.lecture;
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
    async startFutureLecture(l: ILecture) {
        const response = await to($http<ILectureResponse>({
            url: "/startFutureLecture",
            method: "POST",
            params: {doc_id: this.viewctrl!.docId, lecture_code: l.lecture_code},
        }));
        if (!response.ok) {
            return;
        }
        const answer = response.result.data;
        this.showLectureView(answer);
    }

    /**
     * Initializes the window to be lecture view a.k.a. the current user in in lecture.
     * @param response The lecture to be shown.
     */
    showLectureView(response: ILectureResponse) {
        this.lecture = response.lecture;
        this.isLecturer = response.isLecturer;

        this.wallName = "Wall - " + response.lecture.lecture_code;
        if (this.wallName.length > 30) {
            this.wallName = this.wallName.substring(0, 30) + "...";
        }
        this.lectureSettings.inLecture = true;
        this.lectureSettings.useWall = response.useWall;
        this.lectureSettings.useQuestions = response.useQuestions;

        if (this.isLecturer) {
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
        for (const p of people) {
            let oldUser = false;
            for (const pl of peopleList) {
                if (pl.user.id === p.user.id) {
                    oldUser = true;
                    pl.active = p.active;
                    break;
                }
            }

            if (!oldUser) {
                const student = clone(p);
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
        this.lectureSettings.inLecture = false;
        this.closeLectureWallIfOpen();
        this.lecture = undefined;
        this.lecturerTable = [];
        this.studentTable = [];
        this.lectures = [];
        this.futureLectures = [];

        for (const l of answer.lectures) {
            this.lectures.push(l);
        }

        for (const l of answer.futureLectures) {
            this.futureLectures.push(l);
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
        const r = await to($http({
            url: "/extendLecture",
            method: "POST",
            params: {lecture_id: lecture.lecture_id, new_end_time: endTimeDate},
        }));
        if (!r.ok) {
            return;
        }
        lecture.end_time = endTimeDate;
        this.lectureEnded = false;
        this.lectureEndingDialogState = LectureEndingDialogState.NotAnswered;
        $log.info("Lecture extended");
    }

    /**
     * Gets lectureInfo and shows editLecture dialog
     */
    async editLecture(lectureId: string) {
        const params = {lecture_id: lectureId};
        const response = await to($http<ILecture>({
            url: "/showLectureInfoGivenName",
            method: "GET",
            params,
        }));
        if (!response.ok) {
            return;
        }
        const lecture = response.result.data;
        await showLectureDialog(lecture);
    }

    /**
     * Sends http request to end the lecture.
     */
    async endLecture() {
        // TODO: Change to some better confirm dialog.
        const confirmAnswer = window.confirm("Do you really want to end this lecture?");
        if (confirmAnswer) {
            const response = await to($http<ILectureListResponse>({
                url: "/endLecture",
                method: "POST",
                params: {lecture_id: this.lectureOrThrow().lecture_id},
            }));
            if (!response.ok) {
                return;
            }
            const answer = response.result.data;
            this.showBasicView(answer);
            $log.info("Lecture ended, not deleted");
        }
    }

    /**
     * Sends http request to leave the lecture.
     */
    async leaveLecture() {
        const response = await to($http<ILectureListResponse>({
            url: "/leaveLecture",
            method: "POST",
            params: {
                buster: new Date().getTime(),
                lecture_id: this.lectureOrThrow().lecture_id,
            },
        }));
        await this.checkIfInLecture();
    }

    /**
     * Starts long polling for the updates.(messages, questions, lecture ending)
     */
    async startLongPolling() {
        let lastID = -1;
        while (true) {
            if (this.lecture == null) {
                await $timeout(5000);
                continue;
            }

            // By checking "hidden" we avoid the idle timeout (default 60 seconds).
            if (!ifvisible.now("hidden")) {
                const [timeout, last] = await this.pollOnce(lastID);
                lastID = last;
                $rootScope.$applyAsync();
                await $timeout(Math.max(timeout, 1000));
            } else {
                await $timeout(1000);
            }
        }
    }

    async pollOnce(lastID: number): Promise<[number, number]> {
        let buster = "" + new Date().getTime();
        buster = buster.substring(buster.length - 4);
        const r = await to($http<IUpdateResponse>({
            url: "/getUpdates",
            method: "GET",
            params: {
                c: lastID,   //  client_message_id
                d: this.getDocIdOrNull(), // doc_id
                m: this.lectureSettings.useWall ? "t" : null, // get_messages
                q: this.lectureSettings.useQuestions ? "t" : null, // get_questions
                i: this.getCurrentQuestionId(), // current_question_id
                p: this.getCurrentPointsId(), // current_points_id
                b: buster,
            },
        }));
        if (!r.ok) {
            // in case of an error, wait 30 seconds before trying again
            return [30000, lastID];
        }
        const answer = r.result.data;
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
            this.handleEndTimeUpdate(answer);
            if (answer.extra) {
                if (pointsClosed(answer.extra)) {
                    if (currentQuestion) {
                        if (!this.isLecturer) {
                            currentQuestion.close();
                        }
                    } else {
                        $log.error("got points_closed, but there was no currentQuestion");
                    }
                } else if (!alreadyAnswered(answer.extra) && !questionHasAnswer(answer.extra)) {
                    if (
                        // Lecturer asked a question from this window/tab. Show it of course.
                        (this.lastQuestion && getAskedQuestionFromQA(answer.extra.data).asked_id === this.lastQuestion.asked_id)

                        // lecturer has multiple windows/tabs open in the lecture document. Show the question in all of them.
                        || (this.isLecturer && this.viewctrl && this.viewctrl.item.id == this.lecture.doc_id)

                        // The question is not open in any other window/tab, so it needs to be shown in whichever got the
                        // message first. We also need to make sure we are not the lecturer because in that case the dialog
                        // might open in the wrong window (because we specifically want the one where the lecturer
                        // pressed the Ask button).
                        || (!isOpenInAnotherTab(answer.extra.data) && !this.isLecturer)
                    ) {
                        void this.showQuestion(answer.extra);
                    }
                }
            }

            answer.msgs.forEach((msg) => {
                this.wallMessages.push(msg);
            });
            this.newMessagesAmount += answer.msgs.length;
            this.newMessagesAmountText = " (" + this.newMessagesAmount + ")";
            this.wallName = "Wall - " + this.lecture.lecture_code + this.newMessagesAmountText;

            let pollInterval = answer.ms;
            if (isNaN(pollInterval) || pollInterval < 1000) {
                pollInterval = 4000;
            }
            let newLastId = lastID;
            if (answer.msgs.length > 0) {
                newLastId = answer.msgs[answer.msgs.length - 1].msg_id;
            }
            return [pollInterval, newLastId];
        } else if (isNoUpdatesResponse(answer)) {
            this.handleEndTimeUpdate(answer);
            let pollInterval = answer.ms;
            if (isNaN(pollInterval) || pollInterval < 1000) {
                pollInterval = 4000;
            }
            return [pollInterval, lastID];
        }
        return [0, lastID];
    }

    private handleEndTimeUpdate(answer: INoUpdatesResponse) {
        if (answer.question_end_time !== undefined) {
            if (currentQuestion) {
                currentQuestion.updateEndTime(answer.question_end_time);
            } else {
                $log.error("currentQuestion was undefined when trying to update end time");
            }
        }
    }

    getCurrentQuestionId() {
        if (!currentQuestion || currentQuestion.hasResult()) {
            return undefined;
        }
        return currentQuestion.getQuestion().asked_id;
    }

    getCurrentPointsId() {
        if (!currentQuestion || !currentQuestion.hasResult()) {
            return undefined;
        }
        return currentQuestion.getQuestion().asked_id;
    }

    async showQuestion(answer: IQuestionAsked | IQuestionResult) {
        let question: IAskedQuestion;
        if (isAskedQuestion(answer.data)) {
            question = answer.data;
        } else {
            question = answer.data.asked_question;
        }
        let result: IAnswerQuestionResult;
        const aq = getAskedQuestionFromQA(answer.data);
        setStorage(QUESTION_STORAGE, aq.asked_id);
        if (currentQuestion) {
            currentQuestion.setData(answer.data);
            return;
        } else {
            const r = await to(showQuestionAnswerDialog({qa: answer.data, isLecturer: this.isLecturer}));
            if (!r.ok) {
                return;
            }
            result = r.result;
        }
        if (result.type === "pointsclosed") {
            await to($http({
                url: "/closePoints",
                method: "PUT",
                params: {
                    asked_id: result.askedId,
                    buster: new Date().getTime(),
                },
            }));
        } else if (result.type === "closed") {
            // empty
        } else if (result.type === "reask") {
            this.lastQuestion = await askQuestion({askedId: question.asked_id});
        } else if (result.type === "answered") {
            // empty
        } else {
            // reask as new
            this.lastQuestion = await askQuestion({parId: question.par_id, docId: question.doc_id});
        }
    }

    async getQuestionManually(event: Event) {
        const response = await to($http<IAlreadyAnswered | IQuestionAsked | IQuestionHasAnswer | IQuestionResult | null>({
            url: "/getQuestionManually",
            method: "GET",
            params: {
                buster: new Date().getTime(),
            },
        }));
        if (!response.ok) {
            return;
        }
        const answer = response.result.data;
        if (!answer) {
            await showMessageDialog("No running questions.");
        } else if (alreadyAnswered(answer)) {
            await showMessageDialog("You have already answered to the current question.");
        } else if (questionAsked(answer) || questionAnswerReceived(answer)) {
            this.showQuestion(answer);
        }
    }
}
