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

import angular from "angular";
import {IController} from "angular";
import $ from "jquery";
import moment from "moment";
import {timApp} from "tim/app";
import sessionsettings from "tim/session";
import * as utils from "tim/utils";
import {setsetting} from "tim/utils";
import {showDialog} from "../dialog";
import {$http, $log, $rootScope, $timeout, $window} from "../ngimport";
import {ViewCtrl} from "./view/viewctrl";
import {showLectureDialog} from "./createLectureCtrl";

export interface ILectureSettings {
    pollingStopped: boolean;
    inLecture: boolean; lectureMode: (any | boolean);
    wallMinimized: boolean;
    messageName: boolean;
    messageTime: boolean;
    polling: boolean;
    canStart: boolean;
    canStop: boolean;
    useDate: boolean;
    useDuration: boolean;
    dateChosen: boolean;
    durationChosen: boolean;
    showAnswerWindow: boolean;
    showStudentAnswers: boolean;
    gettingAnswers: boolean;
    answeredToLectureEnding: boolean;
    showLectureEnding: boolean;
    lectureEnded: boolean; showLectureForm: boolean;
    showLectureOptions: boolean;
    useQuestions: boolean;
    useWall: boolean;
    useAnswers: boolean;
    useNotPollingDialog: boolean;
}

interface ILecture {
    lecture_code: string;
}

interface IMessage {
    time: string;
    sender: string;
    message: string;
}

//TODO: Painike, josta voisi hakea kysymyksiä.
//TODO: Button, to get questions and wall.
export class LectureController implements IController {
    private static $inject = ["$scope"];
    private docNamePath: string;
    private lectureStartTime: moment.Moment;
    private lectureEndTime: moment.Moment;
    private lectureName: string;
    private msg: string;
    private newMsg: string;
    private wallName: string;
    private messageName: boolean;
    private messageTime: boolean;
    private newMessagesAmount: number;
    private newMessagesAmountText: string;
    private showPoll: boolean;
    private polling: boolean;
    private requestOnTheWay: boolean;
    private canStart: boolean;
    private canStop: boolean;
    private showLectureCreation: boolean;
    private lectures: ILecture[];
    private futureLectures: ILecture[];
    private futureLecture: ILecture;
    private chosenLecture: ILecture;
    private passwordQuess: string;
    private pollingLectures: number[];
    private useDate: boolean;
    private useDuration: boolean;
    private dateChosen: boolean;
    private durationChosen: boolean;
    private durationHour: string;
    private durationMin: string;
    public isLecturer: boolean;
    public lectureId: number;
    private showAnswerWindow: boolean;
    private showStudentAnswers: boolean;
    private studentTable: {}[];
    private lecturerTable: {}[];
    private gettingAnswers: boolean;
    private answeredToLectureEnding: boolean;
    private showLectureEnding: boolean;
    private extend: {extendTime: string, choices: number[]};
    private lectureEnded: boolean;
    private showLectureOptions: boolean;
    private wallMessages: IMessage[];
    private questionTitle: string;
    private clockOffset: number;
    private current_question_id: number;
    private current_points_id: number;
    private settings: any;
    public lectureSettings: ILectureSettings;
    private eventKey: any;
    private timeout: any;
    private viewctrl: ViewCtrl;
    private scope: angular.IScope;
    private lectureAnswer: {};
    private markup: {json: string};
    public questionShown: boolean;
    private lastID: number;
    private wall: JQuery;
    private wallHeight: number;
    private pollingStopped: boolean;

    constructor(scope: angular.IScope) {
        this.scope = scope;
        this.docNamePath = "";
        this.lectureStartTime = moment();
        this.lectureEndTime = moment();
        this.lectureName = "";
        this.msg = "";
        this.newMsg = "";
        this.wallName = "Wall";
        this.messageName = true;
        this.messageTime = true;
        this.newMessagesAmount = 0;
        this.newMessagesAmountText = "";
        this.showPoll = true;
        this.polling = true;
        this.requestOnTheWay = false;
        this.canStart = true;
        this.canStop = false;
        this.showLectureCreation = false;
        this.lectures = [];
        this.futureLectures = [];
        this.futureLecture = null;
        this.chosenLecture = null;
        this.passwordQuess = "";
        this.pollingLectures = [];
        this.useDate = false;
        this.useDuration = false;
        this.dateChosen = false;
        this.durationChosen = false;
        this.durationHour = "";
        this.durationMin = "";
        this.isLecturer = false;
        this.lectureId = null;
        this.showAnswerWindow = false;
        this.showStudentAnswers = false;
        this.studentTable = [];
        this.lecturerTable = [];
        this.gettingAnswers = false;
        this.answeredToLectureEnding = false;
        this.showLectureEnding = false;
        this.extend = {extendTime: "15", choices: [5, 10, 15, 30, 45, 60]};
        this.lectureEnded = false;
        this.showLectureOptions = false;
        this.wallMessages = [];
        this.questionTitle = "";
        this.clockOffset = 0;
        this.current_question_id = null;
        this.current_points_id = null;
        this.settings = sessionsettings;

        //TODO: Move all lecture settings to lectureSettings object, so they will work as ng-model
        this.lectureSettings = {
            pollingStopped: false,
            inLecture: false,
            lectureMode: $window.lectureMode || false,
            wallMinimized: false,
            messageName: true,
            messageTime: true,
            polling: true,
            canStart: true,
            canStop: false,
            useDate: false,
            useDuration: false,
            dateChosen: false,
            durationChosen: false,
            showAnswerWindow: false,
            showStudentAnswers: false,
            gettingAnswers: false,
            answeredToLectureEnding: false,
            showLectureEnding: false,
            lectureEnded: false,
            showLectureForm: false,
            showLectureOptions: false,
            useQuestions: true,
            useWall: true,
            useAnswers: true,
            useNotPollingDialog: true,
        };

        const wall = $("#wall");
        const htmlMessageList = $("#wallMessageList");

        this.checkIfInLecture();
        this.getClockOffset();

        this.eventKey = null;

        let stateKey;
        const keys = {
            hidden: "visibilitychange",
            webkitHidden: "webkitvisibilitychange",
            mozHidden: "mozvisibilitychange",
            msHidden: "msvisibilitychange",
        };

        for (stateKey in keys) {
            if (stateKey in document) {
                this.eventKey = keys[stateKey];
                $log.info(keys[stateKey]);
                break;
            }
        }

        $(document).on("visibilitychange", () => {
            if (document.visibilityState == "hidden") {
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

    /**
     * Makes http request to check if the current user is in lecture.
     * @memberof module:lectureController
     */
    checkIfInLecture() {

        const showRightView = (answer) => {
            if (answer.isInLecture) {
                this.showLectureView(answer);
            } else {
                this.showBasicView(answer);
            }
        };

        $http<{lectures: Array<{lecture_code}>, isInLecture, lectureCode}>({
            url: "/checkLecture",
            method: "GET",
            params: {doc_id: this.viewctrl.docId, buster: new Date().getTime()},
        })
            .then((response) => {
                const answer = response.data;
                let lectureCode = utils.GetURLParameter("lecture");
                /*
                 Check if the lecture parameter is autojoin.
                 If it is and there is only one lecture going on in the document, join it automatically.
                 Otherwise proceed as usual.*/
                const autoJoin = "autojoin";
                if (!answer.lectures && lectureCode === autoJoin) {
                    lectureCode = "";
                }
                if (answer.lectures) {
                    if (lectureCode === autoJoin && answer.lectures.length === 1) {
                        this.chosenLecture = answer.lectures[0];
                        lectureCode = answer.lectures[0].lecture_code;
                    }
                    if (lectureCode === autoJoin && answer.lectures.length > 1) {
                        lectureCode = "";
                    }
                    if (lectureCode === autoJoin && answer.lectures.length <= 0) {
                        showDialog("There are no ongoing lectures for this document.");
                        return showRightView(answer);
                    }
                }
                if (lectureCode) {
                    $http({
                        url: "/lectureNeedsPassword",
                        method: "GET",
                        params: {
                            doc_id: this.viewctrl.docId,
                            lecture_code: lectureCode,
                            buster: new Date().getTime(),
                        },
                    }).then((resp) => {
                        const data = resp.data;
                        const changeLecture = this.joinLecture(lectureCode, data, answer.isInLecture, answer.lectureCode);
                        if (!changeLecture) {
                            showRightView(answer);
                        }
                    }, () => {
                        if (lectureCode === autoJoin) {
                            showDialog("Could not find a lecture for this document.");
                        } else {
                            showDialog(`Lecture ${lectureCode} not found.`);
                        }
                        showRightView(answer);
                    });
                } else {
                    showRightView(answer);
                }
            });

    }

    /**
     * Method to join selected lecture. Checks that lecture is chosen and sends http request to server.
     * @memberof module:lectureController
     * @param name Name of the lecture to be joined.
     * @param codeRequired True if lecture needs password, else false
     * @return {boolean} true, if successfully joined lecture, else false
     */
    joinLecture(name?, codeRequired?, isInLecture?, currentLecture?) {
        let changeLecture = true;
        if (currentLecture) {
            const inLecture = isInLecture || this.lectureSettings.inLecture;
            if (inLecture) {
                if (currentLecture == name) {
                    changeLecture = false;
                } else {
                    changeLecture = $window.confirm(`You are already in lecture ${currentLecture}. Do you want to switch to lecture ${name}?`);
                }
            }
        }
        if (!changeLecture) {
            return false;
        }

        if (codeRequired) {
            this.passwordQuess = $window.prompt("Please enter a password:", "");
        }
        if (this.chosenLecture === null && name === "") {
            $window.alert("Choose lecture to join");
            return false;
        }

        let lectureName = "";
        if (angular.isDefined(name)) {
            lectureName = name;
            $("#currentList").slideUp();
        } else {
            lectureName = this.chosenLecture.lecture_code;
        }

        $http<{lecture_ended, lecture_full, correctPassword, anonLogin}>({
            url: "/joinLecture",
            method: "POST",
            params: {
                doc_id: this.viewctrl.docId,
                lecture_code: lectureName,
                password_quess: this.passwordQuess,
                buster: new Date().getTime(),
            },
        })
            .then((response) => {
                const answer = response.data;
                this.passwordQuess = "";
                const input = $("#passwordInput");
                if (answer.lecture_ended) {
                    showDialog(`Lecture '${name}' has ended`);
                    return false;
                } else if (answer.lecture_full) {
                    showDialog(`Lecture '${name}' is full`);
                    return false;
                } else if (!answer.correctPassword) {
                    showDialog("Wrong access code!");
                    return false;
                } else if (answer.anonLogin) {
                    // if the user was logged in as a temporary user, we must refresh
                    // to update the plugins (they may require login)
                    $window.location.reload();
                } else {
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
                        // this.showLectureOptions = true;
                        this.lectureAnswer = answer;
                        this.showLectureView(this.lectureAnswer);
                        this.lectureSettings.useWall = true;
                        this.lectureSettings.useQuestions = true;
                    }
                    return true;
                }
            }, () => false);
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
            $log.info("Clock offset: ", this.clockOffset);
        }, () => {
            if (this.settings.clock_offset) {
                this.clockOffset = this.settings.clock_offset;
            } else {
                this.clockOffset = 0;
            }
        });
    }

    initEventHandlers() {
        /**
         * Use data.question_id to ask question as new.
         * Use data.asked_id to reask question.
         * Use data.par_id to ask a question from document.
         */
        this.scope.$on("askQuestion", (event, data) => {
            this.markup = data.markup;
            const args: any = {
                lecture_id: data.lecture_id,
                doc_id: data.doc_id,
                buster: new Date().getTime(),
            };
            if (data.asked_id) {
                args.asked_id = data.asked_id;
            } else if (data.question_id) {
                args.question_id = data.question_id; // Remove when moving questions to document finished
            } else {
                args.par_id = data.par_id;
            }
            $http<number>({
                url: "/askQuestion",
                method: "POST",
                params: args,
            })
                .then((response) => {
                    const id = response.data;
                    this.showStudentAnswers = true;
                    if (this.lectureSettings.useAnswers) {
                        // Because of dynamic creation needs to wait 1ms to ensure that the directive is made(maybe?)
                        $timeout(() => {
                            $rootScope.$broadcast("createChart", this.markup.json);
                            this.showStudentAnswers = false;
                        }, 1);

                        const answer = {askedId: id};
                        this.current_question_id = id;
                        this.getLectureAnswers(answer);
                    }
                    $rootScope.$broadcast("setQuestionJson", {
                        markup: this.markup,
                        questionParId: data.par_id,
                        questionId: data.question_id,
                        askedId: id,
                        isAsking: true,
                        isLecturer: this.isLecturer,
                        askedTime: new Date().valueOf() + this.clockOffset,
                        clockOffset: this.clockOffset,
                    });
                    this.showAnswerWindow = true;
                }, (error) => {
                    $log.info(error);
                });
        });

        /*
         Event listener for getLectureId. Emits the lectureId back
         */
        this.scope.$on("getLectureId", () => {
            this.scope.$emit("postLectureId", this.lectureId);
        });

        this.scope.$on("joinLecture", (event, lecture) => {
            this.joinLecture(lecture.lecture_code, lecture.is_access_code, this.lectureSettings.inLecture,
                this.lectureName);
        });

        this.scope.$on("changeQuestionTitle", (event, data) => {
            this.questionTitle = data.questionTitle;
        });

        this.scope.$on("toggleQuestion", (event, data) => {
            this.questionShown = !this.questionShown;
            this.scope.$emit("newQuestion", data);
        });

        /*
         Event listener for getLecture. Emits the boolean value if the user is in lecture
         */
        this.scope.$on("getInLecture", () => {
            this.scope.$emit("postInLecture", this.lectureSettings.inLecture);
        });

        this.scope.$on("showAnswers", (event, x) => {
            this.showStudentAnswers = x;
        });

        /*
         Event listener for closeLectureForm. Closes lecture form.
         */
        this.scope.$on("closeLectureForm", (event, showWall) => {
            if (showWall) {
                this.lectureSettings.useWall = true;
            }
            this.checkIfInLecture();
        });

        /*
         Event listener for closeQuestion. Closes question pop-up.
         */
        this.scope.$on("closeQuestion", () => {
            this.current_question_id = null;
            this.showAnswerWindow = false;
        });

        this.scope.$on("questionStopped", () => {
            this.current_question_id = null;
        });

        /*
         Event listener for answerToQuestion. Closes the answer pop-up and sends answer to server.
         */
        this.scope.$on("answerToQuestion", (event, answer) => {
            this.current_question_id = null;
            if (!this.isLecturer) {
                this.showAnswerWindow = false;
            }
            /*
             var mark = "";
             var answerString = "";
             angular.forEach(answer.answer, function (singleAnswer) {
             answerString += mark + singleAnswer;
             mark = "|";
             });
             */
            $http<{questionLate?}>({
                url: "/answerToQuestion",
                method: "PUT",
                params: {
                    asked_id: answer.askedId,
                    lecture_id: this.lectureId,
                    input: {answers: answer.answer},        // 'answer': answerString,
                    buster: new Date().getTime(),
                },
            })
                .then((response) => {
                    const answer = response.data;
                    if (angular.isDefined(answer.questionLate)) {
                        showDialog(answer.questionLate);
                    }
                }, () => {
                    $log.info("Failed to answer to question");
                });
        });

        this.scope.$on("pointsClosed", (event, askedId) => {
            this.current_points_id = null;
            $http({
                url: "/closePoints",
                method: "PUT",
                params: {
                    asked_id: askedId,
                    lecture_id: this.lectureId,
                    buster: new Date().getTime(),
                },
            }).then(() => {
            }, () => {
                $log.info("Failed to answer to question");
            });
        });

        /*
         Event window for closeAnswerShow. Closes pop-up to show answers and stops gettin more of them.
         */
        this.scope.$on("closeAnswerShow", () => {
            this.showStudentAnswers = false;
            //this.gettingAnswers = false;
        });
    }

    /**
     * Depending on what users wants to see on the wall, makes the msg to correct form. Able to show the
     * name of the sender, time and message. Sender and time are optional.
     * @memberof module:lectureController
     */
    showInfo() {
        this.msg = "";
        let i = 0;
        if (this.messageName && this.messageTime) {
            for (i = 0; i < this.wallMessages.length; i++) {
                this.msg += this.wallMessages[i].sender;
                this.msg += " <" + this.wallMessages[i].time + ">: ";
                this.msg += this.wallMessages[i].message + "\r\n";
            }
        }

        if (!this.messageName && this.messageTime) {
            for (i = 0; i < this.wallMessages.length; i++) {
                this.msg += " <" + this.wallMessages[i].time + ">: ";
                this.msg += this.wallMessages[i].message + "\r\n";
            }
        }

        if (this.messageName && !this.messageTime) {
            for (i = 0; i < this.wallMessages.length; i++) {
                this.msg += this.wallMessages[i].sender + ": ";
                this.msg += this.wallMessages[i].message + "\r\n";
            }
        }

        if (!this.messageName && !this.messageTime) {
            for (i = 0; i < this.wallMessages.length; i++) {
                this.msg += ">" + this.wallMessages[i].message + "\r\n";
            }
        }
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
    //TODO: Change showLectureView so that it doesn't set useWall and useQuestions if they are set in
    // lectureOptions dialog
    useOptions(useQuestions, useWall) {
        this.showLectureView(this.lectureAnswer);
        this.lectureSettings.useWall = useWall;
        this.lectureSettings.useQuestions = useQuestions;
        this.showLectureOptions = false;
    }

    /**
     * Hides the wall if the wall hasn't moved.
     * @memberof module:lectureController
     */
    hideWall() {
        this.lectureSettings.wallMinimized = !this.lectureSettings.wallMinimized;
        const base = this.wall.find("#wallBase");
        this.newMessagesAmount = 0;
        this.newMessagesAmountText = "";
        if (this.lectureSettings.wallMinimized) {
            this.wallHeight = this.wall.height();
            this.wall.height(15);
            base.css("display", "none");
            this.wall.css("min-height", "0");
        } else {
            base.css("display", "");
            this.wall.css("min-height", "");
            this.wall.height(this.wallHeight);
        }
        this.wallName = "Wall - " + this.lectureName;
        this.scope.$apply();
    }

    /**
     * Toggle the lecture creation form.
     * @memberof module:lectureController
     */
    toggleLecture() {
        // TODO
    }

    /**
     * Starts lecture that is in future lecture list.
     * @memberof module:lectureController
     */
    startFutureLecture() {
        $http({
            url: "/startFutureLecture",
            method: "POST",
            params: {doc_id: this.viewctrl.docId, lecture_code: this.futureLecture.lecture_code},
        })
            .then((response) => {
                const answer = response.data;
                this.showLectureView(answer);
            });
    }

    /**
     * Change the usage of wall. Used as lectureWall close callback function.
     * @param wallUsage Whether wall should be displayed or not.
     * @memberof module:lectureController
     */
    changeUsingWall(wallUsage) {
        this.lectureSettings.useWall = wallUsage;
        this.scope.$apply();
    }

    /**
     * Initializes the window to be lecture view a.k.a. the current user in in lecture.
     * @param lecture The lecture to be shown.
     * @memberof module:lectureController
     */
    showLectureView(lecture) {
        this.docNamePath = encodeURI(lecture.doc_name);
        this.isLecturer = lecture.isLecturer;

        this.lectureName = lecture.lectureCode;
        this.wallName = "Wall - " + lecture.lectureCode;
        if (this.wallName.length > 30) {
            this.wallName = this.wallName.substring(0, 30) + "...";
        }
        this.lectureStartTime = lecture.startTime;
        this.lectureEndTime = lecture.endTime;
        this.lectureSettings.inLecture = true;
        this.lectureId = lecture.lectureId;
        this.polling = true;
        this.msg = "";
        this.lectureSettings.useWall = lecture.useWall;
        this.lectureSettings.useQuestions = lecture.useQuestions;

        this.getAllMessages();

        if (this.isLecturer) {
            $rootScope.$broadcast("getQuestions");
            this.canStop = true;
            this.addPeopleToList(lecture.students, this.studentTable);
            this.addPeopleToList(lecture.lecturers, this.lecturerTable);
        }
    }

    /**
     * Adds people entities to give list.
     * @param people name: String, active:String
     * @param peopleList List of people in the lecture.
     * @memberof module:lectureController
     */
    addPeopleToList(people, peopleList) {
        for (let i = 0; i < people.length; i++) {
            let oldUser = false;
            for (let index = 0; index < peopleList.length; index++) {
                if (peopleList[index].id === people[i].user_id) {
                    oldUser = true;
                    peopleList[index].active = people[i].active;
                    break;
                }
            }

            if (!oldUser) {
                const student = {
                    id: people[i].user_id,
                    name: people[i].name,
                    active: people[i].active,
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
    showBasicView(answer) {
        this.docNamePath = encodeURI(this.viewctrl.item.path);
        this.isLecturer = answer.isLecturer;
        if (this.isLecturer) {
            $rootScope.$broadcast("getQuestions");
            this.canStart = true;
            this.canStop = false;
        }
        this.lectureSettings.inLecture = false;
        this.wallMessages = [];
        this.polling = false;
        this.lectureId = -1;
        this.lectureName = "Not running";
        this.showStudentAnswers = false;
        this.showAnswerWindow = false;
        this.lecturerTable = [];
        this.studentTable = [];
        this.lectures = [];
        this.futureLectures = [];

        if (answer === "") {
            return;
        }

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
    extendLecture() {
        const endTimeDate = moment(this.lectureEndTime).add(this.extend.extendTime, "minutes");
        $log.info("extending lecture");
        this.answeredToLectureEnding = true;
        $log.info(endTimeDate);
        $http({
            url: "/extendLecture",
            method: "POST",
            params: {doc_id: this.viewctrl.docId, lecture_id: this.lectureId, new_end_time: endTimeDate},
        })
            .then(() => {
                this.lectureEndTime = endTimeDate;
                this.showLectureEnding = false;
                this.lectureEnded = false;
                this.answeredToLectureEnding = false;
                $log.info("Lecture extended");
            }, () => {
                $log.info("Failed to extend the lecture");
            });
    }

    /**
     * Closes the lecture view and sets answered to lectureEnding to true to prevent multiple quesitions from this.
     * @memberof module:lectureController
     */
    continueLecture() {
        this.answeredToLectureEnding = true;
        this.showLectureEnding = false;
    }

    /**
     * Gets lectureInfo and shows editLecture dialog
     * @memberof module:lectureController
     */
    async editLecture(lectureCode: string) {
        let params: any = {lecture_code: lectureCode, doc_id: this.viewctrl.docId};
        if (this.lectureId >= 0) {
            params = {lecture_id: this.lectureId};
        }
        const response = await $http<{lectureId, lectureCode, lectureStartTime, lectureEndTime, password}>({
            url: "/showLectureInfoGivenName",
            method: "GET",
            params,
        });
        const lecture = response.data;
        await showLectureDialog(this.viewctrl.item, {
            doc_id: this.viewctrl.docId,
            lecture_id: lecture.lectureId,
            lecture_code: lecture.lectureCode,
            start_time: moment(lecture.lectureStartTime),
            end_time: moment(lecture.lectureEndTime),
            password: lecture.password || "",
            max_students: null, // TODO server doesn't give this
        });
    }

    /**
     * Sends http request to end the lecture.
     * @memberof module:lectureController
     */
    endLecture() {
        this.showLectureEnding = false;

        // TODO: Change to some better confirm dialog.
        const confirmAnswer = $window.confirm("Do you really want to end this lecture?");
        if (confirmAnswer) {
            $http({
                url: "/endLecture",
                method: "POST",
                params: {doc_id: this.viewctrl.docId, lecture_id: this.lectureId},
            })
                .then((response) => {
                    const answer = response.data;
                    this.showBasicView(answer);
                    this.chosenLecture = null;
                    this.msg = "";
                    $log.info("Lecture ended, not deleted");

                }, () => {
                    $log.info("Failed to delete the lecture");
                });
        }
    }

    /**
     * Sends http request to delete the lecture.
     * @memberof module:lectureController
     */
    deleteLecture() {
        $http({
            url: "/deleteLecture",
            method: "POST",
            params: {doc_id: this.viewctrl.docId, lecture_id: this.lectureId, buster: new Date().getTime()},
        })
            .then((response) => {
                const answer = response.data;
                this.showBasicView(answer);
                this.lectures.splice(this.lectureId, 1);
                this.chosenLecture = null;
                this.msg = "";
                $log.info("Lecture deleted");

            }, () => {
                $log.info("Failed to delete the lecture");
            });
    }

    /**
     * Sends http request to leave the lecture.
     * @memberof module:lectureController
     */
    leaveLecture(lectureId, confirm) {
        //TODO: better confirm dialog
        this.msg = "";
        $http({
            url: "/leaveLecture",
            method: "POST",
            params: {
                lecture_id: lectureId || this.lectureId,
                doc_id: this.viewctrl.docId,
                buster: new Date().getTime(),
            },
        })
            .then((response) => {
                const answer = response.data;
                this.showBasicView(answer);
            });
    }

    /**
     * Shows lecture creation. //TODO: Something is missing from here
     */
    modifyLecture() {
        this.showLectureCreation = true;
    }

    /**
     * Sends http request to send a message.
     * @param message The message to be sent.
     * @returns {boolean} Whether the message was sent successfully.
     * @memberof module:lectureController
     */
    sendMessageEvent(message) {
        if (message.trim() === "") {
            showDialog("Can't send empty messages");
            return false;
        }

        $http({
            url: "/sendMessage",
            method: "POST",
            params: {message, lecture_id: this.lectureId},
        })
            .then(() => {
                this.newMsg = "";
                //TODO: Fix this to scroll bottom without cheating.
                const wallArea = $("#wallArea");
                wallArea.animate({scrollTop: wallArea[0].scrollHeight * 10}, 1000);

            }, () => {
                $log.info("Can't send message or something");
            });

    }

    /**
     * Sends http request to get all the messages from the current lecture.
     * @memberof module:lectureController
     */
    getAllMessages() {
        this.msg = "";
        $http<{lastid, lectureId, data}>({
            url: "/getAllMessages",
            method: "GET",
            params: {lecture_id: this.lectureId, buster: new Date().getTime()},
        })
            .then((response) => {
                const answer = response.data;
                angular.forEach(answer.data, (msg) => {
                    this.wallMessages.push(msg);
                    if (this.messageName) {
                        this.msg += msg.sender + " ";
                    }
                    if (this.messageTime) {
                        this.msg += "<" + msg.time + ">: ";
                    }

                    this.msg += msg.message + "\n";
                });

                //TODO: Fix this to scroll bottom without cheating.
                const wallArea = $("#wallArea");
                wallArea.animate({scrollTop: wallArea[0].scrollHeight * 10}, 1000);

                this.lastID = answer.lastid;

                if (this.pollingLectures.indexOf(answer.lectureId) === -1) {
                    this.startLongPolling(this.lastID);
                    this.pollingLectures.push(answer.lectureId);
                }
            });
    }

    /**
     * Gets answers from the current lecture to current question.
     * @param answer Lecture to get the answers from.
     * @memberof module:lectureController
     */
    getLectureAnswers(answer) {
        let timeoutLectureAnswers;
        this.gettingAnswers = true;
        $http<{answers, noAnswer}>({
            url: "/getLectureAnswers",
            method: "GET",
            params: {
                asked_id: answer.askedId,
                doc_id: this.viewctrl.docId,
                lecture_id: this.lectureId,
                time: answer.latestAnswer,
                buster: new Date().getTime(),
            },
        })
            .then((response) => {
                const answer = response.data;
                $rootScope.$broadcast("putAnswers", {answers: answer.answers});
                if (this.gettingAnswers && !angular.isDefined(answer.noAnswer)) {
                    $window.clearTimeout(timeoutLectureAnswers);

                    // Odottaa sekunnin ennen kuin pollaa uudestaan.
                    timeoutLectureAnswers = setTimeout(() => {
                        this.getLectureAnswers(answer);
                    }, 1000);
                }

            }, () => {
                $log.info("Couldn't get answers");
            });
    }

    /**
     * Starts long polling for the updates.(messages, questions, lecture ending)
     * @param lastID Last id which was received.
     * @memberof module:lectureController
     */
    startLongPolling(lastID) {
        // this.newMsg = "Alku";

        this.message_longPolling(lastID);
    }

    message_longPolling(lastID) {
        // this.newMsg = "Lähti";
        let timeout;

        if (!lastID) {
            lastID = -1;
        }
        if (this.requestOnTheWay === true) {
            $log.info("Poll multiplication prevented");
            return;
        }
        this.requestOnTheWay = true;
        $http<{
            isLecture, lectureId, lectureEnding, students,
            lecturers, new_end_time, points_closed, question, result, askedId, status, data, lastid
        }>({
            url: "/getUpdates",
            method: "GET",
            params: {
                client_message_id: lastID,
                lecture_id: this.lectureId,
                doc_id: this.viewctrl.docId,
                is_lecturer: this.isLecturer, // Tarkista mielummin serverin päässä
                get_messages: this.lectureSettings.useWall,
                get_questions: this.lectureSettings.useQuestions,
                current_question_id: this.current_question_id || null,
                current_points_id: this.current_points_id || null,
                buster: new Date().getTime(),
            },
        })
            .then((response) => {
                const answer = response.data;
                // this.newMsg = "Täällä";
                this.requestOnTheWay = false;
                if (!answer.isLecture) {
                    this.showBasicView(answer);
                    return;
                }
                if (answer.isLecture != -1) {
                    this.pollingLectures.splice(this.pollingLectures.indexOf(answer.lectureId), 1);
                    if (answer.lectureId !== this.lectureId) {
                        return;
                    }

                    if (answer.lectureEnding !== 100) {
                        if (answer.lectureEnding === 1 && !this.lectureEnded) {
                            this.showLectureEnding = true;
                            this.lectureEnded = true;
                        }
                        if (!this.answeredToLectureEnding) {
                            this.showLectureEnding = true;
                        }
                    }

                    this.addPeopleToList(answer.students, this.studentTable);
                    this.addPeopleToList(answer.lecturers, this.lecturerTable);

                    // If 'new_end_time' is not undefined, extend or end question according to new_end_time
                    if (typeof answer.new_end_time !== "undefined" && !this.isLecturer) {
                        $rootScope.$broadcast("update_end_time", answer.new_end_time);
                        // If 'question' or 'result' is in answer, show question/explanation accordingly
                    } else if (typeof answer.points_closed !== "undefined") {
                        this.current_points_id = null;
                        $rootScope.$broadcast("update_end_time", null);
                    } else if ((answer.question || answer.result)) {
                        if (this.isLecturer) {
                            if (answer.result) {
                                this.current_question_id = null;
                                this.current_points_id = answer.askedId;
                            } else {
                                this.current_question_id = answer.askedId;
                            }
                        } else {
                            this.showQuestion(answer);
                        }
                    }
                } else { // now new updates
                    answer.lastid = lastID;
                }

                $window.clearTimeout(timeout);
                if (this.polling) {

                    this.pollingLectures.push(this.lectureId);
                    // Odottaa sekunnin ennen kuin pollaa uudestaan.
                    timeout = setTimeout(() => {
                        this.message_longPolling(answer.lastid);
                    }, 2000);

                    if (answer.status === "results") {
                        let newMessages = 0;
                        angular.forEach(answer.data, (msg) => {
                            newMessages++;
                            this.wallMessages.push(msg);
                            if (this.messageName && this.messageTime) {
                                this.msg += msg.sender;
                                this.msg += " <" + msg.time + ">: ";
                                this.msg += msg.message + "\r\n";
                            }

                            if (!this.messageName && this.messageTime) {
                                this.msg += " <" + msg.time + ">: ";
                                this.msg += msg.message + "\r\n";

                            }

                            if (this.messageName && !this.messageTime) {
                                this.msg += msg.sender + ": ";
                                this.msg += msg.message + "\r\n";

                            }

                            if (!this.messageName && !this.messageTime) {
                                this.msg += ">" + msg.message + "\r\n";

                            }
                        });
                        this.newMessagesAmount += newMessages;
                        if (this.lectureSettings.wallMinimized) {
                            this.newMessagesAmountText = " (" + this.newMessagesAmount.toString() + ")";
                        } else {
                            this.newMessagesAmountText = "";
                        }
                        this.wallName = "Wall - " + this.lectureName + this.newMessagesAmountText;
                        this.lastID = answer.lastid;
                        const wallArea = $("#wallArea");
                        wallArea.scrollTop(wallArea[0].scrollHeight);
                    } else {
                        $log.info("Sending new poll.");

                    }
                } else {
                    this.pollingStopped = true;
                    $log.info("Got answer but not polling anymore.");
                }
            }, () => {
                // this.newMsg = "Virhe";
                this.requestOnTheWay = false;
                $window.clearTimeout(timeout);
                //Odottaa 30s ennen kuin yrittää uudelleen errorin jälkeen.
                timeout = setTimeout(() => {
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
        this.showAnswerWindow = true;
        let markup = JSON.parse(answer.questionjson);
        if (!markup.json) {
            markup = {json: markup}; // compability for old
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
            this.current_question_id = null;
            this.current_points_id = answer.askedId;
        } else {
            this.current_question_id = answer.askedId;
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

    getQuestionManually(event) {
        $http<{already_answered}>({
            url: "/getQuestionManually",
            method: "GET",
            params: {
                lecture_id: this.lectureId,
                buster: new Date().getTime(),
            },
        })
            .then((response) => {
                const answer = response.data;
                if (!answer) {
                    showDialog("No running questions.");
                } else if (answer.already_answered) {
                    showDialog("You have already answered to the current question.");
                } else {
                    this.showQuestion(answer);
                }
            }, () => {
                $log.info("Couldn't get questions.");
            });
    }

    /**
     * Event for pressing enter while writing message. Sends message.
     * @param event They key press.
     * @memberof module:lectureController
     */
    chatEnterPressed(event) {
        if (event.which === 13) {
            this.sendMessageEvent(this.newMsg);
        }
    }

    /**
     * Event when pressing enter while writing password for lecture. Tries to join lecture
     * @param event The key press.
     * @memberof module:lectureController
     */
    passEnterPressed(event) {
        if (event.which === 13) {
            this.joinLecture();
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
        this.pollingStopped = false;
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
    template: "<div ng-transclude></div>",
    transclude: true,
});
