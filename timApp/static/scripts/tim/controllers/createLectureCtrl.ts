import angular from "angular";
import {IController} from "angular";
import {IModalInstanceService} from "angular-ui-bootstrap";
import $ from "jquery";
import moment from "moment";
import {timApp} from "tim/app";
import {IItem} from "../IItem";
import {ILecture, ILectureFormParams} from "../lecturetypes";
import {$http, $log, $uibModal, $window} from "../ngimport";

/**
 * Lecture creation controller which is used to handle and validate the form data.
 * @module createLectureCtrl
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

export class CreateLectureCtrl implements IController {
    private static $inject = ["$uibModalInstance", "item", "lecture"];
    private useDate: boolean;
    private useDuration: boolean;
    private dateChosen: boolean;
    private durationChosen: boolean;
    private durationHour: string;
    private durationMin: string;
    private lectureId: number;
    private dateCheck: boolean;
    private dueCheck: boolean;
    private errorMessage: string;
    private lectureCode: string;
    private password: string;
    private maxStudents: number;
    private earlyJoining: boolean;
    private dateTimeOptions: EonasdanBootstrapDatetimepicker.SetOptions;
    private startTime: moment.Moment;
    private endTime: moment.Moment;
    private readonly item: IItem;
    private readonly modal: IModalInstanceService;

    constructor(modal: IModalInstanceService,
                item: IItem,
                lecture: ILectureFormParams | null) {
        this.modal = modal;
        this.item = item;
        this.useDate = false;
        this.useDuration = false;
        this.dateChosen = false;
        this.durationChosen = false;
        this.durationHour = "";
        this.durationMin = "";
        this.lectureId = null;
        this.dateCheck = false;
        this.dueCheck = false;
        this.errorMessage = "";
        this.lectureCode = "";
        this.password = "";
        this.maxStudents = 100;
        this.earlyJoining = true;

        this.dateTimeOptions = {
            format: "D.M.YYYY HH:mm:ss",
            showTodayButton: true,
        };
        this.startTime = moment();
        this.dueCheck = true;
        this.earlyJoining = true;
        this.enableDue2();
        if (lecture !== null) {
            this.setLecture(lecture);
        }
    }

    setLecture(data: ILectureFormParams) {
        this.lectureCode = data.lecture_code;
        this.lectureId = data.lecture_id;
        this.startTime = data.start_time;
        this.endTime = data.end_time;
        this.enableDate2();
        if (data.password !== undefined) {
            this.password = data.password;
        }
    }

    $onInit() {

    }

    $postLink() {
        const form = $(".createLecture")[0];

        form.addEventListener("keydown", (event: KeyboardEvent) => {
            if (event.ctrlKey || event.metaKey) {
                switch (String.fromCharCode(event.which).toLowerCase()) {
                    case "s":
                        event.preventDefault();
                        this.submitLecture();
                        break;
                }
            }
        });
    }

    getJoinLectureLink() {
        return `https://${encodeURIComponent(location.host)}/lecture/${this.item.path}?lecture=${encodeURIComponent(this.lectureCode)}`;
    }

    getAutoJoinLink() {
        return `https://${encodeURIComponent(location.host)}/lecture/${this.item.path}?lecture=autojoin`;
    }

    /**
     * This function is called if end date is selected. Sets boolean values to reflect this choice and sets
     * the default end time to 2 hours ahead of start time.
     * @memberof module:createLectureCtrl
     */
    enableDate2() {
        this.dateCheck = true;
        this.dueCheck = false;
        if (this.endTime == null) {
            this.endTime = moment(this.startTime).add(2, "hours");
        }
        this.useDate = true;
        this.useDuration = false;
        this.durationHour = "";
        this.durationMin = "";
    }

    /**
     * Function for enabling fields and buttons for "Duration" and disabling them for "Use date". This function
     * is called when duration is chosen and is chosen by default.
     * @memberof module:createLectureCtrl
     */
    enableDue2() {
        this.dateCheck = false;
        this.dueCheck = true;
        this.useDuration = true;
        this.useDate = false;
        this.durationHour = "02";
        this.durationMin = "00";
    }

    /**
     * Remove border from given element.
     * @param element ID of the field whose border will be removed.
     * @memberof module:createLectureCtrl
     */
    defInputStyle(element) {
        if (element !== null || !element.isDefined) {
            angular.element("#" + element).css("border", "");
        }
    }

    /**
     * Checks if the value is between 0-23
     * @param element The value of the element to be validated.
     * @param val The ID of the input field so user can be notified of the error.
     * @memberof module:createLectureCtrl
     */
    isHour(element, val) {
        if (element === "" || isNaN(element) || element > 23 || element < 0) {
            this.errorize(val, "Hour has to be between 0 and 23.");
        }
    }

    isNumber(element, val) {
        $log.info(element);
        if (!(element === "") && (isNaN(element) || element < 0)) {
            this.errorize(val, "Max number of students must be a positive number or empty.");
        }
    }

    /**
     * Checks if the value is between 0-59
     * @param element The value of the element to be validated.
     * @param val The ID of the input field so user can be notified of the error.
     * @memberof module:createLectureCtrl
     */
    isMinute(element, val) {
        if (element === "" || isNaN(element) || element > 59 || element < 0) {
            this.errorize(val, "Minutes has to be between 0 and 59.");
        }
    }

    /**
     * Checks if the number given is positive.
     * @param element The value of the element to be validated.
     * @param val The ID of the input field so user can be notified of the error.
     * @memberof module:createLectureCtrl
     */
    isPositiveNumber(element, val) {
        if (element === "" || isNaN(element) || element < 0) {
            this.errorize(val, "Number has to be positive.");
        }
    }

    creatingNew() {
        return this.lectureId === null;
    }

    /**
     * Function for creating a new lecture and validation.
     * @memberof module:createLectureCtrl
     */
    async submitLecture() {
        this.removeErrors();
        if (!this.startTime) {
            this.errorize("startTime", "Start time must be entered!");
        }

        /*This checks that "lecture code"-field is not empty.*/
        if (this.lectureCode === "") {
            this.errorize("lCode", "Lecture name must be entered!");
        }

        /*This checks that either "Use date" or "Duration" is chosen for ending time.*/
        if (this.dateCheck === false && this.dueCheck === false) {
            this.errorize("endInfo", "A date or duration must be chosen.");
        }
        this.isNumber(this.maxStudents, "maxStudents");

        if (this.startTime !== null) {
            const lectureStartingInPast = moment().diff(this.startTime) >= 0;

            /* Checks that are run if end date is used*/
            if (this.useDate && this.endTime !== null) {
                if (this.endTime.diff(this.startTime) < 120000) {
                    this.errorize("endDateDiv", "Lecture has to last at least two minutes.");
                }
            }

            /* Check that are run if duration is used. */
            if (this.useDuration) {
                if (this.durationHour === "") {
                    this.durationHour = "00";
                }
                if (this.durationMin === "") {
                    this.durationMin = "00";
                }
                this.isPositiveNumber(this.durationHour, "durationHour");
                this.isPositiveNumber(this.durationMin, "durationMin");
                if (this.durationHour.length <= 0 && this.durationMin.length <= 0) {
                    this.errorize("durationDiv", "Please give a duration.");
                }
                this.endTime = moment(this.startTime)
                    .add(parseInt(this.durationHour), "hours")
                    .add(parseInt(this.durationMin));
                if (this.endTime.diff(this.startTime) < 120000) {
                    this.errorize("durationDiv", "Lecture has to last at least two minutes.");
                }
            }
            let alertMessage = "";
            const lectureEndingInPast = moment().diff(this.endTime) >= 0;
            /* Confirmations if lecture starts and/or ends before the current date */
            if (lectureStartingInPast && this.creatingNew()) {
                alertMessage += "Are you sure you want the lecture to start before now? ";
            }
            if (lectureEndingInPast && this.creatingNew()) {
                alertMessage += "Are you sure that the lecture ends in the past or now and will not run?";
            }
            $log.info(this.errorMessage.length);
            if (alertMessage !== "" && this.errorMessage.length <= 0) {
                if (!$window.confirm(alertMessage)) {
                    if (lectureStartingInPast) {
                        this.errorize("startInfo", "Please select another date and time.");
                    }
                    if (lectureEndingInPast) {
                        this.errorize("endInfo", "Please select another date or duration.");
                    }
                }
            }
        }
        /* If no errors save the lecture to the database */
        if (this.errorMessage.length <= 0) {
            if (this.earlyJoining) {
                this.startTime.subtract(15, "minutes");
            }
            const lectureParams: ILecture = {
                lecture_id: this.lectureId,
                doc_id: this.item.id,
                lecture_code: this.lectureCode,
                password: this.password,
                start_time: this.startTime,
                end_time: this.endTime,
                max_students: this.maxStudents,
            };
            const response = await $http<{lectureId: number}>({
                url: "/createLecture",
                method: "POST",
                params: lectureParams,
            });
            if (!this.creatingNew()) {
                $log.info("Lecture " + response.data.lectureId + " updated.");
            } else {
                $log.info("Lecture created: " + response.data.lectureId);
            }
            this.modal.close(lectureParams);
        }
    }

    /**
     * Changes the border of the element to red.
     * @param inputId The ID of the input field so user can be notified of the error.
     * @param errorText Error text that will be printed if the error occurs.
     * @memberof module:createLectureCtrl
     */
    errorize(inputId, errorText) {
        angular.element("#" + inputId).css("border", "1px solid red");
        if (errorText.length > 0) {
            this.errorMessage += errorText + "<br />";
        }
    }

    /**
     * Calls defInputStyle for all the form elements.
     * @memberof module:createLectureCtrl
     */
    removeErrors() {
        this.errorMessage = "";

        const elementsToRemoveErrorsFrom = [
            "lCode",
            "startInfo",
            "endInfo",
            "durationDiv",
            "durationHour",
            "durationMin",
            "maxStudents",
        ];
        for (let i = 0; i < elementsToRemoveErrorsFrom.length; i++) {
            if (elementsToRemoveErrorsFrom[i] !== undefined) {
                this.defInputStyle(elementsToRemoveErrorsFrom[i]);
            }
        }
    }

    /**
     *  Resets all the values of the form.
     *  @memberof module:createLectureCtrl
     */
    clearForm() {
        this.lectureCode = "";
        this.password = "";
        this.durationMin = "";
        this.durationHour = "";
        this.removeErrors();
        this.useDate = false;
        this.useDuration = true;
        this.dateChosen = false;
        this.durationChosen = false;
        this.dateCheck = false;
        this.lectureId = null;
    }

    /**
     * Function for cancelling the lecture creation.
     * @memberof module:createLectureCtrl
     */
    cancelCreation() {
        this.modal.dismiss(null);
    }
}

timApp.component("timCreateLecture", {
    controller: CreateLectureCtrl,
    controllerAs: "clctrl",
    templateUrl: "/static/templates/start_lecture.html",
});

export async function showLectureDialog(item: IItem, lecture: ILectureFormParams | null = null): Promise<ILecture> {
    const instance: IModalInstanceService = $uibModal.open({
        component: "timCreateLecture",
        resolve: {
            item: () => item,
            lecture: () => lecture,
        },
    });
    return await instance.closed;
}
