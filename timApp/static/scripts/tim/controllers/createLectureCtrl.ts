import angular, {IFormController} from "angular";
import moment from "moment";
import {DurationChoice} from "../components/durationPicker";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../dialog";
import {IItem} from "../IItem";
import {ILecture, ILectureFormParams, ILectureOptions} from "../lecturetypes";
import {$http} from "../ngimport";

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

export class CreateLectureCtrl extends DialogController<{item: IItem, lecture: ILectureFormParams | null}, ILecture, "timCreateLecture"> {
    private useDate: boolean;
    private durationAmount: number;
    private durationType: DurationChoice;
    private lectureId?: number;
    private lectureCode: string;
    private password: string;
    private earlyJoining: boolean;
    private showEarlyJoin: boolean;
    private dateTimeOptions: EonasdanBootstrapDatetimepicker.SetOptions;
    private startTime: moment.Moment | undefined;
    private endTime: moment.Moment | undefined;
    private item: IItem;
    private options: ILectureOptions;
    private form: IFormController;

    constructor() {
        super();
        this.showEarlyJoin = true;
        this.useDate = false;
        this.lectureCode = "";
        this.password = "";
        this.options = {
            max_students: 100, poll_interval: 4, poll_interval_t: 1,
            long_poll: false, long_poll_t: false,
        };

        this.dateTimeOptions = {
            format: "D.M.YYYY HH:mm:ss",
            showTodayButton: true,
        };
        this.startTime = moment();
        this.earlyJoining = true;
        this.enableDue2();
    }

    public getTitle() {
        return "Create lecture";
    }

    setLecture(data: ILectureFormParams) {
        this.showEarlyJoin = false;
        this.earlyJoining = false;
        this.lectureCode = data.lecture_code;
        this.lectureId = data.lecture_id;
        this.startTime = data.start_time;
        this.endTime = data.end_time;
        this.useDate = true;
        this.enableDate2();
        if (data.password != null) {
            this.password = data.password;
        }
        this.options = data.options;
    }

    $onInit() {
        super.$onInit();
        this.item = this.resolve.item;
        if (this.resolve.lecture != null) {
            this.setLecture(this.resolve.lecture);
        }
    }

    handleKey(event: KeyboardEvent) {
        if (event.ctrlKey || event.metaKey) {
            switch (String.fromCharCode(event.which).toLowerCase()) {
                case "s":
                    event.preventDefault();
                    this.submitLecture();
                    break;
            }
        }
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
     */
    enableDate2() {
        if (this.endTime == null) {
            this.endTime = moment(this.startTime).add(2, "hours");
        }
    }

    /**
     * Function for enabling fields and buttons for "Duration" and disabling them for "Use date". This function
     * is called when duration is chosen and is chosen by default.
     */
    enableDue2() {
        if (!this.durationType) {
            this.durationType = "hours";
            this.durationAmount = 2;
        }
    }

    selectText(e: Event) {
        (e.target as HTMLInputElement).select();
    }

    /**
     * Remove border from given element.
     * @param element ID of the field whose border will be removed.
     */
    defInputStyle(element: string) {
        angular.element("#" + element).css("border", "");
    }

    creatingNew() {
        return this.lectureId == null;
    }

    getEndTime() {
        if (this.useDate) {
            return this.endTime;
        } else {
            if (this.startTime == null || this.durationAmount == null || this.durationType == null) {
                return undefined;
            }
            return moment(this.startTime)
                .add(this.durationAmount, this.durationType);
        }
    }

    lectureTooShort() {
        const endTime = this.getEndTime();
        if (!endTime) {
            return false;
        }
        return endTime.diff(this.startTime) < 120000;
    }

    startingBeforeNow() {
        return this.startTime != null && moment().diff(this.startTime) >= 0;
    }

    endingBeforeNow() {
        return this.getEndTime() != null && moment().diff(this.getEndTime()) >= 0;
    }

    /**
     * Function for creating a new lecture and validation.
     */
    async submitLecture() {
        if (this.startTime == null) {
            return;
        }
        if (this.lectureTooShort()) {
            await showMessageDialog("Lecture must last at least two minutes.");
            return;
        }

        this.endTime = this.getEndTime();

        /* If no errors save the lecture to the database */
        if (this.earlyJoining) {
            this.startTime.subtract(15, "minutes");
        }
        const lectureParams = {
            lecture_id: this.lectureId,
            doc_id: this.item.id,
            lecture_code: this.lectureCode,
            password: this.password,
            start_time: this.startTime,
            end_time: this.endTime,
            options: this.options,
        };
        const response = await
            $http.post<ILecture>("/createLecture", lectureParams);
        this.close(response.data);
    }

    /**
     * Function for cancelling the lecture creation.
     */
    cancelCreation() {
        this.dismiss();
    }
}

registerDialogComponent("timCreateLecture",
    CreateLectureCtrl,
    {templateUrl: "/static/templates/start_lecture.html"},
    "clctrl");

export async function showLectureDialog(item: IItem, lecture: ILectureFormParams | null = null): Promise<ILecture> {
    return showDialog<CreateLectureCtrl>("timCreateLecture", {
        item: () => item,
        lecture: () => lecture,
    }).result;
}
