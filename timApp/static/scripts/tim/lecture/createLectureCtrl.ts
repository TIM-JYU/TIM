import angular, {IFormController, IRootElementService, IScope} from "angular";
import moment from "moment";
import {getItem, IItem} from "../item/IItem";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../ui/dialog";
import {DurationChoice} from "../ui/durationPicker";
import {$http} from "../util/ngimport";
import {dateFormat, to} from "../util/utils";
import {ILecture, ILectureFormParams, ILectureOptions} from "./lecturetypes";

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

function isLecture(item: unknown): item is ILecture {
    const l = item as ILecture;
    return l.end_time !== undefined &&
        l.is_access_code !== undefined &&
        l.lecture_code !== undefined &&
        l.doc_id !== undefined &&
        l.is_full !== undefined &&
        l.lecture_id !== undefined &&
        l.options !== undefined &&
        l.password !== undefined &&
        l.start_time !== undefined;
}

export class CreateLectureCtrl extends DialogController<{params: ILectureFormParams}, ILecture> {
    static component = "timCreateLecture";
    static $inject = ["$element", "$scope"] as const;
    private useDate: boolean;
    private durationAmount: number = 2;
    private durationType: DurationChoice = "hours";
    private lectureId?: number;
    private lectureCode: string;
    private password: string;
    private earlyJoining: boolean;
    private showEarlyJoin: boolean;
    private dateTimeOptions: EonasdanBootstrapDatetimepicker.SetOptions;
    private startTime: moment.Moment | undefined;
    private endTime: moment.Moment | undefined;
    private item?: IItem;
    private options: ILectureOptions;
    private form!: IFormController; // initialized in the template
    private submittingLecture = false;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
        this.showEarlyJoin = true;
        this.useDate = false;
        this.lectureCode = "";
        this.password = "";
        this.options = {
            max_students: 300, poll_interval: 4, poll_interval_t: 1,
            long_poll: false, long_poll_t: false,
        };

        this.dateTimeOptions = {
            format: dateFormat,
            showTodayButton: true,
        };
        this.startTime = moment();
        this.earlyJoining = true;
        this.enableDue2();
    }

    public getTitle() {
        return isLecture(this.resolve.params) ? "Edit lecture" : "Create lecture";
    }

    setLecture(data: ILecture) {
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

    async $onInit() {
        super.$onInit();
        this.item = isLecture(this.resolve.params) ? (await getItem(this.resolve.params.doc_id)) : this.resolve.params;
        if (isLecture(this.resolve.params)) {
            this.setLecture(this.resolve.params);
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
        if (!this.item) {
            return "";
        }
        return `${location.protocol}//${encodeURIComponent(location.host)}/lecture/${this.item.path}?lecture=${encodeURIComponent(this.lectureCode)}`;
    }

    getAutoJoinLink() {
        if (!this.item) {
            return "";
        }
        return `${location.protocol}//${encodeURIComponent(location.host)}/lecture/${this.item.path}?lecture=autojoin`;
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
        if (this.startTime == null || this.submittingLecture) {
            return;
        }
        if (!this.item) {
            await showMessageDialog("this.item was unexpectedly null");
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
        this.submittingLecture = true;
        const r = await
            to($http.post<ILecture>("/createLecture", lectureParams));
        this.submittingLecture = false;
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }
        this.close(r.result.data);
    }

    /**
     * Function for cancelling the lecture creation.
     */
    cancelCreation() {
        this.dismiss();
    }
}

registerDialogComponent(CreateLectureCtrl,
    {templateUrl: "/static/templates/start_lecture.html"},
    "clctrl");

export async function showLectureDialog(item: IItem | ILecture): Promise<ILecture> {
    return showDialog(CreateLectureCtrl, {
        params: () => item,
    }).result;
}
