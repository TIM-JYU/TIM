import moment from "moment";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {Component} from "@angular/core";
import {KEY_S} from "tim/util/keycodes";
import {getItem, IItem} from "../item/IItem";
import {showMessageDialog} from "../ui/dialog";
import {DurationChoice} from "../ui/duration-picker.component";
import {$http} from "../util/ngimport";
import {to} from "../util/utils";
import {ILecture, ILectureFormParams, ILectureOptions} from "./lecturetypes";

/**
 * Lecture creation controller which is used to handle and validate the form data.
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
    return (
        l.end_time !== undefined &&
        l.is_access_code !== undefined &&
        l.lecture_code !== undefined &&
        l.doc_id !== undefined &&
        l.is_full !== undefined &&
        l.lecture_id !== undefined &&
        l.options !== undefined &&
        l.password !== undefined &&
        l.start_time !== undefined
    );
}

@Component({
    selector: "tim-lecture-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <form #form="ngForm" (keydown)="handleKey($event)">
                    <div class="form-group form-horz-flex" timErrorState>
                        <label for="lCode">Lecture name</label>
                        <input type="text" class="form-control" name="code" id="lCode" required
                               [(ngModel)]="lectureCode">
                        <tim-error-message></tim-error-message>
                    </div>
                    <div class="form-group form-horz-flex">
                        <label for="lPassword">Access code</label>
                        <input class="form-control" id="lPassword" type="text" name="password" [(ngModel)]="password"
                               placeholder="Optional"/>
                    </div>
                    <div class="form-group form-horz-flex" timErrorState>
                        <label for="maxStudents">Max number of students</label>
                        <input class="form-control" max="999" min="0" id="maxStudents" type="number" name="max_students"
                               [(ngModel)]="options.max_students" placeholder="Optional"/>
                        <tim-error-message></tim-error-message>
                    </div>
                    <div class="form-group form-horz-flex">
                        <label for="startTime" class="control-label">Starting time</label>
                        <tim-datetime-picker id="startTime" [(time)]="startTime"></tim-datetime-picker>
                    </div>
                    <tim-alert severity="warning" *ngIf="startingBeforeNow()">Lecture start time is set in the past.
                    </tim-alert>
                    <div class="checkbox" *ngIf="showEarlyJoin">
                        <label><input type="checkbox" name="earlyJoining"
                                      [(ngModel)]="earlyJoining">
                            Allow students to join 15 minutes prior to the start of the lecture.</label>
                    </div>
                    <div class="form-group form-horz-flex">
                        <label>
                            <input type="radio" name="useDate" (ngModelChange)="enableDate2()"
                                   [(ngModel)]="useDate" [value]="true"/>
                            Ending time
                        </label>
                        <tim-datetime-picker *ngIf="useDate" [(time)]="endTime"></tim-datetime-picker>
                    </div>

                    <div class="form-horz-flex">
                        <label>
                            <input type="radio" name="duration"
                                   [(ngModel)]="useDate" [value]="false"/>
                            Duration
                        </label>
                        <tim-duration-picker [disabled]="useDate"
                                             [required]="!useDate"
                                             [(amount)]="durationAmount"
                                             [(type)]="durationType"></tim-duration-picker>
                    </div>
                    <tim-alert *ngIf="lectureTooShort()">Lecture must last at least two minutes.</tim-alert>
                    <tim-alert severity="warning" *ngIf="endingBeforeNow()">Lecture end time is set in the past.
                    </tim-alert>
                    <div class="form-group">
                        <label for="lLink">Direct link to join lecture:</label>
                        <input class="form-control" (focus)="selectText($event)"
                               readonly id="lLink" type="text"
                               [value]="getJoinLectureLink()"/>
                    </div>
                    <div class="form-group">
                        <label for="lLink2">Link to automatically join lectures for this document:</label>
                        <input class="form-control" (focus)="selectText($event)"
                               readonly id="lLink2" type="text"
                               [value]="getAutoJoinLink()"/>
                    </div>
                    <accordion>
                        <accordion-group [(isOpen)]="advancedOpen">
                            <div accordion-heading>
                                Advanced settings
                                <i class="pull-right glyphicon"
                                   [ngClass]="{'glyphicon-chevron-down': advancedOpen, 'glyphicon-chevron-right': !advancedOpen}"></i>
                            </div>
                            <div class="form-inline">
                                <label>Student poll interval: <input id="poll_interval"
                                                                     type="text"
                                                                     name="poll_interval"
                                                                     maxlength="3"
                                                                     class="form-control"
                                                                     size="3"
                                                                     [(ngModel)]="options.poll_interval"
                                                                     pattern="[0-9]*"> s</label>&ngsp;
                                <div class="checkbox"><label><input id="long_poll" type="checkbox" name="long_poll"
                                                                    [(ngModel)]="options.long_poll"> Use long
                                    polling</label></div>
                            </div>
                            <div class="form-inline">
                                <label>Teacher poll interval: <input id="poll_interval_t"
                                                                     type="text"
                                                                     name="poll_interval_t"
                                                                     maxlength="3" size="3"
                                                                     class="form-control"
                                                                     [(ngModel)]="options.poll_interval_t"
                                                                     pattern="[0-9]*"> s</label>&ngsp;
                                <div class="checkbox"><label><input id="long_poll_t" type="checkbox" name="long_poll_t"
                                                                    [(ngModel)]="options.long_poll_t"> Use long
                                    polling</label></div>
                            </div>
                            <div class="form-group">
                                <label for="teacher_poll">Use teacher poll for</label>
                                <input id="teacher_poll"
                                       type="text"
                                       name="teacher_poll"
                                       maxlength="40"
                                       class="form-control"
                                       size="40"
                                       placeholder="List of usernames to get same poll rate as teacher, separate by ;"
                                       [(ngModel)]="options.teacher_poll">
                            </div>
                        </accordion-group>
                    </accordion>
                </form>
            </ng-container>
            <ng-container footer>
                <button class="timButton"
                        (click)="submitLecture()"
                        [disabled]="!form.valid || submittingLecture">{{creatingNew() ? 'Create' : 'Update'}}
                </button>
                <button class="timButton"
                        (click)="dismiss()">Cancel
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class LectureDialogComponent extends AngularDialogComponent<
    ILectureFormParams,
    ILecture
> {
    protected dialogName = "lecture";
    useDate: boolean;
    durationAmount: number = 2;
    durationType: DurationChoice = "hours";
    private lectureId?: number;
    lectureCode: string;
    password: string;
    earlyJoining: boolean;
    showEarlyJoin: boolean;
    startTime: Date;
    endTime: Date;
    private item?: IItem;
    options: ILectureOptions;
    submittingLecture = false;
    advancedOpen = false;
    showPicker = false;

    constructor() {
        super();
        this.showEarlyJoin = true;
        this.useDate = false;
        this.lectureCode = "";
        this.password = "";
        this.options = {
            max_students: 300,
            poll_interval: 4,
            poll_interval_t: 1,
            long_poll: false,
            long_poll_t: false,
            teacher_poll: "",
        };

        this.startTime = new Date();
        this.endTime = new Date();
        this.earlyJoining = true;
    }

    public getTitle() {
        return isLecture(this.data) ? "Edit lecture" : "Create lecture";
    }

    setLecture(data: ILecture) {
        this.showEarlyJoin = false;
        this.earlyJoining = false;
        this.lectureCode = data.lecture_code;
        this.lectureId = data.lecture_id;
        console.log(data.start_time.toISOString());
        this.startTime = data.start_time.toDate();
        this.endTime = data.end_time.toDate();
        this.useDate = true;
        if (data.password != null) {
            this.password = data.password;
        }
        this.options = data.options;
    }

    ngOnInit() {
        (async () => {
            this.item = isLecture(this.data)
                ? await getItem(this.data.doc_id)
                : this.data;
            if (isLecture(this.data)) {
                this.setLecture(this.data);
            }
        })();
    }

    async handleKey(event: KeyboardEvent) {
        if (event.ctrlKey || event.metaKey) {
            switch (event.which) {
                case KEY_S:
                    event.preventDefault();
                    await this.submitLecture();
                    break;
            }
        }
    }

    getJoinLectureLink() {
        if (!this.item) {
            return "";
        }
        return `${location.protocol}//${encodeURIComponent(
            location.host
        )}/lecture/${this.item.path}?lecture=${encodeURIComponent(
            this.lectureCode
        )}`;
    }

    getAutoJoinLink() {
        if (!this.item) {
            return "";
        }
        return `${location.protocol}//${encodeURIComponent(
            location.host
        )}/lecture/${this.item.path}?lecture=autojoin`;
    }

    /**
     * This function is called if end date is selected. Sets boolean values to reflect this choice and sets
     * the default end time to 2 hours ahead of start time.
     */
    enableDate2() {
        if (this.useDate) {
            this.endTime = moment(this.startTime).add(2, "hours").toDate();
        }
    }

    selectText(e: Event) {
        (e.target as HTMLInputElement).select();
    }

    creatingNew() {
        return this.lectureId == null;
    }

    getEndTime() {
        if (this.useDate) {
            return moment(this.endTime);
        } else {
            if (
                this.startTime == null ||
                this.durationAmount == null ||
                this.durationType == null
            ) {
                return undefined;
            }
            return moment(this.startTime).add(
                this.durationAmount,
                this.durationType
            );
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
        return (
            this.getEndTime() != null && moment().diff(this.getEndTime()) >= 0
        );
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

        const endTime = this.getEndTime();

        /* If no errors save the lecture to the database */
        let startTime = this.startTime;
        if (this.earlyJoining) {
            startTime = moment(this.startTime).subtract(15, "minutes").toDate();
        }
        const lectureParams = {
            lecture_id: this.lectureId,
            doc_id: this.item.id,
            lecture_code: this.lectureCode,
            password: this.password,
            start_time: startTime,
            end_time: endTime,
            options: this.options,
        };
        this.submittingLecture = true;
        const r = await to(
            $http.post<ILecture>("/createLecture", lectureParams)
        );
        this.submittingLecture = false;
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }
        this.close(r.result.data);
    }
}

export async function showLectureDialog(item: IItem | ILecture) {
    return (await angularDialog.open(LectureDialogComponent, item)).result;
}
