import {Component} from "@angular/core";
import {LectureController} from "tim/lecture/lectureController";

@Component({
    selector: "logged-users-tab",
    template: `
        <ng-template i18n>Lecture participants</ng-template>
        <h5 i18n>People logged in: <span>{{lctrl.lecturerTable.length + lctrl.studentTable.length}}</span></h5>
        <h5 i18n>Lecturers (<span>{{lctrl.lecturerTable.length}}</span>)</h5>
        <ng-container *ngIf="lctrl.lecturerTable.length > 0; else noLecturers">
            <ul>
                <li *ngFor="let lecturer of lctrl.lecturerTable">
                    {{lecturer.user.name}} > {{lecturer.activeSecondsAgo | timeSince}}
                </li>
            </ul>
        </ng-container>
        <ng-template #noLecturers>
            <p i18n>No lecturers</p>
        </ng-template>
        <h5 i18n>Students (<span>{{lctrl.studentTable.length}}</span>)</h5>
        <ng-container *ngIf="lctrl.studentTable.length > 0; else noStudents">
            <ul>
                <li *ngFor="let student of lctrl.studentTable">
                    {{student.user.name}} > {{student.activeSecondsAgo | timeSince}}
                </li>
            </ul>
        </ng-container>
        <ng-template #noStudents>
            <p i18n>No students</p>
        </ng-template>
    `,
    styles: [],
})
export class LoggedUsersTabComponent {
    lctrl: LectureController = LectureController.instance;

    constructor() {}
}
