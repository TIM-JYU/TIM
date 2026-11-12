import {Component, ChangeDetectionStrategy} from "@angular/core";
import {LectureController} from "tim/lecture/lectureController";

@Component({
    selector: "logged-users-tab",
    template: `
        <ng-template i18n>Lecture participants</ng-template>
        <h5 i18n>People logged in: <span>{{lctrl.lecturerTable.length + lctrl.studentTable.length}}</span></h5>
        <h5 i18n>Lecturers (<span>{{lctrl.lecturerTable.length}}</span>)</h5>
        @if (lctrl.lecturerTable.length > 0) {
          <ul>
            @for (lecturer of lctrl.lecturerTable; track lecturer) {
              <li>
                {{lecturer.user.name}} > {{lecturer.activeSecondsAgo | timeSince}}
              </li>
            }
          </ul>
        } @else {
          <p i18n>No lecturers</p>
        }
        <h5 i18n>Students (<span>{{lctrl.studentTable.length}}</span>)</h5>
        @if (lctrl.studentTable.length > 0) {
          <ul>
            @for (student of lctrl.studentTable; track student) {
              <li>
                {{student.user.name}} > {{student.activeSecondsAgo | timeSince}}
              </li>
            }
          </ul>
        } @else {
          <p i18n>No students</p>
        }
        `,
    styles: [],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class LoggedUsersTabComponent {
    lctrl: LectureController = LectureController.instance;

    constructor() {}
}
