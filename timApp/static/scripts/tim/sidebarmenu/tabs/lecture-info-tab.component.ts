import {Component, ChangeDetectionStrategy} from "@angular/core";
import type {OnTabSelect} from "tim/sidebarmenu/menu-tab.directive";
import type {ILecture, ILectureListResponse2} from "tim/lecture/lecturetypes";
import type {ViewCtrl} from "tim/document/viewctrl";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {LectureController} from "tim/lecture/lectureController";
import {toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {showMessageDialog} from "tim/ui/showMessageDialog";

@Component({
    selector: "lecture-info-tab",
    template: `
        <ng-template i18n>Lecture</ng-template>
        <h5 i18n>Current Lectures</h5>
        <ul>
          @for (lecture of currentLecturesList; track lecture) {
            <li>
              <a class="space-right" href="/showLectureInfo/{{lecture.lecture_id}}">{{lecture.lecture_code}}</a>
              @if (lctrl.lecture?.lecture_id != lecture.lecture_id) {
                <button
                  class="timButton btn-xs"
                  value="Join" i18n-value
                  (click)="lctrl.joinLecture(lecture)"
                  i18n>Join
                </button>
              }
            </li>
          }
          @if (currentLecturesList.length == 0) {
            <li><p i18n>No current lectures</p></li>
          }
        </ul>
        
        <h5 i18n>Upcoming Lectures</h5>
        <ul>
          @for (lecture of futureLecturesList; track lecture) {
            <li>
              <a href="/showLectureInfo/{{lecture.lecture_id}}">{{lecture.lecture_code}}</a>
            </li>
          }
          @if (futureLecturesList.length == 0) {
            <li><p i18n>No upcoming lectures</p></li>
          }
        </ul>
        
        <h5 i18n>Past Lectures</h5>
        <ul>
          @for (lecture of pastLecturesList; track lecture) {
            <li>
              <a href="/showLectureInfo/{{lecture.lecture_id}}">{{lecture.lecture_code}}</a>
            </li>
          }
          @if (pastLecturesList.length == 0) {
            <li><p i18n>No past lectures</p></li>
          }
        </ul>
        `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class LectureInfoTabComponent implements OnTabSelect {
    currentLecturesList: ILecture[] = [];
    futureLecturesList: ILecture[] = [];
    pastLecturesList: ILecture[] = [];
    vctrl?: ViewCtrl = vctrlInstance;
    lctrl: LectureController = LectureController.instance;

    constructor(private http: HttpClient) {}

    onSelect() {
        void this.toggleLectures();
    }

    private async toggleLectures() {
        if (!this.vctrl) {
            await showMessageDialog($localize`Not currently in document view.`);
            return;
        }
        const response = await toPromise(
            this.http.get<ILectureListResponse2>(
                "/getAllLecturesFromDocument",
                {
                    params: {
                        doc_id: this.vctrl.docId.toString(),
                    },
                }
            )
        );
        if (!response.ok) {
            return;
        }
        const lectures = response.result;
        this.currentLecturesList = lectures.currentLectures;
        this.futureLecturesList = lectures.futureLectures;
        this.pastLecturesList = lectures.pastLectures;
    }
}
