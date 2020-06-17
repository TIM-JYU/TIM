import {Component} from "@angular/core";
import {OnTabSelect} from "tim/sidebarmenu/menu-tab.directive";
import {ILecture, ILectureListResponse2} from "tim/lecture/lecturetypes";
import {ViewCtrl} from "tim/document/viewctrl";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {LectureController} from "tim/lecture/lectureController";
import {showMessageDialog} from "tim/ui/dialog";
import {to2} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";

@Component({
    selector: "lecture-info-tab",
    template: `
        <ng-template i18n="@@lectureInfoTabTitle">Lecture</ng-template>
        <h5 i18n>Current Lectures</h5>
        <ul>
            <li *ngFor="let lecture of currentLecturesList">
                <a href="/showLectureInfo/{{lecture.lecture_id}}">{{lecture.lecture_code}}</a>
                <button class="timButton btn-xs" value="Join" i18n-value i18n>Join</button>
            </li>
            <li *ngIf="currentLecturesList.length == 0"><p i18n>No current lectures</p></li>
        </ul>

        <h5 i18n>Upcoming Lectures</h5>
        <ul>
            <li *ngFor="let lecture of futureLecturesList">
                <a href="/showLectureInfo/{{lecture.lecture_id}}">{{lecture.lecture_code}}</a>
            </li>
            <li *ngIf="futureLecturesList.length == 0"><p i18n>No upcoming lectures</p></li>
        </ul>

        <h5 i18n>Past Lectures</h5>
        <ul>
            <li *ngFor="let lecture of pastLecturesList">
                <a href="/showLectureInfo/{{lecture.lecture_id}}">{{lecture.lecture_code}}</a>
            </li>
            <li *ngIf="pastLecturesList.length == 0"><p i18n>No past lectures</p></li>
        </ul>

        <ng-template i18n="@@notInDocViewError">Not currently in document view.</ng-template>
    `,
})
export class LectureInfoTabComponent implements OnTabSelect {
    currentLecturesList: ILecture[] = [];
    futureLecturesList: ILecture[] = [];
    pastLecturesList: ILecture[] = [];
    vctrl?: ViewCtrl = vctrlInstance;
    lctrl: LectureController = LectureController.instance;

    constructor(private http: HttpClient) {
    }

    onSelect() {
        void this.toggleLectures();
    }

    private async toggleLectures() {
        if (!this.vctrl) {
            await showMessageDialog($localize`:@@notInDocViewError:Not currently in document view.`);
            return;
        }
        const response = await to2(this.http.get<ILectureListResponse2>("/getAllLecturesFromDocument", {
            params: {
                doc_id: this.vctrl.docId.toString(),
            },
        }).toPromise());
        if (!response.ok) {
            return;
        }
        const lectures = response.result;
        this.currentLecturesList = lectures.currentLectures;
        this.futureLecturesList = lectures.futureLectures;
        this.pastLecturesList = lectures.pastLectures;
    }
}
