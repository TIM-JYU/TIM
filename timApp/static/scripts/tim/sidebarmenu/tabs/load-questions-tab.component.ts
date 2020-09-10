import {Component} from "@angular/core";
import {OnTabSelect} from "tim/sidebarmenu/menu-tab.directive";
import {LectureController} from "tim/lecture/lectureController";

@Component({
    selector: "load-questions-tab",
    template: `
        <ng-template i18n="@@loadQuestionsTabTitle">Get question</ng-template>
        <ng-container i18n>Loading question manually...</ng-container>
    `,
})
export class LoadQuestionsTabComponent implements OnTabSelect {
    constructor() {}

    onSelect() {
        void LectureController.instance.getQuestionManually();
    }
}
