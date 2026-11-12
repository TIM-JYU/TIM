import {Component, ChangeDetectionStrategy} from "@angular/core";
import type {OnTabSelect} from "tim/sidebarmenu/menu-tab.directive";
import {LectureController} from "tim/lecture/lectureController";

@Component({
    selector: "load-questions-tab",
    template: `
        <ng-template i18n>Get question</ng-template>
        <ng-container i18n>Loading question manually...</ng-container>
    `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class LoadQuestionsTabComponent implements OnTabSelect {
    constructor() {}

    onSelect() {
        void LectureController.instance.getQuestionManually();
    }
}
