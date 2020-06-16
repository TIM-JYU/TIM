import {Component, Input, OnInit} from "@angular/core";
import {IMenuTab, TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import {LectureController} from "tim/lecture/lectureController";

@Component({
    selector: "load-questions-tab",
    template: `
        <ng-template i18n="@@loadQuestionsTabTitle">Get question</ng-template>
        <ng-container i18n>Loading question manually...</ng-container>
    `,
})
export class LoadQuestionsTabComponent implements IMenuTab {
    @Input() entry!: TabEntry;

    constructor() {
    }

    onSelect() {
        void LectureController.instance.getQuestionManually();
    }
}
