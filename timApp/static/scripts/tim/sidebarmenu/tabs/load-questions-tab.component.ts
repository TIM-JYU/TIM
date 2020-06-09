import {Component, Input, OnInit} from "@angular/core";
import {IMenuTab, TabEntry} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "load-questions-tab",
    template: `
        Loading question manually...
    `,
})
export class LoadQuestionsTabComponent implements OnInit, IMenuTab {
    @Input() entry!: TabEntry;

    constructor() {
    }

    ngOnInit(): void {
    }

}
