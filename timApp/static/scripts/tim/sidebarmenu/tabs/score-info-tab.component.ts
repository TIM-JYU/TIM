import {Component, Input, OnInit} from "@angular/core";
import {IMenuTab, TabEntry} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "score-info-tab",
    template: `
        <ng-template i18n="@@scoreInfoTabTitle">Scoreboard</ng-template>
        <p>
            score-info-tab works!
        </p>
    `,
})
export class ScoreInfoTabComponent implements OnInit, IMenuTab {
    @Input() entry!: TabEntry;

    constructor() {
    }

    ngOnInit(): void {
    }
}
