import {Component, Input, OnInit} from "@angular/core";
import {IMenuTab, TabEntry} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "lecture-info-tab",
    template: `
        <h5 i18n>Current Lectures</h5>
        <ul>
            <li>
                <a href="#">404</a>
                <button class="timButton btn-xs" value="Join" i18n-value i18n>Join</button>
            </li>
            <li><p i18n>No current lectures</p></li>
        </ul>

        <h5 i18n>Upcoming Lectures</h5>
        <ul>
            <li>
                <a href="#">405</a>
            </li>
            <li><p i18n>No upcoming lectures</p></li>
        </ul>

        <h5 i18n>Past Lectures</h5>
        <ul>
            <li>
                <a href="#">406</a>
            </li>
            <li><p i18n>No past lectures</p></li>
        </ul>
    `,
})
export class LectureInfoTabComponent implements OnInit, IMenuTab {
    @Input() entry!: TabEntry;

    constructor() {
    }

    ngOnInit(): void {
    }
}
