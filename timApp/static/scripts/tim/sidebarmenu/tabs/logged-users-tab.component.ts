import {Component, Input, OnInit} from "@angular/core";
import {IMenuTab, TabEntry} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "logged-users-tab",
    template: `
        <h5 i18n>People logged in: <span>None</span></h5>

        <h5 i18n>Lecturers (<span>None</span>)</h5>
        <ul>
            <li>Nönnönnöö > ActiveDate</li>
        </ul>
        <p i18n>No lecturers</p>

        <h5 i18n>Students (<span>None</span>)</h5>
        <ul>
            <li>John Doe > ActiveDate</li>
        </ul>
        <p i18n>No students</p>
    `,
    styles: [],
})
export class LoggedUsersTabComponent implements OnInit, IMenuTab {
    @Input() entry!: TabEntry;

    constructor() {
    }

    ngOnInit(): void {
    }
}
