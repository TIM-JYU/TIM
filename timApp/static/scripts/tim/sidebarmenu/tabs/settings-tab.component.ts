import {Component, Input, OnInit} from "@angular/core";
import {IMenuTab, TabEntry} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "settings-tab",
    template: `
        <ng-container>
            <h5 i18n>Help</h5>
            <a i18n-title title="Open TIM-guide" href="/view/tim/TIM-ohjeet" i18n>User guide</a>
        </ng-container>
        <ng-container>
            <h5>Customize</h5>
            <a href="/settings" i18n>Customize TIM</a>
        </ng-container>
        <ng-container>
            <h5>Folder settings</h5>
            <button class="timButton btn-block" i18n-title title="Set item relevance value" i18n>
                Edit relevance (<span i18n-tooltip tooltip="Current relevance value">???</span>)
            </button>
        </ng-container>
        <ng-container>
            <h5 i18n>Document settings</h5>
            <a i18n-title title="Toggle between showing full and partitioned document">
                Show page in parts/full
            </a>
            <a class="same-line" i18n-title title="Open document partitioning settings">
                <span class="glyphicon glyphicon-cog"></span>
            </a>

            <button class="timButton btn-block" i18n>Edit settings</button>
            <button class="timButton btn-block" title="Set item relevance value" i18n-title i18n>
                Edit relevance (<span i18n-tooltip tooltip="Current relevance value">???</span>)
            </button>
            <button class="timButton btn-block"
                    title="Mark all paragraphs of the document as read"
                    i18n-title
                    i18n>Mark all as read
            </button>
            <button class="timButton btn-block"
                    title="Mark all paragraphs of the document as read"
                    i18n-title
                    i18n>Mark all as translated
            </button>
        </ng-container>
        <ng-container>
            <h5 i18n>Lecture settings</h5>
            <div class="checkbox">
                <label i18n><input type="checkbox"> Show wall</label>
            </div>
            <div class="checkbox">
                <label i18n><input type="checkbox"> Show questions</label>
            </div>
            <div class="checkbox">
                <label i18n><input type="checkbox"> Show answers</label>
            </div>
            <div class="checkbox">
                <label i18n><input type="checkbox"> Show 'not polling' dialog</label>
            </div>
        </ng-container>

        <!--        TODO: check rights for given options-->
        <ng-container>
            <h5 class="same-line" i18n>Print document</h5>
            <a class="same-line" href="https://tim.jyu.fi/view/tim/ohjeita/tulostusohje">
                <span class="glyphicon glyphicon-question-sign" title="Printing help" i18n-title></span>
            </a>
            <button class="timButton btn-block"
                    title="Print using LaTeX (best quality)"
                    i18n-title
                    i18n>Print document
            </button>
            <button class="timButton btn-block"
                    title="Print via browser's print dialog"
                    i18n-title
                    i18n>Browser print
            </button>

            <h5 class="same-line" i18n>Document tags</h5>
            <a class="same-line" href="https://tim.jyu.fi/view/tim/ohjeita/opettajan-ohje#kurssikoodi">
                    <span class="glyphicon glyphicon-question-sign" title="Teachers' help for course code"
                          i18n-title></span>
            </a>
            <button class="timButton btn-block"
                    title="Search with tags"
                    i18n-title
                    i18n>Search with tags
            </button>
            <button class="timButton btn-block"
                    title="Set document as a course main page"
                    i18n-title
                    i18n>Set as course
            </button>

            <h5 class="same-line" i18n>Memos/Minutes</h5>
            <button class="timButton btn-block"
                    title="Create extracts"
                    i18n-title
                    i18n>Create extracts
            </button>
            <button class="timButton btn-block"
                    title="Create minutes"
                    i18n-title
                    i18n>Create minutes
            </button>
            <button class="timButton btn-block"
                    title="Display attachments, check their validity, and merge them into single file."
                    i18n-title
                    i18n>Merge attachments
            </button>
        </ng-container>

        <ng-container>
            <h5 i18n>Groups</h5>
            <ng-container>
                <h6 i18n>Linked course groups</h6>
                <!--                <h6 i18n>Linked <a>course groups</a></h6> TODO: ngIfElse-->
                <ul class="list-unstyled">
                    <li>TODO: ngFor</li>
                </ul>
            </ng-container>
            <ng-container>
                <button class="timButton btn-block"
                        title="Create a new group"
                        i18n-title
                        i18n>Create a new group
                </button>
                <a href="/view/groups" i18n>Browse existing groups</a>
            </ng-container>
        </ng-container>
    `,
})
export class SettingsTabComponent implements OnInit, IMenuTab {
    @Input() entry!: TabEntry;

    constructor() {
    }

    ngOnInit(): void {
    }
}
