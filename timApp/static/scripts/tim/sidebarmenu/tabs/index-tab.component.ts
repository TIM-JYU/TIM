import {Component, Input, OnInit} from "@angular/core";
import {IMenuTab, TabEntry} from "tim/sidebarmenu/menu-tab.directive";

@Component({
    selector: "index-tab",
    template: `
        <h5 i18n>Index <a href="#" title="Go to top" class="pull-right">Go to top</a></h5>
        <ul class="subexp">
            <li>
                <a class="exptoggle">
                    <i class="glyphicon glyphicon-plus"></i>  <!-- And glyphicon-minus -->
                </a>
                <i class="vis-hidden glyphicon glyphicon-plus"></i>  <!-- TODO: Is this needed? -->
                <a class="a1" href="#" target="_self">TODO</a>
                <ul class="list-unstyled" (click)="$event.stopPropagation()">
                    <li>
                        <a class="a2" href="#" target="_self">TODO2</a>
                    </li>
                </ul>
            </li>
        </ul>
    `,
})
export class IndexTabComponent implements OnInit, IMenuTab {
    @Input() entry!: TabEntry;

    constructor() {
    }

    ngOnInit(): void {
    }


}
