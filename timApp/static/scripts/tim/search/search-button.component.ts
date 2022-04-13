import {Component, HostBinding, Input} from "@angular/core";

@Component({
    selector: "tim-search-button",
    template: `
        <button
                class="timButton button-search"
                (click)="displaySearch = !displaySearch"
                title="Display search panel"
                i18n-title>
            <i class="glyphicon glyphicon-search"></i>&nbsp;
            <span i18n>Search</span>
        </button>
        <bootstrap-panel *ngIf="displaySearch" [showClose]="true"
                         class="search-panel"
                         title="TIM document search"
                         i18n-title
                         (closed)="displaySearch = !displaySearch">
            <tim-search-box [folder]="folder"></tim-search-box>
        </bootstrap-panel>
    `,
    host: {},
    styleUrls: ["search-button.component.scss"],
})
export class SearchButtonComponent {
    displaySearch = false;
    @HostBinding("class.floating") @Input() floating = false;
    @Input() folder?: string;
}
