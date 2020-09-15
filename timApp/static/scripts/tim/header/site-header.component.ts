import {Component} from "@angular/core";
import {getVisibilityVars} from "tim/timRoot";

@Component({
    selector: "tim-site-header",
    template: `
        <div class="siteheader" *ngIf="!hide.siteheader">
            <div class="hidden-lg hidden-md hamburger-placeholder" style="width: 27px; height: 1px"></div>
            <a class="logolink" title="To TIM main page" i18n-title *ngIf="!hide.links" href="/">
                <tim-logo *ngIf="!hide.logo"></tim-logo>
            </a>
            <tim-logo *ngIf="hide.links && !hide.logo"></tim-logo>
            <tim-login-menu *ngIf="!hide.login"></tim-login-menu>
            <img *ngIf="!hide.unilogo"
                 alt="Jyväskylän yliopisto"
                 src="/static/images/jyulogo.png"/>
            <button *ngIf="!hide.search"
                    class="timButton"
                    (click)="displaySearch = !displaySearch"
                    title="Display search panel"
                    i18n-title>
                <i class="glyphicon glyphicon-search"></i>&ngsp;
                <ng-container i18n>Search</ng-container>
            </button>
            <bootstrap-panel *ngIf="displaySearch" [showClose]="true"
                             class="search-panel"
                             title="TIM document search"
                             i18n-title
                             (closed)="displaySearch = !displaySearch">
                <tim-search-box></tim-search-box>
            </bootstrap-panel>
        </div>
    `,
    styleUrls: ["./site-header.component.scss"],
})
export class SiteHeaderComponent {
    hide = getVisibilityVars();
    displaySearch = false;
}
