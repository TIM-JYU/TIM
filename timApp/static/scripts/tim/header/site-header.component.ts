import {Component, OnInit} from "@angular/core";
import {getVisibilityVars} from "tim/timRoot";
import {getViewName, TimStorage} from "tim/util/utils";
import {getAvailableViews} from "tim/header/utils";
import * as t from "io-ts";

@Component({
    selector: "tim-site-header",
    template: `
        <div class="siteheader" *ngIf="!hide.siteheader">
            <div class="hidden-lg hidden-md hamburger-placeholder" style="width: 27px; height: 1px"></div>
            <a class="logolink" title="To TIM main page" i18n-title *ngIf="!hide.links" href="/">
                <tim-logo *ngIf="!hide.logo"></tim-logo>
            </a>
            <a class="view-toggle"
               *ngIf="activeView"
               (click)="updateHeaderMenuVisibility()"
                title="Show/hide view menu"
                i18n-title>
                <span>{{activeView}}</span>
                <i class="glyphicon"
                   [class.glyphicon-triangle-bottom]="displayHeader"
                   [class.glyphicon-triangle-top]="!displayHeader"></i>
            </a>
            <tim-logo *ngIf="hide.links && !hide.logo"></tim-logo>
            <tim-login-menu *ngIf="!hide.login"></tim-login-menu>
            <img *ngIf="!hide.unilogo"
                 class="jyu-logo"
                 alt="University of Jyväskylä"
                 i18n-alt
                 src="/static/images/jyulogo.png"/>
            <button *ngIf="!hide.search"
                    class="timButton button-search"
                    (click)="displaySearch = !displaySearch"
                    title="Display search panel"
                    i18n-title>
                <i class="glyphicon glyphicon-search"></i>
                <span i18n>Search</span>
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
export class SiteHeaderComponent implements OnInit {
    hide = getVisibilityVars();
    displaySearch = false;
    activeView?: string;
    displayHeader: boolean = false;
    private lastHeaderHiddenState = new TimStorage(
        "siteHeader_lastHeaderHiddenState",
        t.boolean
    );

    updateHeaderMenuVisibility(newState?: boolean) {
        const el = document.querySelector<HTMLElement>(".doc-header");
        if (!el) {
            return;
        }
        if (newState === undefined) {
            el.hidden = !el.hidden;
        } else {
            el.hidden = newState;
        }
        this.displayHeader = el.hidden;
        this.lastHeaderHiddenState.set(el.hidden);
    }

    ngOnInit(): void {
        const currentView = getViewName();
        const availableViews = getAvailableViews();
        this.activeView = availableViews.find(
            (view) => view.route == currentView
        )?.title;
        this.updateHeaderMenuVisibility(
            this.lastHeaderHiddenState.get() ?? true
        );
    }
}
