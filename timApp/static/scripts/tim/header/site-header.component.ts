import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";
import {getVisibilityVars} from "tim/timRoot";
import {getViewName, TimStorage} from "tim/util/utils";
import {getAvailableViews} from "tim/header/utils";
import * as t from "io-ts";
import {isDocumentGlobals, someglobals} from "tim/util/globals";

@Component({
    selector: "tim-site-header",
    template: `
        <div class="siteheader" *ngIf="!hide.siteheader">
            <div class="hidden-lg hidden-md hamburger-placeholder" style="width: 27px; height: 1px"></div>
            <a class="logolink" title="To TIM main page" i18n-title *ngIf="!hide.links" href="/">
                <tim-logo *ngIf="!hide.logo"></tim-logo>
            </a>
            <a class="view-toggle"
               *ngIf="activeView && !hide.headerNav"
               (click)="toggleViewVisibility()"
               title="Show/hide view menu"
               i18n-title>
                <span>{{activeView}}</span>
                <i class="glyphicon"
                   [class.glyphicon-triangle-bottom]="!displayViewHeader"
                   [class.glyphicon-triangle-top]="displayViewHeader"></i>
            </a>
            <tim-logo *ngIf="hide.links && !hide.logo"></tim-logo>
            <tim-login-menu *ngIf="!hide.login"></tim-login-menu>
            <img *ngIf="!hide.unilogo"
                 class="jyu-logo"
                 alt="University of Jyväskylä"
                 i18n-alt
                 src="/static/images/jyulogo.png"/>
            <tim-search-button [floating]="true" *ngIf="!hide.search"></tim-search-button>
        </div>
    `,
    styleUrls: ["./site-header.component.scss"],
})
export class SiteHeaderComponent implements OnInit {
    hide = getVisibilityVars();
    activeView?: string;
    private displayViewHeaderState?: boolean;
    private lastViewHeaderDisplayState = new TimStorage(
        "siteHeader_lastHeaderHiddenState",
        t.boolean
    );

    get displayViewHeader() {
        if (this.displayViewHeaderState !== undefined) {
            return this.displayViewHeaderState;
        }
        this.displayViewHeader = this.lastViewHeaderDisplayState.get() ?? false;
        return this.displayViewHeaderState!;
    }

    set displayViewHeader(newState: boolean) {
        this.lastViewHeaderDisplayState.set(newState);
        this.displayViewHeaderState = newState;
    }

    toggleViewVisibility() {
        this.displayViewHeader = !this.displayViewHeader;
        this.updateHeaderMenuVisibility();
    }

    updateHeaderMenuVisibility() {
        document.documentElement.style.setProperty(
            "--header-menu-visibility",
            this.displayViewHeader ? "block" : "none"
        );
        document.documentElement.style.setProperty(
            "--header-menu-pull-position",
            this.displayViewHeader ? "0" : "-20px"
        );
    }

    ngOnInit(): void {
        const currentView = getViewName();
        const availableViews = getAvailableViews();
        this.activeView = availableViews.find(
            (view) => view.route == currentView
        )?.title;

        const globals = someglobals();
        if (isDocumentGlobals(globals)) {
            if (globals.docSettings.displayViewInitialState !== undefined) {
                this.displayViewHeader =
                    globals.docSettings.displayViewInitialState;
            }
            // If not set in document settings, allow user setting to determine default header menu open state
            else {
                this.displayViewHeader =
                    globals.userPrefs.always_show_header_menu;
            }
        }

        this.updateHeaderMenuVisibility();
    }
}
