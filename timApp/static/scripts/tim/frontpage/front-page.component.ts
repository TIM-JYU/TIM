import type {IController} from "angular";
import {Component} from "@angular/core";
import type {IBookmarkGroup} from "tim/bookmark/bookmark.service";
import {showLoginDialog} from "tim/user/showLoginDialog";
import {showCourseListDialog} from "tim/document/course/showCourseDialog";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import type {ICourseSettings} from "tim/item/IItem";
import {Users} from "tim/user/userService";
import {genericglobals} from "tim/util/globals";
import {$http} from "tim/util/ngimport";
import {to, to2} from "tim/util/utils";
import type {IVisibilityVars} from "tim/timRoot";
import {getVisibilityVars} from "tim/timRoot";

@Component({
    selector: "tim-start",
    template: `
        <div class="row">
            <div class="col-lg-6 col-lg-offset-3">
                <h1 class="text-center">TIM - The Interactive Material</h1>
            </div>
            <div class="col-lg-3" *ngIf="isLoggedIn()">
                <tim-language-selector [saveOnChange]="true" [(language)]="settings.language"></tim-language-selector>
            </div>
        </div>
        <div class="row">
            <div class="col-md-7 col-md-offset-3">
                <tim-bookmark-folder-box *ngIf="bookmarks"
                                         [bookmarks]="bookmarks"
                                         [displayName]="'My courses' | bookmarkName"
                                         [hideMode]="true"
                                         bookmarkFolderName="My courses">
                </tim-bookmark-folder-box>
            </div>
        </div>
        <div class="row">
            <div class="col-md-5 col-md-offset-2">
                <a href="/view/about">
                    <img class="img-responsive" alt="TIM brochure" i18n-alt src="/static/images/responsive.jpg"/>
                </a>
            </div>
            <div class="col-md-4">
                <h2 i18n>Get started</h2>
                <button *ngIf="!isLoggedIn()" (click)="openLoginDialog(false)" type="button"
                        class="timButton margin-4" i18n>Log in
                </button>
                <button *ngIf="!isLoggedIn() && !hideVars.signup" (click)="openLoginDialog(true)" type="button"
                        class="timButton margin-4"
                        title="Create a TIM account"
                        i18n-title
                        i18n>Sign up
                </button>
                <ul class="list-unstyled">
                    <li *ngIf="isLoggedIn()" class="h5">
                        <a href="/view/{{getCurrentUserFolderPath()}}" i18n>My documents</a>
                    </li>
                    <li class="h5"><a href="/view/" i18n>All documents</a></li>
                    <li class="h5">
                        <a (click)="openCourseListDialog()" href="#" i18n>Available courses</a>
                    </li>
                    <li *ngIf="isLoggedIn()" class="h5">
                        <a (click)="enableCreate()" href="#" i18n>Create a new document</a>
                    </li>
                </ul>
                <bootstrap-panel *ngIf="creatingNew"
                                 title="Create a new document"
                                 i18n-title
                                 [showClose]="true"
                                 (closed)="cancelCreate()">
                    <create-item itemTitle="My document"
                                 i18n-itemTitle
                                 itemLocation="{{getCurrentUserFolderPath()}}"
                                 itemType="document">
                    </create-item>
                </bootstrap-panel>
            </div>
        </div>
        <div class="row">
            <div class="col-md-7 col-md-offset-3">
                <h3 i18n>What is TIM?</h3>
                <p i18n>TIM is a document-based cloud service for producing interactive materials.</p>
            </div>
        </div>
        <div class="row">
            <div class="col-md-3 col-md-offset-3">
                <h3>TIM</h3>
                <ul class="list-unstyled">
                    <li><a href="{{ getIntroLink() }}" i18n>Introduction</a></li>
                    <li><a href="/view/tim/ohjeita/pikaohje" i18n>Quick start</a><sup *ngIf="notFinnish()"> (F)</sup></li>
                    <li><a href="/view/tim/ohjeita/ohjeet" i18n>Content creator guide</a><sup *ngIf="notFinnish()"> (F)</sup></li>
                    <li><a href="/view/tim/TIM-ohjeet" i18n>All guides</a><sup *ngIf="notFinnish()"> (F)</sup></li>
                </ul>
            </div>
            <div class="col-md-4">
                <h3 i18n>Examples <sup *ngIf="notFinnish()">(F)</sup></h3>
                <ul class="list-unstyled">
                    <li><a href="/view/tim/Esimerkkeja-TIMin-mahdollisuuksista" i18n>TIM's possibilities</a></li>
                    <li><a *ngIf="isLoggedIn()" href="/view/tim/Eri-ohjelmointikielia" i18n>
                        Various programming languages</a></li>
                    <li><a *ngIf="isLoggedIn()" href="/view/tim/muita-esimerkkeja" i18n>
                        Usage in different subjects</a></li>
                </ul>
            </div>
        </div>
        <div class="row" *ngIf="notFinnish()">
            <div class="col-md-4 col-md-offset-4 text-center" i18n>
                <sup>(F)</sup> in Finnish
            </div>
        </div>
    `,
})
export class FrontPageComponent implements IController {
    creatingNew: boolean;
    private docListOpen: boolean;
    bookmarks?: IBookmarkGroup[]; // For My courses.
    settings = genericglobals().userPrefs;
    hideVars: IVisibilityVars = getVisibilityVars();

    constructor() {
        this.creatingNew = false;
        this.docListOpen = false;
        this.bookmarks = genericglobals().bookmarks ?? undefined;
    }

    getCurrentUserFolderPath() {
        return Users.getCurrentPersonalFolderPath();
    }

    /**
     * Check whether the current user is logged in.
     */
    isLoggedIn() {
        return Users.isLoggedIn();
    }

    cancelCreate() {
        this.creatingNew = false;
    }

    enableCreate() {
        this.creatingNew = true;
    }

    openLoginDialog(signup: boolean) {
        if (!this.isLoggedIn()) {
            void showLoginDialog({showSignup: signup, addingToSession: false});
        } else {
            void showMessageDialog(`You are already logged in`);
        }
    }

    /**
     * Opens 'Available courses' dialog.
     */
    async openCourseListDialog() {
        const r = await to($http.get<ICourseSettings>(`/courses/settings`));
        if (r.ok) {
            await to2(showCourseListDialog({settings: r.result.data}));
            return;
        }
        void showMessageDialog(
            `Course settings not found: ${r.result.data.error}`
        );
    }

    notFinnish() {
        return Users.getCurrentLanguage() != "fi";
    }

    getIntroLink() {
        const link = "/view/about";
        if (this.notFinnish()) {
            return link + "/en-US";
        } else {
            return link;
        }
    }
}
