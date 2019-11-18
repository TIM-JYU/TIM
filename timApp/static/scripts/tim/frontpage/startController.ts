import {IController} from "angular";
import {ngStorage} from "ngstorage";
import * as createItem from "tim/item/createItem";
import {markAsUsed} from "tim/util/utils";
import {timApp} from "../app";
import {showCourseListDialog} from "../document/course/courseListDialogCtrl";
import {ICourseSettings} from "../item/IItem";
import {showMessageDialog} from "../ui/dialog";
import {FRONT_PAGE_DEFAULT_LANGUAGE, Lang, language} from "../ui/language";
import {showLoginDialog} from "../user/loginDialog";
import {Users} from "../user/userService";
import {genericglobals} from "../util/globals";
import {$http, $localStorage} from "../util/ngimport";
import {to} from "../util/utils";

markAsUsed(createItem);

export class StartCtrl implements IController {
    private creatingNew: boolean;
    private docListOpen: boolean;
    private language: Lang = FRONT_PAGE_DEFAULT_LANGUAGE; // Language to use.
    private bookmarks = {}; // For My courses.
    private storage: ngStorage.StorageService & {language: null | Lang};

    constructor() {
        this.creatingNew = false;
        this.docListOpen = false;
        this.bookmarks = genericglobals().bookmarks; // from base.html
        this.storage = $localStorage.$default({language: null});
    }

    $onInit() {
        this.setLanguage();
    }

    /**
     * Pick the page language from urlPathName or localstorage, otherwise use default.
     * Currently supported: fi, en.
     */
    setLanguage() {
        const urlPathName: string = window.location.pathname;
        switch (urlPathName) {
            case "/fi":
                this.language = "fi";
                // Save to localstorage so the language is remembered when using start page without urlPathName.
                this.storage.language = "fi";
                break;
            case "/en":
                this.language = "en";
                this.storage.language = "en";
                break;
            // For a new language add another case here and button and translations into HTML.
            default:
                // Try local storage, otherwise use default language.
                if (this.storage.language) {
                    this.language = this.storage.language;
                } else {
                    this.language = FRONT_PAGE_DEFAULT_LANGUAGE;
                    // Save to storage here so top bar login is the same language.
                    this.storage.language = this.language;
                }
                break;
        }
        language.lang = this.language;
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

    /**
     * Change page language and save it to the local storage.
     * Currently supported: fi, en.
     * @param changeTo New language abbreviation.
     */
    changeLanguage(changeTo: Lang) {
        this.language = changeTo;
        this.storage.language = changeTo;
        language.lang = changeTo;
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
            void showCourseListDialog({settings: r.result.data});
            return;
        }
        void showMessageDialog(`Course settings not found: ${r.result.data.error}`);
    }

    notFinnish() {
        return this.language != "fi";
    }

    getIntroLink() {
        const link = "/view/tim/TIM-esittely";
        if (this.notFinnish()) {
            return link + "/en";
        } else {
            return link;
        }
    }
}

timApp.component("timStart", {
    controller: StartCtrl,
    template: `
    <div class="row">
        <div class="col-lg-8 col-lg-offset-2">
            <h1 class="text-center">TIM - The Interactive Material</h1>
        </div>
        <div class="col-lg-2">
            <div ng-switch="$ctrl.language" ng-cloak class="pull-right">
                <div ng-switch-when="en">
                    <button class="btn btn-default btn-sm" ng-click="$ctrl.changeLanguage('fi')"
                            title="Vaihda etusivun ja kirjautumisen kieli suomeksi">Suomeksi</button>
                </div>
                <div ng-switch-when="fi">
                    <button class="btn btn-default btn-sm" ng-click="$ctrl.changeLanguage('en')"
                    title="Change start page and login menu language to English">In English</button>
                </div>
            </div>
        </div>
    </div>
    <div class="row">
        <div class="col-md-7 col-md-offset-3">
            <bookmark-folder-box bookmarks="$ctrl.bookmarks" bookmark-folder-name="My courses">
            </bookmark-folder-box>
        </div>
    </div>
    <div class="row">
        <div class="col-md-5 col-md-offset-2">
            <a href="/view/tim/TIM-esittely">
                <img class="img-responsive" alt="TIM-esittely" src="/static/images/responsive.jpg"/>
            </a>
        </div>
        <div class="col-md-4">
            <div ng-switch="$ctrl.language" ng-cloak>
                <!-- English -->
                <div ng-switch-default>
                    <h3>Get started</h3>
                    <button ng-if="!$ctrl.isLoggedIn()" ng-click="$ctrl.openLoginDialog(false)" type="button"
                        class="timButton margin-4" title="Log in with Korppi or TIM">Log in</button>
                    <button ng-if="!$ctrl.isLoggedIn()" ng-click="$ctrl.openLoginDialog(true)" type="button"
                        class="timButton margin-4"
                        title="Create a TIM account (for those who aren't staff or Korppi-users)">Sign up</button>
                    <ul class="list-unstyled">
                        <li ng-if="$ctrl.isLoggedIn()" class="h5">
                            <a href="/view/{{$ctrl.getCurrentUserFolderPath()}}">My documents</a>
                        </li>
                        <li class="h5"><a href="/view/">All documents</a></li>
                        <li class="h5">
                            <a ng-click="$ctrl.openCourseListDialog()" href="#">Available courses</a>
                        </li>
                        <li ng-if="$ctrl.isLoggedIn()" class="h5">
                            <a ng-click="$ctrl.enableCreate()" href="#">Create a new document</a>
                        </li>
                    </ul>
                    <bootstrap-panel ng-if="$ctrl.creatingNew"
                                     title="Create a new document"
                                     show-close="true"
                                     close-fn="$ctrl.cancelCreate()">
                        <create-item item-title="My document"
                                     item-location="{{$ctrl.getCurrentUserFolderPath()}}"
                                     item-type="document">
                        </create-item>
                    </bootstrap-panel>
                </div>
                <!-- Finnish -->
                <div ng-switch-when="fi">
                    <h3>Aloitus</h3>
                    <button ng-if="!$ctrl.isLoggedIn()" ng-click="$ctrl.openLoginDialog(false)" type="button"
                        class="timButton margin-4" title="Kirjaudu sisään Korppi- tai TIM-tunnuksilla">Kirjaudu</button>
                    <button ng-if="!$ctrl.isLoggedIn()" ng-click="$ctrl.openLoginDialog(true)" type="button"
                        class="timButton margin-4"
                        title="Luo TIM-tili (jos et ole Korppi-käyttäjä tai henkilökunnan jäsen)">Luo TIM-tili</button>
                    <ul class="list-unstyled">
                        <li ng-if="$ctrl.isLoggedIn()" class="h5">
                            <a href="/view/{{$ctrl.getCurrentUserFolderPath()}}">Omat dokumentit</a>
                        </li>
                        <li class="h5"><a href="/view/">Kaikki dokumentit</a></li>
                        <li class="h5">
                            <a ng-click="$ctrl.openCourseListDialog()" href="#">Saatavilla olevat kurssit</a>
                        </li>
                        <li ng-if="$ctrl.isLoggedIn()" class="h5">
                            <a ng-click="$ctrl.enableCreate()" href="#">Luo uusi dokumentti</a>
                        </li>
                    </ul>
                    <bootstrap-panel ng-if="$ctrl.creatingNew"
                                     title="Create a new document"
                                     show-close="true"
                                     close-fn="$ctrl.cancelCreate()">
                        <create-item item-title="My document"
                                     item-location="{{$ctrl.getCurrentUserFolderPath()}}"
                                     item-type="document">
                        </create-item>
                    </bootstrap-panel>
                </div>
            </div>
        </div>
    </div>
    <div class="row">
        <div class="col-md-7 col-md-offset-3">
            <h4>{{ 'What is TIM?' | tr }}</h4>
            <p>{{ 'TIM is a document-based cloud service for producing interactive materials.' | tr }}</p>
        </div>
    </div>
    <div class="row">
        <div class="col-md-3 col-md-offset-3">
            <h4>TIM</h4>
            <ul class="list-unstyled">
                <li><a href="{{ $ctrl.getIntroLink() }}">{{ 'Introduction' | tr }}</a></li>
                <li><a href="/view/tim/TIM-ohjeet">{{ 'User guide' | tr }}</a><sup ng-if="$ctrl.notFinnish()"> (F)</sup></li>
            </ul>
        </div>
        <div class="col-md-4">
            <h4>{{ 'Examples' | tr }} <sup ng-if="$ctrl.notFinnish()">(F)</sup></h4>
            <ul class="list-unstyled">
                <li><a href="/view/tim/Esimerkkeja-TIMin-mahdollisuuksista">{{ "TIM's possibilities" | tr }}</a></li>
                <li><a ng-if="$ctrl.isLoggedIn()" href="/view/tim/Eri-ohjelmointikielia">
                        {{ 'Programming languages' | tr }}</a></li>
                <li><a ng-if="$ctrl.isLoggedIn()" href="/view/tim/muita-esimerkkeja">
                        {{ 'Usage in different subjects' | tr }}</a></li>
            </ul>
        </div>
    </div>
    <div class="row" ng-if="$ctrl.notFinnish()">
        <div class="col-md-4 col-md-offset-4 text-muted text-center">
            <sup>(F)</sup> in Finnish
        </div>
    </div>
    `,
});
