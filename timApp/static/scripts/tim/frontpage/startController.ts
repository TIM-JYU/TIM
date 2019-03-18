import {IController} from "angular";
import * as createItem from "tim/item/createItem";
import {markAsUsed} from "tim/util/utils";
import {timApp} from "../app";
import {showCourseListDialog} from "../document/course/courseListDialogCtrl";
import {ICourseSettings, IItem} from "../item/IItem";
import {showMessageDialog} from "../ui/dialog";
import {$http, $window, $localStorage} from "../util/ngimport";
import {to} from "../util/utils";
import {Users} from "../user/userService";
import {ngStorage} from "ngstorage";

markAsUsed(createItem);

export class StartCtrl implements IController {
    private creatingNew: boolean;
    private docListOpen: boolean;
    private defaultLanguage: string = "fi"; // Page default language.
    private language: string = this.defaultLanguage;
    private bookmarks = {};
    private storage: ngStorage.StorageService & {languageStorage: null | string};

    constructor() {
        this.creatingNew = false;
        this.docListOpen = false;
        this.bookmarks = $window.bookmarks; // from base.html
        this.storage = $localStorage.$default({languageStorage: null});
    }

    $onInit() {
        this.setLanguage();
    }

    /**
     * Picks the page language from url suffix or local storage, otherwise use default.
     * Currently supported: fi, en.
     */
    setLanguage() {
        const urlSuffix: string = window.location.pathname;
        switch (urlSuffix) {
            case "/fi": {
                this.language = "fi";
                break;
            }
            case "/en": {
                this.language = "en";
                break;
            }
            default: {
                // Try local storage, otherwise use default.
                if (this.storage.languageStorage) {
                    this.language = this.storage.languageStorage;
                } else {
                    this.language = this.defaultLanguage;
                }
                break;
            }
        }
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
    changeLanguage(changeTo: string) {
        this.language = changeTo;
        this.storage.languageStorage = changeTo;
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
}

timApp.component("timStart", {
    controller: StartCtrl,
    template: `
    <div class="row">
        <div ng-switch="$ctrl.language" ng-cloak class="pull-right">
            <div ng-switch-when="en">
                <button class="timButton btn-sm" ng-click="$ctrl.changeLanguage('fi')"
                        title="Vaihda sivun kieli suomeksi">Suomeksi</button>
            </div>
            <div ng-switch-when="fi">
                <button class="timButton btn-sm" ng-click="$ctrl.changeLanguage('en')"
                title="Change page language to English">In English</button>
            </div>
        </div>
        <h1 class="text-center">TIM - The Interactive Material</h1>
    </div>
    <div class="row">
        <div class="col-md-8 col-md-offset-2">
            <div class="row">
                <div class="col-md-8">
                    <bookmark-folder-box bookmarks="$ctrl.bookmarks" bookmark-folder-name="My courses">
                    </bookmark-folder-box>
                    <a href="/view/tim/TIM-esittely">
                        <img class="img-responsive" alt="TIM-esittely" src="/static/images/responsive.jpg"/>
                    </a>
                </div>
                <div class="col-md-4">
                    <div ng-switch="$ctrl.language" ng-cloak>
                        <div ng-switch-when="en">
                            <h3>Get started</h3>
                            <p ng-if="!$ctrl.isLoggedIn()">To log in or sign up, use the "Log in" button at the top.</p>
                            <ul class="list-unstyled">
                                <li ng-if="$ctrl.isLoggedIn()" class="h5">
                                    <a href="/view/{{$ctrl.getCurrentUserFolderPath()}}">My documents</a>
                                </li>
                                <li class="h5">
                                    <a ng-click="$ctrl.openCourseListDialog()" href="#">Available courses</a>
                                </li>
                                <li class="h5"><a href="/view/">All documents</a></li>
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
                        <div ng-switch-when="fi">
                            <h3>Aloitus</h3>
                            <p ng-if="!$ctrl.isLoggedIn()">Kirjautuaksesi sisään käytä "Log in"
                             -painiketta sivun ylälaidassa.</p>
                            <ul class="list-unstyled">
                                <li ng-if="$ctrl.isLoggedIn()" class="h5">
                                    <a href="/view/{{$ctrl.getCurrentUserFolderPath()}}">Omat dokumentit</a>
                                </li>
                                <li class="h5">
                                    <a ng-click="$ctrl.openCourseListDialog()" href="#">Saatavilla olevat kurssit</a>
                                </li>
                                <li class="h5"><a href="/view/">Selaa dokumentteja</a></li>
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
            <div ng-switch="$ctrl.language" ng-cloak>
                <div ng-switch-when="en">
                    <div class="row">
                        <div class="col-md-4 col-md-offset-2">
                            <h4>TIM</h4>
                            <ul class="list-unstyled">
                                <li><a href="/view/tim/TIM-esittely">Introduction</a><sup> (F)</sup></li>
                                <li><a href="/view/tim/TIM-ohjeet">User guide</a><sup> (F)</sup></li>
                            </ul>
                        </div>
                        <div class="col-md-4">
                            <h4>Examples <sup>(F)</sup></h4>
                            <ul class="list-unstyled">
                                <li><a href="/view/tim/Esimerkkeja-TIMin-mahdollisuuksista">TIM's possibilities</a></li>
                                <li><a ng-if="$ctrl.isLoggedIn()" href="/view/tim/Eri-ohjelmointikielia">
                                        Programming languages</a></li>
                                <li><a ng-if="$ctrl.isLoggedIn()" href="/view/tim/muita-esimerkkeja">
                                        Usage in different subjects</a></li>
                            </ul>
                        </div>
                    </div>
                    <div class="row">
                        <div class="col-md-4 col-md-offset-4 text-muted text-center">
                            <sup>(F)</sup> in Finnish
                        </div>
                    </div>
                </div>
                <div ng-switch-when="fi">
                    <div class="row">
                        <div class="col-md-4 col-md-offset-2">
                            <h4>TIM</h4>
                            <ul class="list-unstyled">
                                <li><a href="/view/tim/TIM-esittely">Esittely</a></li>
                                <li><a href="/view/tim/TIM-ohjeet">Ohjeet</a></li>
                            </ul>
                        </div>
                        <div class="col-md-4">
                            <h4>Esimerkkejä</h4>
                            <ul class="list-unstyled">
                                <li><a href="/view/tim/Esimerkkeja-TIMin-mahdollisuuksista">
                                    TIMin mahdollisuuksia</a></li>
                                <li><a ng-if="$ctrl.isLoggedIn()" href="/view/tim/Eri-ohjelmointikielia">
                                    Ohjelmointikieliä</a></li>
                                <li><a ng-if="$ctrl.isLoggedIn()" href="/view/tim/muita-esimerkkeja">
                                    Käyttö eri oppiaineissa</a></li>
                            </ul>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
    `
});
