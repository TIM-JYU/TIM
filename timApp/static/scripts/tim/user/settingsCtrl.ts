import {IController} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {ConsentType} from "../ui/consent";
import {showMessageDialog} from "../ui/dialog";
import {settingsglobals} from "../util/globals";
import {$http, $timeout} from "../util/ngimport";
import {IOkResponse, to} from "../util/utils";
import {IFullUser} from "./IUser";

export interface ISettings {
    css_combined: string;
    custom_css: string;
    disable_menu_hover: boolean;
    css_files: Record<string, boolean>
}

export async function setConsent(c: ConsentType) {
    const r = await to($http.post<IOkResponse>("/settings/updateConsent", {consent: c}));
    if (r.ok) {
        // nothing to do
    } else {
        void showMessageDialog(r.result.data.error);
    }
}

export interface INotification {
    //
}

export class SettingsCtrl implements IController {
    private saving = false;
    private style: HTMLElementTagNameMap["style"];
    private settings: ISettings;
    private cssFiles: Array<{}>;
    private notifications: INotification[];
    private consent: ConsentType | undefined;
    private storageClear = false;
    private allNotificationsFetched = false;
    private user: IFullUser;
    private deletingAccount = false;
    private deleteConfirmName = "";

    constructor() {
        this.user = settingsglobals().current_user;
        this.settings = settingsglobals().userPrefs;
        this.cssFiles = settingsglobals().css_files;
        this.notifications = settingsglobals().notifications;
        this.updateCss();
        this.style = document.createElement("style");
        this.style.type = "text/css";
        document.getElementsByTagName("head")[0].appendChild(this.style);
    }

    $onInit() {
        this.consent = this.user.consent;
    }

    $doCheck() {
        this.style.innerHTML = this.settings.custom_css;
    }

    async submit() {
        this.saving = true;
        const r = await to($http.post<ISettings>("/settings/save", this.settings));
        if (r.ok) {
            this.settings = r.result.data;
            this.updateCss();
        } else {
            void showMessageDialog(r.result.data.error);
        }
        this.saving = false;
    }

    async updateConsent() {
        if (this.consent == null) {
            return;
        }
        this.saving = true;
        await setConsent(this.consent);
        this.saving = false;
    }

    updateCss() {
        $('link[rel="stylesheet"]').first().attr("href", `/static/generated/${this.settings.css_combined}.css`);
    }

    async clearLocalStorage() {
        window.localStorage.clear();
        this.storageClear = true;
        await $timeout(3000);
        this.storageClear = false;
    }

    async addPrintSettings() {
        const resp = await to($http.get<string>("/static/stylesheets/userPrintSettings.css"));
        if (!resp.ok) {
            return;
        }
        this.settings.custom_css = resp.result.data;
    }

    async getAllNotifications() {
        const resp = await to($http.get<INotification[]>("/notify/all"));
        if (resp.ok) {
            this.notifications = resp.result.data;
            this.allNotificationsFetched = true;
        } else {
            void showMessageDialog(resp.result.data.error);
        }
    }

    showGetAllNotifications() {
        return this.notifications.length === settingsglobals().notificationLimit && !this.allNotificationsFetched;
    }

    beginDeleteAccount() {
        this.deleteConfirmName = "";
        this.deletingAccount = true;
    }

    async deleteAccount() {
        if (window.confirm(`Are you sure you want to delete your account (${this.user.name})?`)) {
            const r = await to($http.post("/settings/account/delete", {}));
            if (r.ok) {
                location.href = "/";
            } else {
                await showMessageDialog(r.result.data.error);
            }
        }
    }
}

timApp.component("timSettings", {
    controller: SettingsCtrl,
    template: `<h1>TIM settings</h1>
<form>
    <bootstrap-panel title="Styles">
        <span ng-if="$ctrl.cssFiles">Available themes:</span>
        <span ng-if="!$ctrl.cssFiles">There are no available themes.</span>
        <br>
        <div ng-repeat="css_file in $ctrl.cssFiles"
             class="checkbox"><label>
            <input type="checkbox"
                   name="settings.css_files[css_file.name]"
                   ng-model="$ctrl.settings.css_files[css_file.name]"
                   ng-change="$ctrl.submit()"
                   ng-disabled="$ctrl.saving">
            <a href="/static/stylesheets/themes/{{ css_file.name }}.scss">
                {{ css_file.name }}</a> - {{ css_file.desc }}
        </label></div>
        <div class="form-group">
            <label for="customCssArea">Custom CSS:</label>
            <textarea rows="15" id="customCssArea" class="form-control"
                      ng-model="$ctrl.settings.custom_css"></textarea>
        </div>
        <button class="timButton" ng-disabled="$ctrl.saving" ng-click="$ctrl.submit()">Save changes</button>
        <tim-loading ng-show="$ctrl.saving"></tim-loading>
        <button class="btn btn-default" ng-click="$ctrl.addPrintSettings()">Add Print Settings</button>

    </bootstrap-panel>
    <bootstrap-panel title="Editor">
        <div class="checkbox">
            <label>
                <input type="checkbox" ng-model="$ctrl.settings.use_document_word_list">
                Use words from the document in ACE editor autocomplete
            </label>
        </div>
        <label>ACE editor additional word list for autocomplete (1 word per line)
            <textarea rows="15" class="form-control" ng-model="$ctrl.settings.word_list"></textarea>
        </label>
        <div>
            <button class="timButton" ng-disabled="$ctrl.saving" ng-click="$ctrl.submit()">Save changes</button>
            <tim-loading ng-show="$ctrl.saving"></tim-loading>
        </div>
    </bootstrap-panel>
    <bootstrap-panel title="Notifications">
        <h4>Subscribed items</h4>
        <p ng-if="$ctrl.notifications.length > 0">You get emails from the following documents and folders:</p>
        <p ng-if="$ctrl.notifications.length === 0">You haven't subscribed to any documents or folders.</p>
        <ul>
            <li ng-repeat="n in $ctrl.notifications">
                <a href="/manage/{{n.item.path}}">
                    <span ng-if="n.item.isFolder" class="glyphicon glyphicon-folder-open"></span>
                    {{n.item.title}}</a>
                <span ng-if="n.email_doc_modify"
                      class="glyphicon glyphicon-pencil"
                      uib-tooltip="Document modifications"></span>
                <span ng-if="n.email_comment_add"
                      class="glyphicon glyphicon-comment"
                      uib-tooltip="New comments"></span>
                <span ng-if="n.email_comment_modify"
                      class="glyphicon glyphicon-comment"
                      uib-tooltip="Comment modifications"></span>
            </li>
        </ul>
        <button ng-click="$ctrl.getAllNotifications()"
                class="timButton"
                ng-if="$ctrl.showGetAllNotifications()">
            Show all
        </button>
        <h4>Exclusion list</h4>
        <p>
            Sometimes you may want to subscribe to emails from a folder but exclude some documents within it.
            Using the list below you can specify which folders and documents should be excluded from your email
            subscriptions.
        </p>
        <p>Type one regular expression per line that should match any part of the path of the folder or document,
        e.g. <code>/ht/</code> would match any path with <code>/ht/</code> in it.</p>
        <div class="form-group">
        <textarea class="form-control" rows="5" ng-model="$ctrl.settings.email_exclude">
        </textarea>
        </div>

        <div>
            <button class="timButton" ng-disabled="$ctrl.saving" ng-click="$ctrl.submit()">Save changes</button>
            <tim-loading ng-show="$ctrl.saving"></tim-loading>
        </div>

    </bootstrap-panel>

    <bootstrap-panel title="Menus">
        <div class="checkbox"><label>
            <input type="checkbox" name="disable_menu_hover" ng-model="$ctrl.settings.disable_menu_hover"
            ng-disabled="$ctrl.saving"> Disable opening menus with mouse hover
        </label></div>
        <button class="timButton" ng-disabled="$ctrl.saving" ng-click="$ctrl.submit()">Save changes</button>
        <tim-loading ng-show="$ctrl.saving"></tim-loading>
    </bootstrap-panel>

    <bootstrap-panel title="Other settings">
        <button class="btn btn-default" ng-click="$ctrl.clearLocalStorage()">Clear local settings storage</button>
        <span ng-if="$ctrl.storageClear">Local storage cleared.</span>
    </bootstrap-panel>
    <bootstrap-panel title="Your account information">
        <p>Id: {{$ctrl.user.id}}</p>
        <p>Username: {{$ctrl.user.name}}</p>
        <p>Full name: {{$ctrl.user.real_name}}</p>
        <p>Email: {{$ctrl.user.email}}</p>
    </bootstrap-panel>
<!--    <bootstrap-panel title="Consent">
        <tim-consent-choice consent="$ctrl.consent"></tim-consent-choice>
        <div>
            <button class="timButton" ng-disabled="$ctrl.saving" ng-click="$ctrl.updateConsent()">Save changes</button>
            <tim-loading ng-show="$ctrl.saving"></tim-loading>
        </div>
    </bootstrap-panel>-->
    <bootstrap-panel title="Delete your account">
        <button class="timButton btn-danger"
                ng-click="$ctrl.beginDeleteAccount()"
                ng-if="!$ctrl.deletingAccount">Delete account...</button>
        <div ng-if="$ctrl.deletingAccount">
            To delete your account, please type your username ({{ $ctrl.user.name }}) in the field below
            and then click "Delete account now".
            <div class="form-inline">
                <input class="form-control"
                       type="text"
                       ng-model="$ctrl.deleteConfirmName">
                <button ng-disabled="$ctrl.user.name !== $ctrl.deleteConfirmName"
                        class="timButton btn-danger"
                        ng-click="$ctrl.deleteAccount()">Delete account now</button>
                <button class="timButton btn-default" ng-click="$ctrl.deletingAccount = false">Cancel</button>
            </div>
        </div>
    </bootstrap-panel>
</form>
    `,
});
