import {IController} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {$http, $window} from "../util/ngimport";

interface ISettings {
    css_combined: string;
    custom_css: string;
}

export class SettingsCtrl implements IController {
    private saving: boolean = false;
    private style: HTMLElementTagNameMap["style"];
    private settings: ISettings;
    private cssFiles: {}[];
    private notifications: {}[];

    constructor() {
        this.settings = $window.settings;
        this.cssFiles = $window.css_files;
        this.notifications = $window.notifications;
        $(".docEditor").change(() => {
            this.style.innerHTML = this.settings.custom_css;
        });

        this.updateCss();
        this.style = document.createElement("style");
        this.style.type = "text/css";
        document.getElementsByTagName("head")[0].appendChild(this.style);
    }

    $onInit() {

    }

    async submit(saveUrl: string) {
        this.saving = true;
        try {
            const response = await $http.post<ISettings>(saveUrl, this.settings);
            this.settings = response.data;
            this.updateCss();
        } catch (e) {
            alert("Failed to save settings.");
        } finally {
            this.saving = false;
        }
    }

    updateCss() {
        $('link[rel="stylesheet"]').first().attr("href", "/static/gen/" + this.settings.css_combined + ".css");
    }

    clearLocalStorage() {
        window.localStorage.clear();
    }

    addPrintSettings() {
        jQuery.get("/static/userPrintSettings.css", (data) => {
            $("#customCssArea").val(data);
            $("#customCssArea").serialize();
        }, "text");
    }
}

timApp.component("timSettings", {
    controller: SettingsCtrl,
    template: `<h1>TIM settings</h1>
<form ng-submit="$ctrl.submit('/settings/save')">
    <bootstrap-panel title="Styles">
        <span ng-if="$ctrl.cssFiles">Available themes:</span>
        <span ng-if="!$ctrl.cssFiles">There are no available themes.</span>
        <br>
        <div ng-repeat="css_file in $ctrl.cssFiles"
             class="checkbox"><label>
            <input type="checkbox"
                   name="settings.css_files[css_file.name]"
                   ng-model="$ctrl.settings.css_files[css_file.name]"
                   ng-change="$ctrl.submit('/settings/save')"
                   ng-disabled="$ctrl.saving">
            <a href="/static/css/{{ css_file.name }}.scss">
                {{ css_file.name }}</a> - {{ css_file.desc }}
        </label></div>
        <div class="form-group">
            <label for="customCssArea">Custom CSS:</label>
            <textarea rows="15" id="customCssArea" class="form-control"
                      ng-model="$ctrl.settings.custom_css"></textarea>
        </div>

        <button class="btn btn-default" ng-click="$ctrl.addPrintSettings();">Add Print Settings</button>

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
    </bootstrap-panel>
    <bootstrap-panel title="Notifications">
        <p>You get emails from the following documents and folders:</p>
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
    </bootstrap-panel>
    <button class="timButton" type="submit" ng-disabled="$ctrl.saving">Save changes</button>
    <span ng-show="$ctrl.saving">Saving...</span>
    <button class="btn btn-default" ng-click="$ctrl.clearLocalStorage()">Clear local settings storage</button>
</form>
    `,
});
