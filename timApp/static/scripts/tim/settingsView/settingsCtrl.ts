import {IController} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {$http, $window} from "../ngimport";

interface ISettings {
    css_combined: string;
    custom_css: string;
}

export class SettingsCtrl implements IController {
    private saving: boolean;
    private style: HTMLElementTagNameMap["style"];
    private settings: ISettings;

    constructor() {
        this.settings = $window.settings;
        $(".docEditor").change(() => {
            this.style.innerHTML = this.settings.custom_css;
        });

        this.updateCss();
        this.style = document.createElement("style");
        this.style.type = "text/css";
        document.getElementsByTagName("head")[0].appendChild(this.style);
    }

    async submit(saveUrl) {
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
}

timApp.component("timSettings", {
    controller: SettingsCtrl,
    template: "<div ng-transclude></div>",
    transclude: true,
});
