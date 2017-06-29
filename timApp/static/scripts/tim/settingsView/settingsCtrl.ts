import $ from "jquery";
import {timApp} from "tim/app";
import {$http, $window} from "../ngimport";

export class SettingsCtrl {
    private saving: boolean;
    private style: HTMLElementTagNameMap["style"];
    private settings: {css_combined: string};

    constructor() {
        this.settings = $window.settings;
        $(".docEditor").change(function() {
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
            const response = await $http.post<{css_combined: string}>(saveUrl, this.settings);
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
