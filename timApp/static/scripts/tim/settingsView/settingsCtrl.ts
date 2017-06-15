import $ from "jquery";
import {timApp} from "tim/app";
import {$http, $window} from "../ngimport";

timApp.controller("SettingsCtrl", ["$scope", function(sc) {
    sc.settings = $window.settings;
    sc.submit = async function(saveUrl) {
        sc.saving = true;
        try {
            const data = await $http.post(saveUrl, sc.settings);
            sc.settings = data;
            sc.updateCss();
        } catch (e) {
            alert("Failed to save settings.");
        } finally {
            sc.saving = false;
        }
    };

    sc.updateCss = function() {
        $('link[rel="stylesheet"]').first().attr("href", "/static/gen/" + sc.settings.css_combined + ".css");
    };

    sc.clearLocalStorage = function() {
        window.localStorage.clear();
    };

    $(".docEditor").change(function() {
        sc.style.innerHTML = sc.settings.custom_css;
    });

    sc.updateCss();
    sc.style = document.createElement("style");
    sc.style.type = "text/css";
    document.getElementsByTagName("head")[0].appendChild(sc.style);
}]);
