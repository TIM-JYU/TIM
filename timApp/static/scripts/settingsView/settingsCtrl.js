var settingsApp = angular.module('timApp');

settingsApp.controller('SettingsCtrl', ['$scope', '$http', function (sc, http) {
    sc.settings = settings;
    sc.submit = function (saveUrl) {
        sc.saving = true;
        http.post(saveUrl, sc.settings).success(
            function (data, status, error, headers) {
            }).success(function (data) {
                sc.settings = data;
                sc.updateCss();
            }).error(function () {
                alert('Failed to save settings.');
            }).then(function () {
                sc.saving = false;
            });
    };

    sc.updateCss = function () {
        $('link[rel="stylesheet"]').first().attr('href', '/static/gen/' + sc.settings.css_combined + '.css');
    };

    $(".docEditor").change(function() {
        sc.style.innerHTML = sc.settings.custom_css;
    });

    sc.updateCss();
    sc.style = document.createElement("style");
    sc.style.type = 'text/css';
    document.getElementsByTagName('head')[0].appendChild(sc.style);
}]);
