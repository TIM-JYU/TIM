var settingsApp = angular.module('timApp');

settingsApp.controller('SettingsCtrl', ['$scope', '$http', function (sc, http) {
    sc.settings = settings;
    sc.submit = function (saveUrl) {
        sc.saving = true;
        http.post(saveUrl, sc.settings).success(
            function (data, status, error, headers) {
            }).error(function () {
                alert('Failed to save settings.')
            }).then(function () {
                sc.saving = false;
            })
    }
}]);