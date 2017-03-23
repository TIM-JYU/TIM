
import {timApp} from "tim/app";
timApp.controller("StartCtrl", ['$scope', '$http', '$window', '$rootScope', '$log', function (sc, http, $window, $rootScope, $log) {
    "use strict";

    sc.creatingNew = false;

    sc.cancelCreate = function () {
        sc.creatingNew = false;
    };
}]);
