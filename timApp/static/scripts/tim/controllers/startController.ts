
import {timApp} from "tim/app";
import * as createItem from "tim/directives/createItem";
import {markAsUsed} from "tim/utils";

markAsUsed(createItem);

timApp.controller("StartCtrl", ["$scope", "$http", "$window", "$rootScope", "$log", function(sc, http, $window, $rootScope, $log) {
    "use strict";

    sc.creatingNew = false;

    sc.cancelCreate = function() {
        sc.creatingNew = false;
    };
}]);
