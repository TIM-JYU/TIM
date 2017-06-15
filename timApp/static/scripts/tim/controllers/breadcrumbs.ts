
import {timApp} from "tim/app";
import {$window} from "../ngimport";

timApp.controller("Breadcrumbs", ["$scope", function(sc) {
    "use strict";
    sc.crumbs = $window.breadcrumbs;
    sc.item = $window.item;
}]);
