
import {timApp} from "tim/app";
timApp.controller('Breadcrumbs', ['$scope', '$window', function (sc, $window) {
    "use strict";
    sc.crumbs = $window.breadcrumbs;
    sc.item = $window.item;
}]);
