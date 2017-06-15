
import {timApp} from "tim/app";
import * as createItem from "tim/directives/createItem";
import {markAsUsed} from "tim/utils";

markAsUsed(createItem);

timApp.controller("StartCtrl", ["$scope", function(sc) {
    "use strict";

    sc.creatingNew = false;

    sc.cancelCreate = function() {
        sc.creatingNew = false;
    };
}]);
