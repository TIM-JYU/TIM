import angular = require("angular");
import moment = require("moment");
import * as aedatetimepicker from "angular-eonasdan-datetimepicker";
import * as ngMessages from "angular-messages";
import * as timer from "angular-timer";
import * as humanizeDuration from "humanize-duration";
import ngSanitize = require("angular-sanitize");
import * as uibootstrap from "angular-ui-bootstrap";
import * as ngFileUpload from "ng-file-upload";
import * as ngStorage from "ngstorage";
import * as oclazyload from "oclazyload";
import * as angularmodules from "tim/angularmodules";
import * as extramodules from "tim/extramodules";
import * as plugins from "tim/plugins";
import {markAsUsed} from "tim/utils";

markAsUsed(ngMessages, timer, aedatetimepicker, ngSanitize, uibootstrap, ngFileUpload, ngStorage, plugins, extramodules, oclazyload);

// timApp's Angular modules:
// base: 'ngMessages', 'timer', 'ae-datetimepicker', 'ngSanitize', 'ui.bootstrap'
// item: 'ngFileUpload'
// view_html: 'oc.lazyLoad', 'ui.ace', 'ngStorage' + plugin modules
// teacher mode: 'ui.grid', 'ui.grid.cellNav', 'ui.grid.selection', 'ui.grid.exporter', 'ui.grid.autoResize'
export const timApp = angular.module("timApp", [
    "ngMessages",
    "timer",
    "ae-datetimepicker",
    "ngSanitize",
    "ui.bootstrap",
    "ngFileUpload",
    "ngStorage",
    "oc.lazyLoad",
].concat(angularmodules));
// disable Angular URL manipulation when using ng-include; from http://stackoverflow.com/a/19825756
timApp.config(["$provide", ($provide) => {
    $provide.decorator("$browser", ["$delegate", ($delegate) => {
        //noinspection TsLint
        $delegate.onUrlChange = () => {
        };
        $delegate.url = () => {
            return "";
        };
        return $delegate;
    }]);
}]);

// disable IE ajax request caching; from https://stackoverflow.com/a/19771501
timApp.config(["$httpProvider", ($httpProvider) => {
    if (!$httpProvider.defaults.headers.get) {
        $httpProvider.defaults.headers.get = {};
    }

    $httpProvider.defaults.headers.get["If-Modified-Since"] = "Mon, 26 Jul 1997 05:00:00 GMT";
    $httpProvider.defaults.headers.get["Cache-Control"] = "no-cache";
    $httpProvider.defaults.headers.get["Pragma"] = "no-cache";
}]);

// Filter to make string URL friendly
timApp.filter("escape", () => {
    "use strict";
    return (str) => {
        return encodeURIComponent(str).replace(/%2F/g, "/");
    };
});

timApp.filter("timdate", ["$filter", ($filter) => {
    let dateFilter = $filter("date");
    return (date) => {
        return dateFilter(date, "dd.MM.yyyy HH:mm:ss");
    };
}]);

timApp.filter("timreldate", ["$filter", ($filter) => {
    return (date) => {
        return moment(date).fromNow();
    };
}]);

timApp.filter("timduration", ["$filter", ($filter) => {
    return (duration) => {
        return moment.duration(duration).humanize();
    };
}]);

timApp.filter("timpreciseduration", ["$filter", ($filter) => {
    return (duration) => {
        return humanizeDuration(moment.duration(duration).asMilliseconds());
    };
}]);

timApp.filter("timtim", ["$filter", ($filter) => {
    let dateFilter = $filter("date");
    return (date) => {
        return dateFilter(date, "HH:mm:ss");
    };
}]);
