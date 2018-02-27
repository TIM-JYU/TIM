import angular, {
    IFilterService, IHttpProvider, IHttpResponseTransformer,
    IModule, IQProvider, IQService, IWindowService,
} from "angular";
import aedatetimepicker from "angular-eonasdan-datetimepicker";
import ngMessages from "angular-messages";
import ngSanitize from "angular-sanitize";
import timer from "angular-timer";
import uibootstrap from "angular-ui-bootstrap";
import humanizeDuration from "humanize-duration";
import {Moment} from "moment";
import moment from "moment";
import ngFileUpload from "ng-file-upload";
import ngStorage from "ngstorage";
import oclazyload from "oclazyload";
import angularmodules from "tim/angularmodules";
import extramodules from "tim/extramodules";
import plugins from "tim/plugins";
import {convertDateStringsToMoments, markAsUsed} from "tim/utils";
import {loadMap} from "./loadMap";
import {injectProviders, injectServices} from "./ngimport";
import {initUserService} from "./services/userService";

markAsUsed(ngMessages, timer, aedatetimepicker, ngSanitize,
    uibootstrap, ngFileUpload, ngStorage, plugins, extramodules, oclazyload);

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
timApp.config(["$provide", ($provide: IModule) => {
    $provide.decorator("$browser", ["$delegate", ($delegate: any) => {
        //noinspection TsLint
        $delegate.onUrlChange = () => {
        };
        $delegate.url = () => {
            return "";
        };
        return $delegate;
    }]);
}]);

timApp.config(["$httpProvider", ($httpProvider: IHttpProvider) => {
    if (!$httpProvider.defaults.headers) {
        $httpProvider.defaults.headers = {};
    }
    if (!$httpProvider.defaults.headers.get) {
        $httpProvider.defaults.headers.get = {};
    }

    // disable IE ajax request caching; from https://stackoverflow.com/a/19771501
    $httpProvider.defaults.headers.get["If-Modified-Since"] = "Mon, 26 Jul 1997 05:00:00 GMT";
    $httpProvider.defaults.headers.get["Cache-Control"] = "no-cache";
    $httpProvider.defaults.headers.get.Pragma = "no-cache";

    // convert ISO 8601 date strings to moment objects
    ($httpProvider.defaults.transformResponse as IHttpResponseTransformer[]).push((responseData) => {
        convertDateStringsToMoments(responseData);
        return responseData;
    });
}]);

timApp.config(["$qProvider", ($qProvider: IQProvider) => {
    // Available in AngularJS 1.6
    // $qProvider.errorOnUnhandledRejections(true);
}]);

// Filter to make string URL friendly
timApp.filter("escape", () => {
    return (str: string) => {
        return encodeURIComponent(str).replace(/%2F/g, "/");
    };
});

timApp.filter("timdate", ["$filter", ($filter: IFilterService) => {
    return (date: string | Moment) => {
        if (typeof date === "string") {
            const dateFilter = $filter("date");
            return dateFilter(date, "dd.MM.yyyy HH:mm:ss");
        }
        if (!date) {
            return "(undefined or null)";
        }
        return date.format("DD.MM.YYYY HH:mm:ss");
    };
}]);

timApp.filter("timreldate", ["$filter", ($filter: IFilterService) => {
    return (date: string) => {
        return moment(date).fromNow();
    };
}]);

timApp.filter("timduration", ["$filter", ($filter: IFilterService) => {
    return (duration: string) => {
        return moment.duration(duration).humanize();
    };
}]);

timApp.filter("timpreciseduration", ["$filter", ($filter: IFilterService) => {
    return (duration: string) => {
        return humanizeDuration(moment.duration(duration).asMilliseconds());
    };
}]);

timApp.filter("timtim", ["$filter", ($filter: IFilterService) => {
    return (date: Moment) => {
        return date.format("HH:mm:ss");
    };
}]);

timApp.config(injectProviders);
timApp.run(injectServices);
timApp.run(initUserService);
timApp.run(loadMap);

// https://stackoverflow.com/questions/35629246/typescript-async-await-and-angular-q-service/41825004#41825004
timApp.run(["$window", "$q", ($window: IWindowService, $q: IQService) => {
    $window.Promise = $q;
}]);
