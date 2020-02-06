import angular, {ICompileProvider, IFilterService, IHttpProvider, IHttpResponseTransformer, IModule} from "angular";
import aedatetimepicker from "angular-eonasdan-datetimepicker";
import ngMessages from "angular-messages";
import ngSanitize from "angular-sanitize";
import timer from "angular-timer";
import uibootstrap from "angular-ui-bootstrap";
import colorpicker from "angularjs-color-picker";
import humanizeDuration from "humanize-duration";
import moment, {Moment} from "moment";
import ngFileUpload from "ng-file-upload";
import ngStorage from "ngstorage";
import {convertDateStringsToMoments, markAsUsed} from "tim/util/utils";
import {tr} from "./ui/language";
import {KEY_ENTER, KEY_S} from "./util/keycodes";
import {injectProviders, injectServices} from "./util/ngimport";

moment.updateLocale("en", {
    week: {dow: 1, doy: 4}, // set Monday as the first day of the week
});

markAsUsed(ngMessages, timer, aedatetimepicker, ngSanitize,
    uibootstrap, ngFileUpload, ngStorage, colorpicker);

// timApp's Angular modules:
// base: 'ngMessages', 'timer', 'ae-datetimepicker', 'ngSanitize', 'ui.bootstrap'
// item: 'ngFileUpload'
// view_html: 'ngStorage' + plugin modules
// teacher mode: 'ui.grid', 'ui.grid.cellNav', 'ui.grid.selection', 'ui.grid.exporter', 'ui.grid.autoResize'
export const timApp = angular.module("timApp", [
    "ngMessages",
    "timer",
    "ae-datetimepicker",
    "ngSanitize",
    "ui.bootstrap",
    "ngFileUpload",
    "ngStorage",
    "color.picker",
]);

interface IDelegate {
    onUrlChange(): void;

    url(): string;
}

// disable Angular URL manipulation when using ng-include; from http://stackoverflow.com/a/19825756
timApp.config(["$provide", ($provide: IModule) => {
    $provide.decorator("$browser", ["$delegate", ($delegate: IDelegate) => {
        $delegate.onUrlChange = () => {
        };
        $delegate.url = () => {
            return "";
        };
        return $delegate;
    }]);
}]);

timApp.config(["$compileProvider", (cp: ICompileProvider) => {
    cp.debugInfoEnabled(false);
    cp.commentDirectivesEnabled(false);
    cp.cssClassDirectivesEnabled(false);

    // many 3rd party libs break if this is enabled
    // cp.strictComponentBindingsEnabled(true);
}]);

timApp.config(["$httpProvider", ($httpProvider: IHttpProvider) => {
    if (!$httpProvider.defaults.headers) {
        $httpProvider.defaults.headers = {};
    }
    if (!$httpProvider.defaults.headers.get) {
        $httpProvider.defaults.headers.get = {};
    }

    // disable IE ajax request caching; from https://stackoverflow.com/a/19771501
    const hdrs = $httpProvider.defaults.headers.get as {[s: string]: string};
    hdrs["If-Modified-Since"] = "Mon, 26 Jul 1997 05:00:00 GMT";
    hdrs["Cache-Control"] = "no-cache";
    hdrs.Pragma = "no-cache";

    // convert ISO 8601 date strings to moment objects
    ($httpProvider.defaults.transformResponse as IHttpResponseTransformer[]).push((responseData, headers) => {
        if (headers("No-Date-Conversion") !== "true") {
            return convertDateStringsToMoments(responseData);
        }
        return responseData;
    });
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

timApp.filter("tr", [() => {
    return tr;
}]);

timApp.directive("onSave", () => {
    return (scope, element, attrs) => {
        element.bind("keydown", (event) => {
            if ((event.ctrlKey && (event.which === KEY_ENTER || event.which === KEY_S)) ||
                (element[0] instanceof HTMLInputElement && event.which === KEY_ENTER)) {
                scope.$apply(() => {
                    scope.$eval(attrs.onSave as string);
                });

                event.preventDefault();
            }
        });
    };
});

timApp.config(injectProviders);
timApp.run(injectServices);

interface ISanitizeProvider {
    addValidAttrs(attrs: string[]): void;
}

timApp.config(["$sanitizeProvider", ($sanitizeProvider: ISanitizeProvider) => {
    $sanitizeProvider.addValidAttrs(["style"]);
}]);
