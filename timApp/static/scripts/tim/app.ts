import angular, {
    ICompileProvider, IExceptionHandlerService,
    IFilterService,
    IHttpProvider,
    IHttpResponseTransformer,
    IModule,
    IParseService, IRootScopeService,
} from "angular";
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
import {KEY_ENTER, KEY_S} from "./util/keycodes";
import {injectProviders, injectServices} from "./util/ngimport";
import TriggeredEvent = JQuery.TriggeredEvent;

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


// Minimal fix for clicks not registering in AngularJS components on iOS devices
// On Safari on iOS, `click` event right away, which causes a "double tap" problem in which
// certain buttons need to be tapped twice before an event handler fires.
// The behaviour appears to be different on different browsers:
// * On Chrome both `click` and `touchstart` events are fired when a button is tapped, which works fine.
// * On iOS Safari, `touchstart` is fired right away, but `click` only after a focus on the element.
//
// In addition, Safari seems to consume events: if `touchstart` is fired (even if it was bubbled up),
// `click` is consumed as well, which prevents another relevant `click` event from firing.
//
// The fix below adds an **additional** ngClick directive on iOS that installs both click and touchstart events.
// On iOS `touchstart` event seems to consume the underlying `click` events, which fixes the issue.
// On PCs this fix doesn't work (it actually causes a doubleclick issue), which is why it's disabled.
//
// More info:
// https://stackoverflow.com/questions/34575510/angular-ng-click-issues-on-safari-with-ios-8-3/34579185#34579185
// https://github.com/angular/angular.js/blob/master/src/ng/directive/ngEventDirs.js#L62

    timApp.directive("ngClick",
    ["$parse", "$rootScope", "$exceptionHandler",
        ($parse: IParseService, $rootScope: IRootScopeService, $exceptionHandler: IExceptionHandlerService) => {
            return {
                restrict: "A",
                compile: ($el, attr) => {
                    const fn = $parse(attr.ngClick as string);
                    return (scope, el) => {
                        const handleClickAndTouch = (event: TriggeredEvent) => {
                            // eslint-disable-next-line @typescript-eslint/tslint/config
                            const callback = () => fn(scope, {$event: event});

                            if (!$rootScope.$$phase) {
                                scope.$apply(callback);
                            } else {
                                try {
                                    callback();
                                } catch (e) {
                                    $exceptionHandler(e as Error);
                                }
                            }
                        };

                        el.on("touchstart click", handleClickAndTouch);
                    };
                },
            };
        }]);


timApp.config(injectProviders);
timApp.run(injectServices);

interface ISanitizeProvider {
    addValidAttrs(attrs: string[]): void;
}

timApp.config(["$sanitizeProvider", ($sanitizeProvider: ISanitizeProvider) => {
    $sanitizeProvider.addValidAttrs(["style"]);
}]);
