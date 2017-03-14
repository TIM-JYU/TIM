import * as angular from 'angular';
import * as moment from 'moment';
import * as humanizeDuration from 'humanize-duration';
import * as ngMessages from 'angular-messages';
import * as timer from 'angular-timer';
import * as aedatetimepicker from 'angular-eonasdan-datetimepicker';
import * as ngSanitize from 'angular-sanitize';
import * as uibootstrap from 'angular-ui-bootstrap';
import * as ngFileUpload from 'ng-file-upload';
import * as ngStorage from 'ngstorage';
import {markAsAngular1Module} from 'tim/angular-utils';

markAsAngular1Module(ngMessages, timer, aedatetimepicker, ngSanitize, uibootstrap, ngFileUpload, ngStorage);

// hack: expose moment in global scope because otherwise angular-eonasdan-datetimepicker cannot find it
declare let window: any;
window.moment = moment;

// timApp's Angular modules:
// base: 'ngMessages', 'timer', 'ae-datetimepicker', 'ngSanitize', 'ui.bootstrap'
// item: 'ngFileUpload'
// view_html: 'oc.lazyLoad', 'ui.ace', 'ngStorage' + plugin modules
// teacher mode: 'ui.grid', 'ui.grid.cellNav', 'ui.grid.selection', 'ui.grid.exporter', 'ui.grid.autoResize'
export const timApp = angular.module('timApp', [
    'ngMessages',
    'timer',
    'ae-datetimepicker',
    'ngSanitize',
    'ui.bootstrap',
    'ngFileUpload',
    'ngStorage',
    'oc.lazyLoad',
]);
// disable Angular URL manipulation when using ng-include; from http://stackoverflow.com/a/19825756
timApp.config(['$provide', function ($provide) {
    $provide.decorator('$browser', ['$delegate', function ($delegate) {
        $delegate.onUrlChange = function () {
        };
        $delegate.url = function () {
            return "";
        };
        return $delegate;
    }]);
}]);

// disable IE ajax request caching; from https://stackoverflow.com/a/19771501
timApp.config(['$httpProvider', function ($httpProvider) {
    if (!$httpProvider.defaults.headers.get) {
        $httpProvider.defaults.headers.get = {};
    }

    $httpProvider.defaults.headers.get['If-Modified-Since'] = 'Mon, 26 Jul 1997 05:00:00 GMT';
    $httpProvider.defaults.headers.get['Cache-Control'] = 'no-cache';
    $httpProvider.defaults.headers.get['Pragma'] = 'no-cache';
}]);

// Filter to make string URL friendly
timApp.filter('escape', function () {
    "use strict";
    return function (str) {
        return encodeURIComponent(str).replace(/%2F/g, '/');
    };
});

timApp.filter('timdate', function ($filter) {
    let dateFilter = $filter('date');
    return function (date) {
        return dateFilter(date, 'dd.MM.yyyy HH:mm:ss');
    }
});

timApp.filter('timreldate', function ($filter) {
    return function (date) {
        return moment(date).fromNow();
    }
});

timApp.filter('timduration', function ($filter) {
    return function (duration) {
        return moment.duration(duration).humanize();
    }
});

timApp.filter('timpreciseduration', function ($filter) {
    return function (duration) {
        return humanizeDuration(moment.duration(duration).asMilliseconds());
    }
});

timApp.filter('timtim', function ($filter) {
    let dateFilter = $filter('date');
    return function (date) {
        return dateFilter(date, 'HH:mm:ss');
    }
});
