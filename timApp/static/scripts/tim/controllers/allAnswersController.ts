
import moment from "moment";
import {timApp} from "tim/app";
timApp.controller("AllAnswersCtrl", ["$uibModalInstance", "$window", "$httpParamSerializer", "$localStorage", "$http", "$log", "options",
    function($uibModalInstance, $window, $httpParamSerializer, $localStorage, $http, $log, options) {
        "use strict";
        moment.locale("en", {
            week: {dow: 1, doy: 4}, // set Monday as the first day of the week
        });
        const $ctrl = this;

        $ctrl.showSort = options.allTasks;
        $ctrl.options = {};
        $ctrl.options.age = "max";
        $ctrl.options.valid = "1";
        $ctrl.options.name = "both";
        $ctrl.options.sort = "username";
        $ctrl.$storage = $localStorage.$default({
            allAnswersOptions: $ctrl.options,
        });

        $ctrl.options = $ctrl.$storage.allAnswersOptions;
        $ctrl.options.periodFrom = $ctrl.options.periodFrom || Date.now();
        $ctrl.options.periodTo = $ctrl.options.periodTo || Date.now();
        $ctrl.datePickerOptionsFrom = {
            format: "D.M.YYYY HH:mm:ss",
            defaultDate: moment($ctrl.options.periodFrom),
            showTodayButton: true,
        };
        $ctrl.datePickerOptionsTo = {
            format: "D.M.YYYY HH:mm:ss",
            defaultDate: moment($ctrl.options.periodTo),
            showTodayButton: true,
        };

        $ctrl.ok = function() {
            if ($ctrl.options.periodFrom) {
                $ctrl.options.periodFrom = $ctrl.options.periodFrom.toDate();
            }
            if ($ctrl.options.periodTo) {
                $ctrl.options.periodTo = $ctrl.options.periodTo.toDate();
            }
            $window.open(options.url + "?" + $httpParamSerializer($ctrl.options), "_blank");
            $uibModalInstance.close("close");
        };

        $ctrl.cancel = function() {
            $uibModalInstance.dismiss("cancel");
        };

        $ctrl.lastFetch = null;
        $http.get("/settings/get/last_answer_fetch").then(function(response) {
            if (response.data.last_answer_fetch) {
                $ctrl.lastFetch = response.data.last_answer_fetch[options.identifier];
                if (!$ctrl.lastFetch) {
                    $ctrl.lastFetch = "no fetches yet";
                }
            }
        }, function(response) {
            $log.error("Failed to fetch last answer time");
        });
    }]);
