
import angular from "angular";
import {timApp} from "tim/app";
import {timLogTime} from "tim/timTiming";

timApp.config(["$httpProvider", function($httpProvider) {
    "use strict";
    timLogTime("timApp config", "view");
    function escapeId(id) {
        return "#" + id.replace(/(:|\.|\[|\]|,|=)/g, "\\$1");
    }

    let interceptor = [
        "$q",
        "$rootScope",
        "$window",
        function($q, $rootScope, $window) {
            let re = /\/[^/]+\/([^/]+)\/answer\/$/;
            return {
                request(config) {
                    if (re.test(config.url)) {
                        let match = re.exec(config.url);
                        let taskIdFull = match[1];
                        let parts = taskIdFull.split(".");
                        let docId = parseInt(parts[0], 10);
                        let taskName = parts[1];
                        let taskId = docId + "." + taskName;
                        if (taskName !== "") {
                            let ab = angular.element("answerbrowser[task-id='" + taskId + "']");
                            if (ab.isolateScope()) {
                                let browserScope = ab.isolateScope();
                                angular.extend(config.data, {abData: browserScope.getBrowserData()});
                            }
                        }
                        let par = angular.element(escapeId(taskIdFull)).parents(".par");
                        angular.extend(config.data, {ref_from: {docId: $window.item.id, par: par.attr("id")}});
                    }
                    return config;
                },
                response(response) {
                    if (re.test(response.config.url)) {
                        let match = re.exec(response.config.url);
                        let taskIdFull = match[1];
                        let parts = taskIdFull.split(".");
                        let docId = parseInt(parts[0], 10);
                        let taskName = parts[1];
                        let taskId = docId + "." + taskName;
                        $rootScope.$broadcast("answerSaved", {
                            taskId,
                            savedNew: response.data.savedNew,
                            error: response.data.error,
                        });
                    }
                    return response;
                },
                responseError(response) {
                    if (re.test(response.config.url)) {
                        let match = re.exec(response.config.url);
                        let taskIdFull = match[1];
                        let parts = taskIdFull.split(".");
                        let docId = parseInt(parts[0], 10);
                        let taskName = parts[1];
                        let taskId = docId + "." + taskName;
                        $rootScope.$broadcast("answerSaved", {
                            taskId,
                            savedNew: false,
                            error: response.data.error,
                        });
                    }
                    return $q.reject(response);
                },
            };
        },
    ];
    $httpProvider.interceptors.push(interceptor);
}]);
