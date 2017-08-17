import angular from "angular";
import {timApp} from "tim/app";
import {timLogTime} from "tim/timTiming";
import {$httpProvider, $q, $rootScope, $window} from "../../ngimport";

timApp.config([() => {
    timLogTime("timApp config", "view");
    function escapeId(id) {
        return "#" + id.replace(/(:|\.|\[|\]|,|=)/g, "\\$1");
    }

    const interceptor = [
        () => {
            const re = /\/[^/]+\/([^/]+)\/answer\/$/;
            return {
                request(config) {
                    if (re.test(config.url)) {
                        const match = re.exec(config.url);
                        const taskIdFull = match[1];
                        const parts = taskIdFull.split(".");
                        const docId = parseInt(parts[0], 10);
                        const taskName = parts[1];
                        const taskId = docId + "." + taskName;
                        if (taskName !== "") {
                            const ab = angular.element("answerbrowser[task-id='" + taskId + "']");
                            if (ab.isolateScope()) {
                                const browserScope = ab.isolateScope<any>();
                                angular.extend(config.data, {abData: browserScope.getBrowserData()});
                            }
                        }
                        const par = angular.element(escapeId(taskIdFull)).parents(".par");
                        angular.extend(config.data, {ref_from: {docId: $window.item.id, par: par.attr("id")}});
                    }
                    return config;
                },
                response(response) {
                    if (re.test(response.config.url)) {
                        const match = re.exec(response.config.url);
                        const taskIdFull = match[1];
                        const parts = taskIdFull.split(".");
                        const docId = parseInt(parts[0], 10);
                        const taskName = parts[1];
                        const taskId = docId + "." + taskName;
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
                        const match = re.exec(response.config.url);
                        const taskIdFull = match[1];
                        const parts = taskIdFull.split(".");
                        const docId = parseInt(parts[0], 10);
                        const taskName = parts[1];
                        const taskId = docId + "." + taskName;
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
