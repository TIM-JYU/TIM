import angular, {IHttpResponse, IRequestConfig} from "angular";
import {timApp} from "tim/app";
import {timLogTime} from "tim/util/timTiming";
import {$httpProvider, $q, $rootScope, $window} from "../util/ngimport";

timApp.config([() => {
    timLogTime("timApp config", "view");

    function escapeId(id: string) {
        return "#" + id.replace(/(:|\.|\[|\]|,|=)/g, "\\$1");
    }

    const interceptor = [
        () => {
            const re = /\/[^/]+\/([^/]+)\/answer\/$/;
            return {
                request(config: IRequestConfig) {
                    if (re.test(config.url)) {
                        const match = re.exec(config.url);
                        if (!match) {
                            return config;
                        }
                        const taskIdFull = match[1];
                        const parts = taskIdFull.split(".");
                        const docId = parseInt(parts[0], 10);
                        const taskName = parts[1];
                        const taskId = docId + "." + taskName;
                        if (taskName !== "") {
                            const ab = $window.item.vctrl.getAnswerBrowser(taskName);
                            if (ab !== undefined) {
                                angular.extend(config.data, {abData: ab.$ctrl.getBrowserData()});
                            }
                        }
                        const par = angular.element(escapeId(taskIdFull)).parents(".par");
                        angular.extend(config.data, {ref_from: {docId: $window.item.id, par: par.attr("id")}});
                    }
                    return config;
                },
                response(response: IHttpResponse<any>) {
                    if (re.test(response.config.url)) {
                        const match = re.exec(response.config.url);
                        if (!match) {
                            return response;
                        }
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
                responseError(response: any) {
                    if (re.test(response.config.url)) {
                        const match = re.exec(response.config.url);
                        if (!match) {
                            return $q.reject(response);
                        }
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
