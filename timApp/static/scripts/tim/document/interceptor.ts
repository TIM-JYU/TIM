import angular, {IHttpResponse, IRequestConfig} from "angular";
import {timApp} from "tim/app";
import {timLogTime} from "tim/util/timTiming";
import {IAnswerSaveEvent} from "../answer/answerbrowser3";
import {$httpProvider, $q, $window} from "../util/ngimport";
import {vctrlInstance} from "./viewctrl";

function handleResponse(taskIdFull: string, response: IHttpResponse<any>, params: IAnswerSaveEvent) {
    const parts = taskIdFull.split(".");
    const docId = parseInt(parts[0], 10);
    const taskName = parts[1];
    const taskId = docId + "." + taskName;
    const v = vctrlInstance;
    if (!v) {
        throw new Error("ViewCtrl was undefined");
    }
    const ab = v.getAnswerBrowser(taskId);
    if (ab) {
        ab.registerNewAnswer(params);
    }
}

timApp.config([() => {
    timLogTime("timApp config", "view");
    const interceptor = [
        () => {
            const re = /\/[^/]+\/([^/]+)\/answer\/$/;
            const re2 = /\/infosForTasks/;  // add here all routes that needs doc
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
                        const v = vctrlInstance;
                        if (!v) {
                            throw new Error("ViewCtrl was undefined");
                        }
                        if (taskName !== "") {
                            const ab = v.getAnswerBrowser(taskId);
                            if (ab) {
                                config.data.abData = ab.getBrowserData();
                            }
                        }
                        const e = document.getElementById(taskIdFull);
                        if (e) {
                            const par = angular.element(e).parents(".par");
                            config.data.ref_from = {docId: $window.item.id, par: par.attr("id")};
                        }
                        config.url += window.location.search;  // make urlmacros possible
                    } else if ( re2.test(config.url)) {
                        config.url += window.location.search;  // make urlmacros possible
                    }
                    return config;
                },
                response(response: IHttpResponse<any>) {
                    if (re.test(response.config.url)) {
                        const match = re.exec(response.config.url);
                        if (!match) {
                            return response;
                        }
                        handleResponse(match[1], response, {
                            error: response.data.error,
                            savedNew: response.data.savedNew,
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
                        handleResponse(match[1], response, {
                            error: response.data.error,
                            savedNew: false,
                        });
                    }
                    return $q.reject(response);
                },
            };
        },
    ];
    $httpProvider.interceptors.push(interceptor);
}]);
