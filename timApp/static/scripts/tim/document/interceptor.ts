import angular, {IHttpInterceptorFactory, IHttpResponse, IRequestConfig} from "angular";
import {timApp} from "tim/app";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {timLogTime} from "tim/util/timTiming";
import {IAnswerSaveEvent} from "../answer/answerbrowser3";
import {$httpProvider, $q} from "../util/ngimport";

function handleResponse(taskIdFull: string, response: IHttpResponse<unknown>, params: IAnswerSaveEvent) {
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
            const tfre = /\/tableForm\/(updateFields|fetchTableData)/;
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
                        const d = config.data as {abData?: unknown, ref_from?: unknown};
                        if (taskName !== "") {
                            const ab = v.getAnswerBrowser(taskId);
                            if (ab) {
                                d.abData = ab.getBrowserData();
                            }
                        }
                        const e = document.getElementById(taskIdFull);
                        if (e) {
                            const par = angular.element(e).parents(".par");
                            d.ref_from = {docId: v.item.id, par: par.attr("id")};
                        }
                        config.url += window.location.search;  // make urlmacros possible, must know the page url
                    } else if (re2.test(config.url)) {
                        config.url += window.location.search;  // make urlmacros possible
                    } else if (tfre.test(config.url)) {
                        config.url += window.location.search.replace("?", "&");  // make urlmacros possible
                    }
                    return config;
                },
                response(response: IHttpResponse<unknown>) {
                    if (re.test(response.config.url)) {
                        const match = re.exec(response.config.url);
                        if (!match) {
                            return response;
                        }
                        const resp = response as IHttpResponse<IAnswerSaveEvent>;
                        handleResponse(match[1], response, {
                            error: resp.data.error,
                            savedNew: resp.data.savedNew,
                        });
                    }
                    return response;
                },
                responseError(response: IHttpResponse<unknown>) {
                    if (re.test(response.config.url)) {
                        const match = re.exec(response.config.url);
                        if (!match) {
                            return $q.reject(response);
                        }
                        const resp = response as IHttpResponse<IAnswerSaveEvent>;
                        handleResponse(match[1], response, {
                            error: resp.data.error,
                            savedNew: false,
                        });
                    }
                    return $q.reject(response);
                },
            };
        },
    ];
    $httpProvider.interceptors.push(interceptor as IHttpInterceptorFactory[]);
}]);
