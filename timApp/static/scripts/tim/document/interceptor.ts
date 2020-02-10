import angular, {IHttpInterceptorFactory, IHttpResponse, IRequestConfig} from "angular";
import {timApp} from "tim/app";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {timLogTime} from "tim/util/timTiming";
import {AnswerBrowserData, IAnswerSaveEvent} from "../answer/answerbrowser3";
import {$httpProvider, $q} from "../util/ngimport";

export function handleAnswerResponse(taskIdFull: string, params: IAnswerSaveEvent) {
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

export function prepareAnswerRequest(
    taskIdFull: string,
    url: string,
) {
    const parts = taskIdFull.split(".");
    const docId = parseInt(parts[0], 10);
    const taskName = parts[1];
    const taskId = docId + "." + taskName;
    const v = vctrlInstance;
    if (!v) {
        throw new Error("ViewCtrl was undefined");
    }
    const d: { abData?: AnswerBrowserData, ref_from?: { docId: number, par?: string } } = {};
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
    return {
        url: url + window.location.search, // make urlmacros possible, must know the page url
        data: d,
    };
}

timApp.config([() => {
    timLogTime("timApp config", "view");
    const interceptor = [
        () => {
            const re = /\/[^/]+\/([^/]+)\/answer\/$/;
            const tfre = /\/tableForm\/(updateFields|fetchTableData)/;
            return {
                request(config: IRequestConfig) {
                    const url = config.url;
                    let newUrl = url;
                    const match = re.exec(url);
                    if (match) {
                        const d = config.data as { abData?: unknown, ref_from?: unknown };
                        const taskIdFull = match[1];
                        const {url: newU, data} = prepareAnswerRequest(taskIdFull, url);
                        config.data = {
                            ...config.data,
                            ...data,
                        };
                        newUrl = newU;
                    } else if (url.includes("/infosForTasks")) {
                        newUrl += window.location.search;  // make urlmacros possible
                    } else if (tfre.test(url)) {
                        newUrl += window.location.search.replace("?", "&");  // make urlmacros possible
                    }
                    config.url = newUrl;
                    return config;
                },
                response(response: IHttpResponse<unknown>) {
                    const match = re.exec(response.config.url);
                    if (!match) {
                        return response;
                    }
                    const resp = response as IHttpResponse<IAnswerSaveEvent>;
                    handleAnswerResponse(match[1], {
                        error: resp.data.error,
                        savedNew: resp.data.savedNew,
                    });
                    return response;
                },
                responseError(response: IHttpResponse<unknown>) {
                    const match = re.exec(response.config.url);
                    if (!match) {
                        return $q.reject(response);
                    }
                    const resp = response as IHttpResponse<IAnswerSaveEvent>;
                    handleAnswerResponse(match[1], {
                        error: resp.data.error,
                        savedNew: false,
                    });
                    return $q.reject(response);
                },
            };
        },
    ];
    $httpProvider.interceptors.push(interceptor as IHttpInterceptorFactory[]);
}]);
