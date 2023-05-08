import type {
    IHttpInterceptorFactory,
    IHttpResponse,
    IRequestConfig,
} from "angular";
import angular from "angular";
import {timApp} from "tim/app";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {timLogTime} from "tim/util/timTiming";
import {
    handleExpiredSession,
    SESSION_VERIFICATION_NEEDED_CODE,
} from "tim/util/session-verify.interceptor";
import type {
    AnswerBrowserData,
    IAnswerSaveEvent,
} from "tim/answer/answer-browser.component";
import {$http, $httpProvider, $q} from "tim/util/ngimport";

export function handleAnswerResponse(
    taskIdFull: string,
    params: IAnswerSaveEvent
) {
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

export function prepareAnswerRequest(taskIdFull: string, url: string) {
    const parts = taskIdFull.split(".");
    const isFull = parts.length === 3;
    const docId = parseInt(parts[0], 10);
    const taskName = parts[1];
    const taskId = docId + "." + taskName;
    const v = vctrlInstance;
    if (!v) {
        throw new Error("ViewCtrl was undefined");
    }
    const d: {
        abData?: AnswerBrowserData;
        ref_from?: {docId: number; par?: string};
    } = {};
    if (taskName !== "") {
        const ab = v.getAnswerBrowser(taskId);
        if (ab) {
            d.abData = ab.getBrowserData();
        }
    }
    let e;
    if (isFull) {
        e = document.getElementById(taskIdFull);
    } else {
        e = document.querySelector(`[id^='${CSS.escape(taskIdFull)}.']`);
    }

    if (e) {
        const par = angular.element(e).parents(".par");
        d.ref_from = {docId: v.item.id, par: par.attr("id")};
    }
    return {
        url: url + window.location.search, // make urlmacros possible, must know the page url
        data: d,
    };
}

export function getTaskIdIfAnswerUrl(url: string): string | undefined {
    const fullUrl = new URL(url, window.location.origin);
    const re = /\/[^/]+\/([^/]+)\/answer$/;
    const match = re.exec(fullUrl.pathname);
    if (match) {
        return decodeURIComponent(match[1]);
    }
    return undefined;
}

timApp.config([
    () => {
        timLogTime("timApp config", "view");
        const interceptor = [
            () => {
                const tfre = /\/tableForm\/(updateFields|fetchTableData)/;
                return {
                    request(config: IRequestConfig) {
                        const url = config.url;
                        let newUrl = url;
                        const taskIdFull = getTaskIdIfAnswerUrl(url);
                        if (taskIdFull) {
                            const {url: newU, data} = prepareAnswerRequest(
                                taskIdFull,
                                url
                            );
                            config.data = {
                                ...config.data,
                                ...data,
                            };
                            newUrl = newU;
                        } else if (url.includes("/infosForTasks")) {
                            newUrl += window.location.search; // make urlmacros possible
                        } else if (tfre.test(url)) {
                            newUrl += window.location.search.replace("?", "&"); // make urlmacros possible
                        }
                        config.url = newUrl;
                        return config;
                    },
                    response(response: IHttpResponse<unknown>) {
                        const taskIdFull = getTaskIdIfAnswerUrl(
                            response.config.url
                        );
                        if (!taskIdFull) {
                            return response;
                        }
                        const resp =
                            response as IHttpResponse<IAnswerSaveEvent>;
                        handleAnswerResponse(taskIdFull, {
                            errors: resp.data.errors,
                            feedback: resp.data.feedback,
                            topfeedback: resp.data.topfeedback,
                            savedNew: resp.data.savedNew,
                            valid: resp.data.valid,
                            refresh: resp.data.refresh,
                            refreshPoints: resp.data.refreshPoints,
                        });
                        return response;
                    },
                    responseError(response: IHttpResponse<unknown>) {
                        const taskIdFull = getTaskIdIfAnswerUrl(
                            response.config.url
                        );
                        if (!taskIdFull) {
                            return $q.reject(response);
                        }
                        const resp =
                            response as IHttpResponse<IAnswerSaveEvent>;
                        handleAnswerResponse(taskIdFull, {
                            errors: resp.data.errors,
                            savedNew: false,
                            valid: false,
                        });
                        return $q.reject(response);
                    },
                };
            },
        ];
        $httpProvider.interceptors.push(
            interceptor as IHttpInterceptorFactory[]
        );
        $httpProvider.interceptors.push([
            () => {
                return {
                    async responseError(
                        response: angular.IHttpResponse<unknown>
                    ) {
                        if (
                            response.status === SESSION_VERIFICATION_NEEDED_CODE
                        ) {
                            await handleExpiredSession();
                            return $http(response.config);
                        }
                        return $q.reject(response);
                    },
                };
            },
        ]);
    },
]);
