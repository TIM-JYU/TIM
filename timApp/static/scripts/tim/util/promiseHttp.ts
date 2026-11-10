import type {HttpErrorResponse} from "@angular/common/http";
import {HttpContext, HttpHeaders} from "@angular/common/http";
import {getHttpClient, withQueryParams} from "tim/util/http";
import {refreshUI} from "tim/util/refresh";
import {LEGACY_ANSWER_HANDLING} from "tim/util/answer.interceptor";

/**
 * A successful response of {@link promiseHttp}.
 */
export interface IHttpResponse<T> {
    data: T;
    status: number;
}

/**
 * The options of a {@link promiseHttp} request.
 */
export interface IRequestConfig {
    params?: object;
    headers?: Record<string, string>;
}

interface IFullRequestConfig extends IRequestConfig {
    url: string;
    method: string;
    data?: unknown;
}

async function request<T>(
    config: IFullRequestConfig
): Promise<IHttpResponse<T>> {
    const method = config.method.toUpperCase();
    try {
        const resp = await new Promise<{body: T | null; status: number}>(
            (resolve, reject) => {
                getHttpClient()
                    .request<T>(
                        method,
                        withQueryParams(config.url, config.params),
                        {
                            body: config.data,
                            headers: new HttpHeaders(config.headers ?? {}),
                            observe: "response",
                            responseType: "json",
                            // The answer routes get the same extra handling that they got from the $http interceptor.
                            context: new HttpContext().set(
                                LEGACY_ANSWER_HANDLING,
                                true
                            ),
                        }
                    )
                    .subscribe({next: resolve, error: reject});
            }
        );
        return {data: resp.body as T, status: resp.status};
    } catch (e) {
        const err = e as HttpErrorResponse;
        // The same shape as an AngularJS $http error: callers read e.g. "r.result.data.error".
        // eslint-disable-next-line no-throw-literal
        throw {data: err.error as unknown, status: err.status};
    } finally {
        refreshUI();
    }
}

/**
 * A promise-based HTTP helper for the code that is not part of any Angular component
 * (the document view and its helper classes).
 *
 * A successful request resolves to `{data, status}`. A failed request rejects with
 * `{data, status}` where `data` is the error response of the server (usually `{error: string}`).
 * The views are refreshed after every request (see {@link refreshUI}).
 *
 * In Angular components and services, inject HttpClient instead.
 */
export const promiseHttp = {
    request,
    get<T>(url: string, config?: IRequestConfig) {
        return request<T>({...config, url, method: "GET"});
    },
    delete<T>(url: string, config?: IRequestConfig) {
        return request<T>({...config, url, method: "DELETE"});
    },
    post<T>(url: string, data?: unknown, config?: IRequestConfig) {
        return request<T>({...config, url, method: "POST", data});
    },
    put<T>(url: string, data?: unknown, config?: IRequestConfig) {
        return request<T>({...config, url, method: "PUT", data});
    },
    patch<T>(url: string, data?: unknown, config?: IRequestConfig) {
        return request<T>({...config, url, method: "PATCH", data});
    },
};
