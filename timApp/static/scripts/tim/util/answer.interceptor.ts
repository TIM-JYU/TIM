import {Injectable} from "@angular/core";
import type {
    HttpEvent,
    HttpHandler,
    HttpInterceptor,
    HttpRequest,
} from "@angular/common/http";
import {
    HTTP_INTERCEPTORS,
    HttpContext,
    HttpContextToken,
    HttpErrorResponse,
    HttpResponse,
} from "@angular/common/http";
import type {Observable} from "rxjs";
import {tap} from "rxjs";
import {
    getTaskIdIfAnswerUrl,
    handleAnswerResponse,
    prepareAnswerRequest,
} from "tim/document/interceptor";
import type {IAnswerSaveEvent} from "tim/answer/answer-browser.component";

/**
 * Opt-in flag for {@link AnswerInterceptor}.
 *
 * HttpClient requests are NOT processed by default, because several plugins already call
 * prepareAnswerRequest/handleAnswerResponse by hand (e.g. AngularPluginBase.postAnswer) and others
 * intentionally post to answer URLs without registering an answer.
 */
export const LEGACY_ANSWER_HANDLING = new HttpContextToken<boolean>(
    () => false
);

/**
 * Returns a context that makes an HttpClient request behave like an AngularJS $http request did:
 *
 * * `.../answer` requests get the answer browser data and the page's URL macros attached,
 *   and the response is registered to the task's answer browser.
 * * `/infosForTasks` and `/tableForm/(updateFields|fetchTableData)` requests get the page's URL macros.
 *
 * Meant for call sites that are migrated from $http: `http.put(url, data, {context: withAnswerHandling()})`.
 */
export function withAnswerHandling(context: HttpContext = new HttpContext()) {
    return context.set(LEGACY_ANSWER_HANDLING, true);
}

const TABLEFORM_URL = /\/tableForm\/(updateFields|fetchTableData)/;

function isPlainObject(o: unknown): o is Record<string, unknown> {
    return (
        typeof o === "object" &&
        o !== null &&
        Object.getPrototypeOf(o) === Object.prototype
    );
}

/**
 * Adds the answer browser data to answer requests and registers the responses to the answer browsers
 * (see tim/document/interceptor.ts for the helper functions).
 */
@Injectable()
export class AnswerInterceptor implements HttpInterceptor {
    intercept(
        request: HttpRequest<unknown>,
        next: HttpHandler
    ): Observable<HttpEvent<unknown>> {
        if (!request.context.get(LEGACY_ANSWER_HANDLING)) {
            return next.handle(request);
        }

        const url = request.url;
        const taskIdFull = getTaskIdIfAnswerUrl(url);
        if (!taskIdFull) {
            let newUrl = url;
            if (url.includes("/infosForTasks")) {
                newUrl += window.location.search;
            } else if (TABLEFORM_URL.test(url)) {
                newUrl += window.location.search.replace("?", "&");
            }
            return next.handle(
                newUrl === url ? request : request.clone({url: newUrl})
            );
        }

        const {url: newUrl, data} = prepareAnswerRequest(taskIdFull, url);
        const body = request.body;
        const newRequest = request.clone({
            url: newUrl,
            body:
                body == null
                    ? data
                    : isPlainObject(body)
                      ? {...body, ...data}
                      : body,
        });

        return next.handle(newRequest).pipe(
            tap({
                next: (event) => {
                    if (!(event instanceof HttpResponse)) {
                        return;
                    }
                    const resp = (event.body ?? {}) as IAnswerSaveEvent;
                    handleAnswerResponse(taskIdFull, {
                        errors: resp.errors,
                        feedback: resp.feedback,
                        topfeedback: resp.topfeedback,
                        savedNew: resp.savedNew,
                        valid: resp.valid,
                        refresh: resp.refresh,
                        refreshPoints: resp.refreshPoints,
                    });
                },
                error: (error: unknown) => {
                    const resp = (
                        error instanceof HttpErrorResponse &&
                        isPlainObject(error.error)
                            ? error.error
                            : {}
                    ) as Partial<IAnswerSaveEvent>;
                    handleAnswerResponse(taskIdFull, {
                        errors: resp.errors,
                        savedNew: false,
                        valid: false,
                    });
                },
            })
        );
    }
}

/**
 * Must be listed before SessionVerify so that a request that is retried after session verification
 * is registered only once, with its final result.
 */
export const AnswerInterceptorProvider = {
    provide: HTTP_INTERCEPTORS,
    useClass: AnswerInterceptor,
    multi: true,
};
