import type {
    HttpEvent,
    HttpHandler,
    HttpInterceptor,
    HttpRequest,
} from "@angular/common/http";
import {HttpClient, HttpXhrBackend} from "@angular/common/http";
import type {Observable} from "rxjs";

// NOTE: This module is imported by a lot of code, so it must not import other TIM modules.
// Otherwise it easily creates an import cycle that breaks the page startup.

let httpClient: HttpClient | undefined;
let standaloneHttpClient: HttpClient | undefined;
let standaloneInterceptors: () => HttpInterceptor[] = () => [];

/**
 * Sets the interceptors of the standalone client that getHttpClient returns before AppModule is bootstrapped.
 * Called by app.module.ts when it is loaded. Not meant to be called from elsewhere.
 */
export function setStandaloneHttpInterceptors(
    interceptors: () => HttpInterceptor[]
) {
    standaloneInterceptors = interceptors;
}

/**
 * Called by AppModule when it is bootstrapped. Not meant to be called from elsewhere.
 */
export function setHttpClient(http: HttpClient) {
    httpClient = http;
}

function encodeUriQuery(val: unknown): string {
    return encodeURIComponent(String(val))
        .replace(/%40/gi, "@")
        .replace(/%3A/gi, ":")
        .replace(/%24/g, "$")
        .replace(/%2C/gi, ",")
        .replace(/%3B/gi, ";")
        .replace(/%20/g, "+");
}

function serializeQueryValue(v: unknown): unknown {
    if (typeof v === "object" && v !== null) {
        return v instanceof Date ? v.toISOString() : JSON.stringify(v);
    }
    return v;
}

/**
 * Serializes an object to a URL query string (without the leading "?").
 *
 * Produces the same output as AngularJS $httpParamSerializer did: keys are sorted, null and undefined
 * values are skipped, arrays become repeated keys, dates become ISO strings and other objects JSON.
 *
 * Prefer this over HttpParams for values that come from the user: HttpParams does not encode "+",
 * so the server would read it as a space.
 */
export function serializeQueryParams(params: object | undefined): string {
    if (!params) {
        return "";
    }
    const obj = params as Record<string, unknown>;
    const parts: string[] = [];
    for (const key of Object.keys(obj).sort()) {
        const value = obj[key];
        if (
            value === null ||
            value === undefined ||
            typeof value === "function"
        ) {
            continue;
        }
        for (const v of Array.isArray(value) ? value : [value]) {
            parts.push(
                encodeUriQuery(key) +
                    "=" +
                    encodeUriQuery(serializeQueryValue(v))
            );
        }
    }
    return parts.join("&");
}

/**
 * Appends the given parameters to the URL as a query string. Returns the URL unchanged if there is nothing to append.
 */
export function withQueryParams(url: string, params: object | undefined) {
    const query = serializeQueryParams(params);
    if (!query) {
        return url;
    }
    return url + (url.includes("?") ? "&" : "?") + query;
}

/**
 * Adds the XSRF token header the same way HttpClientModule does.
 */
class XsrfInterceptor implements HttpInterceptor {
    intercept(
        req: HttpRequest<unknown>,
        next: HttpHandler
    ): Observable<HttpEvent<unknown>> {
        const lcUrl = req.url.toLowerCase();
        if (
            req.method === "GET" ||
            req.method === "HEAD" ||
            lcUrl.startsWith("http://") ||
            lcUrl.startsWith("https://")
        ) {
            return next.handle(req);
        }
        const cookie = /(?:^|;\s*)XSRF-TOKEN=([^;]+)/.exec(document.cookie);
        const token = cookie ? decodeURIComponent(cookie[1]) : undefined;
        if (token && !req.headers.has("X-XSRF-TOKEN")) {
            req = req.clone({headers: req.headers.set("X-XSRF-TOKEN", token)});
        }
        return next.handle(req);
    }
}

/**
 * Creates an HttpClient that does not need an Angular module. It has the same interceptors as AppModule's client
 * (see setStandaloneHttpInterceptors).
 */
function createStandaloneHttpClient(): HttpClient {
    const interceptors: HttpInterceptor[] = [
        ...standaloneInterceptors(),
        new XsrfInterceptor(),
    ];
    const backend: HttpHandler = new HttpXhrBackend({
        build: () => new XMLHttpRequest(),
    });
    const handler = interceptors.reduceRight<HttpHandler>(
        (next, interceptor) => ({
            handle: (req) => interceptor.intercept(req, next),
        }),
        backend
    );
    return new HttpClient(handler);
}

/**
 * Returns the HttpClient of the main Angular module (AppModule), including its interceptors
 * (date conversion, session verification, opt-in answer handling).
 *
 * This is meant for
 *
 *  * plain modules and helper classes that have no access to dependency injection
 *    (the code that used to import $http from tim/util/ngimport), and
 *  * plugin components that need the same behaviour as AngularJS $http had. Plugin modules import
 *    HttpClientModule themselves, so the HttpClient they inject has none of AppModule's interceptors.
 *
 * Other components, directives and services should inject HttpClient normally.
 *
 * AppModule is bootstrapped lazily when the first Angular component of the page is created.
 * Before that (or on a page that never bootstraps it), an equivalent standalone client is returned,
 * so this is safe to call at any time.
 */
export function getHttpClient(): HttpClient {
    if (httpClient) {
        return httpClient;
    }
    if (!standaloneHttpClient) {
        standaloneHttpClient = createStandaloneHttpClient();
    }
    return standaloneHttpClient;
}
