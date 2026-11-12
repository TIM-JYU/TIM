import * as t from "io-ts";
import {isLeft} from "fp-ts/Either";
import {withDefault} from "tim/plugin/attributes";
import type {BodyInit} from "node-fetch";
import fetch from "node-fetch";
import FormData from "form-data";
import ivm from "isolated-vm";
import {BasicReporter} from "tim/plugin/errors";

const RequestFormData = t.type({
    type: t.literal("form-data"),
    data: t.record(t.string, t.string),
});

const RequestSearchParams = t.type({
    type: t.literal("form-urlencoded"),
    params: t.record(t.string, t.string),
});

const RequestJsonBody = t.type({
    type: t.literal("json"),
    data: t.UnknownRecord,
});

const RequestBody = t.union([
    t.string,
    t.undefined,
    RequestFormData,
    RequestSearchParams,
    RequestJsonBody,
]);

const RequestOptions = withDefault(
    t.type({
        method: withDefault(
            t.union([
                t.literal("GET"),
                t.literal("POST"),
                t.literal("PUT"),
                t.literal("DELETE"),
                t.literal("PATCH"),
            ]),
            "GET"
        ),
        query: t.union([t.record(t.string, t.string), t.undefined]),
        headers: withDefault(t.record(t.string, t.string), {}),
        body: RequestBody,
        responseType: withDefault(
            t.union([
                t.literal("json"),
                t.literal("text"),
                t.literal("status"),
            ]),
            "text"
        ),
        timeout: withDefault(t.number, 10000),
    }),
    {
        method: "GET",
        query: undefined,
        body: undefined,
        responseType: "text",
        headers: {},
        timeout: 10000,
    }
);

export async function request(url: unknown, opt: unknown) {
    if (!t.string.is(url)) {
        throw new Error("url must be a string");
    }
    const decodedOpt = RequestOptions.decode(opt);
    if (isLeft(decodedOpt)) {
        const errString = BasicReporter.report(decodedOpt)
            .map((e) => ` - ${e}`)
            .join("\n");
        throw new Error(
            `Invalid request options for the following values:\n${errString}`
        );
    }
    const options = decodedOpt.right;
    let body: BodyInit | undefined;
    const headers: Record<string, string> = {
        ...options.headers,
        "User-Agent": "tim_jsrunner",
    };

    if (options.body === undefined || typeof options.body === "string") {
        body = options.body;
    } else {
        if (options.body.type === "form-data") {
            const formData = new FormData();
            Object.entries(options.body.data).forEach(([key, value]) => {
                formData.append(key, value);
            });
            body = formData;
        } else if (options.body.type === "form-urlencoded") {
            const searchParams = new URLSearchParams();
            Object.entries(options.body.params).forEach(([key, value]) => {
                searchParams.append(key, value);
            });
            body = searchParams;
        } else if (options.body.type === "json") {
            body = JSON.stringify(options.body.data);
            headers["Content-Type"] = "application/json;charset=UTF-8";
        }
    }

    let reqUrl = url;
    if (options.query) {
        const urlObj = new URL(url);
        Object.entries(options.query).forEach(([key, value]) => {
            urlObj.searchParams.append(key, value);
        });
        reqUrl = urlObj.toString();
    }

    const response = await fetch(reqUrl, {
        method: options.method,
        headers,
        body,
    });

    if (options.responseType === "status") {
        return response.status;
    }
    if (options.responseType === "json") {
        return await response.json();
    }
    return await response.text();
}

// isolated-vm does not handle passing Promises well between the isolated environments.
// Therefore, we use the following trick:
// 1. Create a reference object of format {status: boolean; result?: unknown; error?: string}
// 2. Call the request function and bind to then/catch/finally that set the status when the request is done.
// 3. Return the reference object
//
// When the isolated environment calls the function, it will get the reference object and waits for status check in a loop.
// This allows to block the script while it waits for the request to finish.
export function ivmRequest(url: unknown, opts: unknown) {
    const ref = new ivm.Reference<{
        done: boolean;
        result?: unknown;
        error?: string;
    }>({
        done: false,
        result: undefined,
        error: undefined,
    });
    request(url, opts)
        .then((res) => {
            ref.setSync("result", res, {externalCopy: true});
        })
        .catch((err) => {
            let errorMessage = "Unknown error";
            if (!err) {
                errorMessage = "No error available";
            } else if (err instanceof Error) {
                errorMessage = `(${err.name}): ${err.message}`;
            } else {
                errorMessage = String(err);
            }
            ref.setSync("error", errorMessage);
        })
        .finally(() => {
            ref.setSync("done", true);
        });
    return ref;
}
