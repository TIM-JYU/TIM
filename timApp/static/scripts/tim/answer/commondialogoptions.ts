// from https://github.com/gcanti/io-ts/blob/master/index.md#custom-types
import {maybeUndefined} from "tim/plugin/attributes";
import * as t from "io-ts";
import {either} from "fp-ts/Either";

const DateFromString = new t.Type<Date, string, unknown>(
    "DateFromString",
    (u): u is Date => u instanceof Date,
    (u, c) =>
        either.chain(t.string.validate(u, c), (s) => {
            const d = new Date(s);
            return isNaN(d.getTime()) ? t.failure(u, c) : t.success(d);
        }),
    (a) => a.toISOString()
);
export const CommonDialogOptions = t.type({
    period: t.keyof({
        whenever: null,
        day: null,
        week: null,
        month: null,
        other: null,
    }),
    name: t.string,
    valid: t.string,
    periodFrom: maybeUndefined(DateFromString),
    periodTo: maybeUndefined(DateFromString),
});
