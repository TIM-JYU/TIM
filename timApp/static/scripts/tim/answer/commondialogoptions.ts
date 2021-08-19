import {maybeUndefined} from "tim/plugin/attributes";
import * as t from "io-ts";
import {DateFromString} from "tim/util/utils";

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
