import {Left} from "fp-ts/lib/Either";
import * as t from "io-ts";
import {Context} from "io-ts";

function getEssentialContext(c: t.Context) {
    for (let i = c.length - 1; i >= 0; i--) {
        if (isNaN(parseInt(c[i].key, 10))) {
            return c.slice(0, i + 1);
        }
    }
    return c;
}

type MarkupError = Array<{name: string, type: string}>;

function isPrefixOfSome(s: string, others: string[]) {
    for (const o of others) {
        if (o.startsWith(s + ".")) {
            return true;
        }
    }
    return false;
}

function indexOfFirstName(context: Context) {
    return context.findIndex((c) => c.key.match(/^[a-z]/) != null);
}

export function getErrors<A>(v: Left<t.Errors, A>): MarkupError {
    const ps: Array<[string[], string]> = v.value
        .filter((e) => {
            const ind = indexOfFirstName(e.context);
            if (ind >= 0) {
                return e.context.length >= 3 && e.context[0].key === "" && e.context[ind].key === "markup";
            }
        })
        .map((error) => getEssentialContext(error.context))
        .map((error) => [error.slice(indexOfFirstName(error) + 2).map((x) => x.key), error[error.length - 1].type.name]);
    const errs = new Map<string, Set<string>>();
    const knownKeys = ps.map(([keys, _]) => keys.join("."));
    for (const [keys, typ] of ps) {
        const key = keys.join(".");
        // don't report parent fields because it's not useful
        if (isPrefixOfSome(key, knownKeys)) {
            continue;
        }
        // avoid too verbose messages
        let type = typ;
        if (type.length > 50) {
            type = "valid object";
        }
        if (type.startsWith("(") && type.endsWith(")")) {
            type = type.slice(1, type.length - 1);
        }
        type = type.replace(/ \| /g, " or ");
        const vals = errs.get(key);
        if (vals == null) {
            errs.set(key, new Set([type]));
        } else {
            vals.add(type);
        }
    }
    const result = [];
    for (const [key, types] of errs.entries()) {
        result.push({type: Array.from(types).join(" or "), name: key});
    }
    return result;
}
