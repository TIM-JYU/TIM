import * as t from "io-ts";
import type {Result} from "tim/util/utils";
import {to2} from "tim/util/utils";
import {InputDialogKind} from "tim/ui/input-dialog.kind";
import {showInputDialog} from "tim/ui/showInputDialog";

export const TemplateParam = t.intersection([
    t.type({
        default: t.string,
    }),
    t.partial({
        pattern: t.string,
        error: t.string,
        text: t.string,
        what: t.string,
        useDefault: t.boolean,
        flags: t.string,
        select: t.array(t.string),
        radio: t.array(t.string),
    }),
]);

export interface ITemplateParam extends t.TypeOf<typeof TemplateParam> {}

export async function showTemplateReplaceDialog(
    data: string,
    param: ITemplateParam
): Promise<string> {
    const re = new RegExp(param.pattern ?? ".*");

    let extraOptions = {};
    if (param.select) {
        extraOptions = {
            inputType: "select",
            options: param.select,
        };
    }
    if (param.radio) {
        extraOptions = {
            inputType: "radio",
            options: param.radio,
        };
    }

    let replace;
    if (param.useDefault) {
        replace = {ok: true, result: param.default};
    } else {
        replace = await to2(
            showInputDialog({
                isInput: InputDialogKind.InputAndValidator,
                text: param.text ?? `Replacement for ${param.default}`,
                title: "Parameter",
                okText: "OK",
                defaultValue: param.default,
                validator: (input) =>
                    new Promise<Result<string, string>>((res) => {
                        if (!input.match(re)) {
                            return res({
                                ok: false,
                                result: param.error ?? "",
                            });
                        }
                        return res({ok: true, result: input});
                    }),
                ...extraOptions,
            })
        );
    }
    if (!replace.ok) {
        return "";
    }
    let flags = "";
    if (param.what) {
        flags = param.flags ?? "gm";
    }
    const what = param.what ?? "\\\\\\?";

    return data.replace(new RegExp(what, flags), replace.result);
}

export async function replaceTemplateValues(data: string): Promise<string> {
    const replaceList: ITemplateParam[] = [];
    const rows = data.split("\n");
    while (rows?.[0].startsWith("- {")) {
        const s = rows[0].substring(2);
        try {
            const jso = JSON.parse(s);
            if (TemplateParam.is(jso)) {
                replaceList.push(jso);
            }
            rows.shift(); // Don't stay in the loop
        } catch (e) {
            return "" + e + "\n" + s;
        }
    }
    if (!replaceList) {
        return data;
    }
    data = rows.join("\n");

    for (const param of replaceList) {
        data = await showTemplateReplaceDialog(data, param);
        if (!data) {
            return "";
        }
    }
    return data;
}
