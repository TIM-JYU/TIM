import * as t from "io-ts";
import {Result, to2} from "../util/utils";
import {InputDialogKind} from "./input-dialog.kind";
import {showInputDialog} from "./showInputDialog";

export const TemplateParam = t.intersection([
    t.type({
        default: t.string,
        text: t.string,
    }),
    t.partial({
        pattern: t.string,
        error: t.string,
        what: t.string,
        flags: t.string,
    }),
]);

export interface ITemplateParam extends t.TypeOf<typeof TemplateParam> {}

export async function showTemplateReplaceDialog(
    data: string,
    param: ITemplateParam
): Promise<string> {
    const re = new RegExp(param.pattern ?? ".*");
    const replace = await to2(
        showInputDialog({
            isInput: InputDialogKind.InputAndValidator,
            text: param.text,
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
        })
    );
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
                rows.shift();
            }
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
