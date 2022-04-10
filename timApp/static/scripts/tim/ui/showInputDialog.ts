import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {
    InputDialogComponent as IDC,
    InputDialogParams,
} from "tim/ui/input-dialog.component";
import * as t from "io-ts";
import {Result, to2} from "tim/util/utils";
import {InputDialogKind} from "tim/ui/input-dialog.kind";

export async function showInputDialog<T>(p: InputDialogParams<T>) {
    const {InputDialogComponent} = await import("./input-dialog.component");
    return (
        await angularDialog.open<InputDialogParams<T>, T, IDC<T>>(
            InputDialogComponent,
            p
        )
    ).result;
}

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

export async function templateQueryAndReplace(
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

export async function doTemplateQueries(data: string): Promise<string> {
    const replaceList: ITemplateParam[] = [];
    const rows = data.split("\n");
    while (rows?.[0].startsWith('- {"what"')) {
        try {
            const jso = JSON.parse(rows[0].substring(2));
            replaceList.push(jso);
            rows.shift();
        } catch (e) {
            return "" + e + " " + rows[0];
        }
    }
    if (!replaceList) {
        return data;
    }
    data = rows.join("\n");

    for (const param of replaceList) {
        data = await templateQueryAndReplace(data, param);
        if (!data) {
            return "";
        }
    }
    return data;
}
