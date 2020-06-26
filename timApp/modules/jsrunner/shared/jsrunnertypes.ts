import * as t from "io-ts";
import {GenericPluginMarkup, getTopLevelFields, IncludeUsersOption, withDefault} from "tim/plugin/attributes";
import {IToolsResult} from "../server/routes/tools";

export {IncludeUsersOption} from "tim/plugin/attributes";

// t.brand causes problems, so we use the deprecated refinement for now.
export const Max1000 = t.refinement(
    t.number,
    (n) => n >= 0 && n <= 4000,
);

export const JsrunnerMarkup = t.intersection([
    t.partial({
        creditField: t.string,
        defaultPoints: t.number,
        docid: t.boolean,
        failGrade: t.string,
        fieldhelper: t.boolean,
        fields: t.array(t.string),       // fields to use in calculations
        updateFields: t.array(t.string), // Fields to update after run
        paramFields: t.array(t.string),  // fields to bring to calculation as parameter
        gradeField: t.string,
        gradingScale: t.record(t.string, t.number),
        groups: t.array(t.string),
        selectIncludeUsers: t.boolean,
        program: t.string,
        preprogram: t.string,
        postprogram: t.string,
        overrideGrade: t.boolean,
        timeout: Max1000,
        open: t.boolean,
        showInView: t.boolean,
        confirmText: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        includeUsers: withDefault(IncludeUsersOption, "current"),
        autoUpdateTables: withDefault(t.boolean, true),
        autoadd: withDefault(t.boolean, true),
    }),
]);

export interface IJsRunnerMarkup extends t.TypeOf<typeof JsrunnerMarkup> {
}

export const JsrunnerAll = t.intersection([
    t.partial({
        runnable: t.boolean,
    }),
    getTopLevelFields(JsrunnerMarkup),
    t.type({}),
]);

export interface IGroupData {
    set?: Record<string, number[]>;
    add?: Record<string, number[]>;
    remove?: Record<string, number[]>;
}

export interface IError {
    msg: string;
    stackTrace?: string;
}

export type ErrorList = Array<{errors: IError[], user: string}>;

interface AnswerReturnSuccess {
    web:
        {
            output: string,
            errors: ErrorList,
            fatalError?: undefined,
            outdata?: { exportdata?: Array<{ plugin: string, save?: boolean, data: unknown }> },
        };
    savedata: IToolsResult[];
    groups: IGroupData;
}

interface AnswerReturnError {
    web: {output: string, fatalError: IError};
}

export interface IStatData {
    n: number;
    sum: number;
    min: number;
    max: number;
    avg: number;
    sd: number;
}

export interface INumbersObject {
    [hname: string]: number;
}

export type AnswerReturn = AnswerReturnSuccess | AnswerReturnError;
export type AnswerReturnBrowser = Omit<AnswerReturnSuccess & {web: {error?: string}}, "savedata"> | AnswerReturnError;
