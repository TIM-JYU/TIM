import * as t from "io-ts";
import {GenericPluginMarkup, GenericPluginTopLevelFields, withDefault} from "tim/plugin/attributes";

// t.brand causes problems, so we use the deprecated refinement for now.
export const Max1000 = t.refinement(
    t.number,
    (n) => n >= 0 && n <= 1000,
);

export const IncludeUsersOption = t.keyof({
    all: null,
    current: null,
    deleted: null,
});

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
        timeout: Max1000,
        open: t.boolean,
        showInView: t.boolean,
        autoadd: t.boolean,
        validonly: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        includeUsers: withDefault(IncludeUsersOption, "current"),
        autoUpdateTables: withDefault(t.boolean, true),
    }),
]);

export interface IJsRunnerMarkup extends t.TypeOf<typeof JsrunnerMarkup> {
}

export const JsrunnerAll = t.intersection([
    GenericPluginTopLevelFields,
    t.type({markup: JsrunnerMarkup}),
]);

export interface IError {
    msg: string;
    stackTrace?: string;
}

export type ErrorList = Array<{errors: IError[], user: string}>;

interface AnswerReturnSuccess {
    web:
        {output: string, errors: ErrorList, fatalError?: undefined, outdata?: object};
    savedata: {};
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
