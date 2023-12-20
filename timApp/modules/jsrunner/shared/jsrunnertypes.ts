import * as t from "io-ts";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    IncludeUsersOption,
    withDefault,
} from "tim/plugin/attributes";
import type {
    ItemRightActionT,
    IToolsResult,
    NewUserData,
} from "../server/routes/tools";

export {IncludeUsersOption} from "tim/plugin/attributes";

// t.brand causes problems, so we use the deprecated refinement for now.
export const Max1000 = t.refinement(t.number, (n) => n >= 0 && n <= 4000);

export const JsrunnerMarkup = t.intersection([
    t.partial({
        creditField: t.string,
        defaultPoints: t.number,
        docid: t.boolean,
        failGrade: t.string,
        fieldhelper: t.boolean,
        fields: t.array(t.string), // fields to use in calculations
        updateFields: t.array(t.string), // Fields to update after run
        paramFields: t.array(t.string), // fields to bring to calculation as parameter
        peerReviewField: t.string,
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
        nextRunner: t.string,
        timeZoneDiff: t.number,
    }),
    GenericPluginMarkup,
    t.type({
        includeUsers: withDefault(IncludeUsersOption, "current"),
        autoUpdateTables: withDefault(t.boolean, true),
        autoadd: withDefault(t.boolean, true),
    }),
]);

export interface IJsRunnerMarkup extends t.TypeOf<typeof JsrunnerMarkup> {}

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

export interface IItemRightActionData
    extends t.TypeOf<typeof ItemRightActionT> {
    item: string;
    group: string;
}

export interface IError {
    msg: string;
    stackTrace?: string;
}

export interface ErrorEntry {
    errors: IError[];
    user: string;
}

export type ErrorList = Array<ErrorEntry>;
export type ExportData = Array<{plugin: string; save?: boolean; data: unknown}>;

interface AnswerReturnSuccess {
    web: {
        output: string;
        errors: ErrorList;
        fatalError?: undefined;
        outdata?: {
            exportdata?: ExportData;
            areaVisibility?: Record<string, boolean>;
            refresh?: boolean;
            refreshRunJSRunners?: string[];
            md?: string;
            html?: string;
        };
    };
    savedata: IToolsResult[];
    groups: IGroupData;
    itemRightActions: IItemRightActionData[];
    newUsers: NewUserData[];
}

interface AnswerReturnError {
    web: {output: string; fatalError: IError};
}

export interface IStatData {
    n: number;
    sum: number;
    min: number;
    max: number;
    avg: number;
    sd: number;
}

export type INumbersObject = Record<string, number>;

export type AnswerReturn = AnswerReturnSuccess | AnswerReturnError;
export type AnswerReturnBrowser =
    | Omit<AnswerReturnSuccess & {web: {error?: string}}, "savedata">
    | AnswerReturnError;
