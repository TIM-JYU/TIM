import {IChangelogEntry} from "tim/document/editing/IChangelogEntry";
import {UnbrokenSelection} from "tim/document/editing/unbrokenSelection";
import {ParContext} from "tim/document/structure/parContext";
import {ILanguages, ITranslators} from "tim/item/IItem";
import {to} from "tim/util/utils";
import {$http} from "tim/util/ngimport";

export type PendingCollection = Map<string, string>;

export interface IParResponse {
    texts: string;
    js: string[];
    css: string[];
    changed_pars: Record<string, string>;
    version: [number, number];
    duplicates?: Duplicate[];
    original_par?: {md: string; attrs: unknown};
    new_par_ids?: string[];
}

export interface IManageResponse {
    duplicates: Duplicate[];
    versions: Array<IChangelogEntry>;
    fulltext: string;
}

export interface IParInfo {
    par?: ParContext;
    par_next?: string;
    area_start?: string;
    area_end?: string;
}

export type ITags = {
    markread?: boolean;
    marktranslated?: boolean;
};

export interface IExtraData extends IParInfo {
    docId: number;
    isComment?: boolean;
    id?: string; // note id
    forced_classes?: string[];
    access?: string;
    tags: ITags;
}

export type Duplicate = [string, string] | [string, string, "hasAnswers"];

export enum EditType {
    Edit,
    AddAbove,
    AddBelow,
    AddBottom,
    CommentAction,
}

export type EditPosition =
    | {type: EditType.CommentAction; par: ParContext}
    | {type: EditType.Edit; pars: UnbrokenSelection}
    | {type: EditType.AddAbove; par: ParContext}
    | {type: EditType.AddBelow; par: ParContext}
    | {type: EditType.AddBottom};

export function extraDataForServer(data: IExtraData) {
    return {...data, par: data.par?.originalPar.id};
}

/**
 * Goes through the given language list and inserts each of its members into it.
 * @param languages The list of languages to be added
 * @param languageArray The list the languages need to be added to
 */
export function listLanguages(
    languages: ILanguages[],
    languageArray: ILanguages[]
) {
    for (const lang of languages) {
        languageArray.push(lang);
    }
}

/**
 * Fetches the lists of the languages supported by the chosen translator and lists them to front-end's language lists.
 * @param sourceL The list of supported source languages
 * @param docL The list of document languages available
 * @param targetL The list of supported target languages
 * @param translator The chosen translator
 * @returns The last error message that came up.
 */
export async function updateLanguages(
    sourceL: ILanguages[],
    docL: ILanguages[],
    targetL: ILanguages[],
    translator: string
) {
    let error = "";
    let sources = await to(
        $http.post<ILanguages[]>("/translations/source-languages", {
            translator: translator,
        })
    );
    if (sources.ok) {
        listLanguages(sources.result.data, sourceL);
    } else {
        error = sources.result.data.error;
    }

    sources = await to(
        $http.get<ILanguages[]>("/translations/document-languages")
    );
    if (sources.ok) {
        listLanguages(sources.result.data, docL);
    } else {
        error = sources.result.data.error;
    }

    sources = await to(
        $http.post<ILanguages[]>("/translations/target-languages", {
            translator: translator,
        })
    );
    if (sources.ok) {
        listLanguages(sources.result.data, targetL);
    } else {
        error = sources.result.data.error;
    }
    return error;
}

/**
 * Fetches the list of the available translators and adds them to front-end's list of them.
 * @param translators The list the translators will be added to
 * @param includeManual Whether or not the option for manual translation should be included in the list
 * @returns The error message if the request was unsuccessful.
 */
export async function listTranslators(
    translators: ITranslators[],
    includeManual: boolean
) {
    let error = "";
    const sources = await to($http.get<string[]>("/translations/translators"));
    if (sources.ok) {
        for (const translator of sources.result.data) {
            if (translator == "Manual" && !includeManual) {
                continue;
            }
            translators.push({name: translator, available: false});
        }
    } else {
        if (includeManual) {
            translators.push({name: "Manual", available: false});
        }
        error = sources.result.data.error;
    }
    return error;
}

/**
 * Fetches the translators the user can use
 * @param translators the list the translators will be added to
 * @returns The error message if the request was unsuccessful.
 */
export async function availableTranslators(translators: string[]) {
    let error = "";
    const sources = await to(
        $http.get<string[]>("/translations/my-translators")
    );
    translators.push("Manual");
    if (sources.ok) {
        for (const translator of sources.result.data) {
            translators.push(translator);
        }
    } else {
        error = sources.result.data.error;
    }
    return error;
}

/**
 * Checks if the translator is available for the user.
 * @param tr the translator getting checked
 * @param translators the list of translators to check through
 */
export function isOptionAvailable(tr: ITranslators, translators: string[]) {
    for (const translator of translators) {
        if (tr.name == translator) {
            tr.available = true;
        }
    }
}

/**
 * Handles updating data regarding translations.
 * @param sourceLanguages the list of source languages
 * @param documentLanguages the list of document languages
 * @param targetLanguages the list of target languages
 * @param docTranslator the document's translator
 * @param translators the full list of translators
 * @param availableTrs the list of translators the user can use
 * @param errorMessage the error message user is shown if something goes wrong
 * @param translatorAvailable whether or not the chosen translator is available
 * @param includeManual whether or not Manual should be listed among translators
 */
export async function updateTranslationData(
    sourceLanguages: Array<ILanguages>,
    documentLanguages: Array<ILanguages>,
    targetLanguages: Array<ILanguages>,
    docTranslator: string,
    translators: Array<ITranslators>,
    availableTrs: string[],
    errorMessage: string,
    translatorAvailable: boolean,
    includeManual: boolean
) {
    const error = ["", "", ""];
    error[0] = await listTranslators(translators, includeManual);
    error[1] = await updateLanguages(
        sourceLanguages,
        documentLanguages,
        targetLanguages,
        docTranslator
    );
    error[2] = await availableTranslators(availableTrs);
    for (const errors of error) {
        if (errors != "") {
            errorMessage = errors;
            translatorAvailable = false;
        }
    }
    for (const tr of translators) {
        isOptionAvailable(tr, availableTrs);
    }
}
