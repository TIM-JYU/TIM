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
    markchecked?: boolean;
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

/*
Goes through the given language list (languages) and inserts each of its members into languageArray.
 */
export function listLanguages(
    languages: Array<ILanguages>,
    languageArray: Array<ILanguages>
) {
    for (const lang of languages) {
        languageArray.push(lang);
    }
}

/*
Fetches the lists of the languages and lists them to front-end's language lists.
 */
export async function updateLanguages(
    sourceL: Array<ILanguages>,
    docL: Array<ILanguages>,
    targetL: Array<ILanguages>,
    translator: string
) {
    let sources = await to(
        $http.get<ILanguages[]>("/translations/source-languages")
    );
    if (sources.ok) {
        listLanguages(sources.result.data, sourceL);
    }

    sources = await to(
        $http.get<ILanguages[]>("/translations/document-languages")
    );
    if (sources.ok) {
        listLanguages(sources.result.data, docL);
    }

    sources = await to(
        $http.post<ILanguages[]>("/translations/target-languages", {
            translator: translator,
        })
    );
    if (sources.ok) {
        listLanguages(sources.result.data, targetL);
    }
}

/*
Fetches the list of the available translators and adds them to front-end's list of them.
 */
export async function listTranslators(translators: Array<ITranslators>) {
    const sources = await to($http.get<string[]>("/translations/translators"));
    if (sources.ok) {
        for (const translator of sources.result.data) {
            translators.push({name: translator});
        }
    }
}
