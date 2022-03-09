import {IChangelogEntry} from "tim/document/editing/IChangelogEntry";
import {UnbrokenSelection} from "tim/document/editing/unbrokenSelection";
import {ParContext} from "tim/document/structure/parContext";
import {ILanguages} from "tim/item/IItem";
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

export function listLanguages(
    languages: string,
    languageArray: Array<ILanguages>
) {
    while (languages.includes(",")) {
        languageArray.push({
            name: languages.substring(0, languages.indexOf("-")),
            code: languages.substring(
                languages.indexOf("-") + 1,
                languages.indexOf(",")
            ),
        });
        languages = languages.substr(languages.indexOf(",") + 1);
    }

    languageArray.push({
        name: languages.substring(0, languages.indexOf("-")),
        code: languages.substring(languages.indexOf("-") + 1),
    });
}

export async function updateLanguages(
    sourceL: Array<ILanguages>,
    targetL: Array<ILanguages>
) {
    let sources = await to(
        $http.get<string[]>("/translations/source-languages")
    );
    // eslint-disable-next-line @typescript-eslint/no-base-to-string
    let languages = sources.result.data.toString();
    listLanguages(languages, sourceL);

    sources = await to($http.get<string[]>("/translations/target-languages"));
    // eslint-disable-next-line @typescript-eslint/no-base-to-string
    languages = sources.result.data.toString();
    listLanguages(languages, targetL);
}
