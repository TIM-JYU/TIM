export type PendingCollection = Map<string, string>;

export interface IParResponse {
    texts: string;
    js: string[];
    css: string[];
    changed_pars: {[id: string]: string};
    version: [number, number];
    duplicates?: Duplicate[];
    original_par?: {md: string, attrs: unknown};
    new_par_ids?: string[];
}

export interface IManageResponse {
    duplicates: Duplicate[];
    versions: Array<unknown>; // TODO give accurate type
    fulltext: string;
}

export interface IParInfo {
    "ref-id"?: string;
    "ref-doc-id"?: string;
    "ref-t"?: string;
    "ref-attrs"?: string;
    par: string;
    par_next?: string;
    area_start?: string;
    area_end?: string;
}

export interface ITags {markread: boolean; marktranslated?: boolean; }

export interface IExtraData extends IParInfo {
    // attrs: {classes: string[], [i: string]: any};
    docId: number;
    isComment?: boolean;
    id?: string; // note id
    forced_classes?: string[];
    access?: string;
    tags: ITags;
}

export type Duplicate = [string, string] | [string, string, "hasAnswers"];

export interface IChangelogEntry {

}

export enum EditType {
    Edit,
    AddAbove,
    AddBelow,
    AddBottom,
}

export type EditPosition =
    | {type: EditType.Edit, pars: JQuery}
    | {type: EditType.AddAbove, par: JQuery}
    | {type: EditType.AddBelow, par: JQuery}
    | {type: EditType.AddBottom};
