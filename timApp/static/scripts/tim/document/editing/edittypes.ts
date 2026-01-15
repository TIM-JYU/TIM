import type {IChangelogEntry} from "tim/document/editing/IChangelogEntry";
import type {UnbrokenSelection} from "tim/document/editing/unbrokenSelection";
import type {ParContext} from "tim/document/structure/parContext";

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
    changes?: string[];
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
