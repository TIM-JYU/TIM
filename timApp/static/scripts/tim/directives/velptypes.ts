import {Overwrite} from "type-zoo";

export type VelpGroupSelectionType = "show" | "default";

export interface IUIFields {
    show?: boolean;
    selected?: boolean;
    edit?: boolean;
    valid?: boolean;
}

export interface IAnnotationCoordinate {
    par_id: string;
    t: string;
    offset: number;
    el_path: number[];
    node: number;
    depth: number;
}

export interface IAnnotationInterval {
    start: IAnnotationCoordinate;
    end: IAnnotationCoordinate;
}

export interface IAnnotationComment {

}

export interface IAnnotation {
    id: number;
    user_id: number;
    doc_id: number;
    reason: string;
    email: string;
    edit_access: boolean;
    timesince: string;
    creationtime: string;
    newannotation: boolean;
    content: string;
    velp: number;
    points: number;
    color: string | null;
    coord: IAnnotationInterval;
    answer_id: number;
    comments: IAnnotationComment[];
    visible_to: number;
    default_comment: string;
    annotator_name: string;
}

export interface IVelp {
    content: string;
    default_comment: string;
    points: number | null;
    labels: number[];
    used?: number;
    id: number;
    velp_groups: number[];
    visible_to: number;
    color: string | null;
    language_id: string;
    icon_id: number | null;
    valid_until: string | null;
}

export interface ILabel {
    content: string;
    id: number;
    language_id?: string;
}

export interface IVelpGroup {
    name: string;
    target_type: number | null;
    id: number;
    default: boolean;
    edit_access?: boolean;
}

export type INewLabel = Overwrite<ILabel, {id: null | number}> & IUIFields;
export type INewVelp = Overwrite<IVelp, {id: null | number, visible_to: number | null}> & IUIFields;
export type INewVelpGroup = Overwrite<IVelpGroup, {id: null | number}> & IUIFields;

export type ILabelUI = ILabel & IUIFields;
export type IVelpUI = IVelp & IUIFields;
export type IVelpGroupUI = IVelpGroup & IUIFields;

export interface IVelpGroupCollection {
    [index: string]: { id: number, selected?: boolean }[];
}
