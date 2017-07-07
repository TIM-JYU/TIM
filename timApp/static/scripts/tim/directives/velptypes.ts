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
    points: string | null;
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
    points: string;
    labels: ILabel[];
    used?: number;
    id: number;
    velp_groups: number[];
    visible_to: number;
    color: string;
    language_id: string;
    icon_id: number;
    valid_until: string;
}

export interface ILabel {
    content: string;
    id: number;
}

export interface IVelpGroup {
    name: string;
    target_type: number;
    id: number;
    default: boolean;
    edit_access?: boolean;
}
