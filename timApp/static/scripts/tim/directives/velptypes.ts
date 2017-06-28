export type VelpGroupSelectionType = "show" | "default";

export interface IUIFields {
    show?: boolean;
    selected?: boolean;
    edit?: boolean;
    valid?: boolean;
}

export interface IAnnotation {

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
