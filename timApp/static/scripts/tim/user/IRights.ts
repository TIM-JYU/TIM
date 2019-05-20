export interface IRights {
    teacher: boolean;
    manage: boolean;
    editable: boolean;
    can_comment: boolean;
    browse_own_answers: boolean;
    see_answers: boolean;
    [right: string]: boolean;
}
