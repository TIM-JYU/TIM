import type {IUser} from "tim/user/IUser";

export interface IAnswer {
    content: string;
    task_id: string;
    id: number;
    points?: number;
    last_points_modifier: number | null;
    valid: boolean;
    answered_on: string;
}

export interface IAnswerWithUsers extends IAnswer {
    users: IUser[];
}

export type IModelAnswerSettings = {
    lock: boolean;
    count: number;
    revealDate?: string;
    hideText?: string;
    linkText?: string;
    linkTextCount?: number;
    linkTextBeforeCount?: string;
    lockConfirmation?: string;
    lockedLinkText?: string;
    alreadyLocked?: boolean;
    disabled?: boolean | "unless_review";
};
