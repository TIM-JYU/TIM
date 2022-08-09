import {IUser} from "tim/user/IUser";

export interface IAnswer {
    content: string;
    task_id: string;
    id: number;
    points?: number;
    last_points_modifier: number | null;
    valid: boolean;
}

export interface IAnswerWithUsers extends IAnswer {
    users: IUser[];
}

export type IModelAnswerSettings = {
    lock: boolean;
    count: number;
    revealDate?: string;
    linkText?: string;
    linkTextCount?: number;
    lockConfirmation?: string;
    lockedLinkText?: string;
    alreadyLocked?: boolean;
};
