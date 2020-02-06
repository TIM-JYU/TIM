import {IUser} from "tim/user/IUser";

export interface IAnswer {
    content: string;
    id: number;
    points?: number;
    last_points_modifier: number;
    valid: boolean;
}

export interface IAnswerWithUsers extends IAnswer {
    users: IUser[];
}
