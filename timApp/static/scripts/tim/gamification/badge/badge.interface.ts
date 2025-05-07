// Interface representing a badge and its associated details
import type {IUser} from "tim/user/IUser";

export interface IBadge {
    id: number;
    title: string;
    color: string;
    image: number;
    shape: string;
    description: string;
    message: string;
    context_group: string;
    given_by_name: number;
    badgegiven_id: number;
    given: Date;
    created_by_name: string;
    created: Date;
}

// Interface representing a group with a name and description
export interface IGroup {
    id: number;
    name: string;
    description: string;
}

// Interface representing a user
// export interface IUser {
//     id: number;
//     name: string;
//     real_name: string | null;
//     email: string;
// }

// Interface representing a tuple containing a user and a group
export interface IPersonalGroup {
    0: IUser;
    1: IGroup;
}
