import type {IGroup, IUser} from "tim/user/IUser";

/**
 * Interface representing a badge and its associated details
 */
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

/**
 * Interface that extends IGroup contents and adds description field for a pretty group name.
 */
export interface IBadgeGroup extends IGroup {
    description: string;
}

/**
 *  Interface representing a tuple containing a user and a group
 */
export interface IPersonalGroup {
    0: IUser;
    1: IBadgeGroup;
}
