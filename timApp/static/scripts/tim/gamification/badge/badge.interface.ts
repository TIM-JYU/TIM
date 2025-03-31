export interface IBadge {
    id: number;
    title: string;
    color: string;
    image: number;
    shape: string;
    description: string;
    message: string;
    context_group: string;
    created_by: string;
    badgegiven_id: number;
}

export interface IGroup {
    id: number;
    name: string;
}

export interface IUser {
    id: number;
    name: string;
    real_name: string | null;
    email: string;
}

export interface IPersonalGroup {
    0: IUser;
    1: IGroup;
}
