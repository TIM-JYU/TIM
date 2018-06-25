export const TEACHERS_GROUPNAME = "teachers";

export interface IUser {
    id: number;
    name: string;
    real_name: string;
    velped_task_count: number; // TODO needs a separate type
}

export interface IFullUser extends IUser {
    groups: IGroup[];
}

export interface IGroup {
    id: number;
    name: string;
}
