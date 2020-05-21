import {IDocument, IFolder} from "../item/IItem";
import {ConsentType} from "../ui/consent";

export const TEACHERS_GROUPNAME = "teachers";
export const ADMIN_GROUPNAME = "Administrators";

export interface IUser {
    id: number;
    name: string;
    email: string | null;
    real_name: string | null;
}

export interface IUserListEntry {
    velped_task_count: number;
    task_points: number;
    total_points: number;
    velp_points: number;
    user: IUser;
}

export interface IFullUser extends IUser {
    groups: IGroup[];
    consent: ConsentType | undefined;
    folder: IFolder;
    last_name: string | null;
}

export interface IGroup {
    id: number;
    name: string;
    personal_user?: IUser;
}

export interface IManagedGroup extends IGroup {
    admin_doc: IDocument;
}
