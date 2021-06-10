import {IDocument, IFolder} from "../item/IItem";
import {ConsentType} from "../ui/consent";

export const TEACHERS_GROUPNAME = "teachers";
export const ADMIN_GROUPNAME = "Administrators";

export interface IUser {
    id: number;
    name: string;
    email: string | null;
    real_name: string | null;
    student_id?: string | null;
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
    folder: IFolder | null; // Folder is null only when not logged in.
    last_name: string | null;
}

export interface IGroup {
    id: number;
    name: string;
    personal_user?: IUser;
}

export interface IGroupWithSisuPath extends IGroup {
    sisugroup_path: string | null;
    admin_doc?: IDocument;
}

export const sortLang = "fi";

export function sortByRealName(a: IUser, b: IUser) {
    return (a.real_name ?? "").localeCompare(b.real_name ?? "", sortLang);
}
