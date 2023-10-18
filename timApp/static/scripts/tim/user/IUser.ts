import type {Channel} from "tim/messaging/listOptionTypes";
import type {AccessType} from "tim/item/access-role.service";
import type {IDocument, IFolder} from "tim/item/IItem";
import type {ConsentType} from "tim/ui/consent";

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

export enum ContactOrigin {
    Custom = 1,
    Sisu = 2,
    Haka = 3,
}

export interface IUserContact {
    channel: Channel;
    contact: string;
    verified: boolean;
    origin: ContactOrigin;
    primary: boolean;
}

export interface IUserApiKey {
    translator: string;
    APIkey: string;
    usedQuota: number;
    availableQuota: number;
    quotaChecked: boolean;
}

export interface IFullUser extends IUser {
    groups: IGroup[];
    consent: ConsentType | undefined;
    folder: IFolder | null; // Folder is null only when not logged in.
    last_name: string | null;
}

export interface ICurrentUser extends IFullUser {
    locked_access?: AccessType;
    locked_active_groups?: number[];
}

export interface IGroup {
    id: number;
    name: string;
    external_id?: string;
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

export function sortByEmail(a: IUser, b: IUser) {
    if (!a.email && !b.email) {
        return sortByRealName(a, b);
    }
    if (!a.email) {
        return -1;
    }
    if (!b.email) {
        return 1;
    }
    return (a.email ?? "").localeCompare(b.email ?? "", sortLang);
}
