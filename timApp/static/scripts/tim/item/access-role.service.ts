import {Injectable} from "@angular/core";
import type {Duration, Moment} from "moment";
import {shortestUniquePrefixes, toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import type {IGroup} from "tim/user/IUser";
import type {IItem} from "tim/item/IItem";

export interface IGroupWithAdminDocPath extends IGroup {
    admin_doc_path: string | null;
}

export interface IRight {
    type: number;
    duration_to: Moment | null;
    duration_from: Moment | null;
    duration: null | Duration;
    accessible_to: Moment;
    accessible_from: Moment;
    usergroup: IGroupWithAdminDocPath;
    require_confirm?: boolean;
    restricted: boolean;
}

export interface IAccessType {
    id: number;
    name: AccessTypeClearName;
}

export interface IItemWithRights extends IItem {
    grouprights: IRight[];
}

export const accessOrder = {
    view: 1,
    copy: 2,
    edit: 3,
    "see answers": 4,
    teacher: 5,
    manage: 6,
    owner: 7,
};

export enum AccessType {
    View = 1,
    Edit = 2,
    Teacher = 3,
    Manage = 4,
    SeeAnswers = 5,
    Owner = 6,
    Copy = 7,
}

export const accessTypeDisplayNames: Record<AccessType, string> = {
    [AccessType.View]: $localize`View`,
    [AccessType.Edit]: $localize`Edit`,
    [AccessType.Teacher]: $localize`Teacher`,
    [AccessType.Manage]: $localize`Manage`,
    [AccessType.SeeAnswers]: $localize`See answers`,
    [AccessType.Owner]: $localize`Owner`,
    [AccessType.Copy]: $localize`Copy`,
};

export const accessTypeDisplayNamePrefixes: Record<AccessType, string> =
    (() => {
        const keys = Object.keys(accessTypeDisplayNames);
        const values = shortestUniquePrefixes(
            Object.values(accessTypeDisplayNames)
        );
        const entries = keys.map((key, i) => [key, values[i]]);
        return Object.fromEntries(entries);
    })();

export type AccessTypeName = keyof typeof AccessType;

export type AccessTypeClearName = keyof typeof accessOrder;

@Injectable({
    providedIn: "root",
})
export class AccessRoleService {
    constructor(private http: HttpClient) {}

    async lockAccess(access: AccessType | null) {
        const r = await toPromise(
            this.http.post("/access/lock", {
                access_type: access,
            })
        );
        if (r.ok) {
            location.reload();
        }
        return r;
    }

    async lockGroups(groupIds: number[] | null) {
        const r = await toPromise(
            this.http.post("/access/groups/lock", {
                group_ids: groupIds,
            })
        );
        if (r.ok) {
            location.reload();
        }
        return r;
    }
}
