import {HttpClient} from "@angular/common/http";
import type {IBadgeGroup} from "tim/gamification/badge/badge.interface";
import type {IGroup, IUser} from "tim/user/IUser";
import {toPromise} from "tim/util/utils";
import {Injectable} from "@angular/core";

export type BadgeGroupInfo = {
    id: number;
    name: string;
    /** Pretty name from the admin document, null for a group that has none. */
    description: string | null;
    /** Name of the group this one is a subgroup of, null for a top-level group. */
    parent_group: string | null;
};

@Injectable({
    providedIn: "root",
})
export class GroupService {
    constructor(private http: HttpClient) {}

    /**
     * Retrieves a list of users belonging to a specific group
     * @param group
     * @return list of users in the group
     */
    async getUsersFromGroup(group: string): Promise<IUser[]> {
        const response = await toPromise(
            this.http.get<IUser[]>(
                `/groups/members/${encodeURIComponent(group)}`
            )
        );
        if (response.ok) {
            return response.result;
        }
        return [];
    }

    /**
     * Retrieves the subgroups of the given group.
     * @param group parent group name
     */
    async getSubGroups(group: string) {
        return await toPromise(
            this.http.get<IBadgeGroup[]>(
                `/groups/subgroups/${encodeURIComponent(group)}`
            )
        );
    }

    /**
     * Retrieves the subgroups of the given group that the user belongs to.
     * @param group parent group name
     * @param userId user's id
     */
    async getUserSubGroups(group: string, userId: number) {
        const resp = await toPromise(
            this.http.get<IBadgeGroup[]>(
                `/groups/prefix_groups/${userId}/${encodeURIComponent(group)}`
            )
        );
        if (resp.ok) {
            return resp.result;
        }
        return [];
    }

    /**
     * Fetches user's personal group.
     * @param userName user's name
     * @returns user's personal group
     */
    async getPersonalGroup(userName: string) {
        return await toPromise(
            this.http.get<IGroup>(
                `/groups/personal_group/${encodeURIComponent(userName)}`
            )
        );
    }

    /**
     * Retrieves group data: id, internal name, description (pretty name) and the name
     * of the group it is a subgroup of.
     * @param groupName The internal name of the group.
     * @returns The group summary, or undefined if the fetch fails.
     */
    async getCurrentGroup(groupName: string) {
        const response = await toPromise(
            this.http.get<BadgeGroupInfo>(
                `/groups/groupinfo/${encodeURIComponent(groupName)}`
            )
        );
        if (response.ok) {
            return response.result;
        }
    }

    /**
     * Updates the group's description field (also called "pretty name").
     * Does not change the actual group name.
     * @param group_name actual group's name (identifier) provided for the component
     * @param new_name group's new name (pretty name)
     * @returns whether the update was successful
     */
    async updateGroupName(group_name: string, new_name: string) {
        const response = toPromise(
            this.http.post<BadgeGroupInfo>(
                `/groups/pretty_name/${encodeURIComponent(
                    group_name
                )}/${encodeURIComponent(new_name)}`,
                {}
            )
        );
        return await response;
    }

    /**
     * The group that badges are scoped to for the given group: its parent if it is a
     * subgroup, otherwise the group itself.
     *
     * Subgroups used to be recognised by a shared name prefix, so this was the part of
     * the name before the first "-". That gave the wrong answer for any top-level group
     * whose name contains a dash, and for any subgroup not named after its parent.
     *
     * @param groupName The internal name of the group.
     * @returns The context group's name, or undefined if the group cannot be read.
     */
    async getContextGroup(groupName: string): Promise<string | undefined> {
        const info = await this.getCurrentGroup(groupName);
        if (!info) {
            return undefined;
        }
        return info.parent_group ?? info.name;
    }

    /**
     * Queries whether the current user has teacher rights to the specified usergroup.
     * @param group_id The group's id number
     */
    async queryTeacherRightsToGroup(group_id: number): Promise<boolean> {
        // The route answers with a plain ok response; a lack of rights is a 403, so
        // whether the request succeeded is the whole answer.
        const teacherRightQuery = await toPromise(
            this.http.get<{status: string}>(
                `/groups/hasTeacherRightTo/${group_id}`
            )
        );
        return teacherRightQuery.ok;
    }
}
