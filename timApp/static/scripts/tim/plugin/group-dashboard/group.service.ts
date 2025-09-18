import {HttpClient} from "@angular/common/http";
import type {IBadgeGroup} from "tim/gamification/badge/badge.interface";
import type {IGroup, IUser} from "tim/user/IUser";
import {toPromise} from "tim/util/utils";
import {Injectable} from "@angular/core";

export type BadgeGroupInfo = {
    id: number;
    name: string;
    description: string;
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
            this.http.get<IUser[]>(`/groups/members/${group}`)
        );
        if (response.ok) {
            console.log(response.result);
            return response.result;
        }
        return [];
    }

    /**
     * Retrieves all sub-groups whose name starts with the given prefix.
     * @param group group name prefix
     */
    async getSubGroups(group: string) {
        // TODO: implement a better way to get sub groups
        const group_prefix = group.split("-")[0];

        const response = await toPromise(
            this.http.get<IBadgeGroup[]>(`/groups/subgroups/${group_prefix}`)
        );
        if (response.ok) {
            const gs = [];
            for (const g of response.result) {
                gs.push(g.name);
            }
            console.log(`GETTING SUBGROUPS FOR ${group}: ${gs}`);
        }
        return response;

        // return await toPromise(
        //     this.http.get<IBadgeGroup[]>(`/groups/subgroups/${group}`)
        // );
    }

    /**
     * Retrieves all groups with the specified group name prefix, that the user belongs to.
     * @param group group name prefix
     * @param userId user's id
     */
    async getUserSubGroups(group: string, userId: number) {
        const resp = await toPromise(
            this.http.get<IBadgeGroup[]>(
                `/groups/prefix_groups/${userId}/${group}`
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
            this.http.get<IGroup>(`/groups/personal_group/${userName}`)
        );
    }

    /**
     * Retrieves group data including id, internal name, and description (pretty name).
     * @param groupName The full internal name of the group (e.g., "parent-subgroup").
     * @returns An object containing the group's id, name, and description, or null if the fetch fails.
     */
    async getCurrentGroup(groupName: string) {
        const response = await toPromise(
            this.http.get<BadgeGroupInfo>(`/groups/groupinfo/${groupName}`)
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
            this.http.post<{ok: boolean}>(
                `/groups/pretty_name/${group_name}/${new_name}`,
                {}
            )
        );
        return await response;
    }

    /**
     * Extracts the context group (i.e., the parent group) from a full group name.
     * @param fullName The full internal group name, typically in the format "parent-subgroup".
     * @returns The first part of the group name before the dash, representing the context group.
     */
    getContextGroup(fullName: string) {
        // TODO: this needs to be reworked, since there is no set convention for naming
        //  subgroups.
        const parts = fullName.split("-");
        return parts[0];
    }

    /**
     * Queries whether the current user has teacher rights to the specified usergroup.
     * @param group_id The group's id number
     */
    async queryTeacherRightsToGroup(group_id: number): Promise<boolean> {
        // TODO: error handling / error messages for user
        const teacherRightQuery = await toPromise(
            this.http.get<boolean>(`/groups/hasTeacherRightTo/${group_id}`)
        );
        if (teacherRightQuery.ok) {
            return true;
        }
        return false;
    }
}
