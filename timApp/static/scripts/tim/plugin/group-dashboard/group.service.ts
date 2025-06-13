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
     * Retrives a list of users belonging to a specific group
     * @param group
     * @return list of users in the group
     */
    async getUsersFromGroup(group: string): Promise<IUser[]> {
        const response = await toPromise(
            this.http.get<[IUser]>(`/groups/usergroups_members/${group}`)
        );
        if (response.ok) {
            return response.result;
        }
        return [];
    }

    /**
     * Hakee kaikki "aliryhmät", jotka alkaa annetulla <group> parametrilla.
     * @param group ryhmä, jonka avulla aliryhmät haetaan
     */
    async getSubGroups(group: string) {
        return await toPromise(
            this.http.get<IBadgeGroup[]>(`/groups/subgroups/${group}`)
        );
    }

    /**
     * Hakee kaikki "aliryhmät", johon käyttäjä kuuluu.
     * @param group ryhmä, jonka avulla aliryhmät haetaan
     * @param userid käyttäjän id
     */
    async getUserSubGroups(group: string, userid: number) {
        const result = await toPromise(
            this.http.get<[]>(`/groups/users_subgroups/${userid}/${group}`)
        );
        const userSubGroups: IBadgeGroup[] = [];
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    userSubGroups.push(alkio);
                }
            }
        }
        return userSubGroups;
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
        // const response = await toPromise(
        //     this.http.get<IGroup>(`/groups/personal_group/${userName}`)
        // );
        // if (response.ok) {
        //     return response.result;
        // } else {
        //     return response.result.error;
        // }
    }

    /**
     * Retrieves group data including id, internal name, and description (pretty name).
     * @param groupName The full internal name of the group (e.g., "parent-subgroup").
     * @returns An object containing the group's id, name, and description, or null if the fetch fails.
     */
    async getCurrentGroup(groupName: string) {
        const response = toPromise(
            this.http.get<BadgeGroupInfo>(`/groups/pretty_name/${groupName}`)
        );
        const result = await response;

        if (result.ok) {
            return result.result;
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
        const parts = fullName.split("-");
        return parts[0];
    }
}
