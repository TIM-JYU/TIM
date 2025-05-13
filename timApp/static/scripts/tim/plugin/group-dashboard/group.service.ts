import {HttpClient} from "@angular/common/http";
import {Subject} from "rxjs";
import type {IBadgeGroup} from "tim/gamification/badge/badge.interface";
import type {IUser} from "tim/user/IUser";
import {toPromise} from "tim/util/utils";
import {Injectable} from "@angular/core";

@Injectable({
    providedIn: "root",
})
export class GroupService {
    constructor(private http: HttpClient) {}

    /**
     * Hakee kaikki käyttäjät, jotka kuuluvat parametrina annettuun ryhmään.
     * @param group ryhmä, jonka käyttäjiä haetaan.
     */
    async getUsersFromGroup(group: string) {
        const result = await toPromise(
            this.http.get<[]>(`/groups/usergroups_members/${group}`)
        );
        const users: IUser[] = [];
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    users.push(alkio);
                }
            }
        }
        return users;
    }

    /**
     * Hakee kaikki "aliryhmät", jotka alkaa annetulla <group> parametrilla.
     * @param group ryhmä, jonka avulla aliryhmät haetaan
     */
    async getSubGroups(group: string) {
        const result = await toPromise(
            this.http.get<[]>(`/groups/subgroups/${group}`)
        );
        const subGroups: IBadgeGroup[] = [];
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    subGroups.push(alkio);
                }
            }
        }
        return subGroups;
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
     * Fetches user's data along with their personal group's information.
     * @param userName user's name
     * @returns an object that contains user data and personal group data
     */
    async getUserAndPersonalGroup(userName: string | undefined) {
        const response = toPromise(
            this.http.get<any>(`/groups/user_and_personal_group/${userName}`)
        );
        const result = await response;

        if (result.ok) {
            return result.result;
        } else {
            console.error(
                `Failed to fetch personal group for user: ${userName}`
            );
            return null;
        }
    }

    /**
     * Retrieves group data including id, internal name, and description (pretty name).
     * @param groupName The full internal name of the group (e.g., "parent-subgroup").
     * @returns An object containing the group's id, name, and description, or null if the fetch fails.
     */
    async getCurrentGroup(groupName: string | null) {
        const response = toPromise(
            this.http.get<any>(`/groups/pretty_name/${groupName}`)
        );
        const result = await response;

        if (result.ok) {
            return result.result;
        } else {
            console.error("Failed to fetch groups name.");
        }
        return null;
    }

    /**
     * Updates the group's description field (also called "pretty name").
     * Does not change the actual group name.
     * @param group_name actual group's name (identifier) provided for the component
     * @param new_name group's new name (pretty name)
     * @returns whether the update was successful
     */
    async updateGroupName(group_name: string, new_name: string | null) {
        const response = toPromise(
            this.http.post<{ok: boolean}>(
                `/groups/editGroupName/${group_name}/${new_name}`,
                {}
            )
        );
        const result = await response;
        if (result.ok) {
            return result.result;
        }
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
