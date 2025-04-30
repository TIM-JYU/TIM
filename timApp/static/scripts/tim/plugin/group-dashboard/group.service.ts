import {HttpClient} from "@angular/common/http";
import {Subject} from "rxjs";
import {IGroup, IUser} from "tim/gamification/badge/badge.interface";
import {toPromise} from "tim/util/utils";
import {Injectable} from "@angular/core";

@Injectable({
    providedIn: "root",
})
export class GroupService {
    constructor(private http: HttpClient) {}

    private groupNameUpdated = new Subject<{id: number; newName: string}>();
    groupNameUpdated$ = this.groupNameUpdated.asObservable();

    notifyGroupNameChange(id: number, newName: string) {
        this.groupNameUpdated.next({id, newName});
    }

    /**
     * Hakee kaikki käyttäjät, jotka kuuluvat parametrina annettuun ryhmään.
     * @param group ryhmä, jonka käyttäjiä haetaan.
     */
    async getUsersFromGroup(group: string) {
        const result = await toPromise(
            this.http.get<[]>(`/usergroups_members/${group}`)
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
            this.http.get<[]>(`/subgroups/${group}`)
        );
        const subGroups: IGroup[] = [];
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
            this.http.get<[]>(`/users_subgroups/${userid}/${group}`)
        );
        const userSubGroups: IGroup[] = [];
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    userSubGroups.push(alkio);
                }
            }
        }
        return userSubGroups;
    }

    async getUserAndPersonalGroup(userName: string | undefined) {
        const response = toPromise(
            this.http.get<any>(`/user_and_personal_group/${userName}`)
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

    // Gets the current selected group
    async getCurrentGroup(groupName: string | null) {
        const response = toPromise(
            this.http.get<any>(`/groups/current_group_name/${groupName}`)
        );
        const result = await response;

        if (result.ok) {
            return result.result;
        } else {
            console.error("Failed to fetch groups name.");
        }
        return null;
    }

    // Changes the groups name into the
    async updateGroupName(
        group_id: number,
        group_name: string,
        new_name: string | null
    ) {
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

    getContextGroup(fullName: string) {
        const parts = fullName.split("-");
        return parts[0];
    }
}
