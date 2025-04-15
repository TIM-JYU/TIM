import {Injectable} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {lastValueFrom, Observable} from "rxjs";
import {Subject} from "rxjs";
import {toPromise} from "tim/util/utils";
import type {
    IBadge,
    IUser,
    IGroup,
    IPersonalGroup,
    IBadgeHolders,
} from "tim/gamification/badge/badge.interface";
import {documentglobals} from "tim/util/globals";

interface IData {
    context_group?: string;
    group_id: number;
    badge_id?: number;
    message: string;
}

@Injectable({
    providedIn: "root",
})
export class BadgeService {
    // Subject, joka laukaisee updatesignaalin
    private updateBadgeSubject = new Subject<void>();

    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];

    // Observable updatetapahtuman kuunteluun
    updateBadgeList$ = this.updateBadgeSubject.asObservable();

    private groupNameUpdated = new Subject<{id: number; newName: string}>();
    groupNameUpdated$ = this.groupNameUpdated.asObservable();

    notifyBadgeViewerUpdate() {
        this.updateBadgeSubject.next();
    }

    notifyGroupNameChange(id: number, newName: string) {
        this.groupNameUpdated.next({id, newName});
    }

    constructor(private http: HttpClient) {}
    async getAllBadges(): Promise<IBadge[]> {
        try {
            // Create an HTTP request and wait for response
            const response = this.http.get<IBadge[]>("/all_badges");

            // Transform Observable to a Promise and wait for data
            const result = await lastValueFrom(response);

            // Return fetched data or an empty array if database is empty
            return result ?? [];
        } catch (error) {
            // console.error("Error fetching badges:", error);
            // Return an empty array in case of an error
            return [];
        }
    }

    /**
     * Hakee käyttäjän ryhmälle kuuluvat badget ID:n perusteella.
     * Ryhmä voi olla joko henkilökohtainen ryhmä, tai ryhmä, jossa jäseniä on enemmän, kuin yksi.
     * @param id ryhmän ID
     * @param contextGroup käyttäjän kontekstiryhmä
     * @return palauttaa IBadge taulukon
     */
    async getUserBadges(id: number, contextGroup: string) {
        const result = await toPromise(
            this.http.get<[]>(`/groups_badges/${id}/${contextGroup}`)
        );
        const userBadges: IBadge[] = [];
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    userBadges.push(alkio);
                }
            }
        }
        return userBadges.reverse();
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

    /**
     * Ottaa valitun badgen pois käytöstä käyttäjältä.
     * @param badgegivenID badgegiven -tietokantataulukon id, jonka avulla valittu badge poistetaan käytöstä
     */
    async withdrawBadge(badgegivenID: number, context_group: string) {
        const response = toPromise(
            this.http.post<{ok: boolean}>("/withdraw_badge", {
                badge_given_id: badgegivenID,
                context_group: context_group,
            })
        );
        const result = await response;
        if (result.ok) {
            // console.log("badge poistettu käytöstä id:llä: " + badgegivenID);
            this.triggerUpdateBadgeList();
            return {ok: true};
        } else {
            return {ok: false, data: result};
        }
    }

    async withdrawSelectedBadge(
        userid: number,
        badgeid: number,
        contextGroup: string
    ) {
        const response = toPromise(
            this.http.post<{ok: boolean}>("/withdraw_all_badges", {
                badge_id: badgeid,
                usergroup_id: userid,
                context_group: contextGroup,
            })
        );
        const result = await response;
        if (result.ok) {
            this.triggerUpdateBadgeList();
            return {ok: true};
        }
        return {ok: false, data: result};
    }

    async getBadgeHolders(badgeid: number) {
        const response = toPromise(
            this.http.get<IBadgeHolders>(`/badge_holders/${badgeid}`)
        );
        const result = await response;
        if (result.ok) {
            this.triggerUpdateBadgeList();
            return result.result;
        }
        return null;
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

    async assignBadges(data: IData) {
        const response = toPromise(
            this.http.post<{ok: boolean}>("/give_badge", {
                context_group: data.context_group,
                group_id: data.group_id,
                badge_id: data.badge_id,
                message: data.message,
            })
        );

        const result = await response;
        if (result.ok) {
            console.log(`Badge assigned to group/user ID: ${data.group_id}`);
        }

        if (!result.ok) {
            this.showError(
                this.alerts,
                {data: {error: result.result.error.error}},
                "danger"
            );
            return;
        }
        if (data.group_id) {
            this.notifyBadgeViewerUpdate();
        }
    }

    private dialogOpen = false;

    // Checks, if dialog-window is open
    isDialogOpen(): boolean {
        return this.dialogOpen;
    }
    // Sets the dialog-window as "open"
    setDialogOpen(isOpen: boolean): void {
        this.dialogOpen = isOpen;
    }

    // Send a request to update viewer, when a new badge is created
    triggerUpdateBadgeList() {
        this.updateBadgeSubject.next();
    }

    public activeDialogRef: any = null; // Store the active dialog reference

    // Close the active dialog if it exists
    closeActiveDialog(): void {
        if (this.activeDialogRef) {
            this.activeDialogRef.close({}); // Close the current dialog
            this.activeDialogRef = null; // Reset the reference
        }
    }

    // Show errors
    showError(
        alerts: any,
        response: {data: {error: string}},
        type: "warning" | "danger"
    ) {
        const msg = `Error: ${response.data.error ?? response.data}`;
        if (alerts.some((a: any) => a.msg === msg && a.type === type)) {
            return;
        }
        alerts.push({msg, type});
    }

    // Removes an error-message from alerts-list
    closeAlert(alerts: any, index: number) {
        alerts.splice(index, 1);
    }

    // The available icons for badges
    private availableImages = [
        {id: 1, name: "Trophy"},
        {id: 2, name: "Winner"},
        {id: 3, name: "Teamwork"},
        {id: 4, name: "Code"},
        {id: 5, name: "Debug"},
        {id: 6, name: "On_fire"},
        {id: 7, name: "Rocket"},
        {id: 8, name: "Smile"},
        {id: 9, name: "Terminal"},
        {id: 10, name: "Deployed_code"},
        {id: 11, name: "Loop"},
        {id: 12, name: "100_points"},
    ];

    getAvailableImages() {
        return this.availableImages;
    }
}
