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
} from "tim/gamification/badge/badge.interface";
import {documentglobals} from "tim/util/globals";

interface IData {
    given_by: number;
    doc_id: number;
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

    notifyBadgeViewerUpdate() {
        this.updateBadgeSubject.next();
    }

    constructor(private http: HttpClient) {}

    currentDocumentID = documentglobals().curr_item.id;

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
            this.http.get<[]>(
                `/usergroups_members/${this.currentDocumentID}/${group}`
            )
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
     * @param giverID käyttäjän id, joka poistaa badgen käytöstä.
     */
    async withdrawBadge(
        badgegivenID: number,
        giverID: number,
        context_group: string
    ) {
        const response = toPromise(
            this.http.post<{ok: boolean}>("/withdraw_badge", {
                badge_given_id: badgegivenID,
                withdrawn_by: giverID,
                doc_id: this.currentDocumentID,
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

    async getUserAndPersonalGroup(userName: string | undefined) {
        const response = toPromise(
            this.http.get<any>(`/user_and_personal_group/${userName}`)
        );
        const result = await response;

        if (result.ok) {
            console.log(
                `Fetched personal group for user: ${userName}`,
                result.result
            );
            return result.result;
        } else {
            console.error(
                `Failed to fetch personal group for user: ${userName}`
            );
            return null;
        }
    }

    async getCurrentGroup(groupName: string | null) {
        console.log("Fetching group name...");
        const response = toPromise(
            this.http.get<any>(`/current_group_name/${groupName}`)
        );
        const result = await response;

        if (result.ok) {
            console.log(`Fetched group's name: ${groupName}`, result.result);
            return result.result;
        } else {
            console.error("Failed to fetch groups name.");
        }
        return null;
    }

    async assignBadges(data: IData) {
        const response = toPromise(
            this.http.post<{ok: boolean}>("/give_badge", {
                given_by: data.given_by,
                doc_id: this.currentDocumentID,
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

    // Tarkistetaan, onko dialogi-ikkuna auki
    isDialogOpen(): boolean {
        return this.dialogOpen;
    }
    // Asetetaan dialogin tila
    setDialogOpen(isOpen: boolean): void {
        this.dialogOpen = isOpen;
    }

    // Funktio updatetapahtuman lähettämiseen kun luodaan uusi badge creatorilla, se päivitetään giver listaan.
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

    // Näyttää mahdolliset errorit
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

    // Poistaa error-viestin alerts-listasta
    closeAlert(alerts: any, index: number) {
        alerts.splice(index, 1);
    }

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
