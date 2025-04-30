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

    constructor(private http: HttpClient) {}

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
            this.triggerUpdateBadgeList();
            return {ok: true};
        }
        return {ok: false, data: result};
    }

    // async withdrawSelectedBadge(
    //     userid: number,
    //     badgeid: number,
    //     contextGroup: string
    // ) {
    //     const response = toPromise(
    //         this.http.post<{ok: boolean}>("/withdraw_all_badges", {
    //             badge_id: badgeid,
    //             usergroup_id: userid,
    //             context_group: contextGroup,
    //         })
    //     );
    //     const result = await response;
    //     if (result.ok) {
    //         this.triggerUpdateBadgeList();
    //         return {ok: true};
    //     }
    //     return {ok: false, data: result};
    // }
    //
    // async getBadgeHolders(badgeid: number) {
    //     const response = toPromise(
    //         this.http.get<IBadgeHolders>(`/badge_holders/${badgeid}`)
    //     );
    //     const result = await response;
    //     if (result.ok) {
    //         this.triggerUpdateBadgeList();
    //         return result.result;
    //     }
    //     return null;
    // }

    /**
     * Lähettää http post pyynnön, joka antaa käyttäjälle/ryhmälle badgen group- ja badge_id:n perusteella.
     * Jos paluuarvossa on virhe, kutsutaan showError metodia.
     * Kutsutaan notifyBadgeViewerUpdate, joka hoitaa live-päivityksen.
     * @param data sisältää kaikki tarvittavat tiedot http post kutsuun.
     */
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
