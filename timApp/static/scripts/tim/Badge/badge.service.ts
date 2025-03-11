import {Injectable} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {lastValueFrom} from "rxjs";
import {Subject} from "rxjs";
import type {IBadge} from "tim/Badge/badge.interface";
import {toPromise} from "tim/util/utils";

@Injectable({
    providedIn: "root",
})
export class BadgeService {
    private all_badges: IBadge[] = [];

    // Subject, joka laukaisee updatesignaalin
    private updateBadgeSubject = new Subject<void>();

    // Observable updatetapahtuman kuunteluun
    updateBadgeList$ = this.updateBadgeSubject.asObservable();
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
            console.error("Error fetching badges:", error);
            // Return an empty array in case of an error
            return [];
        }
    }

    /**
     * Hakee käyttäjälle kuuluvat badget ID:n perusteella
     * @param id käyttäjän ID
     * @return palauttaa IBadge taulukon
     */
    async getUserBadges(id: number) {
        const response = toPromise(this.http.get<[]>("/groups_badges/" + id));
        const result = await response;
        const userBadges: IBadge[] = [];
        if (result.ok) {
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    userBadges.push(alkio);
                }
                console.log("haettu käyttäjän " + id + " badget");
            }
        }
        return userBadges;
    }

    /**
     * Ottaa valitun badgen pois käytöstä käyttäjältä.
     * @param badgegivenID badgegiven -tietokantataulukon id, jonka avulla valittu badge poistetaan käytöstä
     * @param giverID käyttäjän id, joka poistaa badgen käytöstä.
     */
    withdrawBadge(badgegivenID: number, giverID: number) {
        toPromise(this.http.get(`/withdraw_badge/${badgegivenID}/${giverID}`));
        console.log("Badge poistettu käytöstä id:llä: " + badgegivenID);
    }

    // Funktio updatetapahtuman lähettämiseen
    triggerUpdateBadgeList() {
        this.updateBadgeSubject.next();
    }
}
