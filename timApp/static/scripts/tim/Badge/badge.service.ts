import {Injectable} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {lastValueFrom} from "rxjs";
import {Subject} from "rxjs";
import {IBadge} from "tim/Badge/badge.interface";

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

    // Funktio updatetapahtuman lähettämiseen
    triggerUpdateBadgeList() {
        this.updateBadgeSubject.next();
    }
}
