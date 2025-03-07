import {Injectable} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {lastValueFrom} from "rxjs";

class Badge {}

@Injectable({
    providedIn: "root",
})
export class BadgeService {
    private all_badges: any[] = [];
    constructor(private http: HttpClient) {}

    async getAllBadges(): Promise<Badge[]> {
        try {
            // Create an HTTP request and wait for response
            const response = this.http.get<Badge[]>("/all_badges");

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
}
