import {Injectable} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {lastValueFrom, Observable} from "rxjs";
import {toPromise} from "tim/util/utils";

class Badge {}

@Injectable({
    providedIn: "root",
})
export class BadgeService {
    private all_badges: any[] = [];
    constructor(private http: HttpClient) {}

    async getAllBadges(): Promise<Badge[]> {
        try {
            const result = await lastValueFrom(
                this.http.get<Badge[]>("/all_badges")
            );
            return result ?? []; // Ensure result is always an array
        } catch (error) {
            console.error("Error fetching badges:", error);
            return []; // Return an empty array in case of error
        }
    }
}
