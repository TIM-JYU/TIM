import {Injectable, NgZone} from "@angular/core";
import {Observable} from "rxjs";

@Injectable({
    providedIn: "root",
})
export class EventService {
    private eventSource: EventSource | null = null;

    constructor(private ngZone: NgZone) {}

    listenSSE(url: string): Observable<string[]> {
        return new Observable<string[]>((observer) => {
            this.eventSource = new EventSource(url);

            this.eventSource.onmessage = (event) => {
                // Ensure updates happen inside Angular's zone to trigger change detection
                console.log("Event message");
                console.log(event);
                this.ngZone.run(() => observer.next(event.data));
            };

            this.eventSource.onerror = (error) => {
                observer.error("SSE Error");
                this.eventSource?.close();
            };

            return () => this.eventSource?.close();
        });
    }
}
