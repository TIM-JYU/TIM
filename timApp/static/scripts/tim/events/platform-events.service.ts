import {Injectable} from "@angular/core";
import {patch} from "rxjs-interop";
import {BehaviorSubject, Subject} from "rxjs";
import type {Observable} from "rxjs";

@Injectable({
    providedIn: "root",
})
export class PlatformEventsService {
    private messageSubject = new Subject<string>(); // Used to broadcast messages
    events: string[] = [];
    Events$: BehaviorSubject<string[]>;

    constructor() {
        this.Events$ = new BehaviorSubject<string[]>(this.events);
    }

    sendEvent(e: string): void {
        this.events.push(e);
        this.Events$.next(this.events);
    }

    private messageBus = patch({
        [Symbol.observable]: () => this.messageSubject.asObservable(),

        sendMessage: (message: string) => {
            this.messageSubject.next(message); // Notify all subscribers
        },
    });

    sendMessage(message: string) {
        this.messageBus.sendMessage(message);
    }

    getMessages(): Observable<string> {
        return this.messageBus[Symbol.observable]();
    }
}
