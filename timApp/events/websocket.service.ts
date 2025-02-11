import {Injectable} from "@angular/core";
import type {WebSocketSubject} from "rxjs/webSocket";
import {webSocket} from "rxjs/webSocket";
import type {Observable} from "rxjs";

interface IMessage {
    type: string;
    data: string;
}

@Injectable({
    providedIn: "root",
})
export class WebSocketService {
    private socket$: WebSocketSubject<IMessage>;

    constructor() {
        this.socket$ = webSocket("ws://localhost:443/chat");
    }

    // Send a message to the server
    sendMessage(message: IMessage) {
        this.socket$.next(message);
    }

    // Receive messages from the server
    getMessages(): Observable<IMessage> {
        return this.socket$.asObservable();
    }

    // Close the WebSocket connection
    closeConnection() {
        this.socket$.complete();
    }
}
