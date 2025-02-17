import {Injectable} from "@angular/core";
import type {WebSocketSubject} from "rxjs/webSocket";
import {webSocket} from "rxjs/webSocket";

type IMessage = string;

@Injectable({
    providedIn: "root",
})
export class WebSocketService {
    private socket: WebSocketSubject<IMessage>;
    messages$: IMessage[] = [];

    constructor() {
        this.socket = webSocket("http://localhost/ws");
        this.socket.subscribe({
            next: (msg) => this.messages$.push(msg), // Called whenever there is a message from the server.
            error: (err) => console.log(err), // Called if at any point WebSocket API signals some kind of error.
            complete: () => this.socket.next("disconnect"), // Called when connection is closed (for whatever reason).
        });
    }

    // Send a message to the server
    sendMessage(message: IMessage) {
        console.log("Send happened: ", message);
        this.socket.next(message);
    }

    // Close the WebSocket connection
    closeConnection() {
        this.socket.complete();
    }
}
