import {Injectable} from "@angular/core";
// import type {WebSocketSubject} from "rxjs/webSocket";
// import {webSocket} from "rxjs/webSocket";
import type {Socket} from "socket.io-client";
import {io} from "socket.io-client";

interface IMessage {
    type: string;
    data: string;
}

@Injectable({
    providedIn: "root",
})
export class WebSocketService {
    // private socket: WebSocketSubject<string>;
    private socket: Socket;
    messages$: IMessage[] = [];

    constructor() {
        //        this.socket = webSocket("ws://localhost/");
        this.socket = io("http://localhost", {
            transports: ["websocket"],
        });
        this.socket.on("message", (data: IMessage) => {
            console.log(data);
            this.messages$.push(data);
        });
    }

    // Send a message to the server
    sendMessage(message: IMessage) {
        console.log("Send happened: ", message);
        this.socket.send(message);
        // this.messages$.push(message);
        // this.socket.next(JSON.stringify(message));
    }

    listenSocket() {}

    // Receive messages from the server
    /*    listenSocket(): void {
  console.log("Listening...");
  this.socket.subscribe({
  next: (msg) => this.messages$.push(msg), // Called whenever there is a message from the server.
  error: (err) => console.log(err), // Called if at any point WebSocket API signals some kind of error.
  complete: () => console.log("complete"), // Called when connection is closed (for whatever reason).
  });
  console.log("Messages: ", this.messages$);
  }*/

    // Close the WebSocket connection
    closeConnection() {
        this.socket.off();
        // this.socket.complete();
    }
}
