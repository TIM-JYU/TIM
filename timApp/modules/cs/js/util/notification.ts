/* eslint no-underscore-dangle: ["error", { "allow": ["type_", "src_"] }] */
import {Component, Input} from "@angular/core";
import {OrderedSet} from "./set";

class Notification {
    timeoutHandle?: number;

    constructor(public id: string, public content: string) {}
}

@Component({
    selector: "notification",
    template: `
        <p *ngFor="let notif of notifications">{{notif.content}}</p>`,
})
export class NotificationComponent { // TODO: test

    @Input() timeout?: number;

    notifications: OrderedSet<Notification> = new OrderedSet((e) => e.id);

    push(id: string, str: string, timeout?: number): void;
    push(str: string, timeout?: number): void;
    push(id: string, str?: string | number, timeout?: number) {
        let rstr = id;
        if (typeof str == "string") {
            rstr = str;
        }
        this.remove(id);
        const notification = new Notification(id, rstr);
        this.notifications.push(notification);
        if (typeof str == "number") {
            timeout = str;
        }
        const time = timeout ?? this.timeout;
        if (time) {
            notification.timeoutHandle =  window.setTimeout(() => this.remove(id), time);
        }
    }

    remove(id: string) {
        const timeout = this.notifications.removeByKey(id)?.timeoutHandle;
        if (timeout) {
            window.clearTimeout(timeout);
        }
    }

    clear() {
        this.notifications.clear();
    }

    get length() {
        return this.notifications.length;
    }
}
