import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";
import {toPromise} from "tim/util/utils";
import {Users} from "tim/user/userService";
import {documentglobals} from "tim/util/globals";
import type {IDocument} from "tim/item/IItem";
import {HttpClient} from "@angular/common/http";

interface INotificationSettings {
    email_doc_modify: boolean;
    email_comment_add: boolean;
    email_answer_add: boolean;
    email_comment_modify: boolean;
    email_annotation_add: boolean;
    email_annotation_modify: boolean;
}

@Component({
    selector: "tim-notification-options",
    template: `
        <tim-alert *ngIf="errorMessage" severity="danger">{{errorMessage}}</tim-alert>
        <form>
            <fieldset [disabled]="!canEdit">
                <p>Send me email on</p>
                <div class="checkbox"><label>
                    <input name="email_doc_modify" type="checkbox" [(ngModel)]="notifySettings.email_doc_modify"
                           (ngModelChange)="notifyChanged()"/>Document modifications
                </label></div>
                <div class="checkbox"><label>
                    <input name="email_comment_add" type="checkbox" [(ngModel)]="notifySettings.email_comment_add" 
                           (ngModelChange)="notifyChanged()"/>New comments
                </label></div>
                <div class="checkbox"><label>
                    <input name="email_comment_modify" type="checkbox" [(ngModel)]="notifySettings.email_comment_modify"
                           (ngModelChange)="notifyChanged()"/>Edited comments
                </label></div>
                <div class="checkbox" *ngIf="curItem.rights.teacher"><label>
                    <input name="email_answer_add" type="checkbox" [(ngModel)]="notifySettings.email_answer_add"
                           (ngModelChange)="notifyChanged()"/>New answers to tasks
                </label></div>
                <div class="checkbox"><label>
                    <input name="email_annotation_add" type="checkbox" [(ngModel)]="notifySettings.email_annotation_add" 
                           (ngModelChange)="notifyChanged()"/>New velps
                </label></div>
                <div class="checkbox"><label>
                    <input name="email_annotation_modify" type="checkbox" [(ngModel)]="notifySettings.email_annotation_modify"
                           (ngModelChange)="notifyChanged()"/>Edited velps
                </label></div>
            </fieldset>
        </form>
    `,
})
export class NotificationOptionsComponent implements OnInit {
    curItem: IDocument;
    errorMessage?: string;
    canEdit: boolean = false;
    notifySettings: INotificationSettings = {
        email_answer_add: false,
        email_comment_add: false,
        email_doc_modify: false,
        email_comment_modify: false,
        email_annotation_add: false,
        email_annotation_modify: false,
    };

    constructor(private http: HttpClient) {
        this.curItem = documentglobals().curr_item;
    }

    ngOnInit(): void {
        void this.getNotifySettings();
    }

    async getNotifySettings() {
        if (!Users.isLoggedIn()) {
            this.errorMessage =
                "You must be logged in to edit notification settings";
            return;
        }
        const r = await toPromise(
            this.http.get<INotificationSettings>(`/notify/${this.curItem.id}`)
        );
        if (r.ok) {
            this.notifySettings = r.result;
            this.canEdit = true;
        } else {
            this.errorMessage = `Could not get notification settings. Error message is: ${r.result.error.error}`;
        }
    }

    async notifyChanged() {
        this.errorMessage = undefined;
        this.canEdit = false;
        const r = await toPromise(
            this.http.post(`/notify/${this.curItem.id}`, this.notifySettings)
        );
        this.canEdit = true;
        if (!r.ok) {
            this.errorMessage = `Could not change notification settings. Error message is: ${r.result.error.error}`;
        }
    }
}
