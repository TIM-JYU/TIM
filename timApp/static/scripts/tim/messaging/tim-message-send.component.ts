import {Component, EventEmitter, Input, Output} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {$http} from "../util/ngimport";
import {to, to2} from "../util/utils";
import {Users} from "../user/userService";

interface TimMessageOptions {
    // VIESTIM Keep this updated with MessageOptions class (at timMessage/routes.py). BUT recipients, messageSubject and messageBody are added later at postTimMessage()
    archive: boolean;
    important: boolean;
    messageChannel: boolean;
    pageList: string;
    isPrivate: boolean;
    reply: boolean;
    readReceipt: boolean;
    expires: Date | undefined;
    sender: string | null;
    senderEmail: string | null;
}

@Component({
    selector: "tim-message-send",
    template: `
        <div class="csRunDiv tableEmail" style="padding: 1em;" *ngIf="recipientList">
            <tim-close-button (click)="closeComponent()"></tim-close-button>
            <p>Recipients:</p>
            <p><textarea [(ngModel)]="recipientList" rows="4" cols="40" (input)="somethingChanged()"></textarea>
            </p>
            <p>Subject: <input [(ngModel)]="messageSubject" size="60" (input)="somethingChanged()"></p>
            <p>Message content:</p>
            <p><textarea [(ngModel)]="messageBody" rows="10" cols="70" (input)="somethingChanged()"></textarea></p>
            <span class="cursor-pointer" (click)="toggleOptions(); somethingChanged()"><a><span class="glyphicon"
                             [ngClass]="showOptions ? 'glyphicon-minus' : 'glyphicon-plus'"></span></a>{{showOptions ? "Hide" : "Show"}} message options</span>
            <div *ngIf="showOptions">
            <p *ngIf="!defaultEmail"><label><input type="checkbox"
                                      [(ngModel)]="timMessageOptions.archive" disabled>Archive message</label></p>
            <p *ngIf="!defaultEmail"><label><input type="checkbox"
                                      [(ngModel)]="timMessageOptions.important" disabled>Mark message as important (currently only applies to TIM messages)</label></p>
            <fieldset><p class="bold">Send as (choose at least one of the two)</p><!--<label *ngIf="!defaultEmail"><input type="checkbox" 
                                      [(ngModel)]="timMessageOptions.messageChannel">to recipient's own message channels</label><br/>-->
            <label><input type="checkbox" (change)="notDefault()"
                                      [(ngModel)]="email">email</label><br/>
                <ul *ngIf="email">
                    <li>
                    <label><input type="radio"
                                      [(ngModel)]="defaultEmail" name="defaultEmail" [value]="false">Use TIM to send</label>
                </li><li>
                    <label><input type="radio"
                                      [(ngModel)]="defaultEmail" name="defaultEmail" [value]="true">Use your default email client (recipients will see each others' addresses)</label>
                </li></ul>     
                <ul *ngIf="email && !defaultEmail">
                <li><label><input type="radio"
                                      [(ngModel)]="replyAll" name="replyAll" [value]="false" checked>Recipient only replies to sender (sees message as private)</label></li>
                <li><label><input type="radio"
                                      [(ngModel)]="replyAll" name="replyAll" [value]="true">Recipient replies all by default (sees message as a group message)</label></li>
            </ul>
            <label *ngIf="!defaultEmail"><input type="checkbox" (change)="emptyPageList()"
                                      [(ngModel)]="timMessage">TIM message</label></fieldset><br/>
            <ul>    
                <li><p *ngIf="timMessage && !defaultEmail">Pages to send TIM message to: (enter URL addresses)<br/>(URLs will be automatically shortened)</p></li>
                <li><tim-alert *ngIf="urlError" severity="danger">
                    {{ urlError }}
                </tim-alert></li>
            <li><p *ngIf="timMessage && !defaultEmail"><textarea [(ngModel)]="timMessageOptions.pageList" (change)="checkUrls()" rows="4" cols="70"></textarea></p></li>
            <li><p *ngIf="timMessage && !defaultEmail"><label><input type="checkbox"
                                      [(ngModel)]="timMessageOptions.isPrivate" disabled>Recipient sees TIM message as private</label></p></li>
            <li><p *ngIf="timMessage && !defaultEmail"><label><input type="checkbox"
                                      [(ngModel)]="timMessageOptions.reply">TIM message can be replied to</label></p></li>
            <li><p *ngIf="timMessage && !defaultEmail"><label><input type="checkbox"
                                      [(ngModel)]="timMessageOptions.readReceipt">TIM message can be marked as read</label></p></li>
            <li><p *ngIf="timMessage && !defaultEmail" class="form-group">
                <label for="expiration-selector">TIM message will be removed on:</label>
                <tim-datetime-picker id="expiration-selector"
                                     [(time)]="timMessageOptions.expires"
                                     placeholder="No automatic date">
                </tim-datetime-picker>
            </p></li></ul><br/></div>
            <p>
                <button class="timButton" id="sendButton" [disabled]="disableSendButton()"
                        (click)="sendMessage()">
                    Send
                </button>
                <span class="savedtext" *ngIf="messageMsg"> {{messageMsg}}</span>
            </p>
        </div>
    `,
    styleUrls: ["./tim-message-send.scss"],
})
export class TimMessageComponent {
    /**
     * VIESTIM: This component has a minor bug. If the textfield of recipients is emptied by hand, the component closes
     *  and it can't be reopened unless the recipientList variable changes. A hypothetical fix would be to use a
     *  separate flag in the *ngIf, instead of just the recipientList variable. Then this flag would only be operated to
     *  close when  the component is closed from the x. It would propably requrie change detection for the recipientList
     *  variable, as it's length grows beoynd 0 the the flag is set on?
     */

    @Input()
    recipientList: string = "";
    @Output() recipientListChange = new EventEmitter<string>();
    messageSubject: string = "";
    messageBody: string = "";
    showOptions: boolean = false;
    emailbcc: boolean = false;
    emailbccme: boolean = true;
    email: boolean = true;
    defaultEmail: boolean = false;
    replyAll: boolean | undefined = false;
    messageMsg: string = "";
    timMessage: boolean = false;
    urlError: string = "";
    formChanged: boolean = true;
    timMessageOptions: TimMessageOptions = {
        messageChannel: false,
        archive: false,
        important: false,
        isPrivate: false,
        pageList: "",
        readReceipt: false,
        reply: false,
        expires: undefined,
        sender: Users.getCurrent().real_name,
        senderEmail: Users.getCurrent().email,
    };
    @Input()
    docId?: number;

    constructor(private http: HttpClient) {}

    /**
     * Close the component and propagate the information to parent component.
     */
    closeComponent() {
        this.recipientList = "";
        this.recipientListChange.emit(this.recipientList);
    }

    toggleOptions() {
        this.showOptions = !this.showOptions;
    }

    notDefault() {
        this.defaultEmail = false;
    }

    emptyPageList() {
        this.timMessageOptions.pageList = "";
    }

    somethingChanged() {
        this.formChanged = true;
    }

    // Checks if all mandatory fields have values
    disableSendButton() {
        return (
            !this.formChanged ||
            this.urlError ||
            (!this.timMessageOptions.messageChannel &&
                !this.email &&
                !this.timMessage) ||
            (this.timMessage && !this.timMessageOptions.pageList)
        );
    }

    // Checks if the URLs that the user wants to save TIM message to actually exist in TIM
    // and that the user has at least edit access to them
    // Also shortens existing URLs
    async checkUrls() {
        this.urlError = "";
        const result = await to2(
            this.http
                .post<{shortened_urls: string}>("/timMessage/url_check", {
                    urls: this.timMessageOptions.pageList,
                })
                .toPromise()
        );
        if (!result.ok) {
            this.urlError = result.result.error.error;
            console.error(result.result.error.error);
        } else {
            this.timMessageOptions.pageList = result.result.shortened_urls;
        }
    }

    // resets form to it's initial values
    resetForm() {
        this.messageMsg = "Sent!";
        setTimeout((): void => {
            this.messageMsg = "";
        }, 5000);
        this.messageSubject = "";
        this.showOptions = false;
        this.emailbcc = false;
        this.emailbccme = true;
        this.email = true;
        this.defaultEmail = false;
        this.replyAll = false;
        this.timMessage = false;
        this.formChanged = false;
        this.timMessageOptions = {
            messageChannel: false,
            archive: false,
            important: false,
            isPrivate: false,
            pageList: "",
            readReceipt: false,
            reply: true,
            expires: undefined,
            sender: Users.getCurrent().real_name,
            senderEmail: Users.getCurrent().email,
        };
    }

    // VIESTIM: make it possible to send as TIM message and email at the same time
    public async sendMessage() {
        // send as TIM message
        if (this.timMessage) {
            const result = await this.postTimMessage(this.timMessageOptions);
            console.log(this.timMessageOptions);
            if (!result.ok) {
                console.error(result.result.error.error);
            }
        }
        // send as email in TIM
        if (this.email && !this.defaultEmail) {
            await this.sendEmailTim();
            return;
        }
        // TODO: iPad do not like ;
        if (this.email && this.defaultEmail) {
            let addrs = this.recipientList.replace(/\n/g, ",");
            let bcc = "";
            if (this.emailbcc) {
                bcc = addrs;
                addrs = "";
            }
            if (this.emailbccme) {
                if (bcc) {
                    bcc += ",";
                }
                bcc += Users.getCurrent().email;
            }
            window.location.href =
                "mailto:" +
                addrs +
                "?" +
                "subject=" +
                this.messageSubject +
                "&" +
                "body=" +
                this.messageBody +
                "&" +
                "bcc=" +
                bcc;
            this.resetForm();
        }
        this.resetForm();
    }

    // VIESTIM this helper function helps keeping types in check.
    private postTimMessage(options: TimMessageOptions) {
        const message = {
            messageBody: this.messageBody,
            messageSubject: this.messageSubject,
            recipients: this.recipientList.split(/\n/g),
        };
        return to2(
            this.http.post("/timMessage/send", {options, message}).toPromise()
        );
    }

    async sendEmailTim() {
        if (!this.docId) {
            this.messageMsg = "Cannot send email without docId";
            return;
        }
        this.messageMsg = ""; // JSON.stringify(response);
        const url = `/multiSendEmail/${this.docId}`;
        let response;
        // if reply all is chosen
        if (this.replyAll) {
            response = await to(
                $http.post<string[]>(url, {
                    rcpt: this.recipientList.replace(/\n/g, ";"),
                    subject: this.messageSubject,
                    msg: this.messageBody,
                    bccme: this.emailbccme,
                })
            );
            if (!response.ok) {
                this.messageMsg = response.result.data.error;
            } else {
                this.resetForm();
            }
        } else {
            // if only reply to sender is chosen
            const recipients = this.recipientList.split(/\n/g);
            for (const recipient of recipients) {
                response = await to(
                    $http.post<string[]>(url, {
                        rcpt: recipient,
                        subject: this.messageSubject,
                        msg: this.messageBody,
                        bccme: this.emailbccme,
                    })
                );
                if (!response.ok) {
                    this.messageMsg = response.result.data.error;
                }
            }
            this.resetForm();
        }
    }
}
