import {Component, Input} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {$http} from "../util/ngimport";
import {to, to2} from "../util/utils";
import {TaskId} from "../plugin/taskid";
import {Users} from "../user/userService";

interface CreateMessageOptions {
    // VIESTIM Keep this updated with MessageOptions class (at timMessage/routes.py). BUT recipients, emailsubject and emailbody are added later at postTimMessage()
    // VIESTIM or TODO: change this later when separate email functionality is removed?
    messageChannel: boolean;
    archive: boolean;
    important: boolean;
    isPrivate: boolean;
    pageList: string;
    confirm: boolean;
    reply: boolean;
    replyAll: boolean | undefined;
    expires: Date | undefined;
    sender: string | null;
    senderEmail: string | null;
}

@Component({
    selector: "tim-message-send",
    template: `
        <div class="csRunDiv tableEmail" style="padding: 1em;" *ngIf="emailList">
            <tim-close-button (click)="emailList=''"></tim-close-button>
            <p>Recipients:</p>
            <p><textarea [(ngModel)]="emailList" rows="4" cols="40"></textarea>
            </p>
            <p>Subject: <input [(ngModel)]="emailsubject" size="60"></p>
            <p>Message content:</p>
            <p><textarea [(ngModel)]="emailbody" rows="10" cols="70"></textarea></p>
            <button class="timButton" id="optionsButton" (click)="toggleOptions()">{{showOptions ? "Hide" : "Show"}} message options</button>
            <div *ngIf="showOptions">
            <fieldset><p>Send (choose at least one of the two)</p><!--<label *ngIf="!defaultEmail"><input type="checkbox" 
                                      [(ngModel)]="createMessageOptions.messageChannel">to recipient's own message channels</label><br/>-->
            <label><input type="checkbox" (change)="notDefault()"
                                      [(ngModel)]="email">as email</label><br/>
                <ul *ngIf="email">
                    <li>
                    <label><input type="radio"
                                      [(ngModel)]="defaultEmail" name="defaultEmail" [value]="false">use TIM to send</label>
                </li><li>
                    <label><input type="radio"
                                      [(ngModel)]="defaultEmail" name="defaultEmail" [value]="true">use your default email client (recipients will see each others' addresses)</label>
                </li></ul>     
                <ul *ngIf="email && !defaultEmail">
                <li><label><input type="radio"
                                      [(ngModel)]="createMessageOptions.replyAll" name="replyAll" [value]="false" checked>Recipient only replies to sender (sees message as private)</label></li>
                <li><label><input type="radio"
                                      [(ngModel)]="createMessageOptions.replyAll" name="replyAll" [value]="true" [disabled]="timMessage">Recipient replies all by default (sees message as a group message)</label></li>
            </ul>
            <label *ngIf="!defaultEmail"><input type="checkbox"
                                      [(ngModel)]="timMessage">as TIM message</label></fieldset><br/>
            
            <p *ngIf="!defaultEmail"><label><input type="checkbox"
                                      [(ngModel)]="createMessageOptions.archive">Archive message</label></p>
            <p *ngIf="!defaultEmail"><label><input type="checkbox"
                                      [(ngModel)]="createMessageOptions.important">Mark message as important</label></p>
            <p *ngIf="timMessage && !defaultEmail">Pages to send TIM message to: (enter URL addresses)<br/>(URLs will be automatically shortened)</p>
            <p *ngIf="timMessage && !defaultEmail"><textarea [(ngModel)]="createMessageOptions.pageList" rows="4" cols="70"></textarea></p>
            <p *ngIf="timMessage && !defaultEmail"><label><input type="checkbox"
                                      [(ngModel)]="createMessageOptions.isPrivate">Recipient sees TIM message as private</label></p>
            <p *ngIf="timMessage && !defaultEmail"><label><input type="checkbox"
                                      [(ngModel)]="createMessageOptions.reply">TIM message can be replied to</label></p> 
            <p *ngIf="timMessage && !defaultEmail"><label><input type="checkbox"
                                      [(ngModel)]="createMessageOptions.confirm">TIM message can be marked as read</label></p>
            <p *ngIf="timMessage && !defaultEmail" class="form-group">
                <label for="expiration-selector">TIM message will be removed on:</label>
                <tim-datetime-picker id="expiration-selector"
                                     [(time)]="createMessageOptions.expires"
                                     placeholder="No automatic date">
                </tim-datetime-picker>
            </p><br/></div>
            <p>
                <button class="timButton" id="sendButton" [disabled]="!showSendButton()"
                        (click)="sendMessage()">
                    Send
                </button>
                <span class="savedtext" *ngIf="emailMsg"> Sent!</span>
            </p>
        </div>
    `,
    styleUrls: ["./tim-message-send.scss"],
})
export class TimMessageComponent {
    @Input()
    emailList: string = "";
    emailsubject: string = "";
    emailbody: string = "";
    showOptions: boolean = false;
    emailbcc: boolean = false;
    emailbccme: boolean = true;
    email: boolean = true;
    // emailtim: boolean = true;
    defaultEmail: boolean = false;
    emailMsg: string = "";
    timMessage: boolean = false;
    createMessageOptions: CreateMessageOptions = {
        messageChannel: false,
        archive: false,
        important: false,
        isPrivate: false,
        /* VIESTIM: check that urls listed in pageList exist in TIM (and user has at least edit permission to them) and inform user if not.*/
        /* VIESTIM: shorten urls listed in pageList and show them to user */
        pageList: "",
        confirm: false,
        reply: false,
        replyAll: false,
        expires: undefined,
        sender: Users.getCurrent().real_name,
        senderEmail: Users.getCurrent().email,
    };
    @Input()
    taskId?: TaskId;

    constructor(private http: HttpClient) {}

    toggleOptions() {
        this.showOptions = !this.showOptions;
    }

    notDefault() {
        this.defaultEmail = false;
    }

    // Checks if all mandatory fields have values
    showSendButton() {
        return (
            (this.createMessageOptions.messageChannel ||
                this.email ||
                this.timMessage) &&
            ((this.createMessageOptions.reply &&
                this.createMessageOptions.replyAll != undefined) ||
                !this.createMessageOptions.reply)
        );
    }

    // resets form to it's initial values
    resetForm() {
        this.emailsubject = "";
        this.emailbody = "";
        this.emailbcc = false;
        this.emailbccme = true;
        this.email = true;
        this.defaultEmail = false;
        this.timMessage = false;
        this.createMessageOptions = {
            messageChannel: false,
            archive: false,
            important: false,
            isPrivate: false,
            pageList: "",
            confirm: false,
            reply: true,
            replyAll: false,
            expires: undefined,
            sender: Users.getCurrent().real_name,
            senderEmail: Users.getCurrent().email,
        };
    }

    async sendEmailTim() {
        if (!this.taskId) {
            this.emailMsg = "Cannot send email without taskId";
            return;
        }
        this.emailMsg = ""; // JSON.stringify(response);
        const url = `/multiSendEmail/${this.taskId.docTask().toString()}`;
        const response = await to(
            $http.post<string[]>(url, {
                rcpt: this.emailList.replace(/\n/g, ";"),
                subject: this.emailsubject,
                msg: this.emailbody,
                bccme: this.emailbccme,
            })
        );
        if (response.ok) {
            this.emailMsg = "Sent";
            this.resetForm();
        } else {
            this.emailMsg = response.result.data.error;
        }
    }

    async sendTimMessage() {
        const result = await this.postTimMessage(this.createMessageOptions);
        if (!result.ok) {
            console.error(result.result.error.error);
        } else {
            // If emailMsg != "", text "Sent!" appears next to Send button.
            this.emailMsg = "sent";
            this.resetForm();
        }
    }

    // VIESTIM this helper function helps keeping types in check.
    private postTimMessage(options: CreateMessageOptions) {
        const message = {
            emailbody: this.emailbody,
            emailsubject: this.emailsubject,
            recipients: this.emailList.split(/\n/g),
        };
        const timMessage = {...options, ...message};
        return to2(
            this.http
                .post("/timMessage/send", {options: timMessage})
                .toPromise()
        );
    }

    // TODO: separate TIM messages from here?
    // VIESTIM: make it possible to send as TIM message and email at the same time
    public async sendMessage() {
        // send as TIM message
        if (this.timMessage) {
            await this.sendTimMessage();
        }
        // send as email in TIM
        if (this.email && !this.defaultEmail) {
            await this.sendEmailTim();
            return;
        }
        // TODO: iPad do not like ;
        if (this.email && this.defaultEmail) {
            let addrs = this.emailList.replace(/\n/g, ",");
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
                this.emailsubject +
                "&" +
                "body=" +
                this.emailbody +
                "&" +
                "bcc=" +
                bcc;
            this.resetForm();
        }
    }
}
