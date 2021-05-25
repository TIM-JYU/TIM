import {HttpClient} from "@angular/common/http";
import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {to, to2} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {
    archivePolicyNames,
    ArchiveType,
    Distribution,
    ListOptions,
    MemberInfo,
    ReplyToListChanges,
} from "tim/messaging/listOptionTypes";
import {documentglobals} from "tim/util/globals";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {TableFormModule} from "tim/plugin/tableForm";
import moment, {Moment} from "moment";
import {showInputDialog} from "tim/ui/showInputDialog";
import {InputDialogKind} from "tim/ui/input-dialog.kind";
import {$http} from "tim/util/ngimport";
import {Users} from "../user/userService";

@Component({
    selector: "tim-message-list-admin",
    template: `
        <form class="form-horizontal">
            <h1>Message list management</h1>
            <tim-alert *ngIf="permanentErrorMessage" severity="danger">{{permanentErrorMessage}}</tim-alert>
            <div class="form-group">
                <label for="list-name" class="list-name control-label col-sm-3">List name: </label>
                <div class="col-sm-9">
                    <div class="input-group">
                        <input type="text" class="form-control" name="list-name" id="list-name" disabled
                               [(ngModel)]="listname"/>
                        <div class="input-group-addon" id="domain-indicator">@</div>
                        <select id="domain-select" class="form-control" name="domain-select" [(ngModel)]="domain">
                            <option [disabled]="domains.length < 2" *ngFor="let domain of domains">{{domain}}</option>
                        </select>
                    </div>
                </div>
            </div>
            <div class="form-group" *ngIf="domain">
                <label for="list-description" class="short-description control-label col-sm-3">List address: </label>
                <div class="col-sm-9">
                    <input type="text" class="form-control" name="list-email-address" id="list-email-address"
                           [ngModel]="listAddress()" disabled/>
                </div>
            </div>
            <div class="form-group">
                <label for="list-description" class="short-description control-label col-sm-3">Short
                    description: </label>
                <div class="col-sm-9">
                    <input type="text" class="form-control" name="list-description" id="list-description"
                           [(ngModel)]="listDescription"/>
                </div>
            </div>
            <div class="form-group">
                <label for="list-info" class="long-description control-label col-sm-3">Long description: </label>
                <div class="col-sm-9">
                <textarea name="list-info" class="list-info form-control" id="list-info"
                          [(ngModel)]="listInfo">A more detailed information thingy for this list.</textarea>
                </div>
            </div>
            <div *ngIf="archiveOptions && archive">
                <p class="list-archive-policy-header">Archive policy:</p>
                <!-- Variable archiveoptions is reversed, so indexing for display has to accommodate. -->
                <p class="indented">{{archiveOptions[archiveOptions.length - (archive + 1)].policyName}}</p>
                <!-- Hide radio buttons here, until the changing of archive policy levels is implemented -->
                <!--
                <ul id="archive-policy-list">
                    <li *ngFor="let option of archiveOptions">
                        <label for="archive-{{option.archiveType}}">
                        <input
                                name="items-radio"
                                type="radio"
                                id="archive-{{option.archiveType}}"
                                [value]="option.archiveType"
                                [(ngModel)]="archive"
                        />
                        {{option.policyName}}</label>
                    </li>
                </ul>
                -->
            </div>
            <div class="section">
                <h3>Options</h3>
                <div class="indented">
                    <label>
                        <input type="text" name="list-subject-prefix" [(ngModel)]="listSubjectPrefix">
                        Subject prefix.</label>
                </div>
                <div class="indented">
                    <label>
                        <input type="checkbox" name="notify-owner-on-list-change" id="notify-owner-on-list-change"
                               [(ngModel)]="notifyOwnerOnListChange"/>
                        Notify owners on list changes (e.g. user subscribes).</label>
                </div>
                <div class="indented">
                    <label for="tim-users-can-join">
                        <input type="checkbox" name="tim-users-can-join" [(ngModel)]="timUsersCanJoin">
                        TIM users can freely join this list.</label>
                    <div class="indented-more">
                        <label>
                            <input type="checkbox" name="default-send-right" [(ngModel)]="defaultSendRight"
                                   [disabled]="!timUsersCanJoin">
                            Default send right for new members.</label>
                    </div>
                    <div class="indented-more">
                        <label>
                            <input type="checkbox" name="default-delivery-right" [(ngModel)]="defaultDeliveryRight"
                                   [disabled]="!timUsersCanJoin">
                            Default delivery right for new members.</label>
                    </div>
                </div>
                <div class="indented">
                    <label for="can-user-unsubscribe">
                        <input type="checkbox" name="can-user-unsubscribe" [(ngModel)]="canUnsubscribe">
                        Members can unsubscribe from the list on their own.</label>
                </div>
                <div class="indented">
                    <label for="non-members-can-send">
                        <input type="checkbox" name="non-members-can-send" [(ngModel)]="nonMemberMessagePass">
                        Non members can send messages to list.</label>
                </div>
                <div class="indented">
                    <label for="only-text">
                        <input type="checkbox" name="only-text" [(ngModel)]="onlyText">
                        No HTML messages allowed on the list.</label>
                </div>
                <div class="indented">
                    <label for="allow-attachments">
                        <input type="checkbox" name="allow-attachments" [(ngModel)]="allowAttachments">
                        Allow attachments on the list.</label>
                </div>
                <div class="indented">
                    <label>
                        <input type="checkbox" name="list-answer-guidance" [(ngModel)]="listAnswerGuidance">
                        Guide answers to message list.</label>
                </div>
                <div>
                    <button class="timButton" (click)="saveOptions()">Save changes</button>
                    <tim-alert severity="success" *ngIf="saveSuccessMessage">{{saveSuccessMessage}}</tim-alert>
                    <tim-alert severity="danger" *ngIf="saveFailMessage">{{saveFailMessage}}</tim-alert>
                </div>
                <div id="members-section" class="section">
                    <h3>Members</h3>
                    <div id="add-members-section">
                        <label for="add-multiple-members">Add members</label> <br/>
                        <textarea id="add-multiple-members" name="add-multiple-members"
                                  [(ngModel)]="membersTextField"></textarea>
                        <div>
                            <div>
                                <input type="checkbox" name="new-member-send-right" [(ngModel)]="newMemberSendRight">
                                <label for="new-member-send-right">New member's send right.</label>
                            </div>
                            <div>
                                <input type="checkbox" name="new-member-delivery-right"
                                       [(ngModel)]="newMemberDeliveryRight">
                                <label for="new-member-delivery-right">New member's delivery right.</label>
                            </div>
                        </div>
                        <button (click)="addNewListMember()" class="timButton">Add new members</button>
                        <div id="member-add-feedback">
                            <tim-alert *ngIf="memberAddSucceededResponse"
                                       severity="success">{{memberAddSucceededResponse}}</tim-alert>
                            <tim-alert *ngIf="memberAddFailedResponse"
                                       severity="danger">{{memberAddFailedResponse}}</tim-alert>
                        </div>
                    </div>
                </div>
                <div class="section">
                    <table>
                        <caption>List members</caption>
                        <thead>
                        <tr>
                            <th>Name</th>
                            <th>Email</th>
                            <th>Send right</th>
                            <th>Delivery right</th>
                            <th>Membership ended</th>
                            <th>Removed</th>
                        </tr>
                        </thead>
                        <tbody>
                        <tr *ngFor="let member of membersList">
                            <td>{{member.name}}</td>
                            <td>{{member.email}}</td>
                            <td>
                                <input type="checkbox" [(ngModel)]="member.sendRight"
                                       name="member-send-right-{{member.email}}">
                            </td>
                            <td>
                                <input type="checkbox" [(ngModel)]="member.deliveryRight"
                                       name="member-delivery-right-{{member.email}}">
                            </td>
                            <td>{{member.removed}}</td>
                            <td><input type="checkbox" (click)="membershipChange(member)" [ngModel]="!!member.removed"
                                       name="removed-{{member.email}}"/></td>
                        </tr>
                        </tbody>
                    </table>
                    <button class="timButton" (click)="saveMembers()">Save</button>
                </div>
            </div>
            <div id="email-send">
                <tim-message-send [(recipientList)]="recipients" [docId]="getDocId()"></tim-message-send>
                <button class="timButton" (click)="openEmail()" *ngIf="!recipients">Send message to list</button>
                <div>
                    <tim-alert *ngIf="memberSaveSuccessResponse"
                               severity="success">{{memberSaveSuccessResponse}}</tim-alert>
                    <tim-alert *ngIf="memberSaveFailResponse"
                               severity="danger">{{memberAddFailedResponse}}</tim-alert>
                </div>
            </div>
            <div class="section">
                <h2>List deletion</h2>
                <button class="timButton" (click)="deleteList()">Delete List</button>
            </div>
            <div>
                <h3>Links</h3>
                <div *ngIf="archiveURL">
                    <a [href]="archiveURL">List's archive</a>
                </div>
                <div *ngIf="emailAdminURL">
                    <a [href]="emailAdminURL">Advanced email list settings</a>
                </div>
            </div>
        </form>
    `,
    styleUrls: ["message-list-admin.component.scss"],
})
export class MessageListAdminComponent implements OnInit {
    listname: string = "";

    archive?: ArchiveType;

    domain?: string;
    domains: string[] = [];

    membersTextField?: string;
    membersList: MemberInfo[] = [];

    urlPrefix: string = "/messagelist";

    // Not in use at the moment.
    ownerEmail: string = "";

    archiveOptions = archivePolicyNames;

    notifyOwnerOnListChange: boolean = false;
    timUsersCanJoin?: boolean = false;

    listInfo?: string;
    listDescription?: string;

    emailAdminURL?: string;
    archiveURL?: string;

    canUnsubscribe?: boolean;
    defaultSendRight?: boolean;
    defaultDeliveryRight?: boolean;
    listSubjectPrefix?: string;
    nonMemberMessagePass?: boolean;
    onlyText?: boolean;
    allowAttachments?: boolean;
    // distibution?: Channel[];
    distribution?: Distribution; // TODO: Not in use at the moment. Add this to the UI.

    listReplyToChange?: ReplyToListChanges;
    listAnswerGuidance?: boolean; // Track above enum value in a checkbox.

    newMemberSendRight: boolean = true;
    newMemberDeliveryRight: boolean = true;

    saveSuccessMessage: string = "";
    saveFailMessage: string = "";

    // Response strings used in giving feedback to the user on adding new members to the message list.
    memberAddSucceededResponse: string = "";
    memberAddFailedResponse: string = "";

    memberSaveSuccessResponse: string = "";
    memberSaveFailResponse: string = "";

    // Permanent error messages that cannot be recovered from, e.g. loading failed and reload is needed.
    permanentErrorMessage?: string;

    recipients = "";

    removed?: Moment;

    /**
     * Modifies the member's removed attribute if the member's state is changed.
     * @param member Who's membership on the list is changed.
     */
    membershipChange(member: MemberInfo) {
        if (member.removed) {
            member.removed = undefined;
        } else {
            // Set time stamp when the member was removed.
            member.removed = moment();
        }
    }

    /**
     * The current document's document ID.
     */
    getDocId() {
        return documentglobals().curr_item.id;
    }

    /**
     * Build this list's email address, if there is a domain configured. Otherwise return an empty string.
     */
    listAddress() {
        if (this.domain) {
            return `${this.listname}@${this.domain}`;
        }
        return "";
    }

    /**
     * Initialization procedures.
     */
    async ngOnInit() {
        if (Users.isLoggedIn()) {
            // Get domains.
            await this.getDomains();

            // Load message list options.
            const docId = this.getDocId();
            const result1 = await this.loadValues(docId);

            if (result1.ok) {
                this.setValues(result1.result);
            } else {
                this.permanentErrorMessage = `Loading list options failed: ${result1.result.error.error}`;
                // Loading options failed. Short circuit here, no reason to continue.
                return;
            }

            // Load list members.
            const result2 = await this.getListMembers();

            if (result2.ok) {
                console.log(result2.result);
                this.membersList = result2.result;
            } else {
                this.permanentErrorMessage = `Loading list's members failed: ${result2.result.error.error}`;
            }
        }
    }

    /**
     * Opens the email sending view by adding the list's address to the string of recipients.
     */
    openEmail() {
        this.recipients = this.listAddress();
    }

    /**
     * Get domains ccondigured for email list use.
     */
    private async getDomains() {
        const result = await to2(
            this.http.get<string[]>(`${this.urlPrefix}/domains`).toPromise()
        );
        if (result.ok) {
            this.domains = result.result;
            if (!this.domains.length) {
                this.domain = this.domains[0];
            }
        } else {
            /* Getting an error here is not a problem, since these domains are not (yet) in use other than displaying
             * them in the UI. The UI will probably look a bit funky, but it does not affect functionality right now.
             * In the future, this is a problem if email list could be taken into use after the message list already
             * exists.
             */
        }
    }

    constructor(private http: HttpClient) {}

    /**
     * Compile email addresses separated by line breaks into a list
     * @private
     */
    private parseMembers(): string[] {
        if (!this.membersTextField) {
            return [];
        }
        return this.membersTextField.split("\n").filter((e) => e);
    }

    /**
     * Add new members to message list.
     */
    async addNewListMember() {
        const memberCandidates = this.parseMembers();
        if (memberCandidates.length == 0) {
            return;
        }
        const result = await to2(
            this.http
                .post(`${this.urlPrefix}/addmember`, {
                    member_candidates: memberCandidates,
                    msg_list: this.listname,
                    send_right: this.newMemberSendRight,
                    delivery_right: this.newMemberDeliveryRight,
                })
                .toPromise()
        );
        if (result.ok) {
            // Empty the text field.
            this.membersTextField = undefined;
            this.memberAddSucceededResponse = "New members added.";
        } else {
            this.memberAddFailedResponse = `Adding new members failed: ${result.result.error.error}`;
        }
    }

    /**
     * Get all list members.
     */
    async getListMembers() {
        return to2(
            this.http
                .get<MemberInfo[]>(
                    `${this.urlPrefix}/getmembers/${this.listname}`
                )
                .toPromise()
        );
    }

    /**
     * Helper for list deletion.
     */
    async deleteList() {
        // Ask confirmation from the user.
        await showInputDialog({
            title: "Confirm list deletion",
            text: "Confirm you really want to delete this list.",
            okText: "Delete",
            isInput: InputDialogKind.ValidatorOnly,
            validator: async () => {
                const result = await to(
                    $http.delete(`/messagelist/deletelist`, {
                        params: {
                            listname: this.listname,
                            domain: this.domain ? this.domain : "",
                            permanent: false, // Only non-permanent deletion at this point.
                        },
                    })
                );
                if (result.ok) {
                    // Deletion was successful. Beam us up.
                    location.assign("/view/messagelists");
                    return {ok: true, result: result.result} as const;
                } else {
                    this.permanentErrorMessage = "Deleting the list failed.";
                    return {
                        ok: false,
                        result: result.result.data.error,
                    } as const;
                }
            },
        });
    }

    /**
     * Get values for message list's options.
     * @param docID List is defined by it's management document, so we get list's options and members with it.
     */
    async loadValues(docID: number) {
        return to2(
            this.http
                .get<ListOptions>(`${this.urlPrefix}/getlist/${docID}`)
                .toPromise()
        );
    }

    /**
     * Setting list values after loading.
     * @param listOptions All the options of the list returned from the server.
     */
    setValues(listOptions: ListOptions) {
        this.listname = listOptions.name;
        this.archive = listOptions.archive;
        // Without archive value, there is no reason to continue. Show error and short circuit here.
        if (this.archive == null) {
            this.permanentErrorMessage =
                "Loading the archive value failed. Please reload the page. If reloading the page does not fix the " +
                "problem, then please contact TIM's support and tell about this error.";
            return;
        }

        this.domain = listOptions.domain;

        // No use at the moment.
        // this.ownerEmail = "";

        this.notifyOwnerOnListChange =
            listOptions.notify_owners_on_list_change ?? false;

        this.listInfo = listOptions.list_info;
        this.listDescription = listOptions.list_description;

        this.emailAdminURL = listOptions.email_admin_url;

        // If some type of archiving exists for the list, provide a link to it.
        if (this.archive !== ArchiveType.NONE) {
            this.archiveURL = `/view/archives/${this.listname}`;
        }

        this.timUsersCanJoin = listOptions.tim_users_can_join;

        this.listSubjectPrefix = listOptions.list_subject_prefix;
        this.canUnsubscribe = listOptions.members_can_unsubscribe;
        this.defaultSendRight = listOptions.default_send_right;
        this.defaultDeliveryRight = listOptions.default_delivery_right;
        this.nonMemberMessagePass = listOptions.non_member_message_pass;
        this.onlyText = listOptions.only_text;
        this.allowAttachments = listOptions.allow_attachments;
        this.distribution = listOptions.distribution;
        this.allowAttachments = listOptions.allow_attachments;
        // Convert enum to boolean for tracking this on a checkbox.
        if (listOptions.default_reply_type != null) {
            this.listAnswerGuidance =
                listOptions.default_reply_type !== ReplyToListChanges.NOCHANGES;
            this.listReplyToChange = listOptions.default_reply_type;
        }
        this.removed = listOptions.removed;
        if (this.removed) {
            this.permanentErrorMessage =
                "This message list is not currently in use.";
        }
    }

    /**
     * Save the list options.
     */
    async saveOptions() {
        // Reset a failed saving message.
        this.saveFailMessage = "";
        // There is no reason to send this.removed back to server.
        const result = await this.saveOptionsCall({
            name: this.listname,
            domain: this.domain,
            list_info: this.listInfo,
            list_description: this.listDescription,
            only_text: this.onlyText,
            // If the checkbox for guiding messages to message list is checked, put ADDLIST enum, otherwise NOCHANGES
            // enum as default_reply_type.
            default_reply_type: this.listAnswerGuidance
                ? ReplyToListChanges.ADDLIST
                : ReplyToListChanges.NOCHANGES,
            notify_owners_on_list_change: this.notifyOwnerOnListChange,
            archive: this.archive,
            tim_users_can_join: this.timUsersCanJoin,
            list_subject_prefix: this.listSubjectPrefix,
            members_can_unsubscribe: this.canUnsubscribe,
            default_delivery_right: this.defaultDeliveryRight,
            default_send_right: this.defaultSendRight,
            non_member_message_pass: this.nonMemberMessagePass,
            distribution: this.distribution,
            allow_attachments: this.allowAttachments,
        });
        if (result.ok) {
            this.showTempSaveSuccess();
        } else {
            this.saveFailMessage = `Save failed with an error: ${result.result.error.error}`;
        }
    }

    /**
     * Helper for list saving to keep types in check.
     * @param options All the list options the user saves.
     */
    private saveOptionsCall(options: ListOptions) {
        return to2(this.http.post(`/messagelist/save`, {options}).toPromise());
    }

    /**
     * Save the lists members' state.
     */
    async saveMembers() {
        const resultSaveMembers = await this.saveMembersCall(this.membersList);
        // Give timed feedback to user.
        if (resultSaveMembers.ok) {
            this.memberSaveSuccessResponse = "Saving members succeeded!";
            window.setTimeout(
                () => (this.memberSaveSuccessResponse = ""),
                5 * 1000
            );
        } else {
            this.memberSaveFailResponse = "Saving members failed.";
            window.setTimeout(
                () => (this.memberSaveFailResponse = ""),
                5 * 1000
            );
        }
    }

    /**
     * Makes the actual REST call to save the state of list members'.
     * @param memberList A list of message list members with their information.
     */
    saveMembersCall(memberList: MemberInfo[]) {
        return to2(
            this.http
                .post(`${this.urlPrefix}/savemembers`, {
                    members: memberList,
                    listname: this.listname,
                })
                .toPromise()
        );
    }

    /**
     * Modify the recipient list for tim-message-send component. Adds the message list's email list as the recipient.
     */
    recipientList() {
        if (this.domain) {
            return `${this.listname}@${this.domain}`;
        } else {
            return "";
        }
    }

    /**
     * Shows a timed save success message.
     */
    showTempSaveSuccess() {
        this.saveSuccessMessage = "Save success!";
        window.setTimeout(() => (this.saveSuccessMessage = ""), 5 * 1000);
    }
}

@NgModule({
    declarations: [MessageListAdminComponent],
    exports: [MessageListAdminComponent],
    imports: [CommonModule, FormsModule, TimUtilityModule, TableFormModule],
})
export class NewMsgListModule {}
