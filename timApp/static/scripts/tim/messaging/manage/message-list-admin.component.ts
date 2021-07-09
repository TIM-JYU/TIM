import {HttpClient} from "@angular/common/http";
import {Component, OnInit} from "@angular/core";
import moment, {Moment} from "moment";
import {to, to2} from "../../util/utils";
import {
    archivePolicyNames,
    ArchiveType,
    Distribution,
    GroupAndMembers,
    ListOptions,
    MemberInfo,
    ReplyToListChanges,
} from "../listOptionTypes";
import {documentglobals} from "../../util/globals";
import {showInputDialog} from "../../ui/showInputDialog";
import {InputDialogKind} from "../../ui/input-dialog.kind";
import {$http} from "../../util/ngimport";
import {Users} from "../../user/userService";

@Component({
    selector: "tim-message-list-admin",
    template: `
        <tim-alert *ngIf="permanentErrorMessage" severity="danger">{{permanentErrorMessage}}</tim-alert>
        <div id="actions-panel" class="panel panel-default">
            <div class="panel-heading" i18n>Common actions</div>
            <div class="panel-body">
                <div class="actions">
                    <button class="timButton" (click)="openEmail()" [disabled]="recipients">Send message to list
                    </button>
                    <a class="timButton" *ngIf="archiveURL" [href]="archiveURL">View archives</a>
                </div>
                <tim-message-send [(recipientList)]="recipients" [docId]="getDocId()"></tim-message-send>
            </div>
        </div>
        <form>
            <fieldset [disabled]="savingSettings">
                <div class="panel panel-default">
                    <div class="panel-heading" i18n>List options</div>
                    <div class="panel-body">
                        <tabset class="merged">
                            <tab heading="General" class="grid-tab">
                                <label for="list-name" i18n>List name</label>
                                <input type="text" name="list-name" id="list-name" class="form-control"
                                       [value]="listname"
                                       disabled>
                                <ng-container *ngIf="domain">
                                    <label for="list-email-address" i18n>Email address</label>
                                    <input type="text" class="form-control" name="list-email-address"
                                           id="list-email-address" [ngModel]="listAddress()" disabled/>
                                </ng-container>
                                <label for="list-description" i18n>Short description</label>
                                <input type="text" class="form-control" name="list-description" id="list-description"
                                       [(ngModel)]="listDescription"/>
                                <label for="list-info" i18n>Long description</label>
                                <textarea name="list-info" class="form-control" id="list-info"
                                          [(ngModel)]="listInfo"></textarea>
                            </tab>
                            <tab heading="Email" class="grid-tab">
                                <label for="list-subject-prefix" i18n>Subject prefix</label>
                                <input type="text" id="list-subject-prefix" name="list-subject-prefix"
                                       class="form-control"
                                       [(ngModel)]="listSubjectPrefix">
                                <h4>Advanced</h4>
                                <div *ngIf="emailAdminURL">
                                    <a [href]="emailAdminURL">Advanced email list settings (takes to Mailman)</a>
                                </div>
                            </tab>
                            <tab heading="Archiving" id="tab-archive">
                                <div *ngIf="archiveOptions && archive">
                                    <h4 id="archive-policy-label">Archive policy</h4>
                                    <!-- Hide radio buttons here, until the changing of archive policy levels is implemented -->
                                    <ul role="radiogroup"
                                        aria-labelledby="archive-policy-label">
                                        <li *ngFor="let option of archiveOptions">
                                            <label>
                                                <input
                                                        name="items-radio"
                                                        type="radio"
                                                        [value]="option.archiveType"
                                                        [(ngModel)]="archive"
                                                        disabled
                                                />{{option.policyName}}</label>
                                        </li>
                                    </ul>
                                </div>
                                <div>
                                    <h4 id="archive-options-label">Archive options</h4>
                                    <ul role="radiogroup" aria-labelledby="archive-options-label">
                                        <li>
                                            <label>
                                                <input type="checkbox" name="notify-owner-on-list-change"
                                                       [(ngModel)]="notifyOwnerOnListChange"/>
                                                <ng-container i18n>Notify owners on list changes (e.g. user
                                                    subscribes)
                                                </ng-container>
                                            </label>
                                        </li>
                                        <li>
                                            <label>
                                                <input type="checkbox" name="tim-users-can-join"
                                                       [(ngModel)]="timUsersCanJoin"
                                                       disabled>
                                                <ng-container i18n>TIM users can freely join this list</ng-container>
                                            </label>
                                            <ul>
                                                <li>
                                                    <label>
                                                        <input type="checkbox" name="default-send-right"
                                                               [(ngModel)]="defaultSendRight"
                                                               [disabled]="!timUsersCanJoin">
                                                        <ng-container i18n>Default send right for new members
                                                        </ng-container>
                                                    </label>
                                                </li>
                                                <li>
                                                    <label>
                                                        <input type="checkbox" name="default-delivery-right"
                                                               [(ngModel)]="defaultDeliveryRight"
                                                               [disabled]="!timUsersCanJoin">
                                                        <ng-container i18n>Default delivery right for new members
                                                        </ng-container>
                                                    </label>
                                                </li>
                                            </ul>
                                        </li>
                                        <li>
                                            <label>
                                                <input type="checkbox" name="can-user-unsubscribe"
                                                       [(ngModel)]="canUnsubscribe">
                                                <ng-container i18n>Members can unsubscribe from the list on their own
                                                </ng-container>
                                            </label>
                                        </li>
                                        <li>
                                            <label>
                                                <input type="checkbox" name="non-members-can-send"
                                                       [(ngModel)]="nonMemberMessagePass">
                                                <ng-container i18n>Non members can send messages to list</ng-container>
                                            </label>
                                        </li>
                                        <li>
                                            <label>
                                                <input type="checkbox" name="only-text" [(ngModel)]="onlyText">
                                                <ng-container i18n>No HTML messages allowed on the list</ng-container>
                                            </label>
                                        </li>
                                        <li>
                                            <label>
                                                <input type="checkbox" name="allow-attachments"
                                                       [(ngModel)]="allowAttachments">
                                                <ng-container i18n>Allow attachments on the list</ng-container>
                                            </label>
                                        </li>
                                        <li>
                                            <label>
                                                <input type="checkbox" name="list-answer-guidance"
                                                       [(ngModel)]="listAnswerGuidance">
                                                <ng-container i18n>Direct answers to message list</ng-container>
                                            </label>
                                        </li>
                                    </ul>
                                </div>
                            </tab>
                        </tabset>
                        <div class="save-button">
                            <div>
                                <button class="timButton" (click)="saveOptions()">Save options</button>
                                <tim-loading *ngIf="savingSettings"></tim-loading>
                            </div>
                            <tim-alert severity="success" *ngIf="saveSuccessMessage">{{saveSuccessMessage}}</tim-alert>
                            <tim-alert severity="danger" *ngIf="saveFailMessage">{{saveFailMessage}}</tim-alert>
                        </div>
                    </div>
                </div>
            </fieldset>
        </form>
        <div id="members-tab" class="panel panel-default">
            <div class="panel-heading" i18n>Add members <a class="add-members-help"
                                                           title="Adding members help (in Finnish)" i18n-title
                                                           href="/view/tim/ohjeita/kayttoohjeet-viestilistoille#jäsenten-hallinta"><i
                    class="glyphicon glyphicon-question-sign"></i></a></div>
            <div class="panel-body">
                <textarea id="add-multiple-members" name="add-multiple-members" class="form-control"
                          [(ngModel)]="membersTextField"></textarea>
                <label class="font-weight-normal"><input type="checkbox" name="new-member-send-right"
                                                         [(ngModel)]="newMemberSendRight">
                    <ng-container i18n>Members can send messages to the list</ng-container>
                </label>
                <label class="font-weight-normal"><input type="checkbox" name="new-member-delivery-right"
                                                         [(ngModel)]="newMemberDeliveryRight">
                    <ng-container i18n>Members can receive messages from the list</ng-container>
                </label>
                <div>
                    <button class="timButton" (click)="addNewListMember()">Add new members</button>
                </div>
                <tim-alert *ngIf="memberAddSucceededResponse"
                           severity="success">{{memberAddSucceededResponse}}</tim-alert>
                <tim-alert *ngIf="memberAddFailedResponse"
                           severity="danger">{{memberAddFailedResponse}}</tim-alert>
            </div>
        </div>
        <div id="current-members-tab" class="panel panel-default">
            <div class="panel-heading" i18n>Current members</div>
            <div class="panel-body">
                <table>
                    <thead>
                    <tr class="member-table-row">
                        <th>Name</th>
                        <th>Username</th>
                        <th>Email</th>
                        <th>Send right</th>
                        <th>Delivery right</th>
                        <th>Membership ended</th>
                        <th>Removed</th>
                    </tr>
                    </thead>
                    <tbody>
                    <tr class="member-table-row" *ngFor="let member of membersList">
                        <td>{{member.name}}</td>
                        <td>{{member.username}}</td>
                        <td>{{member.email}}</td>
                        <td>
                            <input type="checkbox" [(ngModel)]="member.sendRight"
                                   name="member-send-right-{{member.email}}">
                        </td>
                        <td>
                            <input type="checkbox" [(ngModel)]="member.deliveryRight"
                                   name="member-delivery-right-{{member.email}}">
                        </td>
                        <td>{{member.removedDisplay}}</td>
                        <td><input type="checkbox" (click)="membershipChange(member)" [ngModel]="!!member.removed"
                                   name="removed-{{member.email}}"/></td>
                    </tr>
                    </tbody>
                </table>

                <ng-container *ngIf="hasGroups">
                    <label for="user-group-view-select">View members of a group</label>
                    <select id="user-group-view-select" class="form-control" [(ngModel)]="currentGroup" name="user-group-view-select"
                            (change)="setGroupMembers()">
                        <option *ngFor="let memberGroup of memberGroups">{{memberGroup}}</option>
                    </select>
                    <table *ngIf="currentGroup">
                        <thead>
                        <tr class="member-table-row">
                            <th>Name</th>
                            <th>Username</th>
                            <th>Email</th>
                        </tr>
                        </thead>
                        <tbody>
                        <tr class="member-table-row" *ngFor="let gMember of groupMembers">
                            <td>{{gMember.name}}</td>
                            <td>{{gMember.username}}</td>
                            <td>{{gMember.email}}</td>
                        </tr>
                        </tbody>
                    </table>
                </ng-container>

                <div>
                    <button class="timButton" (click)="saveMembers()">Save members</button>
                </div>
                <tim-alert *ngIf="memberSaveSuccessResponse"
                           severity="success">{{memberSaveSuccessResponse}}</tim-alert>
                <tim-alert *ngIf="memberSaveFailResponse"
                           severity="danger">{{memberAddFailedResponse}}</tim-alert>
            </div>
        </div>
        <div class="panel panel-danger">
            <div class="panel-heading" i18n>Dangerous actions</div>
            <div class="panel-body">
                <button class="btn btn-danger" (click)="deleteList()" i18n>Delete List</button>
            </div>
        </div>
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

    archiveOptions = archivePolicyNames;

    notifyOwnerOnListChange?: boolean;
    timUsersCanJoin?: boolean;

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
    distribution?: Distribution;
    listReplyToChange?: ReplyToListChanges;
    listAnswerGuidance?: boolean; // Track above enum value in a checkbox.

    // Flags for new members' rights on the list.
    newMemberSendRight: boolean = true;
    newMemberDeliveryRight: boolean = true;

    // Response strings for saving list options.
    savingSettings = false;
    saveSuccessMessage: string = "";
    saveFailMessage: string = "";

    // Response strings used in giving feedback to the user on adding new members to the message list.
    memberAddSucceededResponse: string = "";
    memberAddFailedResponse: string = "";

    // Response strings for saving members' state.
    memberSaveSuccessResponse: string = "";
    memberSaveFailResponse: string = "";

    // Permanent error messages that cannot be recovered from, e.g. loading failed and reload is needed.
    permanentErrorMessage?: string;

    // For using tim-message-send component.
    recipients = "";

    // Timestamp if this message list has been removed.
    removed?: Moment;

    // If groups are members part of list, these hold information about the members of said groups.
    groupMembers?: MemberInfo[];
    // Flag if the UI shows the additional group member area.
    hasGroups: boolean = false;
    // Names of groups to set as selectable.
    memberGroups?: string[];
    // List members which are groups and the members of those groups.
    groupsAndMembers?: GroupAndMembers[];
    // Current group to view on the UI. If undefined, shows no group on the UI.
    currentGroup?: string;

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
                // TODO order members by name.
                this.membersList = result2.result;
                // Set the UI value for removed attribute.
                for (const member of this.membersList) {
                    if (member.removed) {
                        member.removedDisplay = moment(member.removed).format(
                            "DD.MM.YYYY hh:mm"
                        );
                    }
                }
            } else {
                this.permanentErrorMessage = `Loading list's members failed: ${result2.result.error.error}`;
            }

            await this.getGroupMembers();
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
     * @param docID List is defined by its management document, so we get list's options and members with it.
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

        this.notifyOwnerOnListChange = listOptions.notify_owners_on_list_change;

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
        this.savingSettings = true;
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
            allow_attachments: this.allowAttachments,
        });
        this.savingSettings = false;
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
        const tempMembersList = this.membersList;
        // Get rid of removedDisplay property for the members being send to server, as the server does not need it for
        // anything.
        for (const tempMember of tempMembersList) {
            delete tempMember.removedDisplay;
        }
        const resultSaveMembers = await this.saveMembersCall(tempMembersList);
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

    /**
     * Call for members of a user group.
     */
    getGroupMembersCall() {
        return to2(
            this.http
                .get<GroupAndMembers[]>(
                    `${this.urlPrefix}/getgroupmembers/${this.listname}`
                )
                .toPromise()
        );
    }

    /**
     * Get the members of a user group.
     */
    async getGroupMembers() {
        const result = await this.getGroupMembersCall();
        if (result.ok) {
            // TODO Order members by name.
            this.groupsAndMembers = result.result;
            // If there are no groups on this list, then we short circuit here.
            if (this.groupsAndMembers.length === 0) {
                return;
            }
            this.memberGroups = [];
            // Set the groups members are to show on UI.
            this.hasGroups = true;
            // Set the names of all groups to select element.
            for (const gm of this.groupsAndMembers) {
                this.memberGroups.push(gm.groupName);
            }
        } else {
            this.permanentErrorMessage =
                "Loading members of groups failed. Please refresh the browser. If the problem " +
                "persists, please contact TIM's support.";
        }
    }

    /**
     * Helper for setting members of a group to a table.
     */
    setGroupMembers() {
        if (this.currentGroup) {
            // Find the currently selected group where we want to see members.
            const groupAndMembers = this.groupsAndMembers?.find(
                (g) => g.groupName === this.currentGroup
            );
            if (groupAndMembers) this.groupMembers = groupAndMembers.members;
        }
    }
}
