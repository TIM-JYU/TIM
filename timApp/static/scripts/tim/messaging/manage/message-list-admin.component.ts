import {HttpClient} from "@angular/common/http";
import type {OnInit} from "@angular/core";
import {Component, Input} from "@angular/core";
import type {Moment} from "moment";
import moment from "moment";
import {to, to2, toPromise} from "tim/util/utils";
import type {
    Distribution,
    GroupAndMembers,
    ListOptions,
    MemberInfo,
    MessageVerificationType,
} from "tim/messaging/listOptionTypes";
import {
    archivePolicyNames,
    ArchiveType,
    ReplyToListChanges,
} from "tim/messaging/listOptionTypes";
import {documentglobals} from "tim/util/globals";
import {showInputDialog} from "tim/ui/showInputDialog";
import {InputDialogKind} from "tim/ui/input-dialog.kind";
import {$http} from "tim/util/ngimport";
import {Users} from "tim/user/userService";

@Component({
    selector: "tim-message-list-admin",
    template: `
        <tim-alert *ngIf="permanentErrorMessage" severity="danger">{{permanentErrorMessage}}</tim-alert>
        <ng-container *ngIf="list">
            <bootstrap-panel id="actions-panel" title="Common actions" i18n-title>
                <div class="actions">
                    <button class="timButton" (click)="openEmail()" [disabled]="recipients" i18n>
                        Send a message to the list
                    </button>
                    <a class="timButton" *ngIf="archiveURL" [href]="archiveURL" i18n>View archives</a>
                    <a class="timButton" *ngIf="exportArchiveURL" [href]="exportArchiveURL" (click)="exportArchiveClicked = true" i18n>Export archives</a>
                    <ng-container *ngIf="exportArchiveClicked">
                        <span><tim-loading></tim-loading> <ng-container i18n>Please wait...</ng-container></span>    
                    </ng-container>
                </div>
                <tim-message-send [(recipientList)]="recipients" [docId]="getDocId()"></tim-message-send>
            </bootstrap-panel>
            <form>
                <fieldset [disabled]="savingSettings">
                    <bootstrap-panel title="List options" i18n-title>
                        <tabset class="merged">
                            <tab heading="General" i18n-heading class="grid-tab tab-form">
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
                                <input type="text" class="form-control" name="list-description"
                                       id="list-description"
                                       [(ngModel)]="listDescription"/>
                                <label for="list-info" i18n>Long description</label>
                                <textarea name="list-info" class="form-control" id="list-info"
                                          [(ngModel)]="listInfo"></textarea>
                                <h4 i18n>General options</h4>
                                <div>
                                     <ul role="radiogroup" aria-labelledby="general-list-options-label">
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
                                            <label class="not-implemented" title="This option is not implemented yet"
                                                   i18n-title>
                                                <input type="checkbox" name="tim-users-can-join"
                                                       [(ngModel)]="timUsersCanJoin"
                                                       disabled>
                                                <ng-container i18n>TIM users can freely join this list
                                                </ng-container>
                                            </label>
                                            <ul>
                                                <li>
                                                    <label class="not-implemented"
                                                           title="This option is not implemented yet" i18n-title>
                                                        <input type="checkbox" name="default-send-right"
                                                               [(ngModel)]="defaultSendRight"
                                                               [disabled]="!timUsersCanJoin">
                                                        <ng-container i18n>Default send right for new members
                                                        </ng-container>
                                                    </label>
                                                </li>
                                                <li>
                                                    <label class="not-implemented"
                                                           title="This option is not implemented yet" i18n-title>
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
                                                <ng-container i18n>Members can unsubscribe from the list on their
                                                    own
                                                </ng-container>
                                            </label>
                                        </li>
                                        <li>
                                            <label>
                                                <input type="checkbox" name="non-members-can-send"
                                                       [(ngModel)]="nonMemberMessagePass">
                                                <ng-container i18n>Non-members can send messages to list
                                                </ng-container>
                                            </label>
                                        </li>
                                     </ul>
                                </div>
                            </tab>
                            <tab heading="Email" i18n-heading class="grid-tab tab-form">
                                <label for="list-subject-prefix" i18n>Subject prefix</label>
                                <input type="text" id="list-subject-prefix" name="list-subject-prefix"
                                       class="form-control"
                                       [disabled]="verificationType === 'forward'"
                                       [(ngModel)]="listSubjectPrefix">
                                <label for="list-verification-type" i18n>Message delivery verification</label>
                                <div>
                                    <select [(ngModel)]="verificationType" id="list-verification-type" name="list-verification-type" class="form-control">
                                        <option [ngValue]="'none'" i18n>Don't verify</option>
                                        <option [ngValue]="'forward'" i18n>Use sender's verification</option>
                                        <option [ngValue]="'munge_from'" i18n>Use list's verification</option>
                                    </select>
                                    <div class="info-text" [ngSwitch]="verificationType">
                                        <div *ngSwitchCase="'none'" i18n>
                                            <p>
                                                Message verification is disabled by the mailing list. 
                                                All message lists options can be used, but some of them can cause messages being marked as spam.
                                            </p>
                                            <p>
                                                <strong>This option is not recommended!</strong>
                                                Messages sent through the list will most likely be marked as spam by most mail providers.
                                            </p>
                                        </div>
                                        <div *ngSwitchCase="'forward'" i18n>
                                            <p>
                                                The message list forwards messages to the receiver without modifications.
                                                <strong>This verification method disables the following list options:</strong>
                                            </p>
                                            <ul>
                                                <li>Subject prefix</li>
                                                <li>Automatic message headers and footers are reset to empty</li>
                                            </ul>
                                            <p>
                                                This option relies on the verification information provided by the sender's mail provider. 
                                            </p>
                                        </div>
                                        <div *ngSwitchCase="'munge_from'" i18n>
                                            <p>
                                                The message list verifies all messages before sending them to the receiver.
                                                All message lists options can be used and the messages will pass spam filters of most mail providers.
                                            </p>
                                            <p>
                                                <strong>Note: this modifies the sender information!</strong>
                                                Messages sent through the list will have the list's address as the sender.
                                                However, the receiver will see the name of the sender and will be able to reply directly to them.
                                            </p>
                                        </div>
                                    </div>
                                </div>
                                <h4 i18n>List options</h4>
                                <div>
                                    <ul role="radiogroup" aria-labelledby="email-options-label">
                                        <li>
                                            <label>
                                                <input type="checkbox" name="only-text" [(ngModel)]="onlyText">
                                                <ng-container i18n>No HTML messages allowed
                                                </ng-container>
                                            </label>
                                        </li>
                                        <li>
                                            <label>
                                                <input type="checkbox" name="allow-attachments"
                                                       [(ngModel)]="allowAttachments">
                                                <ng-container i18n>Allow attachments</ng-container>
                                            </label>
                                        </li>
                                        <li>
                                            <label>
                                                <input type="checkbox" name="list-answer-guidance"
                                                       [(ngModel)]="listAnswerGuidance">
                                                <ng-container i18n>Redirect answers to message list</ng-container>
                                            </label>
                                        </li>
                                    </ul>
                                </div>
                                <h4 i18n>Advanced</h4>
                                <div *ngIf="emailAdminURL">
                                    <a [href]="emailAdminURL" i18n>Advanced email list settings (takes to
                                        Mailman)</a>
                                </div>
                            </tab>
                            <tab heading="Archiving" i18n-heading id="tab-archive" class="tab-form">
                                <div *ngIf="archiveOptions && archive">
                                    <h4 id="archive-policy-label" i18n>Archive access policy</h4>
                                    <!-- Disable radio buttons here, until the changing of archive policy levels is implemented -->
                                    <ul role="radiogroup"
                                        aria-labelledby="archive-policy-label">
                                        <li *ngFor="let option of archiveOptions">
                                            <label class="not-implemented" title="This option is not implemented yet"
                                                   i18n-title>
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
                            </tab>
                        </tabset>
                        <div class="save-button">
                            <div>
                                <button class="timButton" (click)="saveOptions()" i18n>Save options</button>
                                <tim-loading *ngIf="savingSettings"></tim-loading>
                            </div>
                            <tim-alert severity="success"
                                       *ngIf="saveSuccessMessage">{{saveSuccessMessage}}</tim-alert>
                            <tim-alert severity="danger" *ngIf="saveFailMessage">{{saveFailMessage}}</tim-alert>
                        </div>
                    </bootstrap-panel>
                </fieldset>
                <fieldset [disabled]="addingNewMember">
                    <bootstrap-panel id="members-tab" [titleTemplate]="addNewMemberHeader">
                        <ng-template #addNewMemberHeader>
                            <ng-container i18n>Add members</ng-container>
                            <a class="add-members-help"
                               title="Adding members help (in Finnish)" i18n-title
                               href="/view/tim/ohjeita/kayttoohjeet-viestilistoille#jäsenten-hallinta"><i
                                    class="glyphicon glyphicon-question-sign"></i></a>
                        </ng-template>
                        <div class="contents">
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
                            <div class="save-button">
                                <div>
                                    <button class="timButton" (click)="addNewListMember()" i18n>Add new members</button>
                                    <tim-loading *ngIf="addingNewMember"></tim-loading>
                                </div>
                                <tim-alert *ngIf="memberAddWarning"
                                           severity="warning">{{memberAddWarning}}</tim-alert>
                                <tim-alert *ngIf="memberAddSucceededResponse"
                                           severity="success">{{memberAddSucceededResponse}}</tim-alert>
                                <tim-alert *ngIf="memberAddFailedResponse"
                                           severity="danger">{{memberAddFailedResponse}}</tim-alert>
                            </div>
                        </div>
                    </bootstrap-panel>
                </fieldset>
                <fieldset [disabled]="editingMembers">
                    <bootstrap-panel id="current-members-tab" title="Current members" i18n-title>
                        <div class="contents">
                            <!-- TODO: Implement as TimTable -->
                            <table>
                                <thead>
                                <tr class="member-table-row">
                                    <th i18n>Name</th>
                                    <th i18n>Username</th>
                                    <th i18n>Email</th>
                                    <th i18n>Send right</th>
                                    <th i18n>Delivery right</th>
                                    <th i18n>Membership ended</th>
                                    <th i18n>Removed</th>
                                </tr>
                                </thead>
                                <tbody>
                                <tr class="member-table-row" *ngFor="let member of membersList">
                                    <td>{{member.name}}</td>
                                    <td>{{member.username}}</td>
                                    <td>{{member.email}}</td>
                                    <td>
                                        <input type="checkbox" [(ngModel)]="member.sendRight"
                                               name="member-send-right-{{member.email ?? member.username">
                                    </td>
                                    <td>
                                        <input type="checkbox" [(ngModel)]="member.deliveryRight"
                                               name="member-delivery-right-{{member.email || member.username}}">
                                    </td>
                                    <td>{{member.removedDisplay}}</td>
                                    <td><input type="checkbox" (click)="membershipChange(member)"
                                               [ngModel]="!!member.removed"
                                               name="removed-{{member.email || member.username}}"/></td>
                                </tr>
                                </tbody>
                            </table>

                            <ng-container *ngIf="hasGroups">
                                <label for="user-group-view-select" i18n>View members of a group</label>
                                <select id="user-group-view-select" class="form-control" [(ngModel)]="currentGroup"
                                        name="user-group-view-select"
                                        (change)="setGroupMembers()">
                                    <option *ngFor="let memberGroup of memberGroups">{{memberGroup}}</option>
                                </select>
                                <table *ngIf="currentGroup">
                                    <thead>
                                    <tr class="member-table-row">
                                        <th i18n>Name</th>
                                        <th i18n>Username</th>
                                        <th i18n>Email</th>
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

                            <div class="save-button">
                                <div>
                                    <button class="timButton" (click)="saveMembers()" i18n>Save members</button>
                                    <tim-loading *ngIf="editingMembers"></tim-loading>
                                </div>
                                <tim-alert *ngIf="memberSaveSuccessResponse"
                                           severity="success">{{memberSaveSuccessResponse}}</tim-alert>
                                <tim-alert *ngIf="memberSaveFailResponse"
                                           severity="danger">{{memberAddFailedResponse}}</tim-alert>
                            </div>
                        </div>
                    </bootstrap-panel>
                </fieldset>
            </form>
            <bootstrap-panel title="Dangerous actions" i18n-title severity="danger">
                <button class="btn btn-danger" (click)="deleteList()" i18n>Delete List</button>
            </bootstrap-panel>
        </ng-container>
    `,
    styleUrls: ["message-list-admin.component.scss"],
})
export class MessageListAdminComponent implements OnInit {
    @Input() list?: string;

    listname: string = "";

    archive?: ArchiveType;
    exportArchiveClicked = false;

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
    exportArchiveURL?: string;

    canUnsubscribe?: boolean;
    defaultSendRight?: boolean;
    defaultDeliveryRight?: boolean;
    listSubjectPrefix?: string;
    nonMemberMessagePass?: boolean;
    onlyText?: boolean;
    allowAttachments?: boolean;
    distribution?: Distribution;
    listReplyToChange?: ReplyToListChanges;
    verificationType?: MessageVerificationType;
    listAnswerGuidance?: boolean; // Track above enum value in a checkbox.

    // Flags for new members' rights on the list.
    newMemberSendRight: boolean = true;
    newMemberDeliveryRight: boolean = true;

    // Response strings for saving list options.
    savingSettings = false;
    saveSuccessMessage?: string;
    saveFailMessage?: string;

    // Response strings used in giving feedback to the user on adding new members to the message list.
    memberAddSucceededResponse?: string;
    memberAddWarning?: string;
    memberAddFailedResponse?: string;
    memberAddResultTimeout?: number;

    // Response strings for saving members' state.
    memberSaveSuccessResponse?: string;
    memberSaveFailResponse?: string;

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

    addingNewMember = false;
    editingMembers = false;

    constructor(private http: HttpClient) {}

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
        if (!this.list) {
            this.permanentErrorMessage = $localize`No list specified`;
            return;
        }

        if (Users.isLoggedIn()) {
            // Get domains.
            await this.getDomains();

            // Load message list options.
            const result1 = await this.loadValues();

            if (result1.ok) {
                this.setValues(result1.result);
            } else {
                this.permanentErrorMessage = $localize`Loading list options failed: ${result1.result.error.error}`;
                // Loading options failed. Short circuit here, no reason to continue.
                return;
            }

            await this.updateMemberList();
        }
    }

    /**
     * Opens the email sending view by adding the list's address to the string of recipients.
     */
    openEmail() {
        this.recipients = this.listAddress();
    }

    /**
     * Add new members to message list.
     */
    async addNewListMember() {
        if (this.memberAddResultTimeout) {
            window.clearTimeout(this.memberAddResultTimeout);
            this.memberAddResultTimeout = undefined;
        }
        this.memberAddSucceededResponse = undefined;
        this.memberAddFailedResponse = undefined;
        const memberCandidates = this.parseMembers();
        if (memberCandidates.length == 0) {
            return;
        }
        this.addingNewMember = true;
        const addWarningTimeout = window.setTimeout(() => {
            this.memberAddWarning = $localize`Adding large groups might take longer than usual. Please wait.`;
        }, 2000);
        const result = await toPromise(
            this.http.post(`${this.urlPrefix}/addmember`, {
                member_candidates: memberCandidates,
                msg_list: this.listname,
                send_right: this.newMemberSendRight,
                delivery_right: this.newMemberDeliveryRight,
            })
        );
        this.addingNewMember = false;
        this.memberAddWarning = undefined;
        window.clearTimeout(addWarningTimeout);

        this.memberAddResultTimeout = window.setTimeout(() => {
            this.memberAddSucceededResponse = undefined;
            this.memberAddFailedResponse = undefined;
        }, 5 * 1000);
        if (result.ok) {
            // Empty the text field.
            this.membersTextField = undefined;
            this.memberAddSucceededResponse = $localize`New members added.`;
            await this.updateMemberList();
        } else {
            this.memberAddFailedResponse = $localize`Adding new members failed: ${result.result.error.error}`;
        }
    }

    /**
     * Get all list members.
     */
    async getListMembers() {
        return toPromise(
            this.http.get<MemberInfo[]>(
                `${this.urlPrefix}/getmembers/${this.listname}`
            )
        );
    }

    /**
     * Helper for list deletion.
     */
    deleteList() {
        // Ask confirmation from the user.
        void to2(
            showInputDialog({
                title: $localize`Delete list ${this.listname}`,
                text: $localize`Do you really want to delete list ${this.listname}? Members will not be able to receive or send messages through the list anymore.`,
                okText: $localize`Delete`,
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
                        this.permanentErrorMessage = $localize`Deleting the list failed.`;
                        return {
                            ok: false,
                            result: result.result.data.error,
                        } as const;
                    }
                },
            })
        );
    }

    /**
     * Get values for message list's options.
     */
    async loadValues() {
        return toPromise(
            this.http.get<ListOptions>(
                `${this.urlPrefix}/getlist/${this.list ?? ""}`
            )
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
            this.permanentErrorMessage = $localize`Loading archive information failed. Please reload the page. If reloading the page does not fix the problem, please contact TIM support.`;
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
            this.exportArchiveURL = `/messagelist/archive/export/${this.listname}`;
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
        this.verificationType = listOptions.verification_type;
        console.log(this.verificationType);
        this.removed = listOptions.removed;
        if (this.removed) {
            this.permanentErrorMessage = $localize`This message list has been deleted and thus is not in use.`;
        }
    }

    /**
     * Save the list options.
     */
    async saveOptions() {
        // Reset a failed saving message.
        this.saveFailMessage = undefined;
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
            verification_type: this.verificationType,
        });
        this.savingSettings = false;
        if (result.ok) {
            this.showTempSaveSuccess();
        } else {
            this.saveFailMessage = $localize`Saving failed with an error: ${result.result.error.error}`;
        }
    }

    /**
     * Save the lists members' state.
     */
    async saveMembers() {
        this.editingMembers = true;
        const tempMembersList = this.membersList;
        // Get rid of removedDisplay property for the members being send to server, as the server does not need it for
        // anything.
        for (const tempMember of tempMembersList) {
            delete tempMember.removedDisplay;
        }
        const resultSaveMembers = await this.saveMembersCall(tempMembersList);
        this.editingMembers = false;
        // Give timed feedback to user.
        if (resultSaveMembers.ok) {
            this.memberSaveSuccessResponse = $localize`Member information updated!`;
            window.setTimeout(
                () => (this.memberSaveSuccessResponse = undefined),
                5 * 1000
            );
        } else {
            this.memberSaveFailResponse = $localize`Failed to save member information.`;
            window.setTimeout(
                () => (this.memberSaveFailResponse = undefined),
                5 * 1000
            );
        }
    }

    /**
     * Makes the actual REST call to save the state of list members'.
     * @param memberList A list of message list members with their information.
     */
    saveMembersCall(memberList: MemberInfo[]) {
        return toPromise(
            this.http.post(`${this.urlPrefix}/savemembers`, {
                members: memberList,
                listname: this.listname,
            })
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
        this.saveSuccessMessage = $localize`Saved!`;
        window.setTimeout(
            () => (this.saveSuccessMessage = undefined),
            5 * 1000
        );
    }

    /**
     * Call for members of a user group.
     */
    getGroupMembersCall() {
        return toPromise(
            this.http.get<GroupAndMembers[]>(
                `${this.urlPrefix}/getgroupmembers/${this.listname}`
            )
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
            this.permanentErrorMessage = $localize`Loading group information failed. Please refresh the browser. If the problem persists, please contact TIM support.`;
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
            if (groupAndMembers) {
                this.groupMembers = groupAndMembers.members;
            }
        }
    }

    private async updateMemberList() {
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
            this.permanentErrorMessage = $localize`Loading list's members failed: ${result2.result.error.error}`;
        }

        await this.getGroupMembers();
    }

    /**
     * Get domains ccondigured for email list use.
     */
    private async getDomains() {
        const result = await toPromise(
            this.http.get<string[]>(`${this.urlPrefix}/domains`)
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
     * Helper for list saving to keep types in check.
     * @param options All the list options the user saves.
     */
    private saveOptionsCall(options: ListOptions) {
        return toPromise(this.http.post(`/messagelist/save`, {options}));
    }
}
