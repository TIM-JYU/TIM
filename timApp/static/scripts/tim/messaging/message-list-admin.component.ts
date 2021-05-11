import {HttpClient} from "@angular/common/http";
import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {to2} from "tim/util/utils";
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
import {Users} from "../user/userService";

@Component({
    selector: "tim-message-list-admin",
    template: `
        <form class="form-horizontal">
            <h1>Message list management</h1>
            <div class="form-group">
                <label for="list-name" class="list-name control-label col-sm-3">List name: </label>
                <div class="col-sm-9">
                    <div class="input-group">
                        <input type="text" class="form-control" name="list-name" id="list-name" disabled
                               [(ngModel)]="listname"/>
                        <div class="input-group-addon">@</div>
                        <select id="domain-select" class="form-control" name="domain-select" [(ngModel)]="domain">
                            <option [disabled]="domains.length < 2" *ngFor="let domain of domains">{{domain}}</option>
                        </select>
                    </div>
                </div>
            </div>

            <div class="form-group" *ngIf="domain">
                <label for="list-description" class="short-description control-label col-sm-3">List address: </label>
                <div class="col-sm-9">
                    <input type="text" class="form-control" name="list-description" id="list-description"
                           value="{{listname}}@{{domain}}" disabled/>
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
                <textarea name="list-info" class="list-info form-control"
                          [(ngModel)]="listInfo">A more detailed information thingy for this list.</textarea>
                </div>
            </div>
            <div>
            </div>
            <div>
                <p class="list-archive-policy-header">List archive policy:</p>
                <ul style="list-style-type: none">
                    <li *ngFor="let option of archiveOptions">
                        <input
                                name="items-radio"
                                type="radio"
                                id="archive-{{option.archiveType}}"
                                [value]="option.archiveType"
                                [(ngModel)]="archive"
                        />
                        <label for="archive-{{option.archiveType}}">{{option.policyName}}</label>
                    </li>
                </ul>
            </div>
            <h5>List options</h5>
            <div>
                <input type="text" name="list-subject-prefix" [(ngModel)]="listSubjectPrefix">
                <label for="list-subject-prefix">List subject prefix.</label>
            </div>
            <div>
                <input type="checkbox" name="notify-owner-on-list-change" id="notify-owner-on-list-change"
                       [(ngModel)]="notifyOwnerOnListChange"/>
                <label for="notify-owner-on-list-change">Notify owners on list changes (e.g. user subscribes).</label>
            </div>
            <div>
                <input type="checkbox" name="tim-users-can-join" [(ngModel)]="timUsersCanJoin">
                <label for="tim-users-can-join">TIM users can freely join this list.</label>
            </div>
            <div>
                <input type="checkbox" name="can-user-unsubscribe" [(ngModel)]="canUnsubscribe">
                <label for="can-user-unsubscribe">Members can unsubscribe from the list on their own.</label>
            </div>
            <div>
                <input type="checkbox" name="only-text" [(ngModel)]="onlyText">
                <label for="only-text">No HTML messages allowed on the list.</label>
            </div>
            <div>
                <input type="checkbox" name="non-members-can-send" [(ngModel)]="nonMemberMessagePass">
                <label for="non-members-can-send">Non members can send messages to list.</label>
            </div>
            <div>
                <input type="checkbox" name="allow-attachments" [(ngModel)]="allowAttachments">
                <label for="allow-attachments">Allow attachments on the list.</label>
            </div>
            <div *ngIf="archiveURL">
                <a [href]="archiveURL">List's archive</a>
            </div>
            <div *ngIf="emailAdminURL">
                <a [href]="emailAdminURL">Advanced email list settings</a>
            </div>
            <div>
                <button class="btn btn-default" (click)="save()">Save changes</button>
            </div>
            <div style="padding-top: 5em">
                <label for="add-multiple-members">Add members</label> <br/>
                <textarea id="add-multiple-members" name="add-multiple-members"
                          [(ngModel)]="membersTextField"></textarea>
                <div>
                    <div>
                        <input type="checkbox" name="new-member-send-right" [(ngModel)]="newMemberSendRight">
                        <label for="new-member-send-right">New member's send right.</label>
                    </div>
                    <div>
                        <input type="checkbox" name="new-member-delivery-right" [(ngModel)]="newMemberDeliveryRight">
                        <label for="new-member-delivery-right">New member's delivery right.</label>
                    </div>
                </div>
                <button (click)="addNewListMember()">Add new members</button>
            </div>
            <div>
                <table>
                    <caption>List members</caption>
                    <thead>
                    <tr>
                        <th>Name</th>
                        <th>Email</th>
                        <th>Send right</th>
                        <th>Delivery right</th>
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
                    </tr>
                    </tbody>
                </table>
            </div>
            <div>
                <h2>List deletion</h2>
                <button class="btn btn-default" (click)="deleteList()">Delete List</button>
            </div>
        </form>
    `,
    styleUrls: ["message-list-admin.component.scss"],
})
export class MessageListAdminComponent implements OnInit {
    listname: string = "";

    // List has a private members only archive by default.
    archive: ArchiveType = ArchiveType.GROUPONLY;

    domain?: string;
    domains: string[] = [];

    membersTextField?: string;
    membersList: MemberInfo[] = [];

    urlPrefix: string = "/messagelist";

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
    distribution?: Distribution;
    listReplyToChange?: ReplyToListChanges;

    newMemberSendRight: boolean = true;
    newMemberDeliveryRight: boolean = true;

    ngOnInit(): void {
        if (Users.isLoggedIn()) {
            // Get domains.
            void this.getDomains();

            // Load message list options.
            const docId = documentglobals().curr_item.id;
            void this.loadValues(docId);

            // Load message list's members.
            if (!this.listname) {
                // getListmembers() might launch it's HTTP call before loadValues() finishes with setting listname,
                // so if this happens we schedule the call for list members. The time is a so called sleeve constant,
                // and it is not based on anything other than it seems to work on small scale testing.
                window.setTimeout(() => this.getListMembers(), 2 * 1000);
            } else {
                void this.getListMembers();
            }
        }
    }

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
            console.error(result.result.error.error);
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

    async addNewListMember() {
        const memberCandidates = this.parseMembers();
        if (memberCandidates.length == 0) {
            return;
        }
        const result = await to2(
            this.http
                .post(`${this.urlPrefix}/addmember`, {
                    memberCandidates: memberCandidates,
                    msgList: this.listname,
                    sendRight: this.newMemberSendRight,
                    deliveryRight: this.newMemberDeliveryRight,
                })
                .toPromise()
        );
        if (result.ok) {
            // TODO: Sending succeeded.
            // console.log("Sending members succeeded.");
            this.membersTextField = undefined; // Empty the text field.
        } else {
            // TODO: Sending failed.
            console.error(result.result.error.error);
        }
    }

    /**
     * Get all list members.
     */
    async getListMembers() {
        const result = await to2(
            this.http
                .get<MemberInfo[]>(
                    `${this.urlPrefix}/getmembers/${this.listname}`
                )
                /** .map(response => {
                const array = JSON.parse(response.json()) as any[];
                const memberinfos = array.map(data => new MemberInfo(data));
                return memberinfos;
            )
    }*/
                .toPromise()
        );
        if (result.ok) {
            // console.log(result.result);
            this.membersList = result.result;
        } else {
            console.error(result.result.error.error);
        }
    }

    /**
     * Helper for list deletion.
     */
    async deleteList() {
        // TODO: Confirm with user if they are really sure they want to delete the entire message list. Technically it
        //  could be reversible, but such an hassle that not letting it happen by a single button press should be
        //  allowed.
        const result = await to2(
            this.http
                .delete(`/messagelist/deletelist`, {
                    params: {
                        listname: this.listname,
                        domain: this.domain ? this.domain : "",
                    },
                })
                .toPromise()
        );
        if (result.ok) {
            // TODO: Inform the user deletion was succesfull.
            console.log(result.result);
        } else {
            // TODO: Inform the user deletion was not succesfull.
            console.error(result.result);
        }
    }

    /**
     * Load values for message list.
     * @param docID List is defined by it's management document, so we get list's options and members with it.
     */
    async loadValues(docID: number) {
        const result = await to2(
            this.http
                .get<ListOptions>(`${this.urlPrefix}/getlist/${docID}`)
                .toPromise()
        );
        if (result.ok) {
            this.setValues(result.result);
        } else {
            console.error(result.result.error.error);
            // TODO: Check what went wrong.
        }
    }

    /**
     * Helper for setting list values after loading.
     * @param listOptions
     */
    setValues(listOptions: ListOptions) {
        this.listname = listOptions.name;
        this.archive = listOptions.archive;

        this.domain = listOptions.domain;

        this.ownerEmail = "";

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
        this.listReplyToChange = listOptions.default_reply_type;
    }

    /**
     * Function to initiate, when the user saves the list options.
     */
    async save() {
        const result = await this.saveListOptions({
            name: this.listname,
            domain: this.domain,
            list_info: this.listInfo,
            list_description: this.listDescription,
            only_text: this.onlyText,
            default_reply_type: this.listReplyToChange, // TODO: Option to ask the user.
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
            // console.log("save succee");
        } else {
            console.error("save fail");
        }

        const resultSaveMembers = await this.saveMembersCall(this.membersList);

        if (resultSaveMembers.ok) {
            // VIESTIM: Saving members' state succeeded.
            // console.log("Saving members succeeded.");
        } else {
            // VIESTIM: Saving members' state failed.
            console.error("Saving members failed.");
        }
    }

    /**
     * Helper for list saving to keep types in check.
     * @param options All the list options the user saves.
     */
    private saveListOptions(options: ListOptions) {
        return to2(this.http.post(`/messagelist/save`, {options}).toPromise());
    }

    private saveMembersCall(memberList: MemberInfo[]) {
        return to2(
            this.http
                .post(`${this.urlPrefix}/savemembers`, {
                    members: memberList,
                    listname: this.listname,
                })
                .toPromise()
        );
    }
}

@NgModule({
    declarations: [MessageListAdminComponent],
    exports: [MessageListAdminComponent],
    imports: [CommonModule, FormsModule],
})
export class NewMsgListModule {}
