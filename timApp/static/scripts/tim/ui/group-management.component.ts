import {HttpClient} from "@angular/common/http";
import type {AfterViewInit, OnInit} from "@angular/core";
import {Component, Input} from "@angular/core";
// import moment from "moment";
// import {forEach} from "angular";
import type {IGroup, IUser} from "tim/user/IUser";
import {ADMIN_GROUPNAME} from "tim/user/IUser";
import {showUserGroupDialog} from "tim/user/showUserGroupDialog";
import {documentglobals} from "tim/util/globals";
import {isAdmin, Users} from "tim/user/userService";
import type {Require} from "tim/util/utils";
import {to2, toPromise} from "tim/util/utils";
import type {IGroupManagementSettings} from "tim/document/IDocSettings";
import type {UserGroupDialogParams} from "tim/user/user-group-dialog.component";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {TabDirective} from "ngx-bootstrap/tabs";
import {forEach} from "angular";

export interface GroupMember extends IUser {
    id: number;
    name: string;
    email: string;
    real_name: string;
    student_id: string;
}

export interface Group extends IGroup {
    // Group's doc path, eg. 'test/group01'
    path?: string;
    events?: GroupEvent[];
}

/**
 * Represents an event (exam, meeting, etc.) that is scheduled for a specific group.
 */
export interface GroupEvent {
    event: string;
    timeslot: string;
}

/*
Required document setting:
```
additional_angular_modules:
  - timGroupManagementModule
```

Adding the component to a document:
```
#- {allowangular="true"}
<tim-group-management-console></tim-group-management-console>
```
 */

@Component({
    selector: "tim-group-management-console",
    template: `
        <ng-container>
            <bootstrap-panel id="groups-panel" title="All groups" i18n-title>
                <div id="list-all-groups-setting">
                    <label for="showAllGroups" class="form-control" i18n>
                        <input type="checkbox" id="showAllGroups" [(ngModel)]="showAllGroups"
                               (click)="toggleAllGroupsVisible()"/>
                        <span style="padding-left: 2em;">List all existing groups</span>
                    </label>
                </div>
                <div id="groups-list">
                    <!-- List groups here, include checkboxes for selecting groups to manage -->

                    <!-- Mock groups list -->
                    <div class="pull-left">
                        <!-- TODO: Implement as TimTable -->
                        <table>
                            <thead>
                            <tr class="member-table-row">
                                <th i18n>Select</th>
                                <th i18n>Group name</th>
                                <th *ngIf="isAdmin()" i18n>Group document</th>
                                <th i18n>Exam</th>
                                <th i18n>Timeslot</th>
                            </tr>
                            </thead>
                            <tbody>
                            <tr class="member-table-row" *ngFor="let group of groups">
                                <td>
                                    <input type="checkbox" (click)="toggleGroupSelection(group)"/>
                                </td>
                                <td>{{group.name}}</td>
                                <td *ngIf="isAdmin()">
                                    <a href="/view/{{group.path}}">{{group.path}}</a></td>
                                <td> -</td>
                                <td> -</td>
                            </tr>
                            </tbody>
                        </table>
                    </div>
                    <!-- END groups list-->

                </div>
                <div id="groups-list-controls" style="display: inline-block">
                    <div class="flex">
                        <button class="timButton" (click)="createNewGroup()" i18n>Create a new group</button>
                        <button class="timButton btn-danger" (click)="deleteSelectedGroups()"
                                [disabled]="selectedGroups.length < 1" i18n>Delete selected groups
                        </button>
                        <button class="timButton" (click)="generateLoginCodes(selectedMembers)"
                                [disabled]="selectedGroups.length < 1" i18n>Generate login codes
                        </button>
                        <button class="timButton" (click)="printLoginCodes(selectedGroups)"
                                [disabled]="selectedGroups.length < 1" i18n>Print login codes
                        </button>
                    </div>
                    <tim-alert severity="success"
                               *ngIf="saveGroupSuccessMessage">{{saveGroupSuccessMessage}}</tim-alert>
                    <tim-alert severity="danger" *ngIf="saveGroupFailMessage">{{saveGroupFailMessage}}</tim-alert>
                </div>
            </bootstrap-panel>
            <form>
                <fieldset [disabled]="savingGroupMembers">
                    <bootstrap-panel title="Group members" i18n-title>
                        <tabset class="merged">
                            <!-- Create a new tab for each group that is visible to the current user -->
                            <tab *ngFor="let group of groups" heading="{{group.name}}"
                                 class="grid-tab tab-form" (selectTab)="onGroupTabSelected($event)">
                                <ng-container>
                                    <!-- Member list, with sort and selection controls -->
                                    
                                    <div>
                                        <!-- TODO: Implement as TimTable -->
                                        <table>
                                            <thead>
                                            <tr class="member-table-row">
                                                <th i18n>Select</th>
                                                <th i18n>Name</th>
                                                <th i18n>Username</th>
                                                <th i18n>Email</th>
                                                <th i18n>Login code</th>
                                            </tr>
                                            </thead>
                                            <tbody>
                                            <tr class="member-table-row" *ngFor="let member of members[group.name]">
                                                <td>
                                                    <input type="checkbox" (click)="toggleMemberSelection(member)"/>
                                                </td>
                                                <td>{{member.real_name}}</td>
                                                <td>{{member.name}}</td>
                                                <td>{{member.email}}</td>
                                                <td>{{member.student_id}}</td>
                                            </tr>
                                            </tbody>
                                        </table>
                                    </div>
                                    <!-- END members list-->

                                </ng-container>
                            </tab>
                        </tabset>

                        <div id="members-controls">
                            <button class="timButton" (click)="addMembers()" i18n>
                                Add members
                            </button>
                            <button class="timButton btn-danger" (click)="removeMembers()"
                                    [disabled]="selectedMembers.length < 1" i18n>
                                Remove selected
                            </button>
                            <tim-alert severity="success"
                                       *ngIf="saveMembersSuccessMessage">{{saveMembersSuccessMessage}}</tim-alert>
                            <tim-alert severity="danger"
                                       *ngIf="saveMembersFailMessage">{{saveMembersFailMessage}}</tim-alert>
                        </div>
                    </bootstrap-panel>
                </fieldset>
            </form>

            <!--            <bootstrap-panel title="Dangerous actions" i18n-title severity="danger">-->
            <!--                <button class="btn btn-danger" (click)="deleteList()" i18n>Delete List</button>-->
            <!--            </bootstrap-panel>-->
        </ng-container>
    `,
    styleUrls: ["group-management.component.scss"],
})
export class GroupManagementComponent implements OnInit {
    settings: IGroupManagementSettings = {};

    showAllGroups: boolean;
    // Groups that can manage login code groups, specified with document setting `groups`
    managers: Group[] = [];
    // Groups that are managed via the group management document
    groups: Group[] = [];
    // Members visible on the currently active group tab (in the group members table)
    members: Record<string, GroupMember[]> = {};

    // Currently selected login code groups
    selectedGroups: Group[];

    saveGroupSuccessMessage?: string;
    saveGroupFailMessage?: string;

    // currently selected members
    selectedMembers: GroupMember[];
    saveMembersSuccessMessage?: string;
    saveMembersFailMessage?: string;
    selectedGroupTab?: string;

    // just for visualizing/testing the interface
    // mockMembers: GroupMember[];
    // mockGroups: Group[];
    mockCurrentLoginKeyStart: number;

    // status info for group members table
    savingGroupMembers?: boolean;

    @Input() eventHeading?: string;

    constructor(private http: HttpClient) {
        this.showAllGroups = false;
        this.selectedGroups = [];
        this.selectedMembers = [];
        this.mockCurrentLoginKeyStart = 0;
        // this.settings = {};

        // this.mockMembers = [
        //     {
        //         id: 123456789,
        //         name: "aku",
        //         email: "aku@ankkalinna.com",
        //         real_name: "Aku Ankka",
        //         student_id: "123456789",
        //     },
        //     {
        //         id: 223456789,
        //         name: "hupu",
        //         email: "hupu@ankkalinna.com",
        //         real_name: "Hupu Ankka",
        //         student_id: "223456789",
        //     },
        //     {
        //         id: 323456789,
        //         name: "tupu",
        //         email: "tupu@ankkalinna.com",
        //         real_name: "Tupu Ankka",
        //         student_id: "323456789",
        //     },
        //     {
        //         id: 423456789,
        //         name: "lupu",
        //         email: "lupu@ankkalinna.com",
        //         real_name: "Lupu Ankka",
        //         student_id: "423456789",
        //     },
        //     {
        //         id: 523456789,
        //         name: "iines",
        //         email: "iines@ankkalinna.com",
        //         real_name: "Iines Ankka",
        //         student_id: "523456789",
        //     },
        //     {
        //         id: 623456789,
        //         name: "roope",
        //         email: "roope@ankkalinna.com",
        //         real_name: "Roope Ankka",
        //         student_id: "623456789",
        //     },
        //     {
        //         id: 723456789,
        //         name: "testiankka",
        //         email: "testiankka@ankkalinna.com",
        //         real_name: "Testi Ankka",
        //         student_id: "723456789",
        //     },
        // ];
        // this.mockGroups = [
        //     {
        //         id: 1,
        //         name: "EN1_7A",
        //         path: "/groups/sukol/Sukol_2023_Ankkalinnan-koulu__RU4xXzdB",
        //     },
        //     {
        //         id: 2,
        //         name: "EN1_7B",
        //         path: "/groups/sukol/Sukol_2023_Ankkalinnan-koulu__RU4xXzdC",
        //     },
        //     {
        //         id: 3,
        //         name: "EN3_8A",
        //         path: "/groups/sukol/Sukol_2023_Ankkalinnan-koulu__RU4zXzhB",
        //     },
        //     {
        //         id: 10,
        //         name: "testi01",
        //         path: "/groups/sukol/testi01",
        //     },
        //     {
        //         id: 11,
        //         name: "testi02",
        //         path: "/groups/sukol/testi02",
        //     },
        // ];
    }

    /**
     * The current document's document ID.
     */
    getDocId() {
        return documentglobals().curr_item.id;
    }

    isAdmin() {
        return Users.belongsToGroup(ADMIN_GROUPNAME);
    }

    /**
     * Initialization procedures.
     */
    async ngOnInit() {
        // Load content only for logged in users who are also owners of this management document
        const isDocOwner = await toPromise(
            this.http.get(`/loginCode/checkOwner/${this.getDocId()}`)
        );
        if (!isAdmin() && (!isDocOwner.ok || !Users.isLoggedIn())) {
            return;
        }

        await this.getGroups();
        this.selectedGroupTab = this.groups[0]?.name ?? "";

        for (let g of this.groups) {
            await this.getGroupMembers(g);
        }

        // TODO do we even needs these? we already parse the necessary docsettings
        //      when processing the requests on the server
        // this.settings = {
        //     ...this.viewctrl.docSettings.groupManagement,
        // };

        // await this.getManagers();
        // await this.debugCheck();
    }

    async getManagers() {
        // let managers = this.settings.managers;
        // if (!managers) {
        //     managers = ["sukoladmin1", "sukoladmin2"];
        // }
        const doc_id = this.getDocId();
        const res = await to2(
            toPromise(this.http.get<Group[]>(`/loginCode/managers/${doc_id}`))
        );
        if (res.ok && res.result.ok) {
            this.managers = res.result.result;
        }

        // return;
    }

    /**
     * Fetches the groups that the current user is allowed to manage.
     * Note: current user must be member of one the document's manager groups
     *       specified with the document setting `groupManagement.managers`.
     */
    async getGroups() {
        const doc_id = this.getDocId();
        // check that current user has ownership to this manage document
        const resp = await toPromise(
            this.http.get(`/loginCode/checkOwner/${doc_id}`)
        );
        if (resp.ok) {
            // Filter out groups to which the current user does not have owner rights,
            // unless the (UI option? could be doc setting as well?) option 'List all existing groups' is active.
            // In general, group managers should not have access to groups they did not create,
            // but there are exceptions (for instance, a group manager might need to be substituted suddenly).
            let _showAllGroups: string = this.showAllGroups
                ? `?showAllGroups=${this.showAllGroups}`
                : "";
            const groups = await toPromise<Group[]>(
                this.http.get<Group[]>(
                    `/loginCode/groups/${doc_id}${_showAllGroups}`
                )
            );
            if (groups.ok) {
                this.groups = groups.result;
            }
        }
    }

    async toggleAllGroupsVisible() {
        this.showAllGroups = !this.showAllGroups;
        // refresh
        await this.getGroups();
    }

    async debugCheck() {
        const resp = await toPromise(
            this.http.get<string>("/loginCode/checkRequest")
        );
        if (resp.ok) {
            await to2(showMessageDialog(resp.result.toString()));
        }
    }

    async createNewGroup() {
        // Disable setting group folder and provide a default path for it
        const params: UserGroupDialogParams = {
            // TODO set these in group management docSettings
            canChooseFolder: false,
            // TODO Should be same as 'groupsPath'
            defaultGroupFolder: "sukol",
            encodeGroupName: true,
        };
        // Create a new group
        const res = await to2(showUserGroupDialog(params));
        if (res.ok) {
            // add new group to list
            const group = res.result;
            // TODO We should perhaps fetch or refresh the displayed groups in case of sync issues?
            this.groups.push({
                id: group.id,
                name: group.name,
                path: group.path,
            });
        }
        return;
    }

    deleteSelectedGroups() {
        // Delete selected groups
        // Should probably return a message denoting the names of the removed groups, or an error message
        // Note: remember to clear selectedGroups after deletion
        return;
    }

    generateLoginCodes(members: IUser[]) {
        // Generate temporary login codes for members of the currently selected groups
        // The codes are linked to the individual users via a database table
        // Should check for existing and still valid codes before refreshing
        // to avoid accidentally changing valid ones (also display a warning, perhaps in a dialog).
        members.forEach((member) => {
            const loginkey = `${this.mockCurrentLoginKeyStart++}`;
            member.student_id = `${"0".repeat(9 - loginkey.length)}${loginkey}`;
        });
        // return;
    }

    printLoginCodes(groups: IGroup[]) {
        // Print login codes for selected groups
        // Generate a HTML table and show browser print dialog,
        // optionally export as PDF.
        // see https://developer.mozilla.org/en-US/docs/Web/Guide/Printing
    }

    /**
     * Fetches users belonging to the specified group
     * @param group
     */
    async getGroupMembers(group: Group) {
        const resp = await toPromise(
            // this.http.get<GroupMember[]>(`/show/${group.name}`)
            this.http.get<GroupMember[]>(`/loginCode/members/${group.id}`)
        );
        if (resp.ok) {
            this.members[group.name] = resp.result;
        }
    }

    addMembers() {
        // Add members to the currently active group
        // Should probably add members only to the group in the active tab in the group members view
        // Should display a dialog where the user may provide a list of members to add
        // Support adding members via an existing UserGroup in addition to username, email, and [creating new users on the spot]
        return;
    }

    removeMembers() {
        // Remove selected members from the active group
        // Note: remember to clear selectedMembers after deletion from the group
    }

    toggleMemberSelection(member: GroupMember) {
        if (!this.selectedMembers.includes(member)) {
            this.selectedMembers.push(member);
        } else {
            this.selectedMembers.splice(
                this.selectedMembers.indexOf(member),
                1
            );
        }
    }

    toggleGroupSelection(group: Group) {
        if (!this.selectedGroups.includes(group)) {
            this.selectedGroups.push(group);
        } else {
            this.selectedGroups.splice(this.selectedGroups.indexOf(group), 1);
        }
    }

    onGroupTabSelected(groupTab: TabDirective) {
        this.selectedGroupTab = groupTab.heading;
    }

    // /**
    //  * Helper for setting members of a group to a table.
    //  */
    // setGroupMembers() {
    //     if (this.currentGroup) {
    //         // Find the currently selected group where we want to see members.
    //         const groupAndMembers = this.groupsAndMembers?.find(
    //             (g) => g.groupName === this.currentGroup
    //         );
    //         if (groupAndMembers) {
    //             this.groupMembers = groupAndMembers.members;
    //         }
    //     }
    // }

    // private async updateMemberList() {
    //     // Load list members.
    //     const result2 = await this.getListMembers();
    //
    //     if (result2.ok) {
    //         // TODO order members by name.
    //         this.membersList = result2.result;
    //         // Set the UI value for removed attribute.
    //         for (const member of this.membersList) {
    //             if (member.removed) {
    //                 member.removedDisplay = moment(member.removed).format(
    //                     "DD.MM.YYYY hh:mm"
    //                 );
    //             }
    //         }
    //     } else {
    //         this.permanentErrorMessage = $localize`Loading list's members failed: ${result2.result.error.error}`;
    //     }
    //
    //     await this.getGroupMembers();
    // }
}
