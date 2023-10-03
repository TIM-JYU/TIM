import {HttpClient} from "@angular/common/http";
import type {AfterViewInit, OnInit} from "@angular/core";
import {Component, Input} from "@angular/core";
// import moment from "moment";
// import {forEach} from "angular";
import type {IGroup, IUser} from "tim/user/IUser";
import {ADMIN_GROUPNAME} from "tim/user/IUser";
import {showUserGroupDialog} from "tim/user/showUserGroupDialog";
import {documentglobals} from "tim/util/globals";
import {Users} from "tim/user/userService";
import {to2, toPromise} from "tim/util/utils";
import type {IGroupManagementSettings} from "tim/document/IDocSettings";
import type {UserGroupDialogParams} from "tim/user/user-group-dialog.component";
import {showMessageDialog} from "tim/ui/showMessageDialog";

export interface GroupMember extends IUser {
    id: number;
    name: string;
    email: string;
    real_name: string;
    student_id: string;
}

export interface Group extends IGroup {
    // Group's doc path, eg. '/groups/test/group01'
    path?: string;
    event?: string;
    timeslot?: string;
}

/*
Required document setting:
```
additional_angular_modules:
  - timGroupManagementModule
```
 */

@Component({
    selector: "tim-group-management-console",
    template: `
        <ng-container>
            <bootstrap-panel id="groups-panel" title="All groups" i18n-title>
                <div id="list-all-groups-setting">
                    <label for="showAllGroups" class="form-control" i18n>
                        <input type="checkbox" id="showAllGroups" (click)="showAllAvailableGroups()" />
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
                                <th *ngIf="isAdmin()" i18n>Group file</th>
                                <th i18n>Exam</th>
                                <th i18n>Timeslot</th>
                            </tr>
                            </thead>
                            <tbody>
                            <tr class="member-table-row" *ngFor="let group of groups">
                                <td>
                                    <input type="checkbox" (click)="addToSelectedGroups(group)"/>
                                </td>
                                <td>{{group.name}}</td>
                                <td *ngIf="isAdmin()">
                                    <a href="/view{{group.path}}">{{group.path}}</a></td>
                                <td> - </td>
                                <td> - </td>
                            </tr>
                            </tbody>
                        </table>
                    </div>
                    <!-- END groups list-->

                </div>
                <div id="groups-list-controls" style="display: inline-block">
                    <div class="flex">
                        <button class="timButton" (click)="createNewGroup()" i18n>Create a new group</button>
                        <button class="timButton" (click)="deleteSelectedGroups()"
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
                            <tab *ngFor="let group of mockGroups" heading="{{group.name}}"
                                 class="grid-tab tab-form">
                                <ng-container>
                                    <!-- Member list, with sort and selection controls -->

                                    <!-- Mock members list -->
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
                                            <tr class="member-table-row" *ngFor="let gMember of getGroupMembers(group)">
                                                <td>
                                                    <input type="checkbox" (click)="addToSelectedMembers(gMember)"/>
                                                </td>
                                                <td>{{gMember.real_name}}</td>
                                                <td>{{gMember.name}}</td>
                                                <td>{{gMember.email}}</td>
                                                <td>{{gMember.student_id}}</td>
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
                            <button class="timButton" (click)="removeMembers()"
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
export class GroupManagementComponent implements OnInit, AfterViewInit {
    settings: IGroupManagementSettings;

    showAllGroups: boolean;
    // Groups that can manage login code groups, specified with document setting `groups`
    managers: Group[] = [];
    // Groups that are managed via the group management document
    groups: Group[] = [];

    // Currently selected login code groups
    selectedGroups: Group[];

    saveGroupSuccessMessage?: string;
    saveGroupFailMessage?: string;

    // currently selected members
    selectedMembers: GroupMember[];
    saveMembersSuccessMessage?: string;
    saveMembersFailMessage?: string;

    // just for visualizing/testing the interface
    mockMembers: GroupMember[];
    mockGroups: Group[];
    mockCurrentLoginKeyStart: number;

    // status info for group members table
    savingGroupMembers?: boolean;

    @Input() eventHeading?: string;

    constructor(private http: HttpClient) {
        this.showAllGroups = false;
        this.selectedGroups = [];
        this.selectedMembers = [];
        this.mockCurrentLoginKeyStart = 0;
        this.settings = {
            ...documentglobals().docSettings.groupManagement,
            // managers:
            //     documentglobals().docSettings.groupManagement?.managers,
            // groupsPath:
            //     documentglobals().docSettings.groupManagement?.groupsPath,
            // showAllGroups:
            //     documentglobals().docSettings.groupManagement?.showAllGroups,
        };
        this.mockMembers = [
            {
                id: 123456789,
                name: "aku",
                email: "aku@ankkalinna.com",
                real_name: "Aku Ankka",
                student_id: "123456789",
            },
            {
                id: 223456789,
                name: "hupu",
                email: "hupu@ankkalinna.com",
                real_name: "Hupu Ankka",
                student_id: "223456789",
            },
            {
                id: 323456789,
                name: "tupu",
                email: "tupu@ankkalinna.com",
                real_name: "Tupu Ankka",
                student_id: "323456789",
            },
            {
                id: 423456789,
                name: "lupu",
                email: "lupu@ankkalinna.com",
                real_name: "Lupu Ankka",
                student_id: "423456789",
            },
            {
                id: 523456789,
                name: "iines",
                email: "iines@ankkalinna.com",
                real_name: "Iines Ankka",
                student_id: "523456789",
            },
            {
                id: 623456789,
                name: "roope",
                email: "roope@ankkalinna.com",
                real_name: "Roope Ankka",
                student_id: "623456789",
            },
            {
                id: 723456789,
                name: "testiankka",
                email: "testiankka@ankkalinna.com",
                real_name: "Testi Ankka",
                student_id: "723456789",
            },
        ];
        this.mockGroups = [
            {
                id: 1,
                name: "EN1_7A",
                path: "/groups/sukol/Sukol_2023_Ankkalinnan-koulu__RU4xXzdB",
            },
            {
                id: 2,
                name: "EN1_7B",
                path: "/groups/sukol/Sukol_2023_Ankkalinnan-koulu__RU4xXzdC",
            },
            {
                id: 3,
                name: "EN3_8A",
                path: "/groups/sukol/Sukol_2023_Ankkalinnan-koulu__RU4zXzhB",
            },
            {
                id: 10,
                name: "testi01",
                path: "/groups/sukol/testi01",
            },
            {
                id: 11,
                name: "testi02",
                path: "/groups/sukol/testi02",
            },
        ];
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
    ngOnInit() {
        if (Users.isLoggedIn()) {
            // Load groups visible to the current user
            // Users.isGroupAdmin()
        }
    }

    async ngAfterViewInit() {
        await this.setManagers();
        await this.setGroups();
        await this.debugCheck();
    }

    async setManagers() {
        const docgroups = this.settings.managers;
        if (docgroups) {
            const res = await toPromise(
                this.http.get<Group[]>("/loginCode/managers/" + docgroups)
            );
            if (res.ok) {
                this.managers = res.result;
            }
        }
        return;
    }

    showAllAvailableGroups() {
        if (this.showAllGroups) {
            this.showAllGroups = false;
        } else {
            this.showAllGroups = true;
        }
    }

    /**
     * Fetches the groups that the current user is allowed to manage.
     * Note: current user must be member of one the document's groups
     *       specified with the document (globals) setting `groups`.
     */
    async setGroups() {
        // check that current user has ownership to this manage document
        const resp = await toPromise(
            this.http.get("/loginCode/checkOwner/" + this.getDocId())
        );
        if (resp.ok) {
            const groupsPath = this.settings.groupsPath;
            // TODO check that path corresponds to managing group
            // TODO could just do the thing in backend without worrying about the path?
            if (groupsPath) {
                const groups = await toPromise<Group[]>(
                    this.http.get<Group[]>("/loginCode/groups/" + groupsPath)
                );
                if (groups.ok) {
                    this.groups = groups.result;
                }
            }
        }

        return;
    }

    async debugCheck() {
        const resp = await toPromise(
            this.http.get<string>("/loginCode/checkRequest/" + this.getDocId())
        );
        if (resp.ok) {
            await to2(showMessageDialog(resp.result));
        }
    }

    async createNewGroup() {
        // Disable setting group folder and provide a default path for it
        const params: UserGroupDialogParams = {
            canChooseFolder: false,
            // TODO get default group folder from group management docSettings
            defaultGroupFolder: "sukol",
        };
        // Create a new group
        const res = await to2(showUserGroupDialog(params));
        if (res.ok) {
            // add new group to list
            const group = res.result;
            // TODO Remove this, don't need to add anywhere, visible groups are populated from the groups folder
            //      depending on the users membership in the managing group(s)
            this.mockGroups.push({
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
    getGroupMembers(group: Group): GroupMember[] {
        return this.mockMembers;
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

    addToSelectedMembers(member: GroupMember) {
        if (!this.selectedMembers.includes(member)) {
            this.selectedMembers.push(member);
        }
    }

    addToSelectedGroups(group: Group) {
        if (!this.selectedGroups.includes(group)) {
            this.selectedGroups.push(group);
        }
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
