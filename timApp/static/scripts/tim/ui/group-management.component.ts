import {HttpClient} from "@angular/common/http";
import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";
// import moment from "moment";
import {documentglobals} from "../util/globals";
import {showInputDialog} from "./showInputDialog";
import {InputDialogKind} from "./input-dialog.kind";
// import {$http} from "tim/util/ngimport";
import {Users} from "../user/userService";
import {ADMIN_GROUPNAME, IGroup, IUser} from "tim/user/IUser";
import {to2} from "tim/util/utils";
import {forEach} from "angular";

export interface GroupMember extends IUser {
    id: number;
    name: string;
    email: string;
    real_name: string;
    student_id: string;
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
                    
                    <!-- Mock members list -->
                    <div class="pull-left">
                        <!-- TODO: Implement as TimTable -->
                        <table>
                            <thead>
                            <tr class="member-table-row">
                                <th i18n>Select</th>
                                <th i18n>Group name</th>
                                <th i18n>Group file</th>
                                <th i18n>Exam</th>
                                <th i18n>Timeslot</th>
                            </tr>
                            </thead>
                            <tbody>
                            <tr class="member-table-row" *ngFor="let group of mockGroups">
                                <td>
                                    <input type="checkbox" (click)="addToSelectedGroups(group)"/>
                                </td>
                                <td>{{group.name}}</td>
                                <td *ngIf="isAdmin()">{{group.external_id}}</td>
                                <td> - </td>
                                <td> - </td>
                            </tr>
                            </tbody>
                        </table>
                    </div>
                    <!-- END members list-->
                    
                </div>
                <div id="groups-list-controls" style="display: inline-block">
                    <div class="flex">
                        <button class="timButton" (click)="createNewGroup()" i18n>Create new group</button>
                        <button class="timButton" (click)="deleteSelectedGroups()"
                                [disabled]="selectedGroups.length < 1" i18n>Delete selected
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
                                            <tr class="member-table-row" *ngFor="let gMember of mockMembers">
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
                                Add members</button>
                            <button class="timButton" (click)="removeMembers()"
                                [disabled]="selectedMembers.length < 1" i18n>
                                Remove selected</button>
                            <tim-alert severity="success"
                                       *ngIf="saveMembersSuccessMessage">{{saveMembersSuccessMessage}}</tim-alert>
                            <tim-alert severity="danger" *ngIf="saveMembersFailMessage">{{saveMembersFailMessage}}</tim-alert>
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
    showAllGroups: boolean;

    // currently selected groups
    // TODO should maybe use a different group interface,
    //   or define a new interface for this use in this component
    //   since we may want to store references to group members
    //   in it for convenience
    selectedGroups: IGroup[];

    saveGroupSuccessMessage?: string;
    saveGroupFailMessage?: string;

    // currently selected members
    selectedMembers: IUser[];
    saveMembersSuccessMessage?: string;
    saveMembersFailMessage?: string;

    // just for visualizing/testing the interface
    mockMembers: IUser[];
    mockGroups: IGroup[];
    mockCurrentLoginKeyStart: number;

    // status info for group members table
    savingGroupMembers?: boolean;

    constructor(private http: HttpClient) {
        this.showAllGroups = false;
        this.selectedGroups = [];
        this.selectedMembers = [];
        this.mockCurrentLoginKeyStart = 0;
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
        ];
        this.mockGroups = [
            {
                id: 1,
                name: "EN1_7A",
                external_id: "Sukol_2023_Ankkalinnan-koulu__RU4xXzdB",
            },
            {
                id: 2,
                name: "EN1_7B",
                external_id: "Sukol_2023_Ankkalinnan-koulu__RU4xXzdC",
            },
            {
                id: 3,
                name: "EN3_8A",
                external_id: "Sukol_2023_Ankkalinnan-koulu__RU4zXzhB",
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
    async ngOnInit() {
        if (Users.isLoggedIn()) {
            // Load groups visible to the current user
        }
    }

    showAllAvailableGroups() {
        if (this.showAllGroups) {
            this.showAllGroups = false;
        } else {
            this.showAllGroups = true;
        }
    }

    async createNewGroup() {
        // Create a new group
        // We may want to display a dialog with an option to specify group member to add to the created group
        const res = await to2(
            showInputDialog<string>({
                isInput: InputDialogKind.NoValidator,
                inputType: "textarea",
                text: "Enter group name",
                title: "Create new group",
                okValue: "",
            })
        );
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
            let loginkey = `${this.mockCurrentLoginKeyStart++}`;
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

    addToSelectedMembers(member: IUser) {
        if (!this.selectedMembers.includes(member)) {
            this.selectedMembers.push(member);
        }
    }

    addToSelectedGroups(group: IGroup) {
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
