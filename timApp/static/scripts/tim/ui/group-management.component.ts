import {HttpClient} from "@angular/common/http";
import type {OnInit} from "@angular/core";
import {Component, Input, NgModule} from "@angular/core";
import type {Moment} from "moment";
// import moment from "moment";
import {to, to2, toPromise} from "../util/utils";
import {documentglobals} from "../util/globals";
import {showInputDialog} from "./showInputDialog";
import {InputDialogKind} from "./input-dialog.kind";
// import {$http} from "tim/util/ngimport";
import {Users} from "../user/userService";
import {IGroup, IUser} from "tim/user/IUser";
import {CommonModule} from "@angular/common";
import {DialogModule} from "./angulardialog/dialog.module";
import {TimUtilityModule} from "./tim-utility.module";
import {FormsModule} from "@angular/forms";
import {InputDialogComponent} from "./input-dialog.component";
import {ar} from "date-fns/locale";

@Component({
    selector: "tim-group-management-console",
    template: `
        <ng-container>
            <bootstrap-panel id="groups-panel" title="All groups" i18n-title>
                <div id="groups-list">
                    <!-- List groups here, include checkboxes for selecting groups to manage -->
                </div>
                <div id="groups-list-controls">
                    <div class="flex">
                        <button class="timButton" (click)="createNewGroup()" i18n>Create new group</button>
                        <button class="timButton" (click)="deleteSelectedGroups()"
                                [disabled]="selectedGroups?.length < 1" i18n>Delete selected
                        </button>
                        <button class="timButton" (click)="generateLoginCodes()"
                                [disabled]="selectedGroups?.length < 1" i18n>Generate login codes
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
                            <tab *ngFor="let group of selectedGroups" heading="{{group.name}}"
                                 class="grid-tab tab-form">
                                <ng-container>
                                    <!-- Member list, with sort and selection controls (timTable?) -->
                                </ng-container>
                            </tab>
                        </tabset>
                                
                        <div id="members-controls">
                            <button class="timButton" (click)="addMembers()" i18n>
                                Add members</button>
                            <button class="timButton" (click)="removeMembers()"
                                [disabled]="selectedMembers?.length < 1" i18n>
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
    styleUrls: ["message-list-admin.component.scss"],
})
export class GroupManagementComponent implements OnInit {
    // currently selected groups
    // TODO should maybe use a different group interface,
    //   or define a new interface for this use in this component
    //   since we may want to store references to group members
    //   in it for convenience
    selectedGroups?: IGroup[];
    saveGroupSuccessMessage?: string;
    saveGroupFailMessage?: string;

    // currently selected members
    selectedMembers?: IUser[];
    saveMembersSuccessMessage?: string;
    saveMembersFailMessage?: string;

    // status info for group members table
    savingGroupMembers?: boolean;

    constructor(private http: HttpClient) {}

    /**
     * The current document's document ID.
     */
    getDocId() {
        return documentglobals().curr_item.id;
    }

    /**
     * Initialization procedures.
     */
    async ngOnInit() {
        if (Users.isLoggedIn()) {
            // Load groups visible to the current user
        }
    }

    createNewGroup() {
        // Create a new group
        // We may want to display a dialog with an option to specify group member to add to the created group
        return;
    }

    deleteSelectedGroups() {
        // Delete selected groups
        // Should probably return a message denoting the names of the removed groups, or an error message
        // Note: remember to clear selectedGroups after deletion
        return;
    }

    generateLoginCodes() {
        // Generate temporary login codes for members of the currently selected groups
        // The codes are linked to the individual users via a database table
        // Should check for existing and still valid codes before refreshing
        // to avoid accidentally changing valid ones (also display a warning, perhaps in a dialog).
        return;
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
