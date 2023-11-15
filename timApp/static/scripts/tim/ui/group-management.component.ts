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
import {showUserCreationDialog} from "tim/user/showUserCreationDialog";
import {TabDirective} from "ngx-bootstrap/tabs";
import {forEach} from "angular";
import http from "http";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {ViewCtrl} from "tim/document/viewctrl";
import {vctrlInstance} from "tim/document/viewctrlinstance";

export interface GroupMember extends IUser {
    id: number;
    name: string;
    email: string;
    real_name: string;
    // 'home' org, class, or other identifier that we can use to check
    // if this GroupMember already exists in a situation where we cannot
    // confidently use email, name or real_name for this purpose.
    association: string;
    // login codes will need to be treated like passwords
    login_code: string;

    selected: boolean;
}

export interface Group extends IGroup {
    // Group's doc path, eg. 'test/group01'
    path?: string;
    events?: GroupEvent[];
    selected: boolean;
    allMembersSelected: boolean;
}

/**
 * Represents an event (exam, meeting, etc.) that is scheduled for a specific group.
 */
export interface GroupEvent {
    title: string;
    timeslot: string;

    // List of document ids/paths that are related to the event.
    // Can be used to eg. modify document rights for an event group in the given timeslot
    documents: string[];
}

type Selectable = Group | GroupMember;

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
                               (change)="toggleAllGroupsVisible()"/>
                        <span style="padding-left: 2em;">List all existing groups</span>
                    </label>
                </div>
                <div id="groups-list">
                    <!-- List groups here, include checkboxes for selecting groups to manage -->

                    <!-- Mock groups list -->
                    <div class="pull-left">
                        <!-- TODO: Implement as TimTable -->
                        <!-- TODO: Event and Timeslot should be combined in the future,
                                   so that we can list multiple events for a specific group
                                   in a sensible way. -->
                        <table>
                            <thead>
                            <tr class="member-table-row">
                                <th i18n><input type="checkbox" name="selectAllGroups" [(ngModel)]="allGroupsSelected" (change)="toggleAllGroupsSelected()"/></th>
                                <th i18n>Group name</th>
                                <th *ngIf="isAdmin()" i18n>Group document</th>
                                <th i18n>Event</th>
                                <th i18n>Timeslot</th>
                            </tr>
                            </thead>
                            <tbody>
                            <tr class="member-table-row" *ngFor="let group of groups">
                                <td>
                                    <input type="checkbox" name="toggleGroupSelection_{{group.id}}" [(ngModel)]="group.selected" (change)="toggleGroupSelection(group)"/>
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
                <!-- style="display: inline-block" -->
                <div id="groups-list-controls" >
                    <div class="flex">
                        <button class="timButton" (click)="createNewGroup()" i18n>Create a new group</button>
                        <!-- TODO disable copying if more than one group selected -->
                        <button class="timButton" (click)="copyGroup(this.groups)"
                                [disabled]="!oneSelected(this.groups)" i18n>Copy selected group
                        </button>
                        <button class="timButton btn-danger" (click)="deleteSelectedGroups(this.groups)"
                                [disabled]="anySelected(this.groups)" i18n>Delete selected groups
                        </button>
                        <button class="timButton" (click)="generateLoginCodes(this.groups)"
                                [disabled]="anySelected(this.groups)" i18n>Generate login codes
                        </button>
                        <button class="timButton" (click)="printLoginCodes(this.groups)"
                                [disabled]="anySelected(this.groups)" i18n>Print login codes
                        </button>
                    </div>
                    <tim-alert severity="success"
                               *ngIf="saveGroupSuccessMessage">{{saveGroupSuccessMessage}}</tim-alert>
                    <tim-alert severity="danger" *ngIf="saveGroupFailMessage">{{saveGroupFailMessage}}</tim-alert>
                </div>
                <div>
                    <ng-container *ngIf="!anySelected(this.groups)">
                        <p>Selected groups:</p>
                        <ol>
                            <ng-container *ngFor="let group of groups">
                                <li *ngIf="group.selected">{{group.name}}</li>
                            </ng-container>
                        </ol>
                    </ng-container>
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
                                                <th i18n><input type="checkbox" name="selectAllMembers_{{group.id}}" [(ngModel)]="group.allMembersSelected" (change)="toggleAllMembersSelected(group)"/></th>
                                                <th i18n>Name</th>
                                                <th i18n>Username</th>
                                                <th i18n>Email</th>
                                                <th i18n>Login code</th>
                                            </tr>
                                            </thead>
                                            <tbody>
                                            <tr class="member-table-row" *ngFor="let member of members[group.name]">
                                                <td>
                                                    <input type="checkbox" name="toggleSelection_{{member.id}}" [(ngModel)]="member.selected" (change)="toggleMemberSelection(group)" />
                                                </td>
                                                <td>{{member.real_name}}</td>
                                                <td>{{member.name}}</td>
                                                <td>{{member.email}}</td>
                                                <td>{{member.login_code}}</td>
                                            </tr>
                                            </tbody>
                                        </table>
                                    </div>
                                    <!-- END members list-->

                                </ng-container>
                                <ng-container>
                                    <div id="members-controls">
                                        <button class="timButton" (click)="addMembers(group)" i18n>
                                            Create new members
                                        </button>
                                        <button class="timButton" (click)="addExistingMembers(this.groups)" i18n>
                                            Add existing users
                                        </button>
                                        <button class="timButton btn-danger" (click)="removeMembers(group)"
                                                [disabled]="anySelected(this.members[group.name])" i18n>
                                            Remove selected
                                        </button>
                                        <tim-alert severity="success"
                                                   *ngIf="saveMembersSuccessMessage">{{saveMembersSuccessMessage}}</tim-alert>
                                        <tim-alert severity="danger"
                                                   *ngIf="saveMembersFailMessage">{{saveMembersFailMessage}}</tim-alert>
                                    </div>
                                </ng-container>
                                <ng-container *ngIf="!anySelected(this.members[group.name])">
                                    <p>Selected members:</p>
                                        <ol>
                                            <ng-container *ngFor="let member of this.members[group.name]">
                                                <li *ngIf="member.selected">{{member.name}}</li>
                                            </ng-container>
                                        </ol>
                                </ng-container>
                            </tab>
                        </tabset>

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

    // Currently selected groups
    allGroupsSelected: boolean;
    saveGroupSuccessMessage?: string;
    saveGroupFailMessage?: string;

    // currently selected members
    selectedGroupTab?: string;
    saveMembersSuccessMessage?: string;
    saveMembersFailMessage?: string;

    // status info for group members table
    savingGroupMembers?: boolean;

    @Input() eventHeading?: string;
    private viewctrl!: Require<ViewCtrl>;

    constructor(private http: HttpClient) {
        this.viewctrl = vctrlInstance!;
        // this.settings = this.viewctrl.docSettings.groupManagement ?? {}; // ngOnInit
        this.showAllGroups = false;
        this.allGroupsSelected = false;
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

        this.settings = {
            ...this.viewctrl.docSettings.groupManagement,
        };

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

    /**
     * Checks that at least one Gourp or GroupMember is selected.
     * @param selectables list of selectable Groups or GroupMembers
     */
    anySelected(selectables: GroupMember[] | Group[]) {
        return !selectables?.some((s) => s.selected) ?? false;
    }

    /**
     * Checks that exactly one Group is selected.
     * @param groups list of selectable Groups
     */
    oneSelected(groups: Group[]) {
        let count = 0;
        for (let g of groups) {
            if (g.selected) count++;
            if (count > 1) return false;
        }
        return count == 1;
    }

    async toggleAllGroupsVisible() {
        this.showAllGroups = !this.showAllGroups;
        // refresh
        await this.getGroups();
    }

    toggleGroupSelection(group: Group) {
        this.allGroupsSelected = this.groups.every((g) => g.selected);
    }

    toggleAllGroupsSelected() {
        for (const g of this.groups) {
            g.selected = this.allGroupsSelected;
        }
    }

    async createNewGroup() {
        // Disable setting group folder and provide a default path for it
        const params: UserGroupDialogParams = {
            // TODO set these in group management docSettings
            canChooseFolder: false,
            // TODO Should be same as 'groupsPath' as it should always be set
            defaultGroupFolder:
                this.settings.groupsPath ?? "sukol/2023/testikoulu",
            encodeGroupName: true,
        };
        // Create a new group
        const res = await to2(showUserGroupDialog(params));
        if (res.ok) {
            // add new group to list
            const group = res.result;

            this.groups.push({
                id: group.id,
                name: group.name,
                path: group.path,
                selected: false,
                allMembersSelected: false,
            });
        }
    }

    /**
     * Copies the selected Group (including memberships) into a new Group.
     * @param groups
     */
    async copyGroup(groups: Group[]) {
        let selected;
        for (let g of groups) {
            if (g.selected) {
                selected = g;
                break;
            }
        }
        if (selected) {
            const path = selected!.path;
            const folder = path.slice(
                path.indexOf("/") + 1,
                path.lastIndexOf("/")
            );
            const params: UserGroupDialogParams = {
                canChooseFolder: false,
                defaultGroupFolder: folder,
                encodeGroupName: true,
            };
            const res = await to2(showUserGroupDialog(params));
            if (res.ok) {
                const group: Group = {
                    id: res.result.id,
                    name: res.result.name,
                    path: res.result.path,
                    selected: false,
                    allMembersSelected: false,
                };
                this.groups.push(group);

                const copyres = await toPromise(
                    this.http.post(
                        `/groups/copymemberships/${selected.name}/${group.name}`,
                        {}
                    )
                );
                if (copyres.ok) {
                    // refresh group members
                    await this.getGroupMembers(group);
                }
            }
        }
    }

    /**
     * Delete selected groups from the database
     * @param groups
     */
    async deleteSelectedGroups(groups: Group[]) {
        // Delete selected groups
        let selected: Group[] = [];
        for (const g of groups) {
            if (g.selected) selected.push(g);
        }
        const confirmTitle = $localize`Delete groups`;
        const confirmMessage = $localize`Are you certain you wish to delete the following groups?\nThis action cannot be undone!\n\nGroups to be deleted:\n`;
        const confirmGroups = selected.map((g) => g.name).join("\n");

        const res = await to2<boolean>(
            showConfirm(confirmTitle, `${confirmMessage}${confirmGroups}`)
        );

        const group_ids = selected.map((g) => g.id);

        if (res.ok) {
            // call server group delete
            if (selected.length == 1) {
                const deleteResp = await toPromise(
                    this.http.delete(`/groups/delete/${selected[0].id}`)
                );
            } else if (selected.length > 1) {
                // mass delete groups
                const deleteResp = await toPromise(
                    this.http.delete(`/groups/delete`, {body: {ids: group_ids}})
                );
            } else {
                return;
            }
            // remove from group record
            selected.map((group) => delete this.members[group.name]);
            // remove group from groups array
            const indices = selected.map((group) => this.groups.indexOf(group));
            indices.map((index) => this.groups.splice(index, 1));

            // update ui
            await this.getGroups();
            this.selectedGroupTab = this.groups[0]?.name ?? "";
        }
    }

    generateLoginCodes(groups: Group[]) {
        // Generate temporary login codes for members of the currently selected groups
        // The codes are linked to the individual users via a database table
        // Should check for existing and still valid codes before refreshing
        // to avoid accidentally changing valid ones (also display a warning, perhaps in a dialog).
        // members.forEach((member) => {
        //     const loginkey = `${this.mockCurrentLoginKeyStart++}`;
        //     member.student_id = `${"0".repeat(9 - loginkey.length)}${loginkey}`;
        // });
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

    protected async addMembers(group: Group) {
        // Add members to the group in the currently active group members tab
        // Should probably add members only to the group in the active tab in the group members view
        // Should display a dialog where the user may provide a list of members to add
        // Support adding members via an existing UserGroup in addition to username, email, and [creating new users on the spot]
        // TODO refactor to enable import users en masse, via CSV for example
        const creationParams = {
            group: group.name,
            // Defaults to "Association" if not set in doc settings
            associationType: this.settings.associationType,
        };
        const resp = await to2(showUserCreationDialog(creationParams));
        if (resp.ok) {
            let newUser: GroupMember = resp.result;
            this.members[group.name].push(newUser);
        }
    }

    async addExistingMembers(groups: Group[]) {
        // TODO dialog and interface for selecting users from all existing groups
        //      so that managers don't have to create each user themselves (and also avoids
        //      having multiple accounts for each member/user
    }

    protected async removeMembers(group: Group) {
        // Remove selected members from the currently active group
        // Note: remember to clear selectedMembers after deletion from the group
        let selected = this.members[group.name].filter((m) => m.selected);

        const resp = await toPromise(
            this.http.post<{}>(`/groups/removemember/${group.name}`, {
                names: selected.map((mn) => mn.name),
            })
        );
        if (resp.ok) {
            let result: Record<string, string[]> = resp.result;
            const removed = result["removed"].join("\n");
            const not_in_group = result["does_not_belong"].join("\n");
            const not_exist = result["not_exist"].join("\n");

            let msg = `${removed ? "Removed members:\n" + removed + "\n" : ""}
                              ${
                                  not_in_group
                                      ? "Following users do not belong to group '" +
                                        group +
                                        "':\n" +
                                        not_in_group +
                                        "\n"
                                      : ""
                              }
                              ${
                                  not_exist
                                      ? "Could not find user(s):\n" + not_exist
                                      : ""
                              }`;
            await to2(showMessageDialog(msg));
            for (let s of selected) {
                this.members[group.name].splice(
                    this.members[group.name].indexOf(s),
                    1
                );
            }
        }
    }

    toggleMemberSelection(group: Group) {
        // if selecting a member results in all of the group's members being selected,
        // set the corresponding flag value to reflect that
        group.allMembersSelected = this.members[group.name].every(
            (m) => m.selected
        );
    }

    toggleAllMembersSelected(group: Group) {
        for (const m of this.members[group.name]) {
            m.selected = group.allMembersSelected;
        }
    }

    // TODO this is not needed since we generate separate group members controls for each group tab
    onGroupTabSelected(groupTab: TabDirective) {
        this.selectedGroupTab = groupTab.heading;
    }
}
