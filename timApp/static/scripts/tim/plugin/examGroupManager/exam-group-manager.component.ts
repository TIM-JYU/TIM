import {HttpClientModule} from "@angular/common/http";
import type {OnInit} from "@angular/core";
import {
    type ApplicationRef,
    Component,
    type DoBootstrap,
    Input,
    NgModule,
} from "@angular/core";
import type {IGroup, IUser} from "tim/user/IUser";
import {ADMIN_GROUPNAME} from "tim/user/IUser";
import {showUserGroupDialog} from "tim/user/showUserGroupDialog";
import {documentglobals} from "tim/util/globals";
import {isAdmin, Users} from "tim/user/userService";
import type {Require} from "tim/util/utils";
import {to2, toPromise} from "tim/util/utils";
import type {UserGroupDialogParams} from "tim/user/user-group-dialog.component";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import type {TabDirective} from "ngx-bootstrap/tabs";
import {TabsModule} from "ngx-bootstrap/tabs";
import {showConfirm} from "tim/ui/showConfirmDialog";
import type {ViewCtrl} from "tim/document/viewctrl";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import * as t from "io-ts";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    withDefault,
} from "tim/plugin/attributes";
import {UserImportDialogComponent} from "tim/plugin/examGroupManager/user-import-dialog.component";
import {showUserImportDialog} from "tim/plugin/examGroupManager/showUserImportDialog";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {UserCreationDialogComponent} from "tim/plugin/examGroupManager/user-creation-dialog.component";
import {showUserCreationDialog} from "tim/plugin/examGroupManager/showUserCreationDialog";
import {LoginCodeGenerationDialogComponent} from "tim/plugin/examGroupManager/login-code-generation-dialog.component";
import {showLoginCodeGenerationDialog} from "tim/plugin/examGroupManager/showLoginCodeGenerationDialog";

export interface GroupMember extends IUser {
    id: number;
    name: string;
    email: string;
    real_name: string;
    // additional information on the member not conveyed by other properties,
    // such as name of class, homegroup, etc.
    extra_info: string;
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

/**
 * Used to define which elements of the Group Management Component are visible.
 */
// type ViewOptions = Record<string, Record<string, boolean>>;
interface ViewOptions {
    groups: {
        selectionControls: boolean;
        name: boolean;
        document: boolean;
        fullDocPath: boolean;
        memberCount: boolean;
        event: boolean;
        timeslot: boolean;
    };
    members: {
        selectionControls: boolean;
        name: boolean;
        username: boolean;
        email: boolean;
        extra_info: boolean;
        logincode: boolean;
    };
}

const ExamManagerMarkup = t.intersection([
    t.type({
        managers: withDefault(t.array(t.string), []),
        groupsPath: withDefault(t.string, ""),
        showAllGroups: withDefault(t.boolean, false),
    }),
    t.partial({
        extraInfoTitle: t.string,
    }),
    GenericPluginMarkup,
]);

const ExamManagerFields = t.intersection([
    getTopLevelFields(ExamManagerMarkup),
    t.type({}),
]);

/**
 * A group management component, that can be used to manage groups and group members.
 * It is intended for 'one-off' type situations like course exams, where it is not desirable
 * to require complete user accounts for students (such as when the course itself is not held in TIM).
 * Instead, the component can be used to create minimal user accounts with temporary login codes for logging into TIM.
 *
 * Events (exams) can be added for groups, so that group permissions to the event documents can be propagated automatically.
 */
@Component({
    selector: "tim-exam-group-manager",
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
                                <th i18n *ngIf="this.viewOptions.groups.selectionControls">
                                    <input type="checkbox" name="selectAllGroups" [(ngModel)]="allGroupsSelected"
                                           (change)="toggleAllGroupsSelected()"/></th>
                                <th i18n *ngIf="this.viewOptions.groups.name">Group name</th>
                                <th *ngIf="isAdmin() && this.viewOptions.groups.document" i18n>Group document</th>
                                <th i18n *ngIf="this.viewOptions.groups.memberCount">Number of students</th>
                                <th i18n *ngIf="this.viewOptions.groups.event">Event</th>
                                <th i18n *ngIf="this.viewOptions.groups.timeslot">Timeslot</th>
                            </tr>
                            </thead>
                            <tbody>
                            <tr class="member-table-row" *ngFor="let group of groups">
                                <td *ngIf="this.viewOptions.groups.selectionControls">
                                    <input type="checkbox" name="toggleGroupSelection_{{group.id}}"
                                           [(ngModel)]="group.selected" (change)="toggleGroupSelection(group)"/>
                                </td>
                                <td *ngIf="this.viewOptions.groups.name">{{group.name}}</td>
                                <td *ngIf="isAdmin() && this.viewOptions.groups.document">
                                    <a href="/view/{{group.path}}">{{getGroupDocPath(group)}}</a>
                                </td>
                                <td *ngIf="this.viewOptions.groups.memberCount">{{getGroupMemberCount(group)}}</td>
                                <td *ngIf="this.viewOptions.groups.event"> -</td>
                                <td *ngIf="this.viewOptions.groups.timeslot"> -</td>
                            </tr>
                            </tbody>
                        </table>
                    </div>
                    <!-- END groups list-->

                </div>
                <!-- style="display: inline-block" -->
                <div id="groups-list-controls">
                    <div class="flex">
                        <button class="timButton" (click)="createNewGroup()" i18n>Create a new group</button>
                        <button class="timButton" (click)="copyGroup(this.groups)"
                                [disabled]="!oneSelected(this.groups)" i18n>Copy selected group
                        </button>
                        <button class="timButton btn-danger" (click)="deleteSelectedGroups(this.groups)"
                                [disabled]="!anySelected(this.groups)" i18n>Delete selected groups
                        </button>
                        <button class="timButton" (click)="generateLoginCodes()"
                                [disabled]="!anySelected(this.groups)" i18n>Generate login codes
                        </button>
                        <button class="timButton" (click)="printLoginCodes(this.groups)"
                                [disabled]="!anySelected(this.groups)" i18n>Print login codes
                        </button>
                    </div>
                    <tim-alert severity="success"
                               *ngIf="saveGroupSuccessMessage">{{saveGroupSuccessMessage}}</tim-alert>
                    <tim-alert severity="danger" *ngIf="saveGroupFailMessage">{{saveGroupFailMessage}}</tim-alert>
                </div>
                <div>
                    <ng-container *ngIf="anySelected(this.groups)">
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
                                                <th i18n *ngIf="this.viewOptions.members.selectionControls">
                                                    <input type="checkbox" name="selectAllMembers_{{group.id}}"
                                                           [(ngModel)]="group.allMembersSelected"
                                                           (change)="toggleAllMembersSelected(group)"/></th>
                                                <th i18n *ngIf="this.viewOptions.members.name">Name</th>
                                                <th i18n *ngIf="this.viewOptions.members.username">Username</th>
                                                <th i18n
                                                    *ngIf="this.viewOptions.members.extra_info">{{this.markup['extraInfoTitle'] ?? "Extra info"}}</th>

                                                <th i18n *ngIf="this.viewOptions.members.email">Email</th>
                                                <th i18n *ngIf="this.viewOptions.members.logincode">Login code</th>
                                            </tr>
                                            </thead>
                                            <tbody>
                                            <tr class="member-table-row" *ngFor="let member of members[group.name]">
                                                <td *ngIf="this.viewOptions.members.selectionControls">
                                                    <input type="checkbox" name="toggleSelection_{{member.id}}"
                                                           [(ngModel)]="member.selected"
                                                           (change)="toggleMemberSelection(group)"/>
                                                </td>
                                                <td *ngIf="this.viewOptions.members.name">{{member.real_name}}</td>
                                                <td *ngIf="this.viewOptions.members.username">{{member.name}}</td>
                                                <td *ngIf="this.viewOptions.members.extra_info">{{member.extra_info}}</td>
                                                <td *ngIf="this.viewOptions.members.email">{{member.email}}</td>
                                                <td *ngIf="this.viewOptions.members.logincode">{{member.login_code}}</td>
                                            </tr>
                                            </tbody>
                                        </table>
                                    </div>
                                    <!-- END members list-->

                                </ng-container>
                                <ng-container>
                                    <div id="members-controls">
                                        <button class="timButton" (click)="addMembers(group)" i18n>
                                            Add new student
                                        </button>
                                        <button class="timButton" (click)="importUsers(group)" i18n>
                                            Import students from Wilma
                                        </button>
                                        <br/>
                                        <ng-container *ngIf="this.groups.length > 1">
                                            <span class="copy-members-field">
                                                <button class="timButton" (click)="copyMembers(this.copyMembersTarget)"
                                                        i18n [disabled]="!anySelected(this.members[group.name])">
                                                    Copy students to group
                                                </button>
                                                <select id="copyMembersTarget" name="copyMembersTarget"
                                                        class="form-control"
                                                        [(ngModel)]="this.copyMembersTarget">
                                                    <option *ngFor="let group of this.groups"
                                                            value="{{group.id}}">{{group.name}}</option>
                                                </select>
                                            </span>
                                        </ng-container>
                                        <button class="timButton btn-danger" (click)="removeMembers(group)"
                                                [disabled]="!anySelected(this.members[group.name])" i18n>
                                            Remove selected
                                        </button>
                                        <tim-alert severity="success"
                                                   *ngIf="saveMembersSuccessMessage">{{saveMembersSuccessMessage}}</tim-alert>
                                        <tim-alert severity="danger"
                                                   *ngIf="saveMembersFailMessage">{{saveMembersFailMessage}}</tim-alert>
                                    </div>
                                </ng-container>
                                <ng-container *ngIf="anySelected(this.members[group.name])">
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

        </ng-container>
    `,
    styleUrls: ["exam-group-manager.component.scss"],
})
export class ExamGroupManagerComponent
    extends AngularPluginBase<
        t.TypeOf<typeof ExamManagerMarkup>,
        t.TypeOf<typeof ExamManagerFields>,
        typeof ExamManagerFields
    >
    implements OnInit
{
    requiresTaskId = false;
    viewOptions!: ViewOptions;
    showAllGroups: boolean = false;
    // Groups that can manage login code groups, specified with document setting `groups`
    managers: Group[] = [];
    // Groups that are managed via the group management document
    groups: Group[] = [];
    // Members visible on the currently active group tab (in the group members table)
    members: Record<string, GroupMember[]> = {};

    // Currently selected groups
    allGroupsSelected: boolean = false;
    saveGroupSuccessMessage?: string;
    saveGroupFailMessage?: string;

    // currently selected members
    selectedGroupTab?: string;
    saveMembersSuccessMessage?: string;
    saveMembersFailMessage?: string;

    // which group to add selected members into
    copyMembersTarget: number = -1;

    // status info for group members table
    savingGroupMembers?: boolean;

    @Input() eventHeading?: string;
    private viewctrl!: Require<ViewCtrl>;

    private initOpts() {
        this.viewctrl = vctrlInstance!;
        // this.settings = this.viewctrl.docSettings.groupManagement ?? {}; // ngOnInit
        this.showAllGroups = false;
        this.allGroupsSelected = false;
        // TODO set these with component/plugin attributes
        this.viewOptions = {
            groups: {
                selectionControls: true,
                name: true,
                document: true,
                fullDocPath: false,
                memberCount: true,
                event: false,
                timeslot: true,
            },
            members: {
                selectionControls: true,
                name: true,
                username: true,
                email: false,
                extra_info: true,
                logincode: true,
            },
        };
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

    getAttributeType() {
        return ExamManagerFields;
    }

    getDefaultMarkup() {
        return {};
    }

    /**
     * Initialization procedures.
     */
    async ngOnInit() {
        this.initOpts();
        // Load content only for logged in users who are also owners of this management document
        const isDocOwner = await toPromise(
            this.http.get(`/loginCode/checkOwner/${this.getDocId()}`)
        );
        if (!isAdmin() && (!isDocOwner.ok || !Users.isLoggedIn())) {
            return;
        }

        await this.getGroups();
        this.selectedGroupTab = this.groups[0]?.name ?? "";

        for (const g of this.groups) {
            await this.getGroupMembers(g);
        }
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
            const showAllGroups: string = this.showAllGroups
                ? `?showAllGroups=${this.showAllGroups.toString()}`
                : "";
            const groups = await toPromise<Group[]>(
                this.http.get<Group[]>(
                    `/loginCode/groups/${doc_id}${showAllGroups}`
                )
            );
            if (groups.ok) {
                this.groups = groups.result;
            }
        }
    }

    getGroupDocPath(group: Group): string {
        if (group !== undefined) {
            if (this.viewOptions.groups.fullDocPath) {
                return group.path!;
            }
            return group.path!.slice(
                group.path!.lastIndexOf("/") + 1,
                group.path!.length
            );
        }
        return "";
    }

    /**
     * Checks that at least one Group or GroupMember is selected.
     * @param selectables list of selectable Groups or GroupMembers
     */
    anySelected(selectables: GroupMember[] | Group[]) {
        return selectables?.some((s) => s.selected) ?? false;
    }

    /**
     * Checks that exactly one Group is selected.
     * @param groups list of selectable Groups
     */
    oneSelected(groups: Group[]) {
        let count = 0;
        for (const g of groups) {
            if (g.selected) {
                count++;
            }
            if (count > 1) {
                return false;
            }
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

    private getSelectedGroups(): Group[] {
        const selected: Group[] = [];
        for (const g of this.groups) {
            if (g.selected) {
                selected.push(g);
            }
        }
        return selected;
    }

    getGroupMemberCount(group: Group): number {
        return this.members[group.name] !== undefined
            ? this.members[group.name].length
            : 0;
    }

    private async getMembersFromSelectedGroups() {
        const groups = this.getSelectedGroups();
        const group_ids = groups.map((g) => g.id);

        const url = `/loginCode/members/from_groups`;
        const response = await toPromise(
            this.http.post<GroupMember[]>(url, {
                ids: group_ids,
            })
        );
        if (response.ok) {
            return response.result;
        }
    }

    async createNewGroup() {
        // Disable setting group folder and provide a default path for it
        // TODO: give owner permissions to the created group
        //  for groups listed under the doc setting managers,
        //  so that we don't have to set permissions manually
        const params: UserGroupDialogParams = {
            // TODO set these in group management docSettings
            canChooseFolder: false,
            // TODO Should be same as 'groupsPath' as it should always be set
            defaultGroupFolder: this.markup.groupsPath,
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
        let selected: Group | undefined;
        for (const g of groups) {
            if (g.selected) {
                selected = g;
                break;
            }
        }
        if (selected) {
            const path = selected.path;
            const folder = path
                ? path.slice(path.indexOf("/") + 1, path.lastIndexOf("/"))
                : "";
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
        const selected: Group[] = this.getSelectedGroups();
        const confirmTitle = $localize`Delete groups`;
        const confirmMessage = $localize`Are you certain you wish to delete the following groups?\nThis action cannot be undone!\n\nGroups to be deleted:\n`;
        const confirmGroups = selected.map((g) => g.name).join("\n");

        const res = await to2<boolean>(
            showConfirm(confirmTitle, `${confirmMessage}${confirmGroups}`)
        );

        const group_ids = selected.map((g) => g.id);

        if (res.ok) {
            if (!selected.length) {
                return;
            } else {
                await toPromise(
                    this.http.delete(`/groups/delete`, {body: {ids: group_ids}})
                );
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

    async generateLoginCodes() {
        // Generate temporary login codes for members of the currently selected groups
        // The codes are linked to the individual users via a database table
        // TODO: Should check for existing and still valid codes before refreshing
        //       to avoid accidentally changing valid ones (also display a warning).
        // TODO: dialog that allows to set the activation_start and activation_end properties
        const members = await this.getMembersFromSelectedGroups();
        if (members !== undefined) {
            const params = {members: members};
            const res = await to2(showLoginCodeGenerationDialog(params));
            if (res.ok) {
                // fetch login codes for the UI
            }

            // const url = `/loginCode/generateCodes`;
            // const response = await toPromise(
            //     this.http.post<UserCode[]>(url, {
            //         members: members,
            //     })
            // );
            // if (response.ok) {
            //     // const usercodes = response.result;
            //     // for (const code of usercodes) {
            //     //     const m = members.find((me) => me.id == code.id);
            //     //     if (m !== undefined) {
            //     //         m.login_code = code.code;
            //     //     }
            //     // }
            //     //
            //     // const msg = `${"Created user codes:\n" + usercodes}`;
            //     // await to2(showMessageDialog(msg));
            // }
        }
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
            this.http.get<GroupMember[]>(`/loginCode/members/${group.id}`)
        );
        if (resp.ok) {
            this.members[group.name] = resp.result;
        }
    }

    /**
     * Add members to the group in the currently active group members tab
     * @param group currently active group
     */
    async addMembers(group: Group) {
        const creationDialogParams = {
            group: group.id,
            // Defaults to "Extra info" if not set in doc settings
            extra_info: this.markup.extraInfoTitle,
            hiddenFields: ["username", "email"],
        };
        const resp = await to2(showUserCreationDialog(creationDialogParams));
        if (resp.ok) {
            const newUser: GroupMember = resp.result;
            this.members[group.name].push(newUser);
        }
    }

    /**
     * Opens a dialog where users can input a formatted list of user information.
     * The list will then be parsed into user attributes, which are used to create new users.
     * The new users will then be added to the specified group.
     * @param group Group into which new users will be added as members.
     */
    async importUsers(group: Group) {
        const importDialogParams = {
            group: group.id,
        };
        const resp = await to2(showUserImportDialog(importDialogParams));
        if (resp.ok) {
            const newUsers: GroupMember[] = resp.result;
            this.members[group.name].push(...newUsers);
        }
    }

    async copyMembers(group_id: number) {
        const source: Group = this.groups.filter(
            (g) => g.name === this.selectedGroupTab
        )[0];
        // FIXME: the value returned from option.value is not really a number, but a string
        const target: Group = this.groups.filter(
            (g) => String(g.id) === String(group_id)
        )[0];

        if (source !== undefined && target !== undefined) {
            const selected = this.getSelectedMembers(source);
            const uids = selected.map((m) => m.id);
            const url = `/loginCode/addManyMembers/${group_id}`;
            const b = {ids: uids};
            const response = await toPromise(
                this.http.post<GroupMember[]>(url, b)
            );
            if (response.ok) {
                const newMembers = response.result;
                this.members[target.name].push(...newMembers);
            }
        }
    }

    async removeMembers(group: Group) {
        // Remove selected members from the currently active group
        // Note: remember to clear selectedMembers after deletion from the group
        const selected = this.getSelectedMembers(group);

        const resp = await toPromise(
            this.http.post<Record<string, string[]>>(
                `/groups/removemember/${group.name}`,
                {
                    names: selected.map((mn) => mn.name),
                }
            )
        );
        if (resp.ok) {
            const result: Record<string, string[]> = resp.result;
            const removed = result.removed.join("\n");
            const not_in_group = result.does_not_belong.join("\n");
            const not_exist = result.not_exist.join("\n");

            const msg = `${removed ? "Removed members:\n" + removed + "\n" : ""}
                              ${
                                  not_in_group
                                      ? "Following users do not belong to group '" +
                                        group.name +
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
            for (const s of selected) {
                this.members[group.name].splice(
                    this.members[group.name].indexOf(s),
                    1
                );
            }
        }
    }

    private getSelectedMembers(group: Group): GroupMember[] {
        return this.members[group.name].filter((m) => m.selected);
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

@NgModule({
    declarations: [
        ExamGroupManagerComponent,
        UserImportDialogComponent,
        UserCreationDialogComponent,
        LoginCodeGenerationDialogComponent,
    ],
    exports: [ExamGroupManagerComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        // TimTableModule,
        TabsModule.forRoot(),
        DialogModule,
    ],
})
export class ExamGroupManagerModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

registerPlugin(
    "tim-exam-group-manager",
    ExamGroupManagerModule,
    ExamGroupManagerComponent
);
