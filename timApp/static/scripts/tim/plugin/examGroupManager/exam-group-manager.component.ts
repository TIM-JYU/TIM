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
import {documentglobals, someglobals} from "tim/util/globals";
import {isAdmin, Users} from "tim/user/userService";
import type {Require} from "tim/util/utils";
import {to2, toPromise} from "tim/util/utils";
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
import {ExamGroupCreateDialogComponent} from "tim/plugin/examGroupManager/exam-group-create-dialog.component";
import {showExamGroupCreateDialog} from "tim/plugin/examGroupManager/showExamGroupCreateDialog";
import {PurifyModule} from "tim/util/purify.module";

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

export interface ExamGroup extends IGroup {
    // Group's doc path, eg. 'test/group01'
    admin_doc_path?: string;
    readableName: string;
    events?: GroupEvent[];
    isDirectOwner: boolean;
    selected?: boolean;
    allMembersSelected?: boolean;
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

const ViewOptionsT = t.partial({
    groups: t.partial({
        selectionControls: t.boolean,
        name: t.boolean,
        document: t.boolean,
        fullDocPath: t.boolean,
        memberCount: t.boolean,
        event: t.boolean,
        timeslot: t.boolean,
    }),
    members: t.partial({
        selectionControls: t.boolean,
        name: t.boolean,
        username: t.boolean,
        email: t.boolean,
        extraInfo: t.boolean,
        loginCode: t.boolean,
    }),
});

interface ViewOptions extends t.TypeOf<typeof ViewOptionsT> {}

const ExamManagerMarkup = t.intersection([
    t.type({
        groupsPath: withDefault(t.string, ""),
        showAllGroups: withDefault(t.boolean, false),
    }),
    t.partial({
        extraInfoTitle: t.string,
        show: ViewOptionsT,
        groupNamePrefix: t.string,
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
        <form>
            <div class="info-box">
                <tim-alert *ngIf="error">
                    <div [innerHTML]="error | purify"></div>
                </tim-alert>
                <span *ngIf="loading" class="loading-spinner">
                    <tim-loading></tim-loading> 
                    <ng-container i18n>Loading, please wait...</ng-container>
                </span>
            </div>
            <fieldset [disabled]="loading">
                <bootstrap-panel id="groups-panel" title="All exam groups" i18n-title>
                    <div id="list-all-groups-setting">
                        <label for="showAllGroups" i18n>
                            <input type="checkbox" id="showAllGroups" [(ngModel)]="showAllGroups"
                                   (ngModelChange)="refreshVisibleGroups()"/>
                            <span>Show all school's exam groups</span>
                        </label>
                    </div>
                    <div id="groups-list">
                        <!-- List groups here, include checkboxes for selecting groups to manage -->

                        <!-- Mock groups list -->
                        <!-- TODO: Implement as TimTable -->
                        <!-- TODO: Event and Timeslot should be combined in the future,
                                   so that we can list multiple events for a specific group
                                   in a sensible way. -->
                        <table class="group-table">
                            <thead>
                            <tr class="member-table-row">
                                <th i18n *ngIf="this.viewOptions?.groups?.name">Group name</th>
                                <th *ngIf="isAdmin() && this.viewOptions?.groups?.document" i18n>Group document</th>
                                <th i18n *ngIf="this.viewOptions?.groups?.memberCount">Number of students</th>
                                <th i18n *ngIf="this.viewOptions?.groups?.event">Event</th>
                                <th i18n *ngIf="this.viewOptions?.groups?.timeslot">Timeslot</th>
                                <th i18n>Actions</th>
                                <th i18n>Delete</th>
                            </tr>
                            </thead>
                            <tbody>
                            <tr class="member-table-row" *ngFor="let group of visibleGroups">
                                <td *ngIf="this.viewOptions?.groups?.name">{{ group.readableName }}</td>
                                <td *ngIf="isAdmin() && this.viewOptions?.groups?.document">
                                    <a href="/view/{{group.admin_doc_path}}">{{ getGroupDocPath(group) }}</a>
                                </td>
                                <td *ngIf="this.viewOptions?.groups?.memberCount">{{ getGroupMemberCount(group) }}</td>
                                <td *ngIf="this.viewOptions?.groups?.event"> -</td>
                                <td *ngIf="this.viewOptions?.groups?.timeslot"> -</td>
                                <td>
                                    <button
                                            class="btn btn-primary btn-xs"
                                            title="Duplicate group"
                                            i18n-title
                                            (click)="copyGroup(group)"
                                    >
                                        <i class="glyphicon glyphicon-duplicate"></i>
                                    </button>
                                </td>
                                <td>
                                    <button
                                            class="btn btn-danger btn-xs"
                                            title="Delete group"
                                            (click)="deleteGroup(group)"
                                    >
                                        <i class="glyphicon glyphicon-trash"></i>
                                    </button>
                                </td>
                            </tr>
                            </tbody>
                        </table>
                        <!-- END groups list-->
                    </div>
                    <div class="groups-list-controls">
                        <button class="timButton" (click)="createNewGroup()" i18n>Create a new exam group</button>
                    </div>
                    <!--                <div>-->
                    <!--                    <ng-container *ngIf="anySelected(this.groups)">-->
                    <!--                        <p>Selected groups:</p>-->
                    <!--                        <ol>-->
                    <!--                            <ng-container *ngFor="let group of groups">-->
                    <!--                                <li *ngIf="group.selected">{{group.name}}</li>-->
                    <!--                            </ng-container>-->
                    <!--                        </ol>-->
                    <!--                    </ng-container>-->
                    <!--                </div>-->
                </bootstrap-panel>
                <bootstrap-panel title="Group members" i18n-title>
                    <tabset class="merged">
                        <!-- Create a new tab for each group that is visible to the current user -->
                        <tab *ngFor="let group of visibleGroups" heading="{{group.readableName}}"
                             class="grid-tab tab-form" (selectTab)="onGroupTabSelected($event)">
                            <ng-container>
                                <!-- Member list, with sort and selection controls -->

                                <div>
                                    <!-- TODO: Implement as TimTable -->
                                    <table class="group-table">
                                        <thead>
                                        <tr class="member-table-row">
                                            <th i18n *ngIf="this.viewOptions?.members?.selectionControls">
                                                <input type="checkbox" name="selectAllMembers_{{group.id}}"
                                                       [(ngModel)]="group.allMembersSelected"
                                                       (change)="toggleAllMembersSelected(group)"/></th>
                                            <th i18n *ngIf="this.viewOptions?.members?.name">Name</th>
                                            <th i18n *ngIf="this.viewOptions?.members?.username">Username</th>
                                            <th i18n
                                                *ngIf="this.viewOptions?.members?.extraInfo">{{ this.markup['extraInfoTitle'] ?? "Extra info" }}
                                            </th>

                                            <th i18n *ngIf="this.viewOptions?.members?.email">Email</th>
                                            <th i18n *ngIf="this.viewOptions?.members?.loginCode">Login code
                                            </th>
                                        </tr>
                                        </thead>
                                        <tbody>
                                        <tr class="member-table-row" *ngFor="let member of members[group.name]">
                                            <td *ngIf="this.viewOptions?.members?.selectionControls">
                                                <input type="checkbox" name="toggleSelection_{{member.id}}"
                                                       [(ngModel)]="member.selected"
                                                       (change)="toggleMemberSelection(group)"/>
                                            </td>
                                            <td *ngIf="this.viewOptions?.members?.name">{{ member.real_name }}</td>
                                            <td *ngIf="this.viewOptions?.members?.username">{{ member.name }}</td>
                                            <td *ngIf="this.viewOptions?.members?.extraInfo">{{ member.extra_info }}</td>
                                            <td *ngIf="this.viewOptions?.members?.email">{{ member.email }}</td>
                                            <td *ngIf="this.viewOptions?.members?.loginCode">{{ member.login_code }}</td>
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
                                    <button class="timButton" (click)="generateLoginCodes()"
                                            [disabled]="!anySelected(this.visibleGroups)" i18n>Generate login
                                        codes
                                    </button>
                                    <button class="timButton" (click)="printLoginCodes(this.visibleGroups)"
                                            [disabled]="!anySelected(this.visibleGroups)" i18n>Print login codes
                                    </button>
                                    <button class="timButton btn-danger" (click)="removeMembers(group)"
                                            [disabled]="!anySelected(this.members[group.name])" i18n>
                                        Remove selected
                                    </button>
                                    <tim-alert severity="success"
                                               *ngIf="saveMembersSuccessMessage">{{ saveMembersSuccessMessage }}
                                    </tim-alert>
                                    <tim-alert severity="danger"
                                               *ngIf="saveMembersFailMessage">{{ saveMembersFailMessage }}
                                    </tim-alert>
                                </div>
                            </ng-container>
                            <ng-container *ngIf="anySelected(this.members[group.name])">
                                <p>Selected members:</p>
                                <ol>
                                    <ng-container *ngFor="let member of this.members[group.name]">
                                        <li *ngIf="member.selected">{{ member.name }}</li>
                                    </ng-container>
                                </ol>
                            </ng-container>
                        </tab>
                    </tabset>
                </bootstrap-panel>
            </fieldset>
        </form>
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
    viewOptions?: ViewOptions;
    showAllGroups: boolean = false;
    // Groups that can manage login code groups, specified with document setting `groups`
    managers: ExamGroup[] = [];
    // Groups that are managed via the group management document
    visibleGroups: ExamGroup[] = [];
    allGroups: ExamGroup[] = [];
    // Members visible on the currently active group tab (in the group members table)
    members: Record<string, GroupMember[]> = {};
    error?: string;

    // Currently selected groups
    allGroupsSelected: boolean = false;

    // currently selected members
    selectedGroupTab?: string;
    saveMembersSuccessMessage?: string;
    saveMembersFailMessage?: string;

    // which group to add selected members into
    copyMembersTarget: number = -1;

    // status info for group members table
    loading: boolean = false;

    @Input() eventHeading?: string;
    private viewctrl!: Require<ViewCtrl>;

    private initOpts() {
        this.viewctrl = vctrlInstance!;
        // this.settings = this.viewctrl.docSettings.groupManagement ?? {}; // ngOnInit
        this.showAllGroups = false;
        this.allGroupsSelected = false;
        this.viewOptions = this.markup.show;
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
        super.ngOnInit();
        this.initOpts();

        const globals = someglobals();
        const rights = globals.curr_item?.rights;
        if (!isAdmin() && !Users.isLoggedIn() && !rights?.owner) {
            // TODO: Emit error message
            return;
        }

        await this.getGroups();
        this.selectedGroupTab = this.visibleGroups[0]?.name ?? "";

        // for (const g of this.visibleGroups) {
        //     await this.getGroupMembers(g);
        // }
    }

    /**
     * Fetches the groups that the current user is allowed to manage.
     * Note: current user must be member of one the document's manager groups
     *       specified with the document setting `groupManagement.managers`.
     */
    async getGroups() {
        this.loading = true;
        this.error = undefined;
        // Filter out groups to which the current user does not have owner rights,
        // unless the (UI option? could be doc setting as well?) option 'List all existing groups' is active.
        // In general, group managers should not have access to groups they did not create,
        // but there are exceptions (for instance, a group manager might need to be substituted suddenly).
        const groups = await toPromise(
            this.http.get<ExamGroup[]>(`/examGroupManager/groups`, {
                params: {
                    ...this.getPar()!.par.getJsonForServer(),
                },
            })
        );
        if (groups.ok) {
            this.allGroups = groups.result;
        } else {
            this.error = $localize`Could not fetch groups. Details: ${groups.result.error}`;
        }
        this.refreshVisibleGroups();
        this.loading = false;
    }

    getGroupDocPath(group: ExamGroup): string {
        if (group !== undefined) {
            if (this.viewOptions?.groups?.fullDocPath) {
                return group.admin_doc_path!;
            }
            return group.admin_doc_path!.slice(
                group.admin_doc_path!.lastIndexOf("/") + 1,
                group.admin_doc_path!.length
            );
        }
        return "";
    }

    /**
     * Checks that at least one Group or GroupMember is selected.
     * @param selectables list of selectable Groups or GroupMembers
     */
    anySelected(selectables: GroupMember[] | ExamGroup[]) {
        return selectables?.some((s) => s.selected) ?? false;
    }

    /**
     * Checks that exactly one Group is selected.
     * @param groups list of selectable Groups
     */
    oneSelected(groups: ExamGroup[]) {
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

    refreshVisibleGroups() {
        this.visibleGroups = this.showAllGroups
            ? this.allGroups
            : this.allGroups.filter((g) => g.isDirectOwner);
        if (
            !this.selectedGroupTab ||
            !this.visibleGroups.find((g) => g.name === this.selectedGroupTab)
        ) {
            this.selectedGroupTab = this.visibleGroups[0].name;
        }
    }

    toggleGroupSelection(group: ExamGroup) {
        this.allGroupsSelected = this.visibleGroups.every((g) => g.selected);
    }

    toggleAllGroupsSelected() {
        for (const g of this.visibleGroups) {
            g.selected = this.allGroupsSelected;
        }
    }

    private getSelectedGroups(): ExamGroup[] {
        const selected: ExamGroup[] = [];
        for (const g of this.visibleGroups) {
            if (g.selected) {
                selected.push(g);
            }
        }
        return selected;
    }

    getGroupMemberCount(group: ExamGroup): number {
        return this.members[group.name] !== undefined
            ? this.members[group.name].length
            : 0;
    }

    private async getMembersFromSelectedGroups() {
        const groups = this.getSelectedGroups();
        const group_ids = groups.map((g) => g.id);

        const url = `/examGroupManager/members/from_groups`;
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
        const res = await to2(
            showExamGroupCreateDialog({
                folderPath: this.markup.groupsPath,
                groupPrefix: this.markup.groupNamePrefix,
            })
        );
        if (!res.ok) {
            return;
        }
        this.allGroups.push(res.result);
        this.refreshVisibleGroups();
    }

    /**
     * Copies the selected Group (including memberships) into a new Group.
     */
    async copyGroup(fromGroup: ExamGroup) {
        const res = await to2(
            showExamGroupCreateDialog({
                folderPath: this.markup.groupsPath,
                groupPrefix: this.markup.groupNamePrefix,
            })
        );
        if (!res.ok) {
            return;
        }
        this.loading = true;
        this.error = undefined;
        const toGroup = res.result;
        this.allGroups.push(toGroup);
        this.refreshVisibleGroups();
        const copyRes = await toPromise(
            this.http.post(`/examGroupManager/copyMembers`, {
                from_id: fromGroup.id,
                to_id: toGroup.id,
            })
        );
        if (!copyRes.ok) {
            await showMessageDialog(
                $localize`Could not copy group members. Details: ${copyRes.result.error}`
            );
            this.loading = false;
            return;
        }
        await this.getGroupMembers(toGroup);
        this.loading = false;
    }

    /**
     * Delete the exam group.
     */
    async deleteGroup(group: ExamGroup) {
        this.error = undefined;
        const confirmTitle = $localize`Delete exam group`;
        const confirmMessage = $localize`Are you sure you want to delete group '${group.readableName}'?\nThis action cannot be undone!\n`;

        const deleteOk = await showConfirm(confirmTitle, `${confirmMessage}`);
        if (!deleteOk) {
            return;
        }

        this.loading = true;
        const res = await toPromise(
            this.http.post(`/examGroupManager/deleteGroup`, {
                group_id: group.id,
            })
        );
        if (!res.ok) {
            await showMessageDialog(
                $localize`Could not delete group. Details ${res.result.error}`
            );
            this.loading = false;
            return;
        }
        // update ui
        await this.getGroups();
        this.loading = false;
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

            // const url = `/examGroupManager/generateCodes`;
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
    async getGroupMembers(group: ExamGroup) {
        this.loading = true;
        this.error = undefined;
        const resp = await toPromise(
            this.http.get<GroupMember[]>(
                `/examGroupManager/members/${group.id}`
            )
        );
        this.loading = false;
        if (resp.ok) {
            this.members[group.name] = resp.result;
        } else {
        }
    }

    /**
     * Add members to the group in the currently active group members tab
     * @param group currently active group
     */
    async addMembers(group: ExamGroup) {
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
    async importUsers(group: ExamGroup) {
        const importDialogParams = {
            group: group.id,
        };
        const resp = await to2(showUserImportDialog(importDialogParams));
        if (resp.ok) {
            const newUsers: GroupMember[] = resp.result;
            this.members[group.name].push(...newUsers);
        }
    }

    async removeMembers(group: ExamGroup) {
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

    private getSelectedMembers(group: ExamGroup): GroupMember[] {
        return this.members[group.name].filter((m) => m.selected);
    }

    toggleMemberSelection(group: ExamGroup) {
        // if selecting a member results in all of the group's members being selected,
        // set the corresponding flag value to reflect that
        group.allMembersSelected = this.members[group.name].every(
            (m) => m.selected
        );
    }

    toggleAllMembersSelected(group: ExamGroup) {
        for (const m of this.members[group.name]) {
            m.selected = group.allMembersSelected ?? false;
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
        ExamGroupCreateDialogComponent,
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
        PurifyModule,
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
