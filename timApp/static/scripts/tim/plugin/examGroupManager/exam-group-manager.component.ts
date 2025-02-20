import {HttpClientModule} from "@angular/common/http";
import type {OnInit, PipeTransform} from "@angular/core";
import {EventEmitter, Output} from "@angular/core";
import {Pipe} from "@angular/core";
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
import {timeout} from "tim/util/utils";
import {formatNumberCode} from "tim/util/utils";
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
import {ExamGroupCreateDialogComponent} from "tim/plugin/examGroupManager/exam-group-create-dialog.component";
import {showExamGroupCreateDialog} from "tim/plugin/examGroupManager/showExamGroupCreateDialog";
import {PurifyModule} from "tim/util/purify.module";
import {SPLIT_EVERY} from "tim/user/user-code-login.component";
import {ButtonsModule} from "ngx-bootstrap/buttons";

export interface GroupMember extends IUser {
    id: number;
    name: string;
    email: string;
    real_name: string;
    // additional information on the member not conveyed by other properties,
    // such as name of class, homegroup, etc.
    extraInfo?: string;
    // login codes will need to be treated like passwords
    login_code?: string;
    extraTime?: boolean;
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
    examDocId?: number;
    memberCount: number;
    currentExamDoc?: number;
    examState: number;
    loginCodesActive: boolean;
    examStarted: boolean;
    examMainGroupEnded: boolean;
    examEnded: boolean;
    studentsLoggedIn: boolean;
    studentsReady: boolean;
    accessAnswersTo?: string;
    allowAccess: boolean;
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
        exam: t.boolean,
        timeslot: t.boolean,
    }),
    members: t.partial({
        selectionControls: t.boolean,
        name: t.boolean,
        username: t.boolean,
        email: t.boolean,
        extraInfo: t.boolean,
        loginCode: t.boolean,
        extraTime: t.boolean,
    }),
});

interface ViewOptions extends t.TypeOf<typeof ViewOptionsT> {}

const ExamT = t.type({
    name: t.string,
    docId: t.number,
    url: t.union([t.string, t.null]),
});

const ExamWithPracticeT = t.intersection([
    ExamT,
    t.partial({
        practice: t.union([ExamT, t.null]),
    }),
]);

export interface Exam extends t.TypeOf<typeof ExamT> {}

export interface ExamWithPractice extends t.TypeOf<typeof ExamWithPracticeT> {}

const ExamManagerMarkup = t.intersection([
    t.type({
        groupsPath: withDefault(t.string, ""),
        showAllGroups: withDefault(t.boolean, false),
        exams: withDefault(t.array(ExamWithPracticeT), []),
    }),
    t.partial({
        extraInfoTitle: t.string,
        show: ViewOptionsT,
        groupNamePrefix: t.string,
        practiceExam: ExamT,
    }),
    GenericPluginMarkup,
]);

const ExamManagerFields = t.intersection([
    getTopLevelFields(ExamManagerMarkup),
    t.type({}),
]);

@Pipe({
    name: "formatLoginCode",
    pure: true,
})
class FormatLoginCodePipe implements PipeTransform {
    transform(value?: string): string | undefined {
        if (!value) {
            return value;
        }
        return formatNumberCode(value, SPLIT_EVERY);
    }
}

@Component({
    selector: "tim-toggle",
    template: `
        <div class="btn-group" [class.btn-group-xs]="!enabledButton || !disabledButton">
            <ng-container *ngIf="!enabledButton || !disabledButton; else customButtons">
                <button [disabled]="disabled" type="button" class="btn"
                        [class.btn-default]="value" [class.btn-danger]="!value"
                        (click)="toggle()"
                >
                    <code>O</code>
                </button>
                <button [disabled]="disabled" type="button" class="btn"
                        [class.btn-default]="!value" [class.btn-success]="value"
                        (click)="toggle()"
                >
                    <code>I</code>
                </button>
            </ng-container>
            <ng-template #customButtons>
                <button [disabled]="disabled" type="button" 
                        class="btn" 
                        [class.btn-success]="!value" 
                        [class.btn-danger]="value" 
                        (click)="toggle()">
                    {{!value ? enabledButton : disabledButton}}
                </button>
            </ng-template>
        </div>
    `,
    styles: [
        `
            code {
                font-size: 1.8em !important;
            }
            button {
                outline: none !important;
            }
        `,
    ],
})
export class ToggleComponent {
    @Input() value: boolean = false;
    @Input() enabledButton?: string;
    @Input() disabledButton?: string;
    @Output() valueChange: EventEmitter<boolean> = new EventEmitter();

    @Input() disabled: boolean = false;

    toggle() {
        this.value = !this.value;
        this.valueChange.emit(this.value);
    }
}

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
                <span [class.invisible]="!loading" class="loading-spinner">
                    <tim-loading></tim-loading> 
                    <ng-container i18n>Loading, please wait...</ng-container>
                </span>
            </div>
            <fieldset [disabled]="loading">
                <bootstrap-panel id="groups-panel" title="1. Manage exam groups" i18n-title>
                    <div id="list-all-groups-setting">
                        <label for="showAllGroups">
                            <input type="checkbox" id="showAllGroups" name="showAllGroups" [(ngModel)]="showAllGroups"
                                   (ngModelChange)="refreshVisibleGroups()"/>
                            <span i18n>Show all school's exam groups</span>
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
                                <th i18n *ngIf="this.viewOptions?.groups?.exam">Exam</th>
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
                                <td *ngIf="this.viewOptions?.groups?.memberCount">{{ group.memberCount }}</td>
                                <td *ngIf="this.viewOptions?.groups?.exam">{{ examByDocId.get(group.examDocId ?? -1)?.name ?? "-" }}</td>
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
                                            i18n-title
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
                    <div class="button-controls">
                        <button class="timButton" (click)="createNewGroup()" i18n>Create a new exam group</button>
                    </div>
                </bootstrap-panel>
                <bootstrap-panel title="2. Manage students" i18n-title>
                    <tabset class="merged">
                        <!-- Create a new tab for each group that is visible to the current user -->
                        <tab *ngFor="let group of visibleGroups"
                             heading="{{group.readableName}}"
                             [active]="group.selected ?? false"
                             [id]="group.name"
                             class="grid-tab tab-form"
                             (selectTab)="group.selected = true; onGroupTabSelected($event)"
                             (deselect)="group.selected = false">
                            <ng-container>
                                <!-- Member list, with sort and selection controls -->
                                <div>
                                    <h4 i18n>Exam group members</h4>
                                    <!-- TODO: Implement as TimTable -->
                                    <table class="group-table">
                                        <thead>
                                        <tr class="member-table-row">
                                            <th *ngIf="this.viewOptions?.members?.selectionControls">
                                                <input type="checkbox" name="selectAllMembers_{{group.id}}"
                                                       [(ngModel)]="group.allMembersSelected"
                                                       (change)="toggleAllMembersSelected(group)"/></th>
                                            <th i18n *ngIf="this.viewOptions?.members?.name">Name</th>
                                            <th i18n *ngIf="this.viewOptions?.members?.username">Username</th>
                                            <th *ngIf="this.viewOptions?.members?.extraInfo">
                                                <ng-container *ngIf="this.markup['extraInfoTitle']; else defaultExtraTitle">{{ this.markup['extraInfoTitle'] }}</ng-container>
                                                <ng-template #defaultExtraTitle><ng-container i18n>Extra</ng-container></ng-template>
                                            </th>
                                            <th i18n *ngIf="this.viewOptions?.members?.extraTime">Extra time?</th>
                                            <th i18n *ngIf="this.viewOptions?.members?.email">Email</th>
                                            <th i18n *ngIf="this.viewOptions?.members?.loginCode">Login code</th>
                                            <th i18n>Delete</th>
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
                                            <td *ngIf="this.viewOptions?.members?.extraInfo">{{ member.extraInfo }}</td>
                                            <td *ngIf="this.viewOptions?.members?.extraTime" class="text-center">
                                                <input id="user-extra-time-{{member.id}}"
                                                       name="user-extra-time-{{member.id}}"
                                                       type="checkbox"
                                                       [ngModel]="member.extraTime"
                                                       (ngModelChange)="toggleExtraTime(group, member, $event)"
                                                />
                                            </td>
                                            <td *ngIf="this.viewOptions?.members?.email">{{ member.email }}</td>
                                            <td *ngIf="this.viewOptions?.members?.loginCode">
                                                <code>{{ member.login_code | formatLoginCode }}</code>
                                            </td>
                                            <td>
                                                <button
                                                        class="btn btn-danger btn-xs"
                                                        title="Delete member"
                                                        i18n-title
                                                        (click)="removeMember(group, member)"
                                                >
                                                    <i class="glyphicon glyphicon-trash"></i>
                                                </button>
                                            </td>
                                        </tr>
                                        </tbody>
                                    </table>
                                </div>
                                <!-- END members list-->

                            </ng-container>
                            <ng-container>
                                <div class="button-controls">
                                    <button class="timButton" (click)="addMembers(group)" i18n>
                                        Add new student
                                    </button>
                                    <button class="timButton" (click)="importUsers(group)" i18n>
                                        Import students from Wilma
                                    </button>
                                    <!--                                    <button class="timButton btn-danger" (click)="removeMembers(group)"-->
                                    <!--                                            [disabled]="!anySelected(this.members[group.name])" i18n>-->
                                    <!--                                        Remove selected-->
                                    <!--                                    </button>-->
                                </div>
                                <h4 i18n>Login codes</h4>
                                <div class="button-controls">
                                    <button class="timButton" (click)="generateLoginCodes(group)" i18n>
                                        Generate new login codes
                                    </button>
                                    <button class="timButton" *ngIf="group.examDocId" (click)="printLoginCodes(group)"
                                            i18n>
                                        Print login codes (Main exam)
                                    </button>
                                    <button class="timButton" *ngIf="examByDocId.get(group.examDocId ?? -1)?.practice ?? markup['practiceExam']"
                                            (click)="printLoginCodes(group, true)" i18n>
                                        Print login codes (Practice exam)
                                    </button>
                                </div>
                            </ng-container>
                            <!--                            <ng-container *ngIf="anySelected(this.members[group.name])">-->
                            <!--                                <p>Selected members:</p>-->
                            <!--                                <ol>-->
                            <!--                                    <ng-container *ngFor="let member of this.members[group.name]">-->
                            <!--                                        <li *ngIf="member.selected">{{ member.name }}</li>-->
                            <!--                                    </ng-container>-->
                            <!--                                </ol>-->
                            <!--                            </ng-container>-->
                        </tab>
                    </tabset>
                </bootstrap-panel>
                <bootstrap-panel title="3. Manage exams" i18n-title>
                    <tabset class="merged">
                        <tab *ngFor="let group of visibleGroups"
                             heading="{{group.readableName}}"
                             [active]="group.selected ?? false"
                             [id]="group.name"
                             (selectTab)="group.selected = true; onGroupTabSelected($event)"
                             (deselect)="group.selected = false">
                            
                            <tim-alert severity="success" *ngIf="examReset" i18n>
                                The exam is now ended and login codes are disabled. You can now select and start another exam below.
                            </tim-alert>
                            <p *ngIf="!group.currentExamDoc" i18n>
                                Begin by selecting the exam to be organized for the group.
                            </p>
                            <div class="select-exam">
                                <label for="current-exam-doc-{{group.id}}" i18n>Select exam</label>
                                <select id="current-exam-doc-{{group.id}}" name="current-exam-doc-{{group.id}}" class="form-control"
                                        [ngModel]="group.currentExamDoc"
                                        (ngModelChange)="confirmSelectExam(group, $event)">
                                    <option *ngIf="examByDocId.get(group.examDocId ?? -1) ?? markup['practiceExam']"
                                            [ngValue]="examByDocId.get(group.examDocId ?? -1)?.practice?.docId ?? markup['practiceExam']?.docId ?? -1">
                                        {{ examByDocId.get(group.examDocId ?? -1)?.practice?.name ?? markup['practiceExam']?.name ?? "" }}
                                    </option>
                                    <option *ngIf="group.examDocId && examByDocId.get(group.examDocId)"
                                            [ngValue]="examByDocId.get(group.examDocId)?.docId">
                                        {{ examByDocId.get(group.examDocId)?.name }}
                                    </option>
                                </select>
                            </div>
                            
                            <div *ngIf="group.currentExamDoc" class="button-controls mt">
                                <a target="exam-doc" class="timButton" href="/view/{{group.currentExamDoc}}" i18n>
                                    View exam document and audio/video materials
                                </a>
                                <a target="exam-doc" class="timButton" href="/teacher/{{group.currentExamDoc}}?group={{group.name}}" i18n>
                                    Review and correct student answers
                                </a>
                            </div>
                            
                            <div *ngIf="group.allowAccess" class="mt">
                                <tim-alert  severity="warning" i18n>
                                    You are showing exam answers to the students.<br>
                                    To start a new exam, stop answer reviewing in section '4. Show answers to students'.
                                </tim-alert>
                            </div>
                            <fieldset [disabled]="!group.currentExamDoc || group.allowAccess">
                                <h5 i18n>Hold an exam</h5>

                                <p i18n>
                                    Complete each step below to hold an exam.
                                    The checklist updates automatically with the current progress.
                                </p>
                                
                                <div class="checklist">
                                    <div [class.disabled]="!group.currentExamDoc || group.allowAccess">
                                        <div class="cb">
                                            <input type="checkbox" title="Mark as done" i18n-title
                                                   [checked]="group.examState > 0"
                                                   (change)="checkExamStateEvent(group, 0, $event)"
                                            >
                                        </div>
                                        <div>
                                            <div>
                                                <span i18n>Activate login codes</span>
                                            </div>
                                            <div class="small" *ngIf="group.examState <= 0" i18n>
                                                Press the toggle button to activate login codes for the exam group
                                            </div>
                                            <strong class="small text-success" *ngIf="group.examState > 0" i18n>
                                                The login codes are active! Students can now log in using the login
                                                codes.
                                            </strong>
                                        </div>
                                        <div>
                                            <tim-toggle [(value)]="group.loginCodesActive"
                                                        (valueChange)="toggleActivateLoginCodes(group)"
                                                        [disabled]="false"
                                                        enabledButton="Activate login codes"
                                                        i18n-enabledButton
                                                        disabledButton="Disable login codes"
                                                        i18n-disabledButton
                                            ></tim-toggle>
                                        </div>
                                    </div>
                                    <div [class.disabled]="group.examState < 1">
                                        <div class="cb">
                                            <input #cbStudentsLoggedIn [disabled]="group.examState < 1"
                                                   type="checkbox"
                                                   title="Mark as done"
                                                   i18n-title
                                                   [checked]="group.examState > 1"
                                                   (change)="checkExamStateEvent(group, 1, $event)"
                                            >
                                        </div>
                                        <div>
                                            <div><ng-container i18n>Ask students to log in to the exam page:</ng-container>
                                                <a *ngIf="group.currentExamDoc; else noExam"
                                                   [href]="getGroupSelectedExamUrl(group)"><code>{{ getGroupSelectedExamUrl(group) }}</code></a>
                                                <ng-template #noExam><ng-container i18n>Not selected</ng-container></ng-template>
                                            </div>
                                        </div>
                                        <div>
                                            <tim-toggle [(value)]="group.studentsLoggedIn"
                                                        (valueChange)="checkExamStateElement(group, 1, cbStudentsLoggedIn, $event)"
                                                        [disabled]="group.examState < 1"
                                                        enabledButton="Mark as done"
                                                        i18n-enabledButton
                                                        disabledButton="Mark not done"
                                                        i18n-disabledButton
                                            ></tim-toggle>
                                        </div>
                                    </div>
                                    <div [class.disabled]="group.examState < 2">
                                        <div class="cb">
                                            <input #cbStudentsReady [disabled]="group.examState < 2"
                                                   type="checkbox"
                                                   title="Mark as done"
                                                   i18n-title
                                                   [checked]="group.examState > 2"
                                                   (change)="checkExamStateEvent(group, 2, $event)"
                                            >
                                        </div>
                                        <div i18n>
                                            Check that the students have logged in and are ready to begin the exam
                                        </div>
                                        <div>
                                            <tim-toggle [(value)]="group.studentsReady"
                                                        (valueChange)="checkExamStateElement(group, 2, cbStudentsReady, $event)"
                                                        [disabled]="group.examState < 1"
                                                        enabledButton="Mark as done"
                                                        i18n-enabledButton
                                                        disabledButton="Mark not done"
                                                        i18n-disabledButton
                                            ></tim-toggle>
                                        </div>
                                    </div>
                                    <div [class.disabled]="group.examState < 3">
                                        <div class="cb">
                                            <input [disabled]="group.examState < 3"
                                                   type="checkbox"
                                                   title="Mark as done"
                                                   i18n-title
                                                   [checked]="group.examState > 3"
                                                   (change)="checkExamStateEvent(group, 3, $event)"
                                            >
                                        </div>
                                        <div>
                                            <span i18n>Begin exam</span>
                                            <div class="small" *ngIf="group.examState <= 3" i18n>
                                                Press the toggle button to start the exam
                                            </div>
                                            <strong class="small text-success" *ngIf="group.examState > 3" i18n>
                                                The exam has started! Students can now access the exam. Audio and video materials can be accessed via the "View exam document" link.
                                            </strong>
                                        </div>
                                        <div>
                                            <tim-toggle [(value)]="group.examStarted"
                                                        (valueChange)="toggleBeginExam(group)"
                                                        [disabled]="group.examState < 3"
                                                        enabledButton="Start exam"
                                                        i18n-enabledButton
                                                        disabledButton="Interrupt exam"
                                                        i18n-disabledButton
                                            ></tim-toggle>
                                        </div>
                                    </div>
                                    <div [class.disabled]="group.examState < 4">
                                        <div class="cb">
                                            <input [disabled]="group.examState < 4"
                                                   type="checkbox"
                                                   title="Mark as done"
                                                   i18n-title
                                                   [checked]="group.examState > 4"
                                                   (change)="checkExamStateEvent(group, 4, $event)"
                                            >
                                        </div>
                                        <div>
                                            <div i18n>End the exam for all except students with additional time</div>
                                            <div class="small" *ngIf="group.examState <= 4" i18n>
                                                Press the toggle button to end the exam for the main student group.
                                            </div>
                                            <strong class="small text-success" *ngIf="group.examState > 4" i18n>
                                                Exam ended for main group! Students with additional time can continue
                                                the exam.
                                            </strong>
                                        </div>
                                        <div>
                                            <tim-toggle [(value)]="group.examMainGroupEnded"
                                                        (valueChange)="toggleEndExamMainGroup(group)"
                                                        [disabled]="group.examState < 4"
                                                        enabledButton="End exam for main group"
                                                        i18n-enabledButton
                                                        disabledButton="Resume exam for main group"
                                                        i18n-disabledButton
                                            ></tim-toggle>
                                        </div>
                                    </div>
                                    <div [class.disabled]="group.examState < 5">
                                        <div class="cb">
                                            <input [disabled]="group.examState < 5"
                                                   type="checkbox"
                                                   title="Mark as done"
                                                   [checked]="group.examState > 5"
                                                   (change)="checkExamStateEvent(group, 5, $event)"
                                            >
                                        </div>
                                        <div>
                                            <div i18n>End the exam for all students</div>
                                            <div class="small" *ngIf="group.examState <= 5" i18n>
                                                Press the toggle button to end the exam for all students
                                            </div>
                                            <strong class="small text-success" *ngIf="group.examState > 5" i18n>
                                                Exam ended for all students! Remember to disable the login codes.
                                            </strong>
                                        </div>
                                        <div>
                                            <tim-toggle [(value)]="group.examEnded"
                                                        (valueChange)="toggleEndExamAll(group)"
                                                        [disabled]="group.examState < 5"
                                                        enabledButton="End exam for all students"
                                                        i18n-enabledButton
                                                        disabledButton="Resume exam for all students"
                                                        i18n-disabledButton
                                            ></tim-toggle>
                                        </div>
                                    </div>
                                    <div [class.disabled]="group.examState < 6">
                                        <div class="cb">
                                            <input [disabled]="group.examState < 6"
                                                   type="checkbox"
                                                   title="Mark as done"
                                                   i18n-title
                                                   [checked]="group.examState > 6"
                                                   (change)="checkExamStateEvent(group, 6, $event)"
                                            >
                                        </div>
                                        <div>
                                            <div i18n>Disable login codes</div>
                                            <div class="small" i18n>
                                                Remember to disable the login codes after the exam has ended
                                            </div>
                                        </div>
                                        <div>
                                            <button [disabled]="group.examState < 6"
                                                    (click)="disableLoginCodesAndResetExam(group)"
                                                    class="btn btn-success" i18n>
                                                Disable login codes
                                            </button>
                                        </div>
                                    </div>
                                </div>
                            </fieldset>
                        </tab>
                    </tabset>
                </bootstrap-panel>
                <bootstrap-panel title="4. Show answers to students" i18n-title>
                    <tabset class="merged">
                        <tab *ngFor="let group of visibleGroups"
                             heading="{{group.readableName}}"
                             [active]="group.selected ?? false"
                             [id]="group.name"
                             (selectTab)="group.selected = true; onGroupTabSelected($event)"
                             (deselect)="group.selected = false">
                            <tim-alert *ngIf="group.examState > 0" severity="warning" i18n>
                                You can show answers only when they don't have an active exam running.<br>
                                Stop the exam and disable login codes in section '3. Manage exams' to enable showing answers.
                            </tim-alert>
                            <tim-toggle 
                                    [(value)]="group.allowAccess"
                                    [disabled]="group.examState > 0"
                                    (valueChange)="toggleAllowRestrictedAccess(group)"
                                    enabledButton="Begin showing answers to students"
                                    i18n-enabledButton
                                    disabledButton="End showing answers to students"
                                    i18n-disabledButton
                            >
                            </tim-toggle>
                            <p class="mt">
                                <strong i18n>Note: You can only show the answers for the main exam ({{ examByDocId.get(group.examDocId!)?.name }}) and not for the practice exam.</strong>
                            </p>
                            <p>
                                <strong class="text-success" *ngIf="group.allowAccess" i18n>
                                    Students can access the answers to the exam '{{ examByDocId.get(group.examDocId!)?.name }}'. The access is automatically disabled
                                    on {{toReadableDate(group.accessAnswersTo ?? '')}}.
                                </strong>
                            </p>
                            <p>
                                <strong *ngIf="!group.allowAccess" i18n>
                                    Press the button above to allow students to review their answers for 1 hour.
                                </strong>
                            </p>
                            <h5 i18n>Guide</h5>
                            <ol>
                                <li i18n>Make sure the exam is ended and login codes are disabled in section <i>'3. Manage exams'</i></li>
                                <li i18n>Press the <i>'Begin showing answers to students'</i> button to allow students to review their answers for 1 hour.</li>
                                <li i18n>Ask students to log in to the exam page using their login codes: <a [href]="getExamUrl(examByDocId.get(group.examDocId!))"><code>{{ getExamUrl(examByDocId.get(group.examDocId!)) }}</code></a></li>
                                <li i18n>Students can open the exam using the <i>'Open exam'</i> button.</li>
                                <li i18n>Students can now review their answers. Students cannot submit new answers but can see their answers and model answers (if they are included).</li>
                                <li i18n>To end the view right, press the <i>'End showing answers to students button'</i>. The right is automatically disabled in 1 hour.</li>
                            </ol>
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
    radioValue = "false";
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
    examByDocId = new Map<number, ExamWithPractice>();

    allowRestrictedAccess: boolean = false;

    // Currently selected groups
    allGroupsSelected: boolean = false;

    // currently selected members
    selectedGroupTab?: string;

    // which group to add selected members into
    copyMembersTarget: number = -1;

    // status info for group members table
    loading: boolean = false;

    examReset: boolean = false;

    @Input() eventHeading?: string;
    private viewctrl!: Require<ViewCtrl>;

    private initOpts() {
        this.viewctrl = vctrlInstance!;
        this.showAllGroups = false;
        this.allGroupsSelected = false;
        this.viewOptions = this.markup.show;

        for (const exam of this.markup.exams) {
            this.examByDocId.set(exam.docId, exam);
            if (exam.practice) {
                this.examByDocId.set(exam.practice.docId, exam.practice);
            }
        }
        if (this.markup.practiceExam) {
            this.examByDocId.set(
                this.markup.practiceExam.docId,
                this.markup.practiceExam
            );
        }
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
            for (const group of groups.result) {
                this.refreshGroupExamState(group);
            }
            this.allGroups = groups.result;
        } else {
            this.error = $localize`Could not fetch groups. Details: ${groups.result.error.error}`;
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

    async refreshVisibleGroups() {
        this.visibleGroups = this.showAllGroups
            ? this.allGroups
            : this.allGroups.filter((g) => g.isDirectOwner);
        const prevSelectedGroupTab = this.selectedGroupTab;
        let curGroup = this.visibleGroups.find(
            (g) => g.name === this.selectedGroupTab
        );
        if (!this.selectedGroupTab || !curGroup) {
            this.selectedGroupTab = this.visibleGroups[0].name;
            this.visibleGroups[0].selected = true;
        }
        curGroup = this.visibleGroups.find(
            (g) => g.name === this.selectedGroupTab
        );
        if (prevSelectedGroupTab !== this.selectedGroupTab && curGroup) {
            await this.getGroupMembers(curGroup);
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
    private getSelectedGroup(): ExamGroup {
        return this.visibleGroups.filter(
            (g) => g.name === this.selectedGroupTab
        )[0];
    }

    private getMembersOf(group: ExamGroup): GroupMember[] {
        let res = this.members[group.name];
        if (!res) {
            res = [];
            this.members[group.name] = res;
        }
        return res;
    }

    getGroupMemberCount(group: ExamGroup): number {
        return this.getMembersOf(group).length;
    }

    async createNewGroup() {
        const res = await to2(
            showExamGroupCreateDialog({
                folderPath: this.markup.groupsPath,
                groupPrefix: this.markup.groupNamePrefix,
                exams: this.markup.exams,
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
                exams: this.markup.exams,
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
                $localize`Could not copy group members. Details: ${copyRes.result.error.error}`
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
                $localize`Could not delete group. Details ${res.result.error.error}`
            );
            this.loading = false;
            return;
        }
        // update ui
        await this.getGroups();
        this.loading = false;
    }

    async generateLoginCodes(group: ExamGroup) {
        this.loading = true;
        this.error = undefined;
        const members = this.getMembersOf(group);
        if (!members) {
            return;
        }

        const hasSomeWithCode = members.some((m) => m.login_code);
        if (hasSomeWithCode) {
            const confirmTitle = $localize`Generate new login codes`;
            const confirmMessage = $localize`Some students already have login codes in exam group '${group.readableName}'.\nGenerating new codes will overwrite the existing ones.\n\nProceed?`;
            const generateOk = await showConfirm(confirmTitle, confirmMessage);
            if (!generateOk) {
                this.loading = false;
                return;
            }
        }

        const res = await toPromise(
            this.http.post("/examGroupManager/generateCodes", {
                group_id: group.id,
            })
        );

        if (!res.ok) {
            await showMessageDialog(
                $localize`Could not generate login codes. Details: ${res.result.error.error}`
            );
            this.loading = false;
            return;
        }

        await this.getGroupMembers(group);
        this.loading = false;
    }

    printLoginCodes(group: ExamGroup, practice: boolean = false) {
        const members = this.getMembersOf(group);
        if (members.some((m) => !m.login_code)) {
            void showMessageDialog(
                $localize`Some students don't have a login code yet.\nPlease generate login codes first.`
            );
            return;
        }
        const {doc_id, par_id} = this.getPar()!.par.getJsonForServer();
        const urlParams = new URLSearchParams();
        urlParams.append("doc_id", doc_id.toString());
        urlParams.append("par_id", par_id.toString());
        urlParams.append("practice", practice.toString());
        window.open(
            `/examGroupManager/printCodes/${group.id}?${urlParams.toString()}`,
            "_blank"
        );
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
        if (!resp.ok) {
            this.error = $localize`Could not fetch group members. Details: ${resp.result.error.error}`;
            return;
        }
        this.members[group.name] = resp.result;
        group.memberCount = resp.result.length;
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
            this.getMembersOf(group).push(newUser);
            group.memberCount++;
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
        if (!resp.ok) {
            return;
        }
        await this.getGroupMembers(group);
    }

    async removeMember(group: ExamGroup, member: GroupMember) {
        const confirmTitle = $localize`Remove member`;
        const confirmMessage = $localize`Are you sure you want to remove ${member.real_name} from group '${group.readableName}'?`;
        const removeOk = await showConfirm(confirmTitle, confirmMessage);
        if (!removeOk) {
            return;
        }

        this.loading = true;
        this.error = undefined;
        const resp = await toPromise(
            this.http.post(`/groups/removemember/${group.name}`, {
                names: [member.name],
            })
        );

        if (!resp.ok) {
            await showMessageDialog(
                $localize`Could not remove member. Details: ${resp.result.error.error}`
            );
            this.loading = false;
            return;
        }
        const members = this.getMembersOf(group);
        members.splice(members.indexOf(member), 1);
        group.memberCount--;

        this.loading = false;
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
            const members = this.getMembersOf(group);
            for (const s of selected) {
                members.splice(members.indexOf(s), 1);
            }
            group.memberCount = members.length;
        }
    }

    private getSelectedMembers(group: ExamGroup): GroupMember[] {
        return this.getMembersOf(group).filter((m) => m.selected);
    }

    toggleMemberSelection(group: ExamGroup) {
        // if selecting a member results in all of the group's members being selected,
        // set the corresponding flag value to reflect that
        group.allMembersSelected = this.getMembersOf(group).every(
            (m) => m.selected
        );
    }

    toggleAllMembersSelected(group: ExamGroup) {
        for (const m of this.getMembersOf(group)) {
            m.selected = group.allMembersSelected ?? false;
        }
    }

    async onGroupTabSelected(groupTab: TabDirective) {
        this.selectedGroupTab = groupTab.id;
        const curGroup = this.visibleGroups.find(
            (g) => g.name === this.selectedGroupTab
        );
        console.log(this.selectedGroupTab, curGroup);
        if (curGroup) {
            await this.getGroupMembers(curGroup);
        }
    }

    async handleExamStateToggle(
        group: ExamGroup,
        state: number,
        checked: boolean
    ) {
        if (checked) {
            switch (state) {
                case 0:
                    await showMessageDialog(
                        $localize`Activate login codes by pressing the toggle button on the right.`,
                        true
                    );
                    return false;
                case 1:
                    await this.setExamState(group, 2);
                    break;
                case 2:
                    await this.setExamState(group, 3);
                    break;
                case 3:
                    await showMessageDialog(
                        $localize`Begin exam by pressing the toggle button on the right.`,
                        true
                    );
                    return false;
                case 4:
                    await showMessageDialog(
                        $localize`End the exam by pressing the toggle button on the right.`,
                        true
                    );
                    return false;
                case 5:
                    await showMessageDialog(
                        $localize`End the exam by pressing the toggle button on the right.`,
                        true
                    );
                    return false;
                case 6:
                    await showMessageDialog(
                        $localize`Disable login codes by pressing the toggle button on the right.`,
                        true
                    );
                    return false;
            }
        } else {
            if (group.examState > 5 && state <= 5) {
                await showMessageDialog(
                    $localize`You first need to resume the exam by using the "Resume exam for all students" button.`,
                    true
                );
                return true;
            }
            if (group.examState > 4 && state <= 4) {
                await showMessageDialog(
                    $localize`You first need to resume the exam by using the "Resume exam for main group" button.`,
                    true
                );
                return true;
            }
            if (group.examState > 3 && state <= 3) {
                await showMessageDialog(
                    $localize`You first need to interrupt the exam by using the "Interrupt exam" button.`,
                    true
                );
                return true;
            }
            if (state == 0) {
                await showMessageDialog(
                    $localize`Disable login codes by pressing the toggle button on the right.`,
                    true
                );
                return true;
            }

            switch (state) {
                case 1:
                    await this.setExamState(group, 1);
                    break;
                case 2:
                    await this.setExamState(group, 2);
                    break;
                case 4:
                    await this.setExamState(group, 4);
                    break;
            }
        }

        return checked;
    }

    async checkExamStateEvent(group: ExamGroup, state: number, event: Event) {
        await this.checkExamStateElement(
            group,
            state,
            event.target as HTMLInputElement
        );
    }

    async checkExamStateElement(
        group: ExamGroup,
        state: number,
        el: HTMLInputElement,
        checked?: boolean
    ) {
        el.checked = await this.handleExamStateToggle(
            group,
            state,
            checked ?? el.checked
        );
        this.refreshGroupExamState(group);
    }

    async toggleActivateLoginCodes(group: ExamGroup) {
        if (group.examStarted) {
            await showMessageDialog(
                $localize`You first need to interrupt the exam by using the "Interrupt exam" button.`,
                true
            );
            group.loginCodesActive = true;
            return;
        }

        if (group.examState == 0) {
            await this.setExamState(group, 1);
            return;
        }
        await this.setExamState(group, 0);
    }

    async toggleBeginExam(group: ExamGroup) {
        if (group.examMainGroupEnded || group.examEnded) {
            await showMessageDialog(
                $localize`You can't resume the exam that has already ended. End the exam and restart it if needed.`,
                true
            );
            group.examStarted = true;
            return;
        }
        if (!group.examStarted) {
            const res = await showConfirm(
                $localize`Interrupt exam?`,
                $localize`Are you sure you want to interrupt the exam while it is running?\nIf you want to end the exam, use the "End exam" buttons.`
            );
            if (!res) {
                group.examStarted = true;
                return;
            }
        }

        if (group.examState == 3) {
            // Begin
            await this.setExamState(group, 4);
            return;
        }
        await this.setExamState(group, 3);
    }

    async toggleEndExamMainGroup(group: ExamGroup) {
        if (group.examEnded) {
            await showMessageDialog(
                $localize`You can't resume the exam that has already ended. End the exam and restart it if needed.`,
                true
            );
            group.examMainGroupEnded = true;
            return;
        }
        if (!group.examMainGroupEnded) {
            const res = await showConfirm(
                $localize`Resume exam?`,
                $localize`Are you sure you want resume the exam?`
            );
            if (!res) {
                group.examMainGroupEnded = true;
                return;
            }
        }
        if (group.examState == 4) {
            await this.setExamState(group, 5);
            return;
        }
        await this.setExamState(group, 4);
    }

    async toggleEndExamAll(group: ExamGroup) {
        if (!group.examEnded) {
            const res = await showConfirm(
                $localize`Resume exam?`,
                $localize`Are you sure you want resume the exam?`
            );
            if (!res) {
                group.examEnded = true;
                return;
            }
        }
        if (group.examState == 5) {
            await this.setExamState(group, 6);
            return;
        }
        await this.setExamState(group, 5);
    }

    async disableLoginCodesAndResetExam(group: ExamGroup) {
        await this.setExamState(group, 0);
        await this.selectExam(group, null);
        this.refreshGroupExamState(group);
        this.examReset = true;
    }

    getGroupCurrentExamInfo(group: ExamGroup) {
        if (group.currentExamDoc) {
            return this.examByDocId.get(group.currentExamDoc);
        }
        return undefined;
    }

    getExamUrl(exam?: ExamWithPractice) {
        if (!exam) {
            return "";
        }
        if (exam.url) {
            return exam.url;
        }
        const baseUrl = window.location.origin;
        return `${baseUrl}/view/${exam.docId}`;
    }

    getGroupSelectedExamUrl(group: ExamGroup) {
        const exam = this.getGroupCurrentExamInfo(group);
        if (!exam) {
            return $localize`Not selected`;
        }
        return this.getExamUrl(exam);
    }

    async setExamState(group: ExamGroup, newState: number) {
        this.loading = true;
        const res = await toPromise(
            this.http.post(`/examGroupManager/setExamState`, {
                group_id: group.id,
                new_state: newState,
            })
        );
        this.loading = false;
        if (!res.ok) {
            await showMessageDialog(
                $localize`Could not set exam state. Details: ${res.result.error.error}`
            );
            this.refreshGroupExamState(group);
            return;
        }
        group.examState = newState;
        this.refreshGroupExamState(group);
    }

    refreshGroupExamState(group: ExamGroup) {
        group.loginCodesActive = false;
        group.examEnded = false;
        group.examMainGroupEnded = false;
        group.examStarted = false;
        group.studentsLoggedIn = false;
        group.studentsReady = false;
        if (group.accessAnswersTo) {
            const d = new Date(group.accessAnswersTo);
            const now = new Date();
            group.allowAccess = d > now;
        } else {
            group.allowAccess = false;
        }
        if (group.examState >= 1) {
            group.loginCodesActive = true;
        }
        if (group.examState >= 2) {
            group.studentsLoggedIn = true;
        }
        if (group.examState >= 3) {
            group.studentsReady = true;
        }
        if (group.examState >= 4) {
            group.examStarted = true;
        }
        if (group.examState >= 5) {
            group.examMainGroupEnded = true;
        }
        if (group.examState >= 6) {
            group.examEnded = true;
        }
    }

    async selectExam(group: ExamGroup, examDoc: number | null) {
        this.loading = true;
        const res = await toPromise(
            this.http.post<Partial<ExamGroup>>("/examGroupManager/setExamDoc", {
                group_id: group.id,
                exam_doc: examDoc,
            })
        );
        this.loading = false;
        if (!res.ok) {
            await showMessageDialog(
                $localize`Could not set the exam. Details: ${res.result.error.error}`
            );
            return;
        }
        Object.assign(group, res.result);
        this.refreshGroupExamState(group);
    }

    async confirmSelectExam(group: ExamGroup, examDoc: number) {
        this.examReset = false;
        const prevExam = group.currentExamDoc;

        if (
            prevExam !== undefined &&
            prevExam != examDoc &&
            group.examState > 0
        ) {
            const res = await showConfirm(
                $localize`Change exam?`,
                $localize`The exam has already been started.\nChanging the exam will reset the exam.\n\nAre you sure you want to change the exam?`
            );
            if (!res) {
                group.currentExamDoc = undefined;
                await timeout();
                group.currentExamDoc = prevExam;
                return;
            }
        }

        await this.selectExam(group, examDoc);
    }

    async toggleExtraTime(
        group: ExamGroup,
        member: GroupMember,
        toggle: boolean
    ) {
        this.loading = true;
        const res = await toPromise(
            this.http.post<Partial<GroupMember>>(
                "/examGroupManager/updateMemberInfo",
                {
                    group_id: group.id,
                    user_id: member.id,
                    extraTime: toggle,
                }
            )
        );
        this.loading = false;
        if (!res.ok) {
            await showMessageDialog(
                $localize`Could not update member info. Details: ${res.result.error.error}`
            );
            await timeout();
            const prev = member.extraTime;
            member.extraTime = undefined;
            await timeout();
            member.extraTime = prev;
            return;
        }

        Object.assign(member, res.result);
        await timeout();
        member.extraTime = toggle;
    }

    async toggleAllowRestrictedAccess(group: ExamGroup) {
        this.loading = true;
        this.error = undefined;
        const res = await toPromise(
            this.http.post<{accessAnswersTo: string}>(
                "/examGroupManager/toggleAnswerReview",
                {
                    group_id: group.id,
                    state: group.allowAccess,
                }
            )
        );
        this.loading = false;
        if (!res.ok) {
            await showMessageDialog(
                $localize`Could not toggle answer review. Details: ${res.result.error.error}`
            );
            return;
        } else {
            group.accessAnswersTo = res.result.accessAnswersTo;
        }
    }

    toReadableDate(date: string): string {
        console.log(date);
        return new Date(date).toLocaleString(Users.getCurrentLocale(), {
            dateStyle: "long",
            timeStyle: "short",
        });
    }
}

@NgModule({
    declarations: [
        ExamGroupManagerComponent,
        UserImportDialogComponent,
        UserCreationDialogComponent,
        ExamGroupCreateDialogComponent,
        FormatLoginCodePipe,
        ToggleComponent,
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
        ButtonsModule,
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
