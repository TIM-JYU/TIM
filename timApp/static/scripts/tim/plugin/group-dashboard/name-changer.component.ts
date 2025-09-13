import type {OnInit} from "@angular/core";
import {Component, Input, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormControl, ReactiveFormsModule, Validators} from "@angular/forms";
import {genericglobals, manageglobals} from "tim/util/globals";
import type {IFolder, IFullDocument} from "tim/item/IItem";
import type {BadgeGroupInfo} from "tim/plugin/group-dashboard/group.service";
import {GroupService} from "tim/plugin/group-dashboard/group.service";
import type {ICurrentUser} from "tim/user/IUser";
import {toPromise} from "tim/util/utils";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {HttpClient} from "@angular/common/http";
import {PurifyModule} from "tim/util/purify.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {IErrorAlert} from "tim/gamification/badge/badge.interface";

@Component({
    selector: "tim-name-changer",
    template: `
            <div class="name-changer">
                <p><ng-container i18n>Current group name: </ng-container> <b>{{ displayedName }}</b></p>
                <p><ng-container i18n>New name: </ng-container><b>{{prettyName}}</b></p>
            
                <div class="buttons-section">
                    <button *ngIf="canEditName" (click)="toggleInput()"><ng-container i18n>Change name</ng-container></button>
                </div>
                <ng-container *ngIf="showInput">
                    <div class="input-buttons">
                        <input [formControl]="newName" placeholder="Enter new group name" maxlength="30"/>
                        <button (click)="saveName()" [disabled]="newName.invalid"><ng-container i18n>Save</ng-container></button>
                        <div *ngIf="newName.invalid && newName.touched" class="error-message">
                            <p *ngIf="newName.hasError('maxlength')" i18n>Title is too long (max 30 characters).</p>
                        </div>
                    </div>
                    <div class="char-counter">
                        {{ newName.value?.length || 0 }} / 30 <ng-container i18n>characters</ng-container>
                    </div>
                </ng-container>
                <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type"
                               [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                        <div [innerHTML]="alert.msg | purify"></div>
                </tim-alert>
            </div>
       
    `,
    styleUrls: ["./name-changer.component.scss"],
})
export class NameChangerComponent implements OnInit {
    @Input() group!: string;
    groupName: string | null = null;
    prettyName: string | null = null;
    subGroup: string | undefined;
    group_id: number | undefined;
    item: IFullDocument | IFolder | undefined;
    newName = new FormControl("", [Validators.required]);
    displayedName: string | null | undefined;
    canEditName: boolean | undefined;
    showInput: boolean = false;
    showFullName = true;
    user: ICurrentUser | null = null;
    doc: IFullDocument | IFolder | undefined;
    alerts: Array<IErrorAlert> = [];

    constructor(
        private groupService: GroupService,
        protected badgeService: BadgeService,
        private http: HttpClient
    ) {}

    ngOnInit(): void {
        this.item = manageglobals().curr_item;
        this.getGroup();
    }

    /**
     * Fetch group from group service with the name that user has provided for component.
     * Access group's full name with .name, pretty_name with .description and id with .id
     */
    async getGroup() {
        const error = await this.badgeService.checkConnectionError(this.alerts);
        if (error) {
            return;
        }
        if (!this.group) {
            return;
        }

        const response = await toPromise(
            this.http.get<BadgeGroupInfo>(`/groups/groupinfo/${this.group}`)
        );
        if (!response.ok) {
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error: response.result.error.error,
                    },
                },
                "danger"
            );
            return;
        }
        const fetchedGroup = response.result;

        this.groupName = fetchedGroup.name;
        this.group_id = fetchedGroup.id;

        this.prettyName = fetchedGroup.description || "";
        this.displayedName = this.showFullName ? this.groupName : this.subGroup;

        // TODO: error handling / error messages for user
        const teacherRightQuery = await toPromise(
            this.http.get<boolean>(`/groups/hasTeacherRightTo/${this.group_id}`)
        );
        let teacherRight: boolean = false;
        if (teacherRightQuery.ok) {
            teacherRight = true;
        }
        this.canEditName =
            genericglobals().current_user.groups.find(
                (g) => g.id == this.group_id
            ) !== undefined || teacherRight;
    }

    /**
     * Saves a new pretty name (description) for the current group,
     * updates local prettyName variable for display in user interface.
     */
    async saveName() {
        const noConnectionAvailable =
            await this.badgeService.checkConnectionError(this.alerts);
        if (noConnectionAvailable) {
            return;
        }
        if (
            !this.newName.valid ||
            !this.groupName ||
            !this.group_id ||
            !this.newName.value
        ) {
            return;
        }

        const newPrettyName = this.newName.value;

        await this.groupService.updateGroupName(this.groupName, newPrettyName);

        this.prettyName = newPrettyName;
        this.newName.setValue("");
        this.showInput = !this.showInput;
    }

    toggleInput() {
        this.showInput = !this.showInput;
    }

    protected readonly alert = alert;
}

@NgModule({
    declarations: [NameChangerComponent],
    exports: [NameChangerComponent],
    imports: [
        CommonModule,
        ReactiveFormsModule,
        PurifyModule,
        TimUtilityModule,
    ],
})
export class NameChangerModule {}
