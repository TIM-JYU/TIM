import type {OnInit} from "@angular/core";
import {Component, Input, NgModule, EventEmitter, Output} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormControl, ReactiveFormsModule, Validators} from "@angular/forms";
import {manageglobals} from "tim/util/globals";
import type {IFolder, IFullDocument} from "tim/item/IItem";
import {GroupService} from "tim/plugin/group-dashboard/group.service";
import type {ICurrentUser} from "tim/user/IUser";
import {toPromise} from "tim/util/utils";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {HttpClient} from "@angular/common/http";
import {PurifyModule} from "tim/util/purify.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

@Component({
    selector: "tim-name-changer",
    template: `
            <div class="name-changer">
                <p><ng-container i18n>Current group name: </ng-container> <b>{{ displayedName }}</b></p>
                <p><ng-container i18n>New name: </ng-container><b>{{prettyName}}</b></p>
            
                <div class="buttons-section">
                    <button *ngIf="canEditName" (click)="toggleInput()"><ng-container i18n>Change group name</ng-container></button>
                </div>
                <div *ngIf="showInput" class="input-buttons">
                    <input [formControl]="newName" placeholder="Enter new group name"/>
                    <button (click)="saveName()" [disabled]="newName.invalid"><ng-container i18n>Save</ng-container></button>
                </div>
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
    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];

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
        if (await this.checkConnectionError()) {
            return;
        }
        if (!this.group) {
            return;
        }

        const fetchedGroup = await this.groupService.getCurrentGroup(
            this.group
        );
        if (!fetchedGroup) {
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error: `Unexpected error. ${this.group} not found.`,
                    },
                },
                "danger"
            );
            return;
        }
        this.groupName = fetchedGroup.name;
        this.group_id = fetchedGroup.id;

        this.prettyName = fetchedGroup.description || "";
        this.displayedName = this.showFullName ? this.groupName : this.subGroup;

        this.canEditName = fetchedGroup.edit_access;
    }

    /**
     * Saves a new pretty name (description) for the current group,
     * updates local prettyName variable for display in user interface.
     */
    async saveName() {
        const noConnectionAvailable = await this.checkConnectionError();
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

    /**
     * Tests connection with check_connection route.
     * If there is error with result, calls showError method via badge-service and returns true.
     * If no errors, returns false.
     */
    async checkConnectionError() {
        const result = await toPromise(this.http.get(`/check_connection/`));
        if (!result.ok) {
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error: "Unexpected error. Check your internet connection.",
                    },
                },
                "danger"
            );
            return true;
        }
        return false;
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
