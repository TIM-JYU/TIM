import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule, ViewEncapsulation} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {BadgeModule} from "tim/gamification/badge/badge.component";
import {BadgeService} from "tim/gamification/badge/badge.service";
import type {
    IBadge,
    IGroup,
    IPersonalGroup,
} from "tim/gamification/badge/badge.interface";
import {Subscription} from "rxjs";
import {Users} from "tim/user/userService";
import type {IUser} from "tim/user/IUser";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {MessageDialogComponent} from "tim/ui/message-dialog.component";
import {HostListener} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {toPromise} from "tim/util/utils";
import {GroupService} from "tim/plugin/group-dashboard/group.service";

@Component({
    selector: "tim-badge-viewer",
    template: `
        <ng-container *ngIf="!hasPermissionToComponent">
            <div class="viewer-container">
                <h2 class="badge-heading">User Badges </h2>
                <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                    <div [innerHTML]="alert.msg"></div>
                </tim-alert>
            </div>
        </ng-container>
        
        <ng-container *ngIf="hasPermissionToComponent">
        <div class="viewer-container">
            <h2 class="badge-heading">User Badges ({{realName}})</h2>
            <ng-container *ngIf="badges.length === 0">
                <p class="no-badges-txt">No user badges</p>
            </ng-container>
            <ng-container *ngIf="badges.length > 0">
                
                <div class="sort-select">
                    <select id="user-sort-select" [(ngModel)]="selectedSort" (ngModelChange)="onSortChange($event)">
                      <option value="newest">Newest</option>
                      <option value="oldest">Oldest</option>
                      <option value="az">A-Z</option>
                      <option value="za">Z-A</option>
                    </select>
                </div>
                
                <div class="user-badges">
                    <div class="badge-card" *ngFor="let badge of sortedBadges">
                        <tim-badge
                                title="{{badge.title}}"
                                color="{{badge.color}}"
                                shape="{{badge.shape}}"
                                [image]="badge.image"
                                description="{{badge.description}}"
                                message="{{badge.message}}"
                                [disableDialogWindow]="false"                                
                                (click)="openDialog(badge)">
                        </tim-badge>
                    </div>
                </div>
            </ng-container>

            <ng-container *ngIf="userSubGroups.length > 0">
                <div class="subgroups" *ngFor="let group of userSubGroups">
                    <h2 class="badge-heading">Group Badges ({{ groupPrettyNames.get(group.id) || group.name }})</h2>

                    <ng-container *ngIf="groupBadgesMap.get(group.id)?.length == 0">
                        <p class="no-badges-txt">No group badges</p>
                    </ng-container>

                    <ng-container *ngIf="groupBadgesMap.get(group.id)?.length || 0 > 0">
                        <div class="sort-select">
                            <select
                                [id]="'group-sort-' + group.id"
                                [ngModel]="groupSortMap.get(group.id) || 'newest'"
                                (ngModelChange)="onGroupSortChange(group.id, $event)"
                            >
                                <option value="newest">Newest</option>
                                <option value="oldest">Oldest</option>
                                <option value="az">A-Z</option>
                                <option value="za">Z-A</option>
                            </select>
                        </div>
                        <div class="users-group-badges">
                            <div class="badge-card" *ngFor="let badge of groupBadgesMap.get(group.id)">
                                <tim-badge
                                        title="{{badge.title}}"
                                        color="{{badge.color}}"
                                        shape="{{badge.shape}}"
                                        [image]="badge.image"
                                        description="{{badge.description}}"
                                        message="{{badge.message}}"
                                        [disableDialogWindow]="false"
                                        (click)="openDialog(badge)">
                                </tim-badge>
                            </div>
                        </div>
                    </ng-container>
                </div>
            </ng-container>
        </div>
        </ng-container>
    `,
    styleUrls: ["badge-viewer.component.scss"],
    encapsulation: ViewEncapsulation.None,
})
export class BadgeViewerComponent implements OnInit {
    personalGroup?: IPersonalGroup;
    realName: string | null = null;
    selectedUser?: IUser | null = null;
    badges: IBadge[] = [];
    userSubGroups: IGroup[] = [];
    @Input() badgegroupContext?: string;
    @Input() badgeuserContext?: string;
    private subscription: Subscription = new Subscription();
    groupBadgesMap = new Map<number, IBadge[]>();
    groupPrettyNames: Map<number, string> = new Map();
    disableDialogWindow?: boolean;

    availableImages: {id: number; name: string}[] = [];
    availableShapes: {id: string; value: string}[] = [];
    availableColors: {id: string; forCreatorList: string}[] = [];

    selectedSort: string = "newest";
    sortedBadges: IBadge[] = [];
    groupSortMap: Map<number, string> = new Map();

    hasPermissionToComponent: boolean = false;
    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService,
        private groupService: GroupService
    ) {}

    // Finds and presents the badges of the user and their assigned group
    ngOnInit() {
        this.availableImages = this.badgeService.getAvailableImages();
        this.availableShapes = this.badgeService.getAvailableShapes();
        this.availableColors = this.badgeService.getAvailableColors();
        if (Users.isLoggedIn()) {
            this.initializeData().then(() => {
                this.getBadges();
            });
        }
        this.subscription = this.badgeService.updateBadgeList$.subscribe(() => {
            this.getBadges();
            this.getGroupBadges();
        });
        this.subscription.add(
            this.badgeService.groupNameUpdated$.subscribe((update) => {
                this.groupPrettyNames.set(update.id, update.newName);
            })
        );
    }

    /**
     * Checks if badgeuser or badgegroupContext are falsy.
     * Fetches user's personal group with userContext.
     * Sets this.realName with personalGroup.
     * Calls getUserSubGroups method.
     */
    private async initializeData() {
        if (!this.badgeuserContext || !this.badgegroupContext) {
            return;
        }
        this.personalGroup = await this.groupService.getUserAndPersonalGroup(
            this.badgeuserContext
        );
        if (!this.personalGroup) {
            return;
        }
        this.realName = this.personalGroup["0"].real_name;
        this.getUserSubGroups(
            this.badgegroupContext,
            this.personalGroup?.["0"].id
        );
    }

    /**
     * Listens for the pressing of the "Esc" key and closes the open dialog if the key is pressed.
     * This method acts as a listener, and when the Esc key is pressed, it calls the
     * `closeDialog` method, which closes the open dialog.
     *
     * @param event - the event that contains information about the key press.
     * @returns void
     */

    @HostListener("document:keydown", ["$event"])
    onEscapeClick(event: KeyboardEvent): void {
        if (event.key === "Escape") {
            this.closeDialog();
        }
    }

    /**
     * Listens for a left mouse button click and closes the open dialog if one is open.
     * The `closeDialog` method is called only if the user clicks somewhere other than the badge.
     *
     * @param event - the event that contains information about the mouse click.
     * @returns void
     */

    @HostListener("document:click", ["$event"])
    onLeftClick(event: MouseEvent): void {
        if (event.button === 0) {
            const targetElement = event.target as HTMLElement;
            if (targetElement.closest(".badge-card")) {
                return;
            }
            if (targetElement.closest(".badge-dialog-window")) {
                return;
            }
            this.closeDialog();
        }
    }

    /**
     * Sulkee avoimena olevan dialogin kutsuttaessa
     */
    closeDialog(): void {
        if (this.badgeService.activeDialogRef) {
            this.badgeService.closeActiveDialog();
        }
    }

    /**
     * Opens a dialog displaying information about a badge object.
     *
     * This method first closes any open dialogs, then opens a new one to display
     * details from the provided badge object.
     *
     * @param badge - An object of type `IBadge` containing the details of the badge to display.
     * @returns Promise<void> - An asynchronous method that resolves once the dialog is closed.
     */

    async openDialog(badge: IBadge): Promise<void> {
        if (this.disableDialogWindow) {
            this.badgeService.closeActiveDialog();
            return;
        }

        const formattedBadgeTime: string = new Date(badge.given).toLocaleString(
            "fi-FI",
            {
                hour: "2-digit",
                minute: "2-digit",
                day: "2-digit",
                month: "2-digit",
                year: "numeric",
            }
        );

        // Creates a dialog-window about the data of a badge
        this.badgeService.closeActiveDialog();
        const iconName = this.getImageNameById(badge.image);
        const colorName = this.getColorNameById(badge.color);
        const shapeName = this.getShapeNameById(badge.shape);

        this.badgeService.activeDialogRef = await angularDialog.open(
            MessageDialogComponent,
            {
                message: `
                    <div class="badge-dialog-window">
                        <b>${badge.title}</b><br><br>
                        <b>Description:</b> ${badge.description}<br>
                        <b>Message:</b> ${badge.message}<br><br>
                        <b>Icon:</b> ${iconName}<br>
                        <b>Color:</b> ${colorName}<br>
                        <b>Shape:</b> ${shapeName}<br><br>
                        <b>Given time:</b> ${formattedBadgeTime}<br>
                        <b>Created by:</b> ${badge.created_by_name}<br>
                        <b>Given by:</b> ${badge.given_by_name}<br>                     
                    </div>                                                
            `,
                modal: false,
            }
        );

        // Wait for the dialog to close and handle rejection when closed via the X button
        try {
            await this.badgeService.activeDialogRef.result;
        } catch (error) {
            // console.log here if needed
        } finally {
            this.badgeService.activeDialogRef = null;
        }
    }

    /**
     * Retrieves the image name of the clicked badge based on its ID.
     *
     * @param id - The `image.id` of the clicked badge.
     * @returns string - The image name corresponding to the given ID, or "Image not found" if unavailable.
     */

    getImageNameById(id: number): string {
        const selectedBadgeImageName = this.availableImages.find(
            (img) => img.id === id
        );
        return selectedBadgeImageName
            ? selectedBadgeImageName.name
            : "Image not found";
    }

    getShapeNameById(id: string): string {
        const selectedBadgeShapeName = this.availableShapes.find(
            (shpe) => shpe.id === id
        );
        return selectedBadgeShapeName
            ? selectedBadgeShapeName.value
            : "Shape not found";
    }

    getColorNameById(id: string): string {
        const selectedBadgeColorName = this.availableColors.find(
            (clr) => clr.id === id
        );
        return selectedBadgeColorName
            ? selectedBadgeColorName.forCreatorList
            : "Color not found";
    }

    /**
     * Resets badges and makes http get request to receive user/group's badges.
     * If request returns error, showError method is called via badge-service.
     * If there are no errors, hasPermissionToComponent is set to true.
     * this.badges pointer is set to point to userBadges.
     * Finally, onSortChange method is called.
     */
    async getBadges() {
        this.emptyTable(this.badges);
        const result = await toPromise(
            this.http.get<[]>(
                `/groups_badges/${this.personalGroup?.["1"].id}/${this.badgegroupContext}`
            )
        );
        if (!result.ok) {
            if (result.result.error.error == undefined) {
                this.badgeService.showError(
                    this.alerts,
                    {
                        data: {
                            error: "Unexpected error. Check your internet connection.",
                        },
                    },
                    "danger"
                );
                return;
            }
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error: result.result.error.error,
                    },
                },
                "danger"
            );
            return;
        }
        this.hasPermissionToComponent = true;

        const userBadges: IBadge[] = [];
        if (result.result != undefined) {
            for (const alkio of result.result) {
                userBadges.push(alkio);
            }
        }
        this.badges = userBadges;
        this.onSortChange(this.selectedSort);
    }

    /**
     * Gets the user's subgroups under the specified main group.
     *
     * **Comment prettyname section**
     *
     * @param groupContext Main group
     * @param userID User's id
     */
    async getUserSubGroups(groupContext: string, userID: number) {
        this.userSubGroups = await this.groupService.getUserSubGroups(
            groupContext,
            userID
        );
        this.getGroupBadges();

        for (const sb of this.userSubGroups) {
            const prettyName = await this.groupService.getCurrentGroup(sb.name);
            if (prettyName) {
                this.groupPrettyNames.set(sb.id, prettyName.description);
            }
        }
    }

    /**
     * Sets every user's subgroup's ID and badges to groupBadgesMap.
     */
    async getGroupBadges() {
        this.groupBadgesMap.clear();
        if (!this.badgegroupContext) {
            console.error("Failed to retrieve the context group.");
            return;
        }
        for (const group of this.userSubGroups) {
            this.groupBadgesMap.set(
                group.id,
                await this.badgeService.getBadges(
                    group.id,
                    this.badgegroupContext
                )
            );
        }
    }

    onSortChange(sortType: string) {
        this.sortedBadges = this.badgeService.sortBadges(this.badges, sortType);
    }

    onGroupSortChange(groupId: number, sortType: string) {
        this.groupSortMap.set(groupId, sortType);
        const originalBadges = this.groupBadgesMap.get(groupId) || [];
        const sorted = this.badgeService.sortBadges(originalBadges, sortType);
        this.groupBadgesMap.set(groupId, sorted);
    }

    ngOnDestroy() {
        this.subscription.unsubscribe();
    }

    /**
     * Resets table from argument.
     */
    emptyTable<T>(table: T[]) {
        while (table.length > 0) {
            table.pop();
        }
    }
}

@NgModule({
    declarations: [BadgeViewerComponent],
    imports: [
        CommonModule,
        FormsModule,
        HttpClientModule,
        BadgeModule,
        TimUtilityModule,
    ],
    exports: [BadgeViewerComponent],
})
export class BadgeViewerModule {}
