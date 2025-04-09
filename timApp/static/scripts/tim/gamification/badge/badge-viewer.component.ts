import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
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

@Component({
    selector: "tim-badge-viewer",
    template: `
        <ng-container *ngIf="!teacherPermission">
            <div class="viewer-container">
                <h2 class="badge-heading">User Badges </h2>
                <tim-alert *ngFor="let alert of alerts; let i = index" [severity]="alert.type" [closeable]="true" (closing)="badgeService.closeAlert(this.alerts, i)">
                    <div [innerHTML]="alert.msg"></div>
                </tim-alert>
            </div>
        </ng-container>
        
        <ng-container *ngIf="teacherPermission">
        <div class="viewer-container">
            <h2 class="badge-heading">User Badges ({{badgeuserContext}})</h2>
            <ng-container *ngIf="badges.length === 0">
                <p class="no-badges-txt">No user badges</p>
            </ng-container>
            <ng-container *ngIf="badges.length > 0">
                <div class="user-badges">
                    <div class="badge-card" *ngFor="let badge of badges">
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
})
export class BadgeViewerComponent implements OnInit {
    personalGroup?: IPersonalGroup;
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

    teacherPermission: boolean = false;
    alerts: Array<{
        msg: string;
        type: "warning" | "danger";
        id?: string;
    }> = [];

    constructor(
        private http: HttpClient,
        protected badgeService: BadgeService
    ) {}

    /**
     * Kuuntelee "Esc"-näppäimen painallusat ja sulkee aviomen dialogin, mikäli näppäintä
     * painetaan. Metodi on kuuntelija, ja jos Esc-näppäintä painaa, metodi kutsuu
     * closeDialog - metodia, joka sulkee auki olevan dilaogin.
     *
     * @param event - tapahtuma, joka sisätlää tietoja näppäimen painalluksesta.
     * @returns void
     */
    @HostListener("document:keydown", ["$event"])
    onEscapeClick(event: KeyboardEvent): void {
        if (event.key === "Escape") {
            this.closeDialog();
        }
    }

    /**
     * Kuuntelee vasemman hiirennapin painallusta ja sulkee avoimena olevan dialogin, jos
     * jos sellainen on auki. CloseDialog - metodia kutsutaan vain, jos käyttäjä painaa
     * mistä tahansa muualta kuin badgesta.
     *
     * @param event - tapahtuma, joka sisältää tietoja näppäimen painalluksesta.
     * @returns void
     */
    @HostListener("document:click", ["$event"])
    onLeftClick(event: MouseEvent): void {
        if (event.button === 0) {
            const targetElement = event.target as HTMLElement;
            if (targetElement.closest(".badge-card")) {
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
     * Avaa dialogin, joka näyttää tietoja badge-objektista.
     *
     * Tämä metodi sulkee ensin avoimet dialogit jos niitä on ja avaa sitten uuden,
     * jossa näytetään badgen tiedot badge-objektista.
     *
     * @param badge - IBadge-tyyppinen objekti, sisältää tiedot näytettävästä badge-objektista.
     * @returns Promise<void> - Metodi ei palauta mitään, mutta on asynkroninen
     * ja odottaa dialogin sukeutumista.
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

        this.badgeService.closeActiveDialog();
        const iconName = this.getImageNameById(badge.image);
        this.badgeService.activeDialogRef = await angularDialog.open(
            MessageDialogComponent,
            {
                message: `
                    <b>${badge.title}</b><br><br>
                    <b>Description:</b> ${badge.description}<br>
                    <b>Message:</b> ${badge.message}<br><br>  
                    <b>Icon:</b> ${iconName}<br>
                    <b>Color:</b> ${badge.color}<br>
                    <b>Shape:</b> ${badge.shape}<br><br>  
                    <b>Given time:</b> ${formattedBadgeTime}<br> 
                    <b>Created by:</b> ${badge.given_by_name}<br>     
                    <b>Given by:</b> ${badge.created_by_name}<br>                             
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
     * Hakee klikatun badgen imagen nimen bagen id:n perusteella
     * @param id - klikatun badgen image.id
     * @returns - id:n perusteella haettu badgen imagen nimi tai "Image not found"
     * jos sitä ei löydy
     */
    getImageNameById(id: number): string {
        const selectedBadgeImageName = this.availableImages.find(
            (img) => img.id === id
        );
        return selectedBadgeImageName
            ? selectedBadgeImageName.name
            : "Image not found";
    }

    /**
     * Tyhjentää badge -taulukon ja kutsuu badge-servicen metodia joka hakee käyttäjälle kuuluvat badget.
     */
    async getBadges() {
        this.emptyBadges(this.badges);
        if (!this.personalGroup) {
            console.error("Failed to retrieve the user's personal group ID.");
            return;
        }
        if (!this.badgegroupContext) {
            console.error("Failed to retrieve the context group.");
            return;
        }

        const result = await toPromise(
            this.http.get<[]>(
                `/groups_badges/${this.personalGroup?.["1"].id}/${this.badgegroupContext}`
            )
        );
        const userBadges: IBadge[] = [];
        if (result.ok) {
            this.teacherPermission = true;
            if (result.result != undefined) {
                for (const alkio of result.result) {
                    userBadges.push(alkio);
                }
            }
        }
        if (!result.ok) {
            this.badgeService.showError(
                this.alerts,
                {
                    data: {
                        error:
                            result.result.error.error +
                            " If you are a teacher of this context-group, please contact TIM admin.",
                    },
                },
                "danger"
            );
            return;
        }
        this.badges = userBadges;
    }

    async getUserSubGroups(groupContext: string, userid: number) {
        this.userSubGroups = await this.badgeService.getUserSubGroups(
            groupContext,
            userid
        );
        this.getGroupBadges();

        for (const sb of this.userSubGroups) {
            const prettyName = await this.badgeService.getCurrentGroup(sb.name);
            if (prettyName) {
                this.groupPrettyNames.set(sb.id, prettyName.description);
            }
        }
    }

    async getGroupBadges() {
        this.groupBadgesMap.clear();
        if (!this.badgegroupContext) {
            console.error("Failed to retrieve the context group.");
            return;
        }
        for (const group of this.userSubGroups) {
            this.groupBadgesMap.set(
                group.id,
                await this.badgeService.getUserBadges(
                    group.id,
                    this.badgegroupContext
                )
            );
        }
    }

    ngOnInit() {
        this.availableImages = this.badgeService.getAvailableImages();
        if (Users.isLoggedIn()) {
            this.InitializeData().then(() => {
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

    private async InitializeData() {
        if (!this.badgeuserContext || !this.badgegroupContext) {
            return;
        }
        this.personalGroup = await this.badgeService.getUserAndPersonalGroup(
            this.badgeuserContext
        );
        if (!this.personalGroup) {
            return;
        }
        this.getUserSubGroups(
            this.badgegroupContext,
            this.personalGroup?.["0"].id
        );
    }

    ngOnDestroy() {
        this.subscription.unsubscribe();
    }

    /**
     * Tyhjentää attribuuttina annetun taulukon
     */
    emptyBadges(badges: IBadge[]) {
        while (badges.length > 0) {
            badges.pop();
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
