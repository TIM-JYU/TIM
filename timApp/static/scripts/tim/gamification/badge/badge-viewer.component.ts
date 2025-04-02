import type {OnInit} from "@angular/core";
import {Input} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {
    BadgeComponent,
    BadgeModule,
} from "tim/gamification/badge/badge.component";
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

@Component({
    selector: "tim-badge-viewer",
    template: `
        <div class="viewer-container">

            <h2 class="badge-heading">User Badges ({{badgeuserContext}})</h2>
            <ng-container *ngIf="badges.length == 0">
                <p class="no-badges-txt">No user badges</p>
            </ng-container>
            <ng-container *ngIf="badges.length > 0">
                <div class="user_badges" (wheel)="onScroll($event)">
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
                    <h2 class="badge-heading">Group Badges ({{ group.name }})</h2>

                    <ng-container *ngIf="groupBadgesMap.get(group.id)?.length == 0">
                        <p class="no-badges-txt">No group badges</p>
                    </ng-container>

                    <ng-container *ngIf="groupBadgesMap.get(group.id)?.length || 0 > 0">
                        <div class="users_group_badges" (wheel)="onScroll($event)">
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
    disableDialogWindow?: boolean;

    givenBadges?: IBadge[];

    constructor(
        private http: HttpClient,
        private badgeService: BadgeService,
        private dialogService: BadgeService
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
        if (this.dialogService.activeDialogRef) {
            this.dialogService.closeActiveDialog();
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
            this.dialogService.closeActiveDialog();
            return;
        }

        this.dialogService.closeActiveDialog();

        this.dialogService.activeDialogRef = await angularDialog.open(
            MessageDialogComponent,
            {
                message: `
                    <b>${badge.title}</b><br><br>
                    <b>Description:</b> ${badge.description}<br>
                    <b>Message:</b> ${badge.message}<br>
                    <b>Icon:</b>${badge.image}<br>
                    <b>Color:</b> ${badge.color}<br>
                    <b>Shape:</b> ${badge.shape}<br> 
                    <b>Given time:</b> ${badge.given}<br> 
                    <b>Created by:</b> ${badge.given_by_name}<br>     
                    <b>Given by:</b> ${badge.created_by_name}<br>                              
            `,
                modal: false,
            }
        );

        // Wait for the dialog to close
        await this.dialogService.activeDialogRef.result;
        this.dialogService.activeDialogRef = null; // Reset the reference after closing
    }

    /**
     * Tyhjentää badge -taulukon ja kutsuu badge-servicen metodia joka hakee käyttäjälle kuuluvat badget.
     * @param id käyttäjän id (tällähetkellä käytetään sisäänkirjautuneen käyttäjän ID:tä)
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
        this.badges = await this.badgeService.getUserBadges(
            this.personalGroup?.["1"].id,
            this.badgegroupContext
        );
    }

    async getUserSubGroups(groupContext: string, userid: number) {
        this.userSubGroups = await this.badgeService.getUserSubGroups(
            groupContext,
            userid
        );
        this.getGroupBadges();
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

    onScroll(event: WheelEvent) {
        const element = event.currentTarget as HTMLElement;
        const scrollable = element.scrollWidth > element.clientWidth;
        if (scrollable) {
            const targetElement = event.currentTarget as HTMLElement;
            const scrollAmount = event.deltaY * 0.5;
            targetElement.scrollLeft += scrollAmount;
            event.preventDefault();
        }
    }

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.InitializeData().then(() => {
                this.getBadges();
            });
        }
        this.subscription = this.badgeService.updateBadgeList$.subscribe(() => {
            this.getBadges();
            this.getGroupBadges();
        });
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
    imports: [CommonModule, FormsModule, HttpClientModule, BadgeModule],
    exports: [BadgeViewerComponent],
})
export class BadgeViewerModule {}
