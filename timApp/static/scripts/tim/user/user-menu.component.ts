import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";
import type {IVisibilityVars} from "tim/timRoot";
import {getVisibilityVars} from "tim/timRoot";
import {showLoginDialog} from "tim/user/showLoginDialog";
import {
    AccessRoleService,
    AccessType,
    accessTypeDisplayNamePrefixes,
    accessTypeDisplayNames,
} from "tim/item/access-role.service";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {showGroupLockDialog} from "tim/item/active-group-lock-dialog/showGroupLockDialog";
import {to2} from "tim/util/utils";
import type {IUser} from "tim/user/IUser";
import {Users} from "tim/user/userService";

/**
 * Displays the current user name and the number of additional
 * users logged in the session and opens a dropdown menu with log out and other options.
 */
@Component({
    selector: "tim-user-menu",
    template: `
        <div class="btn-group" dropdown container="body">
            <button type="button" [title]="buttonTitle"
                    class="btn btn-primary dropdown-toggle user-button" dropdownToggle>
                <span class="user-name">
                    <ng-container *ngIf="accessTypePrefix">{{accessTypePrefix}}:</ng-container>
                    {{ getCurrentUser().real_name }}
                    <ng-container i18n
                                  *ngIf="numSession() > 0">and {numSession(), plural, =1 {one other} other {{{numSession()}} others}}</ng-container>
                </span>
                &nbsp;<span class="caret"></span>
            </button>
            <ul class="dropdown-menu"
                *dropdownMenu
                role="menu"
                aria-labelledby="single-button">
                <ng-container *ngIf="!hideOptions.userMenuOptions">
                    <li role="menuitem"><a
                            href="/view/{{ getCurrentUser().folder!.path }}" i18n>My documents</a></li>

                    <li role="menuitem">
                        <a (click)="addUser()" role="button">
                            <ng-container i18n>Add a user to this session</ng-container>...
                        </a>
                    </li>

                    <li role="menuitem" dropdown #dropdown="bs-dropdown"  placement="right" container="body">
                        <a (mouseenter)="dropdown.show()" (click)="$event.stopPropagation()" class="dropdown-item dropdown-toggle">
                            <ng-container i18n>Lock access</ng-container>
                            <span class="caret caret-right"></span>
                        </a>
                        <ul *dropdownMenu class="dropdown-menu access-selector" role="menu">
                            <li role="menuitem" *ngFor="let access of lockableAccesses">
                                <a class="dropdown-item" (click)="lockAccess(access)">
                                    <span>{{accessTypeDisplayNames[access]}}</span>
                                    <i class="glyphicon glyphicon-ok"
                                       [class.lazyHidden]="currentLockedAccess != access"></i>
                                </a>
                            </li>
                            <li class="divider"></li>
                            <li role="menuitem">
                                <a class="dropdown-item" (click)="lockAccess(null)" i18n>Disable</a>
                            </li>
                        </ul>
                    </li>
                    
                    <li role="menuitem">
                        <a (click)="openGroupDialog()" i18n>Change active groups</a>
                    </li>

                    <li class="divider"></li>
                </ng-container>
                <li *ngIf="!isLoggingOut" role="menuitem">
                    <a (click)="beginLogout($event)" role="button">
                        <ng-container *ngIf="restoreContextUser; else normalContext">
                            <ng-container i18n>Switch to '{{ restoreContextUser }}'</ng-container>
                        </ng-container>
                        <ng-template #normalContext [ngSwitch]="numSession() > 0">
                            <ng-container *ngSwitchCase="true" i18n>Log everyone out</ng-container>
                            <ng-container *ngSwitchCase="false" i18n>Log out</ng-container>    
                        </ng-template>
                    </a>
                </li>
                <li role="menuitem" *ngFor="let u of getSessionUsers()">
                    <a (click)="logout(u)" role="button" i18n>Log {{ u.real_name }} out</a>
                </li>
            </ul>
        </div>
    `,
    styleUrls: ["./user-menu.component.scss"],
})
export class UserMenuComponent implements OnInit {
    hideOptions: IVisibilityVars = getVisibilityVars();
    isLoggingOut = false;
    currentLockedAccess?: AccessType;
    lockableAccesses = [
        AccessType.View,
        AccessType.Edit,
        AccessType.Teacher,
        AccessType.Manage,
    ];
    accessTypeDisplayNames = accessTypeDisplayNames;
    accessTypePrefix?: string;
    buttonTitle = $localize`You're logged in`;
    private groupSwitchOpened = false;
    restoreContextUser: string | null = null;

    constructor(private access: AccessRoleService) {}

    ngOnInit(): void {
        if (!this.isLoggedIn()) {
            return;
        }

        this.currentLockedAccess = this.getCurrentUser().locked_access;
        const lockedActiveGroups = this.getCurrentUser().locked_active_groups;
        const hasLockedActiveGroups =
            lockedActiveGroups !== undefined && lockedActiveGroups !== null;
        if (hasLockedActiveGroups || this.currentLockedAccess) {
            this.buttonTitle = $localize`You're logged in`;
            this.accessTypePrefix = "";

            if (this.currentLockedAccess) {
                this.accessTypePrefix = `${
                    accessTypeDisplayNamePrefixes[this.currentLockedAccess]
                }${this.accessTypePrefix}`;
                this.buttonTitle = $localize`${
                    this.buttonTitle
                } (access locked to "${
                    accessTypeDisplayNames[this.currentLockedAccess]
                }")`;
            }

            if (
                lockedActiveGroups !== undefined &&
                lockedActiveGroups !== null
            ) {
                this.accessTypePrefix = `(*)${this.accessTypePrefix}`;
                this.buttonTitle = $localize`${this.buttonTitle} (group access locked)`;
            }
        }

        this.restoreContextUser = Users.restoreUser;
    }

    isLoggedIn = () => Users.isRealUser();

    getCurrentUser = () => Users.getCurrent();
    getSessionUsers = () => Users.getSessionUsers();

    numSession() {
        return Users.getSessionUsers().length;
    }

    /**
     * Add another user to the session using login dialog.
     */
    addUser() {
        void showLoginDialog({showSignup: false, addingToSession: true});
    }

    logout = (user: IUser) => Users.logout(user);

    beginLogout($event: Event) {
        this.logout(this.getCurrentUser());
    }

    async lockAccess(access: AccessType | null) {
        const r = await this.access.lockAccess(access);
        if (!r.ok) {
            await showMessageDialog(
                $localize`Could not lock access: ${r.result.error.error}`
            );
        }
    }

    async openGroupDialog() {
        if (this.groupSwitchOpened) {
            return;
        }
        this.groupSwitchOpened = true;
        await to2(showGroupLockDialog());
        this.groupSwitchOpened = false;
    }
}
