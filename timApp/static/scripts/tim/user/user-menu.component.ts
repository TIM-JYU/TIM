import {Component} from "@angular/core";
import {getVisibilityVars, IVisibilityVars} from "tim/timRoot";
import {showLoginDialog} from "tim/user/showLoginDialog";
import {IUser} from "./IUser";
import {Users} from "./userService";

/**
 * Displays the current user name and the number of additional
 * users logged in the session and opens a dropdown menu with log out and other options.
 */
@Component({
    selector: "tim-user-menu",
    template: `
        <div class="btn-group" dropdown>
            <button type="button" title="You're logged in" i18n-title class="btn btn-primary dropdown-toggle" dropdownToggle>
                {{ getCurrentUser().real_name }} <span i18n
                    *ngIf="numSession() > 0">and {numSession(), plural, =1 {one other} other {{{numSession()}} others}}</span>
                &nbsp;<span class="caret"></span>
            </button>
            <ul class="dropdown-menu"
                *dropdownMenu
                role="menu"
                aria-labelledby="single-button">
                <ng-container *ngIf="!hideOptions.userMenuOptions">
                    <li role="menuitem"><a
                            href="/view/{{ getCurrentUser().folder!.path }}" i18n="@@myDocuments">My documents</a></li>
                    <li role="menuitem"><a
                            (click)="addUser()"
                            href="#">
                        <ng-container i18n="@@addToSession">Add a user to this session</ng-container>...</a></li>
                    <li class="divider"></li>
                </ng-container>
                <li *ngIf="!loggingout" role="menuitem">
                    <a (click)="beginLogout($event)"
                       href="#" [ngSwitch]="numSession() > 0">
                        <ng-container *ngSwitchCase="true" i18n>Log everyone out</ng-container>
                        <ng-container *ngSwitchCase="false" i18n>Log out</ng-container>
                    </a>
                </li>
                <li role="menuitem" *ngFor="let u of getSessionUsers()">
                    <a (click)="logout(u)"
                       href="#" i18n>Log {{ u.real_name }} out</a>
                </li>
            </ul>
        </div>
    `,
})
export class UserMenuComponent {
    hideOptions: IVisibilityVars = getVisibilityVars();
    loggingout = false;

    isLoggedIn = () => Users.isLoggedIn();

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
}
