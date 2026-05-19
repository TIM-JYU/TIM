import {Component, EventEmitter, Input, Output} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {NgForOf, NgIf} from "@angular/common";
import type {TokenLimitForUser} from "./userpolicy";
import {UserPolicyComponent} from "./userpolicy";

enum UserSortMode {
    Username = "username",
    TokensSpent = "tokens_spent",
    HasPolicy = "has_policy",
}

export interface UserData {
    username: string;
    tokens_spent: number;
    hasPolicy: boolean;
    policy: TokenLimitForUser;
}

export interface UserDataRequest {
    user_count: number;
}

@Component({
    selector: "usercontrol",
    standalone: true,
    template: `
        <div class="user-table">

            <div class="table-row header-row">
                <div>Username</div>
                <div>Spent Tokens</div>
                <div>Policy</div>
            </div>

            <button
                *ngFor="let user of userData"
                type="button"
                class="table-row user-row"
                [class.selected]="selectedUser === user"
                (click)="clickedOnUser(user)"
            >

                <div>
                    {{ user.username }}
                </div>

                <div>
                    {{ user.tokens_spent }}
                </div>

                <div>
                    {{ user.hasPolicy ? 'YES' : 'NO' }}
                </div>

            </button>

        </div>

        <div>
            <userpolicy *ngIf="selectedUser != null"
                        [userLimits]="currentUserLimit"
                        (isInInvalidState)="invalidUserPolicyState = $event"
            >
            </userpolicy>
            <div class="usercontrol-save-button-div">
                <button class="btn btn-primary usercontrol-save-button" style="margin: 2px;"
                        *ngIf="selectedUser != null"
                        [disabled]="invalidUserPolicyState"
                        (click)="clickedPolicySave()">
                    Save
                </button>
            </div>
        </div>

    `,
    imports: [DialogModule, NgForOf, UserPolicyComponent, NgIf],
})
export class UserControlComponent {
    invalidUserPolicyState: boolean = false;

    selectedUser: UserData | null = null;
    currentUserLimit: TokenLimitForUser = {
        token_cap_enabled: false,
        token_cap: null,
        time_window_enabled: false,
        window_unit: "h",
        window_value: null,
        token_cap_for_window: null,
    };

    private currentSort: UserSortMode = UserSortMode.TokensSpent;
    private reverseSort: boolean = false;
    userData: undefined | UserData[];
    private fetchedUsersCap: number = 10;

    @Output() policySaveRequest = new EventEmitter<UserData>();
    @Output() userDataRequest = new EventEmitter<number>();
    @Input() policyDataSaveResp: undefined | string;

    @Input() set setUserData(value: undefined | UserData[]) {
        this.userData = value;
    }

    sortUsers(): void {
        this.userData?.sort((a, b) => {
            let result = 0;

            switch (this.currentSort) {
                case UserSortMode.Username:
                    result = a.username.localeCompare(b.username);
                    break;

                case UserSortMode.TokensSpent:
                    result = a.tokens_spent - b.tokens_spent;
                    break;

                case UserSortMode.HasPolicy:
                    result = Number(a.hasPolicy) - Number(b.hasPolicy);
                    break;
            }

            return this.reverseSort ? result : -result;
        });
    }

    clickedOnUser(user: UserData) {
        if (user === this.selectedUser) {
            this.selectedUser = null;
            return;
        }

        this.selectedUser = user;
        this.currentUserLimit = user.policy;
    }

    clickedPolicySave(): void {
        if (this.selectedUser == null) {
            return;
        }
        this.policySaveRequest.emit(this.selectedUser);
    }
}
