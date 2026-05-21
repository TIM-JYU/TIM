import {Component, EventEmitter, Input, Output} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {NgForOf, NgIf} from "@angular/common";
import {PurifyModule} from "tim/util/purify.module";
import type {JsonValue} from "tim/util/jsonvalue";
import type {TokenLimitForUser} from "./userpolicy";
import {UserPolicyComponent} from "./userpolicy";

enum UserSortMode {
    Username = "username",
    TokensSpent = "tokens_spent",
    HasPolicy = "has_policy",
}

export interface UserData extends Record<string, JsonValue> {
    username: string;
    user_id: number;
    tokens_spent: number;
    hasPolicy: boolean;
    policy: TokenLimitForUser;
}

@Component({
    selector: "usercontrol",
    standalone: true,
    template: `
        <div class="user-table">

            <div class="table-row header-row">

                <button
                    type="button"
                    class="header-button"
                    (click)="clickedSort(UserSortMode.Username)"
                >
                    Username {{ getSortArrow(UserSortMode.Username) }}
                </button>

                <button
                    type="button"
                    class="header-button"
                    (click)="clickedSort(UserSortMode.TokensSpent)"
                >
                    Spent Tokens {{ getSortArrow(UserSortMode.TokensSpent) }}
                </button>

                <button
                    type="button"
                    class="header-button"
                    (click)="clickedSort(UserSortMode.HasPolicy)"
                >
                    Policy {{ getSortArrow(UserSortMode.HasPolicy) }}
                </button>

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
            <div class="error" *ngIf="this.localPolicySaveResponse.error"
                 [innerHTML]="this.localPolicySaveResponse.error | purify"></div>
            <div *ngIf="this.localPolicySaveResponse.result"
                 [innerHTML]="this.localPolicySaveResponse.result | purify"></div>
        </div>

    `,
    imports: [DialogModule, NgForOf, UserPolicyComponent, NgIf, PurifyModule],
})
export class UserControlComponent {
    readonly UserSortMode = UserSortMode;
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

    localPolicySaveResponse: {result: string; error: string} = {
        result: "",
        error: "",
    };

    @Output() policySaveRequest = new EventEmitter<UserData>();
    @Output() userDataRequest = new EventEmitter<void>();

    @Input() set policySaveResponse(v: {result: string; error: string}) {
        this.localPolicySaveResponse.result = v.result;
        this.localPolicySaveResponse.error = v.error;

        if (!v.error && this.selectedUser) {
            this.updatePolicyState();
        }
    }
    @Input() set setUserData(value: undefined | UserData[]) {
        this.userData = value;
    }

    getSortArrow(mode: UserSortMode): string {
        if (this.currentSort !== mode) {
            return "";
        }

        return this.reverseSort ? "▲" : "▼";
    }

    clickedSort(mode: UserSortMode): void {
        if (this.currentSort === mode) {
            this.reverseSort = !this.reverseSort;
        } else {
            this.currentSort = mode;
            this.reverseSort = false;
        }

        this.sortUsers();
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

            return this.reverseSort ? -result : result;
        });
    }

    clickedOnUser(user: UserData) {
        if (user === this.selectedUser) {
            this.selectedUser = null;
            return;
        }

        this.selectedUser = user;
        this.currentUserLimit = user.policy;

        this.clearMessages();
    }

    updatePolicyState() {
        const selectedUser = this.selectedUser;

        if (!selectedUser || !this.userData) {
            return;
        }

        const hasPolicy =
            selectedUser.policy.time_window_enabled ||
            selectedUser.policy.token_cap_enabled;

        const updatedUser: UserData = {
            ...selectedUser,
            hasPolicy,
        };

        this.selectedUser = updatedUser;

        this.userData = this.userData.map((u) =>
            u.user_id === selectedUser.user_id ? updatedUser : u
        );
    }

    clickedPolicySave(): void {
        if (this.selectedUser == null) {
            return;
        }
        this.policySaveRequest.emit(this.selectedUser);
    }

    clearMessages(): void {
        this.localPolicySaveResponse.error = "";
        this.localPolicySaveResponse.result = "";
    }
}
