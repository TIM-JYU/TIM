import {Component, EventEmitter, Input, Output} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {NgForOf, NgIf} from "@angular/common";
import type {JsonValue} from "tim/util/jsonvalue";

export interface TokenLimitForUser extends Record<string, JsonValue> {
    token_cap_enabled: boolean;
    token_cap: number | null;
    time_window_enabled: boolean;
    window_unit: string;
    window_value: number | null;
    token_cap_for_window: number | null;
}

@Component({
    selector: "userpolicy",
    standalone: true,
    template: `
        <!-- Add time window restrictions for users -->
        <div class="settings-row">
            <div class="checkbox">
                <label>
                    <input type="checkbox"
                           [(ngModel)]="userLimits.token_cap_enabled"
                           (ngModelChange)="emitValidity()">
                    Enable timeless token limit
                </label>
            </div>

            <div *ngIf="userLimits.token_cap_enabled" class="settings-section-body">
                <input type="number"
                       class="form-control"
                       [(ngModel)]="userLimits.token_cap"
                       (ngModelChange)="emitValidity()"
                >
            </div>
            <div class="error" *ngIf="isInvalidTokenCap">
                Input should be a positive integer or zero
            </div>

            <div class="checkbox">
                <label>
                    <input type="checkbox"
                           [(ngModel)]="userLimits.time_window_enabled"
                           (ngModelChange)="emitValidity()">
                    Enable time window token limits
                </label>
            </div>

            <div *ngIf="userLimits.time_window_enabled">
                <div class="form-group">
                    <label class="col-sm-2 control-label time-window-label">
                        <span class="time-window-label-span">Tokens</span>
                        <input type="number" class="form-control restriction-inputs"
                               placeholder="5000"
                               [(ngModel)]="userLimits.token_cap_for_window"
                               (ngModelChange)="emitValidity()"
                        >
                    </label>
                    <div class="error" *ngIf="isInvalidWindowTokens">
                        Input should be a positive integer
                    </div>
                    <div>
                        <label class="col-sm-2 control-label time-window-label">
                            <span class="time-window-label-span">Window</span>
                            <input type="number" class="form-control restriction-inputs"
                                   placeholder="5"
                                   [(ngModel)]="userLimits.window_value"
                                   (ngModelChange)="emitValidity()"
                            >
                            <select class="form-control restriction-inputs"
                                    [(ngModel)]="userLimits.window_unit">
                                <option *ngFor="let timeUnit of timeUnitOptions"
                                        [ngValue]="timeUnit.value">{{ timeUnit.label }}
                                </option>
                            </select>
                        </label>
                    </div>
                    <div class="error" *ngIf="isInvalidWindowTime">
                        Input should be a positive integer
                    </div>
                </div>
            </div>
        </div>
    `,
    imports: [FormsModule, NgIf, NgForOf],
})
export class UserPolicyComponent {
    @Output() isInInvalidState = new EventEmitter<boolean>();
    @Input() userLimits!: TokenLimitForUser;

    timeUnitOptions: {value: string; label: string}[] = [
        {value: "min", label: "Minutes"},
        {value: "h", label: "Hours"},
        {value: "d", label: "Days"},
    ];

    @Output() userLimitsChange = new EventEmitter<TokenLimitForUser>();

    isValidNonNegativeInt(
        value: number | null,
        biggerThanZero: boolean = false
    ): boolean {
        const result = value !== null && Number.isInteger(value);
        if (biggerThanZero) {
            return result && value > 0;
        }
        return result && value >= 0;
    }

    isValidNumberInput(
        enabled: boolean,
        value: number | null,
        biggerThanZero: boolean = false
    ): boolean {
        return !enabled || this.isValidNonNegativeInt(value, biggerThanZero);
    }

    get isInvalidTokenCap(): boolean {
        return !this.isValidNumberInput(
            this.userLimits.token_cap_enabled,
            this.userLimits.token_cap
        );
    }

    get isInvalidWindowTokens(): boolean {
        return !this.isValidNumberInput(
            this.userLimits.time_window_enabled,
            this.userLimits.token_cap_for_window,
            true
        );
    }

    get isInvalidWindowTime(): boolean {
        return !this.isValidNumberInput(
            this.userLimits.time_window_enabled,
            this.userLimits.window_value,
            true
        );
    }

    emitValidity(): void {
        this.isInInvalidState.emit(
            this.isInvalidTokenCap ||
                this.isInvalidWindowTime ||
                this.isInvalidWindowTokens
        );
    }
}
