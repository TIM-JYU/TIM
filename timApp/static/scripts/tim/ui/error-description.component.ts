import {Input, Component} from "@angular/core";

@Component({
    selector: "tim-error-description",
    template: `
        <ng-container [ngSwitch]="error">
            <ng-container *ngSwitchCase="'EmailOrPasswordNotMatch'" i18n>Email or password is incorrect.</ng-container>
            <ng-container *ngSwitchCase="'EmailOrPasswordNotMatchUseHaka'" i18n>
                Email or password is incorrect. Haka members (e.g. universities) can use Haka login.
            </ng-container>
            <ng-container *ngSwitchCase="'WrongTempPassword'" i18n>Incorrect temporary password.</ng-container>
            <ng-container *ngSwitchCase="'PasswordsNotMatch'" i18n>Passwords do not match.</ng-container>
            <ng-container *ngSwitchCase="'PasswordTooShort'" i18n>Password is too short.</ng-container>
            <ng-container *ngSwitchCase="'UserAlreadyExists'" i18n>User already exists.</ng-container>
            <ng-container *ngSwitchCase="'AmbiguousAccount'" i18n>
                Two different accounts for this email were found. Please contact TIM administrators to merge them.
            </ng-container>
            <ng-container *ngSwitchCase="'IPNotAllowed'" i18n>Login is not allowed from this IP address.</ng-container>
            <ng-container *ngSwitchCase="'PasswordResetDisabled'" i18n>Password reset is disabled.</ng-container>
            <ng-container *ngSwitchDefault i18n>Error: <span [innerHTML]="error | purify"></span></ng-container>
        </ng-container>
    `,
})
export class ErrorDescriptionComponent {
    @Input() error!: string;
}
