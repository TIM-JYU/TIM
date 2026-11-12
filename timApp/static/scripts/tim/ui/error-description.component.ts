import {Input, Component, ChangeDetectionStrategy} from "@angular/core";

@Component({
    selector: "tim-error-description",
    template: `
@switch (error) {
  @case ('EmailOrPasswordNotMatch') {
    <ng-container i18n>Email or password is incorrect.</ng-container>
  }
  @case ('EmailOrPasswordNotMatchUseHaka') {
    <ng-container i18n>
                Email or password is incorrect. Haka members (e.g. universities) can use Haka login.
            </ng-container>
  }
  @case ('WrongTempPassword') {
    <ng-container i18n>Incorrect temporary password.</ng-container>
  }
  @case ('PasswordsNotMatch') {
    <ng-container i18n>Passwords do not match.</ng-container>
  }
  @case ('PasswordTooShort') {
    <ng-container i18n>Password is too short.</ng-container>
  }
  @case ('UserAlreadyExists') {
    <ng-container i18n>User already exists.</ng-container>
  }
  @case ('AmbiguousAccount') {
    <ng-container i18n>
                Two different accounts for this email were found. Please contact TIM administrators to merge them.
            </ng-container>
  }
  @case ('IPNotAllowed') {
    <ng-container i18n>Login is not allowed from this IP address.</ng-container>
  }
  @case ('PasswordResetDisabled') {
    <ng-container i18n>Password reset is disabled.</ng-container>
  }
  @default {
    <ng-container i18n>Error: <span [innerHTML]="error | purify"></span></ng-container>
  }
}
`,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class ErrorDescriptionComponent {
    @Input() error!: string;
}
