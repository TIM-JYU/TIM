// import type {OnChanges, SimpleChanges} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
import {saveCurrentScreenPar} from "tim/document/parhelpers";
import {genericglobals, isDocumentGlobals} from "tim/util/globals";

const loginErrors: Record<string, string> = {
    InvalidLoginCode: $localize`Invalid login code. Check that you entered it correctly.`,
    LoginCodeNotYetActive: $localize`Login code not yet active. Make sure you use the correct code or ask the code to be activated.`,
    LoginCodeExpired: $localize`Login code expired. Make sure you use the correct code or ask for a new code.`,
};

@Component({
    selector: "tim-user-code-login",
    template: `
        <tim-alert *ngIf="loginError">{{ loginError }}</tim-alert>
        <div class="login-message" *ngIf="loginMessage" [innerHTML]="loginMessage | purify"></div>
        <form (submit)="login()">
            <fieldset [disabled]="loggingIn">
                <input focusMe 
                       [enable]="doFocus"
                       class="form-control code-input"
                       id="login-code"
                       name="login-code"
                       type="text"
                       autocomplete="off"
                       inputmode="numeric"
                       pattern="[0-9]*"
                       [value]="loginCode"
                       [selectionStart]="selectionPos"
                       [selectionEnd]="selectionPos"
                       (input)="onInput($event)"
                       (keydown)="onKey($event)"
                >
                <input class="timButton" type="submit" value="Log in" i18n-value [disabled]="!canLogin">
                <div class="login-progress" *ngIf="loggingIn"><tim-loading></tim-loading> <ng-container i18n>Logging in, please wait...</ng-container></div>
            </fieldset>
        </form>
    `,
    styleUrls: ["./user-code-login.component.scss"],
})
export class UserCodeLoginComponent {
    maxLength = 12;
    selectionPos = 0;
    split = 4;
    loginCode: string = "    -    -    ";
    loggingIn = false;
    loginError: string | null = null;
    loginMessage: string | null = null;
    doFocus = true;

    constructor(private http: HttpClient) {}

    ngOnInit() {
        const globals = genericglobals();
        if (isDocumentGlobals(globals) && globals.loginMessage) {
            this.loginMessage = globals.loginMessage;
        }
    }

    get cleanCode() {
        return this.loginCode.replace(/\D/g, "");
    }

    get canLogin() {
        return this.cleanCode.length === this.maxLength;
    }

    private updateSelection(el: HTMLInputElement) {
        let index = -1;
        for (let i = 0; i < this.loginCode.length; i++) {
            if (/\d/.test(this.loginCode[i])) {
                index = i;
            }
        }
        // Sync values for both Angular and DOM to prevent cursor jumping
        this.selectionPos = index + 1;
        el.setSelectionRange(this.selectionPos, this.selectionPos);
    }

    onInput(e: Event) {
        const el = e.target as HTMLInputElement;
        this.loginCode = el.value;
        this.reformatCode();
        el.value = this.loginCode;
        this.updateSelection(el);
    }

    onKey(e: KeyboardEvent) {
        this.loginError = null;
        const el = e.target as HTMLInputElement;

        let cur = this.cleanCode;
        if (e.key === "Backspace") {
            e.preventDefault();
            cur = cur.slice(0, -1);
        } else {
            return;
        }
        this.loginCode = cur;
        this.reformatCode();
        this.updateSelection(el);
    }

    reformatCode() {
        const clean = this.cleanCode.substring(0, this.maxLength);
        let newCode = "";
        for (let i = 0; i < this.maxLength; i++) {
            if (i > 0 && i % this.split === 0) {
                newCode += "-";
            }
            newCode += clean[i] || " ";
        }
        this.loginCode = newCode;
    }

    async login() {
        this.loginError = null;
        this.loggingIn = true;
        this.doFocus = false;

        const r = await toPromise(
            this.http.post("/loginCode/login", {
                login_code: this.cleanCode,
            })
        );

        if (r.ok) {
            saveCurrentScreenPar();
            window.location.reload();
        } else {
            this.loggingIn = false;
            this.doFocus = true;
            this.loginError =
                loginErrors[r.result.error.error] || r.result.error.error;
        }
    }
}
