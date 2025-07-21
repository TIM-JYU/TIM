import {Component, NgModule} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {FormsModule} from "@angular/forms";
import {genericglobals} from "tim/util/globals";
import {getLangLink, toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import type {IUser} from "tim/user/IUser";
import {Users} from "tim/user/userService";

@Component({
    selector: "tos-dialog",
    template: `
        <div class="modal-bg">
        </div>
        <tim-dialog-frame [minimizable]="false" [showCloseIcon]="false">
            <ng-container i18n header>
                Terms of Service Update
            </ng-container>
            <ng-container body>
            <div class="modal-body">
                <h3 i18n>We have updated our Terms of Service</h3>
                <p i18n>To continue using TIM, you need to review and agree to the updated terms.</p>
                <p i18n>By marking the checkbox and clicking "Confirm", you confirm that you have read and accepted the Terms of Service.</p>
                <div class="form-group">
                    <div class="checkbox">
                        <label>
                            <input type="checkbox" name="agree" [(ngModel)]="agreeToTerms">
                            <ng-container i18n>I have read and agree to the </ng-container><a [href]="termsOfServiceLink" target="_blank" rel="noopener noreferrer"><ng-container i18n>Terms of Service.</ng-container></a>
                        </label>
                    </div>
                </div>
            </div>
            </ng-container>
            <ng-container footer>
                <div *ngIf="errorMessage">
                    <div class="alert alert-danger alert-dismissible">
                        <button type="button" class="close" (click)="dismissAlert()"><span>&times;</span></button>
                        <p class="text-left"><strong><span class="glyphicon glyphicon-exclamation-sign"></span><ng-container> There was an error.</ng-container></strong></p>
                        <p class="text-left">{{errorMessage}}</p>
                    </div>
                </div>
                <div class="form-group">
                    <button class="timButton" [disabled]="!agreeToTerms" (click)="confirmAgreement()"><span class="glyphicon glyphicon-ok"></span><ng-container i18n> Confirm</ng-container></button>
                    <button class="timButton btn-danger" (click)="beginLogout()"><span class="glyphicon glyphicon-remove"></span><ng-container i18n> Log out</ng-container></button>
                </div>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class TosDialogComponent extends AngularDialogComponent<
    null,
    undefined
> {
    protected dialogName = "Terms of Service Agreement";
    termsOfServicePath = genericglobals().footerDocs.termsOfService;
    agreeToTerms = false;
    errorMessage = "";

    constructor(private http: HttpClient) {
        super();
    }

    get termsOfServiceLink() {
        return getLangLink("/view/" + this.termsOfServicePath);
    }

    async confirmAgreement() {
        this.errorMessage = "";
        const r = await toPromise(this.http.post("/tosagree", {}));
        if (!r.ok) {
            const status = r.result.status ?? 0;
            if (status <= 0) {
                this.errorMessage = $localize`Could not connect to server. Check your internet connection and try again.`;
            } else if (r.result.error.error === "anonymous user error") {
                this.errorMessage = $localize`Could not save the agreement: Anonymous users cannot perform this action.`;
            } else {
                this.errorMessage = $localize`Could not save the agreement. Try to refresh the page, if the problem persists you can contact TIM support at tim@jyu.fi`;
            }
        } else {
            this.dismiss();
        }
    }

    dismissAlert() {
        this.errorMessage = "";
    }

    logout(user: IUser) {
        Users.logout(user);
    }

    beginLogout() {
        const user = Users.getCurrent();
        this.logout(user);
    }
}
@NgModule({
    declarations: [TosDialogComponent],
    exports: [TosDialogComponent],
    imports: [TimUtilityModule, CommonModule, DialogModule, FormsModule],
})
export class TosDialogModule {}
