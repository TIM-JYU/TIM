import {Component, NgModule} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {FormsModule} from "@angular/forms";
import {genericglobals} from "tim/util/globals";
import {getLangLink, toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";

@Component({
    selector: "tos-dialog",
    template: `
        <div class="modal-bg">
        </div>
        <tim-dialog-frame [minimizable]="false" [showCloseIcon]="false">
            <ng-container i18n header>
                Terms of Service Agreement
            </ng-container>
            <ng-container body>
            <div class="modal-body">
                <p>The Terms of Services have been updated and to continue to use TIM, your agreement is needed.</p>
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
                        <p class="text-left" i18n><strong><span class="glyphicon glyphicon-exclamation-sign"></span> There was an error.</strong></p>
                        <p class="text-left">{{errorMessage}}</p>
                    </div>
                </div>
                <div class="form-group">
                    <button class="timButton" [disabled]="!agreeToTerms" (click)="confirmAgreement()" i18n>Confirm</button>
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
            this.errorMessage = $localize`Could not save the agreement: ${r.result.error.error}`;
        } else {
            this.dismiss();
        }
    }

    dismissAlert() {
        this.errorMessage = "";
    }
}
@NgModule({
    declarations: [TosDialogComponent],
    exports: [TosDialogComponent],
    imports: [TimUtilityModule, CommonModule, DialogModule, FormsModule],
})
export class TosDialogModule {}
