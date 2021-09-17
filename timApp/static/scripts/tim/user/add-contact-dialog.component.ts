import {Component, NgModule} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {AngularDialogComponent} from "../ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {to2} from "../util/utils";
import {Channel} from "../messaging/listOptionTypes";
import {TimUtilityModule} from "../ui/tim-utility.module";

/**
 * User can add additional contact information to be stored in TIM.
 */
@Component({
    selector: "add-contact-dialog",
    template: `
        <tim-dialog-frame [minimizable]="false">
            <ng-container header>
                {{dialogName}}
            </ng-container>
            <ng-container body>
                <form>
                    <fieldset [disabled]="saving || verificationSent">
                        <div class="form-group">
                            <label class="control-label" for="name-select">
                                Channel</label>
                            <select class="form-control" name="channel-select" [(ngModel)]="chosenChannel">
                                <option value="email">Email</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label class="control-label" for="contact-info-text">
                                Contact info</label>
                            <input class="form-control" type="text" name="contact-info-text"
                                   [(ngModel)]="contactInfo">
                        </div>
                        <div>
                            <tim-alert *ngIf="verificationSent" severity="success">
                                A verification link was sent to this contact.
                                Please check possible spam filters.
                            </tim-alert>
                            <tim-alert *ngIf="addError" severity="danger">
                                Could not add the new contact: {{addError}}
                            </tim-alert>
                        </div>
                    </fieldset>
                </form>
            </ng-container>
            <ng-container footer>
                <tim-loading *ngIf="saving" style="margin-right: 1em;"></tim-loading>
                <button class="timButton"
                        (click)="addNewContact()"
                        [disabled]="!(chosenChannel && contactInfo) || verificationSent">
                    Add
                </button>
                <button class="timButton" (click)="dismiss()">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class AddContactDialogComponent extends AngularDialogComponent<
    unknown,
    void
> {
    dialogName: string = "Add new contact information";

    // What type of contact info user is adding.
    chosenChannel?: Channel;
    // The contact information.
    contactInfo?: string;

    verificationSent = false;
    saving = false;
    addError?: string;

    // Used only to get HttpClient initialized.
    constructor(private http: HttpClient) {
        super();
    }

    // Send a new contact information for a user to server.
    async addNewContact() {
        this.saving = true;
        // Call the server.
        const result = await to2(
            this.http
                .post<{requireVerification: boolean}>(
                    "/settings/contacts/add",
                    {
                        contact_info_type: this.chosenChannel,
                        contact_info: this.contactInfo,
                    }
                )
                .toPromise()
        );
        this.saving = false;
        if (result.ok) {
            this.verificationSent = result.result.requireVerification;
        } else {
            this.addError = result.result.error.error;
        }
    }
}

@NgModule({
    declarations: [AddContactDialogComponent],
    imports: [DialogModule, FormsModule, TimUtilityModule, CommonModule],
})
export class AddContactDialogModule {}
