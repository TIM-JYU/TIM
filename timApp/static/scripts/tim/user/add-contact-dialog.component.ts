import {Component, NgModule} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {AngularDialogComponent} from "../ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {toPromise} from "../util/utils";
import {Channel} from "../messaging/listOptionTypes";
import {TimUtilityModule} from "../ui/tim-utility.module";
import {ContactOrigin, IUserContact} from "./IUser";

/**
 * User can add additional contact information to be stored in TIM.
 */
@Component({
    selector: "tim-add-contact-dialog",
    template: `
        <tim-dialog-frame [minimizable]="false">
            <ng-container header i18n>
                Add new contact information
            </ng-container>
            <ng-container body>
                <form>
                    <fieldset [disabled]="saving || saved">
                        <div class="form-group">
                            <label class="control-label" for="name-select" i18n>Channel</label>
                            <select class="form-control" name="channel-select" [(ngModel)]="chosenChannel">
                                <option value="email" i18n>Email</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label class="control-label" for="contact-info-text" i18n>Contact info</label>
                            <input class="form-control" type="text" name="contact-info-text"
                                   [(ngModel)]="contactInfo">
                        </div>
                        <div>
                            <tim-alert *ngIf="verificationSent" severity="success" i18n>
                                A verification link was sent to this contact.
                                Please check possible spam filters.
                            </tim-alert>
                            <tim-alert *ngIf="addError" severity="danger" i18n>
                                Could not add the new contact: {{addError}}
                            </tim-alert>
                        </div>
                    </fieldset>
                </form>
            </ng-container>
            <ng-container footer>
                <i class="glyphicon glyphicon-ok success" *ngIf="saved"></i>
                <tim-loading *ngIf="saving" style="margin-right: 1em;"></tim-loading>
                <button class="timButton"
                        (click)="addNewContact()"
                        [disabled]="!(chosenChannel && contactInfo) || saved" i18n>
                    Add
                </button>
                <button class="timButton" (click)="dismiss()">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class AddContactDialogComponent extends AngularDialogComponent<
    {onAdd: (contact: IUserContact) => void},
    void
> {
    dialogName: string = "AddContact";

    // Email is the only channel for now
    chosenChannel: Channel = Channel.EMAIL;
    contactInfo?: string;

    verificationSent = false;
    saved = false;
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
        const result = await toPromise(
            this.http.post<{requireVerification: boolean}>("/contacts/add", {
                contact_info_type: this.chosenChannel,
                contact_info: this.contactInfo,
            })
        );
        this.saving = false;
        if (result.ok) {
            this.verificationSent = result.result.requireVerification;
            this.saved = true;
            this.data.onAdd({
                channel: this.chosenChannel,
                contact: this.contactInfo!,
                verified: false,
                origin: ContactOrigin.Custom,
                primary: false,
            });
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
