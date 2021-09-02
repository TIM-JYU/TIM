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
        <tim-dialog-frame>
            <ng-container header>
                {{dialogName}}
            </ng-container>
            <ng-container body>
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
                    <tim-alert *ngIf="verificationSend" severity="success">A message with a verification link will be
                        sent to the contact info you provided. Remember to check your spam folder if you don't see the
                        email. You may close this
                        dialog.
                    </tim-alert>
                    <tim-alert *ngIf="notValidEmailError" severity="danger">The email you attempted to provide is of
                        invalid form. Please check it.
                    </tim-alert>
                    <tim-alert *ngIf="alreadyVerifiedError" severity="danger">You have already verified this contact
                        information.
                    </tim-alert>
                    <tim-alert *ngIf="unhandledChannelError" severity="danger">An error has occured in TIM that has
                        prevented the sending of a verification message.
                    </tim-alert>
                    <tim-alert *ngIf="noChannelChosenError" severity="danger">Choose a channel for the user contact
                        information.
                    </tim-alert>
                    <tim-alert *ngIf="noInfoError" severity="danger">The contact information field is empty.
                    </tim-alert>
                    <tim-alert *ngIf="unspecifiedError" severity="danger">An error has occured in TIM.
                    </tim-alert>
                </div>
            </ng-container>
            <ng-container footer>
                <button class="timButton" (click)="addNewContact()" [disabled]="!(chosenChannel && contactInfo)">Add
                </button>
                <button class="timButton" (click)="dismiss()">Cancel</button>
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

    // Flag if the contact info was accepted by TIM.
    verificationSend = false;
    // Flag if the user tried to provided an invalid email address.
    notValidEmailError = false;
    // Flag if user has already verified a contact info.
    alreadyVerifiedError = false;
    // Flag if user tries to add a contact info to a channel that is not for some reason handled in TIM.
    unhandledChannelError = false;
    // Flag if a generic, unspecified error happened.
    unspecifiedError = false;
    // Flag if the user did not choose a channel for their new contact info. This should only happen if the user fiddled
    // with the sending with dev tools.
    noChannelChosenError = false;
    // Flag if the user left the text field empty.  This should only happen if the user fiddled with the sending with
    // dev tools.
    noInfoError = false;

    // Used only to get HttpClient initialized.
    constructor(private http: HttpClient) {
        super();
    }

    // Send a new contact information for a user to server.
    async addNewContact() {
        // Reset error messages.
        this.notValidEmailError = false;
        this.alreadyVerifiedError = false;
        this.unspecifiedError = false;
        this.unhandledChannelError = false;
        this.verificationSend = false;
        this.noInfoError = false;
        this.noChannelChosenError = false;

        // Sending contact info is meaningless if contact info and it's channel isn't given.
        if (!this.chosenChannel) {
            this.noChannelChosenError = true;
            return;
        }
        if (!this.contactInfo) {
            this.noInfoError = true;
            return;
        }

        // Call the server.
        const result = await to2(
            this.http
                .post("/settings/contacts/add", {
                    contact_info_type: this.chosenChannel,
                    contact_info: this.contactInfo,
                })
                .toPromise()
        );
        if (result.ok) {
            this.verificationSend = true;
        } else {
            switch (result.result.error.error) {
                case "01":
                    // Users email is not a valid email address.
                    this.notValidEmailError = true;
                    break;
                case "02":
                    // User has already verified the contact info.
                    this.alreadyVerifiedError = true;
                    break;
                case "03":
                    // A slip-up has happened and TIM does not handle the message channel where the user has attempted
                    // to add a new contact info.
                    this.unhandledChannelError = true;
                    break;
                default:
                    // Something unspecified has happened on the server.
                    this.unspecifiedError = true;
                    break;
            }
        }
    }
}

@NgModule({
    declarations: [AddContactDialogComponent],
    imports: [DialogModule, FormsModule, TimUtilityModule, CommonModule],
})
export class AddContactDialogModule {}
