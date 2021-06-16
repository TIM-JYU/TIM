import {Component, NgModule} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {AngularDialogComponent} from "../ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {to2} from "../util/utils";
import {Channel} from "../messaging/listOptionTypes";

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
                <div>
                    <label>
                        Channel
                        <select name="channel-select" [(ngModel)]="chosenChannel">
                            <option value="email">Email</option>
                        </select>
                    </label>
                </div>
                <div>
                    <label>
                        Contact info
                        <input type="text" name="contact-info-text" [(ngModel)]="contactInfo">
                    </label>
                </div>
            </ng-container>
            <ng-container footer>
                <button class="timButton" (click)="addNewContact()" [disabled]="!(chosenChannel && contactInfo)">Add</button>
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

    // Used only to get HttpClient initialized.
    constructor(private http: HttpClient) {
        super();
    }

    // Send a new contact information for a user to server.
    async addNewContact() {
        // Sending contact info is meaningless if contact info and it's channel isn't given.
        if (!(this.chosenChannel && this.contactInfo)) {
            // TODO: Give UI feedback that channel isn't chosen?
            return;
        }
        const result = await to2(
            this.http
                .post("/verification/addnewcontact", {
                    contact_info_type: this.chosenChannel,
                    contact_info: this.contactInfo,
                })
                .toPromise()
        );
        if (result.ok) {
            // TODO: For a good result, give some indication in the UI.
            //  Should we auto-update the UI to show the new contact info immediately?
            console.log("hyvin meni");
        } else {
            // TODO: For a bad result, give some indication in the UI.
            console.error(result.result.error.error);
        }
    }
}

@NgModule({
    declarations: [AddContactDialogComponent],
    imports: [DialogModule, FormsModule],
})
export class AddContactDialogModule {}
