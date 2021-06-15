import {Component, NgModule} from "@angular/core";
import {AngularDialogComponent} from "../ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {HttpClient} from "@angular/common/http";

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
                        <select name="channel-select">
                            <option>Email</option>
                            <option>Discord</option>
                        </select>
                    </label>
                </div>
                <div>
                    <label>
                        Contact info
                        <input type="text" name="contact-info-text">
                    </label>
                </div>
            </ng-container>
            <ng-container footer>
                <button class="timButton">Add</button>
                <button class="timButton">Cancel</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class AddContactDialogComponent extends AngularDialogComponent<
    unknown,
    void
> {
    dialogName: string = "Add new contact information";

    constructor(private http: HttpClient) {
        super();
    }

    ngInit() {}
}

@NgModule({
    declarations: [AddContactDialogComponent],
    imports: [DialogModule],
})
export class AddContactDialogModule {}
