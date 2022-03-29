import {Component, NgModule, OnInit} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {AngularDialogComponent} from "../ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {toPromise} from "../util/utils";
import {TimUtilityModule} from "../ui/tim-utility.module";
import {ITranslators} from "../item/IItem";
import {listTranslators} from "../document/editing/edittypes";
import {IUserAPIKey} from "./IUser";

/**
 * User can add translator API keys to be stored in TIM. (code source: add-contact-dialog.component.ts)
 */
@Component({
    selector: "add-api-key-dialog",
    template: `
        <tim-dialog-frame [minimizable]="false">
            <ng-container header i18n>
                Add new translator API key
            </ng-container>
            <ng-container body>
                <form>
                    <fieldset [disabled]="saving || saved">
                        <div class="form-group">
                            <label class="control-label" for="name-select" i18n>Translator</label>
                            <select class="form-control" name="channel-select" [(ngModel)]="chosenTranslator">
                                <option *ngFor="let translator of this.translators"
                                        [hidden]="translator.name ==='Manual'" value="{{translator.name}}"
                                >{{translator.name}}</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label class="control-label" for="api-key-text" i18n>API key</label>
                            <input class="form-control" type="text" name="api-key-text"
                                   [(ngModel)]="apiKey">
                        </div>
                        <div>
                            <tim-alert *ngIf="verificationSent" severity="success" i18n>
                                The API key was added successfully!
                            </tim-alert>
                            <tim-alert *ngIf="addError" severity="danger" i18n>
                                Could not add the new API key: {{addError}}
                            </tim-alert>
                        </div>
                    </fieldset>
                </form>
            </ng-container>
            <ng-container footer>
                <i class="glyphicon glyphicon-ok success" *ngIf="saved"></i>
                <tim-loading *ngIf="saving" style="margin-right: 1em;"></tim-loading>
                <button class="timButton"
                        (click)="addNewAPIKey()"
                        [disabled]="!(chosenTranslator && apiKey) || saved" i18n>
                    Add
                </button>
                <button class="timButton" (click)="dismiss()">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class AddAPIKeyDialogComponent extends AngularDialogComponent<
    {onAdd: (key: IUserAPIKey) => void},
    void
> {
    translators: Array<ITranslators> = [];

    ngOnInit() {
        listTranslators(this.translators);
    }

    dialogName: string = "AddAPIKey";

    // Email is the only channel for now
    chosenTranslator: string = "";
    apiKey?: string;

    verificationSent = false;
    saved = false;
    saving = false;
    addError?: string;

    // Used only to get HttpClient initialized.
    constructor(private http: HttpClient) {
        super();
    }

    // Send a new contact information for a user to server.
    async addNewAPIKey() {
        this.saving = true;
        // Call the server.
        const result = await toPromise(
            this.http.post<{requireVerification: boolean}>("/apikeys/add", {
                translator: this.chosenTranslator,
                apikey: this.apiKey,
            })
        );
        this.saving = false;
        if (result.ok) {
            this.verificationSent = result.result.requireVerification;
            this.saved = true;
            this.data.onAdd({
                translator: this.chosenTranslator,
                APIkey: this.apiKey!,
            });
            this.dismiss();
        } else {
            this.addError = result.result.error.error;
        }
    }
}

@NgModule({
    declarations: [AddAPIKeyDialogComponent],
    imports: [DialogModule, FormsModule, TimUtilityModule, CommonModule],
})
export class AddAPIKeyDialogModule {}
