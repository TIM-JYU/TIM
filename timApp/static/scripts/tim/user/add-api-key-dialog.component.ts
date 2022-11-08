/**
 * The dialog component for adding translator API keys to user settings
 * @author Noora Jokela
 * @author Sami Viitanen
 * @date 21.3.2022
 * @licence MIT
 * @copyright 2022 TIMTra project authors
 */

import {Component, NgModule} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {toPromise} from "tim/util/utils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {ITranslator, ITranslatorUsage} from "tim/item/IItem";
import {listTranslators} from "tim/document/languages";
import type {IUserApiKey} from "tim/user/IUser";

/**
 * User can add translator API keys to be stored in TIM. (code source: add-contact-dialog.component.ts)
 */
@Component({
    selector: "tim-add-api-key-dialog",
    template: `
        <tim-dialog-frame [minimizable]="false" [dialogOptions]="dialogOptions" [dialogName]="dialogName" [align]="'center'">
            <ng-container header i18n>
                Add new machine translator API key
            </ng-container>
            <ng-container body>
                <form>
                    <fieldset [disabled]="saving || saved">
                        <div class="form-group">
                            <label class="control-label" for="name-select" i18n>Machine translator</label>
                            <select class="form-control" name="channel-select" [(ngModel)]="chosenTranslator">
                                <option *ngFor="let translator of this.translators"
                                        value="{{translator.name}}"
                                >{{translator.name}}</option>
                            </select>
                        </div>
                        <div class="form-group">
                            <label class="control-label" for="api-key-text" i18n>API key</label>
                            <input class="form-control" type="text" name="api-key-text"
                                   [(ngModel)]="apiKey">
                        </div>
                        <div>
                            <tim-alert *ngIf="added" severity="success" i18n>
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
                <a href="https://tim.jyu.fi/view/tim/ohjeita/dokumenttien-konekaantaminen/en-GB#adding-a-translator-authentication-key">
                    <span class="glyphicon glyphicon-question-sign" style="margin-right: 1em; font-size: x-large; vertical-align: middle;" title="Help with machine translation setup" i18n-title></span>
                </a>
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
    {onAdd: (key: IUserApiKey) => void},
    void
> {
    translators: ITranslator[] = [];

    async ngOnInit() {
        const r = await listTranslators(false);
        if (r.ok) {
            this.translators = r.result;
        }
    }

    dialogName: string = "AddAPIKey";

    chosenTranslator: string = "";
    apiKey?: string;

    saved = false;
    saving = false;
    addError?: string;
    added = false;

    // Used only to get HttpClient initialized.
    constructor(private http: HttpClient) {
        super();
    }

    /**
     * Sends a new API key for a user to server.
     */
    async addNewAPIKey() {
        const validateResponse = await this.validateAPIKey();

        if (validateResponse.ok) {
            this.saving = true;
            // Call the server.
            const result = await toPromise(
                this.http.put("/translations/apiKeys", {
                    translator: this.chosenTranslator,
                    apikey: this.apiKey,
                })
            );
            this.saving = false;
            this.added = true;

            if (result.ok) {
                this.saved = true;
                this.data.onAdd({
                    translator: this.chosenTranslator,
                    APIkey: this.apiKey!,
                    availableQuota: 0,
                    usedQuota: 0,
                    quotaChecked: false,
                });
                this.dismiss();
                this.added = false;
            } else {
                this.added = false;
                this.addError = result.result.error.error;
            }
        } else {
            this.addError = validateResponse.result.error.error;
        }
    }

    /**
     * Sends a request to our server to check the validity of the API key to be added.
     * @return Response from server, if anything else than 200 OK we know the key was not valid.
     */
    async validateAPIKey() {
        return await toPromise(
            this.http.post<ITranslatorUsage>("/translations/apiKeys/validate", {
                translator: this.chosenTranslator,
                apikey: this.apiKey,
            })
        );
    }
}

@NgModule({
    declarations: [AddAPIKeyDialogComponent],
    imports: [DialogModule, FormsModule, TimUtilityModule, CommonModule],
})
export class AddAPIKeyDialogModule {}
