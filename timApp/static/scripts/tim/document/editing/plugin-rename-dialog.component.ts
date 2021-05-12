import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {to2} from "tim/util/utils";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {KEY_S} from "tim/util/keycodes";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {FormsModule} from "@angular/forms";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {
    Duplicate,
    IExtraData,
    IManageResponse,
    IParResponse,
} from "./edittypes";

export interface IManageRenameParams {
    duplicates: Duplicate[];
    original_par?: never;
    new_par_ids?: never;
    extraData?: never;
}

export interface IParRenameParams {
    duplicates: Duplicate[];
    original_par?: {md: string; attrs: unknown};
    new_par_ids: string[];
    extraData: IExtraData;
}

function isFromPar(
    p: IParRenameParams | IManageRenameParams
): p is IParRenameParams {
    return (p as IParRenameParams).extraData != null;
}

export type IRenameParams = IManageRenameParams | IParRenameParams;

export type RenameResult = IParResponse | IManageResponse;

@Component({
    selector: "tim-plugin-rename-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <p>There are multiple objects with the same task name in this document. Plugins with duplicate task
                    names will
                    not work properly. Rename the duplicates by writing new names in the field(s) below and click
                    "Save", choose
                    "Rename automatically" or "Ignore" to proceed without renaming.</p>
                <strong>Rename duplicates:</strong>
                <form #form (keydown)="keyHandler($event)">
                    <div *ngFor="let dupe of getDuplicates(); let index = index" class="form-group form-horz-flex">
                        <label for="field{{index}}" [innerText]="dupe[0]"></label>
                        <input type="text"
                               class="form-control"
                               name="field{{index}}"
                               id="field{{index}}" 
                               required
                               [(ngModel)]="newNames[index]">
                        <i *ngIf="dupe[2]"
                           class="glyphicon glyphicon-warning-sign"
                           tooltip="There are answers related to this task.
                   They might be lost upon renaming this task."></i>
                    </div>
                </form>
            </ng-container>
            <ng-container footer>
                <button [disabled]="form.invalid" class="timButton"
                        title="Rename task names with given names (Ctrl + S)"
                        (click)="renameTaskNamesClicked(false)">Save
                </button>
                <button class="timButton" title="Rename duplicate task names automatically"
                        (click)="renameTaskNamesClicked(true)">Rename automatically
                </button>
                <button class="timButton" title="Proceed without renaming"
                        (click)="dismiss()">Ignore
                </button>
                <button class="timButton" title="Return to editor"
                        (click)="cancelPluginRenameClicked()">Cancel
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class PluginRenameDialogComponent extends AngularDialogComponent<
    IRenameParams,
    RenameResult
> {
    protected dialogName = "PluginRename";
    newNames: string[] = [];

    constructor(private http: HttpClient) {
        super();
    }

    getTitle() {
        return "Rename duplicate task names";
    }

    ngOnInit() {}

    getDuplicates() {
        return this.data.duplicates;
    }

    /**
     * Called when user wants to cancel changes after entering duplicate task-ids
     */
    async cancelPluginRenameClicked() {
        // Cancels recent changes to paragraph/document
        if (isFromPar(this.data)) {
            await to2(
                this.http
                    .post("/cancelChanges/", {
                        docId: this.data.extraData.docId,
                        newPars: this.data.new_par_ids,
                        originalPar: this.data.original_par,
                        parId: this.data.extraData.par?.originalPar.id,
                    })
                    .toPromise()
            );
        }
        this.dismiss();
    }

    /**
     * Function that handles different cases of user input in plugin rename form
     * after user has saved multiple plugins with the same taskname
     * @param renameAutomatically - Whether to rename the tasks automatically
     */
    async renameTaskNamesClicked(renameAutomatically: boolean) {
        const duplicateData = [];
        const duplicates = this.getDuplicates();

        if (renameAutomatically) {
            if (duplicates.length > 0) {
                for (const dupe of duplicates) {
                    duplicateData.push([dupe[0], "", dupe[1]]);
                }
            }
        } else {
            // use the given names
            for (let j = 0; j < duplicates.length; j++) {
                duplicateData.push([
                    duplicates[j][0],
                    this.newNames[j],
                    duplicates[j][1],
                ]);
            }
        }
        // Save the new task names for duplicates
        const response = await to2(
            this.http
                .post<IParResponse | IManageResponse>("/postNewTaskNames/", {
                    duplicates: duplicateData,
                    docId: this.data.extraData?.docId,
                })
                .toPromise()
        );
        if (!response.ok) {
            return;
        }
        const data = response.result;
        // If no new duplicates were found
        if (!data.duplicates || data.duplicates.length <= 0) {
            this.close(data);
            return;
        }

        // If there still are duplicates, refresh the form
        if (data.duplicates.length > 0) {
            await showMessageDialog(
                "Some duplicates are still remaining. Please rename them. Click OK to update the form."
            );
            this.data.duplicates = data.duplicates;
        }
    }

    keyHandler(e: KeyboardEvent) {
        if (e.ctrlKey) {
            if (e.keyCode === KEY_S) {
                this.renameTaskNamesClicked(false);
                e.preventDefault();
            }
        }
    }
}

@NgModule({
    declarations: [PluginRenameDialogComponent],
    imports: [
        BrowserModule,
        DialogModule,
        TimUtilityModule,
        HttpClientModule,
        TooltipModule.forRoot(),
        FormsModule,
    ],
})
export class PluginRenameDialogModule {}
