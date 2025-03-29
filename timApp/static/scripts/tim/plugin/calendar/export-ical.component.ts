import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";
import {defaultWuffMessage, to2, toPromise} from "tim/util/utils";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";

@Component({
    selector: "export-ical-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header i18n>
                Export calendar as iCal
            </ng-container>
            <ng-container body>
                <h3 i18n>Export options</h3>
                <form>
                    <fieldset [disabled]="exportingICal">
                        <section>
                            <div class="flex align-center export-options">
                                <input id="min-booked-by" type="checkbox" name="min-booked-by" [(ngModel)]="exportOwnedMinByToggle">
                                <label for="min-booked-by" i18n>
                                    Include events booked by at least
                                </label>
                                <input [disabled]="!exportOwnedMinByToggle" [(ngModel)]="ownedMinBy" id="min-booked-by-value" class="form-control short-box" type="number" name="min-booked-by-value" min="0">
                                <label for="min-booked-by-value" i18n>people.</label>
                            </div>
                        </section>
                    </fieldset>
                </form>
                
                <div *ngIf="icsURL" class="ics-url-results">
                    <p i18n>
                        Copy the following URL to import the calendar into your calendar application:
                    </p>
                    <input class="form-control" [value]="icsURL" readonly (click)="copyAndSelectUrl(icalUrlInput)" #icalUrlInput>
                </div>
            </ng-container>
            <ng-container footer>
                <tim-loading *ngIf="exportingICal"></tim-loading>
                <button [disabled]="exportingICal" class="timButton" type="button" (click)="doExport()" i18n>
                    Export calendar
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["export-ical.component.scss"],
})
export class ExportIcalComponent extends AngularDialogComponent<
    unknown,
    unknown
> {
    protected dialogName = "ExportICal";
    exportingICal = false;
    icsURL?: string;
    exportDone?: string;
    exportOwnedMinByToggle = false;
    ownedMinBy: number = 1;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit(): void {}

    async doExport() {
        this.icsURL = undefined;
        this.exportingICal = true;
        const result = await toPromise(
            this.http.get("/calendar/export", {
                responseType: "text",
            })
        );
        if (result.ok) {
            const url = result.result;
            // Parse URL
            const urlObj = new URL(url);
            // Add query parameters
            if (this.exportOwnedMinByToggle) {
                urlObj.searchParams.set(
                    "showBookedByMin",
                    this.ownedMinBy.toString()
                );
            }
            this.icsURL = urlObj.toString();
        } else {
            const err: string = result.result.error as unknown as string;
            let errObj: {error: string} | undefined;
            try {
                errObj = JSON.parse(err);
            } catch (e) {}
            if (result.result.error.error) {
                await showMessageDialog(result.result.error.error);
            } else if (errObj?.error) {
                await showMessageDialog(errObj.error);
            } else {
                await showMessageDialog(defaultWuffMessage);
            }
        }
        this.exportingICal = false;
    }

    async copyAndSelectUrl(icalUrlInput: HTMLInputElement) {
        if (!this.icsURL) {
            return;
        }
        icalUrlInput.select();
        await to2(navigator.clipboard.writeText(this.icsURL));
    }
}

@NgModule({
    declarations: [ExportIcalComponent],
    imports: [
        CommonModule,
        DialogModule,
        FormsModule,
        TimUtilityModule,
        DialogModule,
    ],
})
export class ExportIcalComponentModule {}
