import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule, Pipe} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import moment from "moment";
import humanizeDuration from "humanize-duration";
import {IItem} from "../../item/IItem";
import {ReadonlyMoment, to2} from "../../util/utils";
import {TimUtilityModule} from "../../ui/tim-utility.module";
import {DatetimePickerModule} from "../../ui/datetime-picker/datetime-picker.component";
import {DurationChoice} from "../../ui/duration-picker.component";
import {showInputDialog} from "../../ui/showInputDialog";
import {InputDialogKind} from "../../ui/input-dialog.kind";
import {IGroup} from "../../user/IUser";
import {isAdmin} from "../../user/userService";

interface IInterval {
    every: number;
    period: DurationChoice;
}

interface IScheduledFunction {
    block_id: number;
    name: string;
    expires: ReadonlyMoment;
    last_run_at: ReadonlyMoment | null;
    total_run_count: number;
    interval: IInterval;
    owners: IGroup[];
    doc_path: string;
}

@Pipe({name: "timinterval"})
class IntervalPipe {
    transform(value: IInterval): string {
        return humanizeDuration(
            moment.duration(value.every, value.period).asMilliseconds()
        );
    }
}

@Pipe({name: "timownerstr"})
class OwnerPipe {
    transform(value: IGroup[]): string {
        return value.map((g) => g.name).join(", ");
    }
}

@Component({
    selector: "tim-scheduled-functions-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header i18n>
                Manage scheduled functions
            </ng-container>
            <ng-container body>
                <h2 i18n>Current functions</h2>
                <p *ngIf="functions.length === 0" i18n>There are no functions.</p>
                <ul>
                    <li *ngFor="let t of functions">
                        <a href="/view/{{t.doc_path}}#{{t.name}}">{{t.doc_path}}#{{t.name}}</a>,
                        {{t.owners | timownerstr}},
                        <ng-container i18n>expires:</ng-container>
                        {{t.expires | timdate}},
                        <ng-container i18n>interval:</ng-container>
                        {{t.interval | timinterval}},
                        <ng-container i18n>last run:</ng-container>&ngsp;
                        <ng-container
                                *ngIf="t.last_run_at; else norun">{{t.last_run_at | timdate}}</ng-container>
                        <ng-container>,</ng-container>&ngsp; <!-- avoid space before comma -->
                        <ng-container i18n>run count:</ng-container>
                        {{t.total_run_count}}
                        <button class="btn btn-danger btn-xs" (click)="deleteFunction(t)">
                            <i class="glyphicon glyphicon-trash"></i>
                        </button>
                    </li>
                </ul>

                <div class="checkbox" *ngIf="isAdmin">
                    <label i18n>
                        <input type="checkbox"
                               (change)="fetchFunctions()"
                               [(ngModel)]="showAllUsers"> Show functions from all users
                    </label>
                </div>
                <ng-template #norun>
                    <ng-container i18n>never</ng-container>
                </ng-template>
                <h2 i18n>Add a new function</h2>
                <div class="add-function">
                    <label for="newFunction" i18n>Id of plugin to run</label>
                    <input id="newFunction"
                           class="form-control"
                           [(ngModel)]="functionName"
                           type="text">
                    <label i18n>Interval</label>
                    <tim-duration-picker
                            [required]="true"
                            [(amount)]="functionDurAmount"
                            [(type)]="functionDurType"
                    ></tim-duration-picker>
                    <label i18n>Expires</label>
                    <tim-datetime-picker [(time)]="expires"></tim-datetime-picker>
                    <label i18n>Other function parameters (in YAML format)</label>
                    <textarea [(ngModel)]="params" class="form-control"></textarea>
                    <button [disabled]="!functionName || !functionDurAmount || !expires"
                            (click)="addFunction()"
                            class="timButton" i18n>Add
                    </button>
                </div>
                <tim-alert *ngIf="error">{{error}}</tim-alert>
            </ng-container>
            <ng-container footer>
                <button class="timButton" type="button" (click)="close({})" i18n>Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["./schedule-dialog.component.scss"],
})
export class ScheduleDialogComponent extends AngularDialogComponent<
    IItem,
    unknown
> {
    protected dialogName = "ScheduledFunctions";

    functions: IScheduledFunction[] = [];
    error?: string;
    functionName = "";
    functionDurAmount = 24;
    functionDurType: DurationChoice = "hours";
    expires?: Date = moment().add(1, "weeks").toDate();
    params = "";
    showAllUsers = false;
    isAdmin = isAdmin();

    constructor(private http: HttpClient) {
        super();
    }

    async ngOnInit() {
        await this.fetchFunctions();
    }

    async fetchFunctions() {
        this.error = undefined;
        const r = await to2(
            this.http
                .get<IScheduledFunction[]>("/scheduling/functions", {
                    params: {all_users: this.showAllUsers.toString()},
                })
                .toPromise()
        );
        if (!r.ok) {
            this.error = r.result.error.error;
        } else {
            this.functions = r.result;
        }
    }

    async addFunction() {
        this.error = undefined;
        if (
            this.expires &&
            moment(this.expires).diff(moment(), "year", true) > 1
        ) {
            const ans = await to2(
                showInputDialog({
                    isInput: InputDialogKind.NoValidator,
                    okValue: true,
                    text: $localize`The function expiration date is quite far in the future (over a year). Are you sure?`,
                    title: $localize`The function expiration date is quite far in the future`,
                })
            );
            if (!ans.ok || !ans.result) {
                return;
            }
        }
        const yamlcheck = await to2(
            this.http
                .get("/scheduling/parseYaml", {params: {yaml: this.params}})
                .toPromise()
        );
        if (!yamlcheck.ok) {
            this.error = yamlcheck.result.error.error;
            return;
        }
        const r = await to2(
            this.http
                .post("/scheduling/functions", {
                    args: yamlcheck.result,
                    doc_id: this.data.id,
                    plugin_name: this.functionName,
                    expires: this.expires,
                    interval: moment.duration(
                        this.functionDurAmount,
                        this.functionDurType
                    ),
                })
                .toPromise()
        );
        if (!r.ok) {
            this.error = r.result.error.error;
        } else {
            this.functionName = "";
            await this.fetchFunctions();
        }
    }

    async deleteFunction(t: IScheduledFunction) {
        const ans = await to2(
            showInputDialog({
                isInput: InputDialogKind.NoValidator,
                okValue: true,
                text: $localize`Are you sure you want to delete this function?`,
                title: $localize`Delete function?`,
            })
        );
        if (!ans.ok || !ans.result) {
            return;
        }
        const r = await to2(
            this.http.delete(`/scheduling/functions/${t.block_id}`).toPromise()
        );
        if (!r.ok) {
            this.error = r.result.error.error;
        } else {
            await this.fetchFunctions();
        }
    }
}

@NgModule({
    declarations: [ScheduleDialogComponent, IntervalPipe, OwnerPipe],
    imports: [
        BrowserModule,
        DialogModule,
        TimUtilityModule,
        FormsModule,
        DatetimePickerModule,
    ],
})
export class ScheduleDialogModule {}
