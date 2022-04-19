import moment from "moment";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {BrowserModule} from "@angular/platform-browser";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {DatetimePickerModule} from "tim/ui/datetime-picker/datetime-picker.component";
import * as t from "io-ts";
import {CommonDialogOptions} from "tim/answer/commondialogoptions";
import {ReadonlyMoment} from "tim/util/readonlymoment";
import {maybeUndefined} from "tim/plugin/attributes";
import {$httpParamSerializer} from "../util/ngimport";
import {TimStorage, toPromise} from "../util/utils";

const AnswersDialogOptions = t.intersection([
    t.type({
        print: t.keyof({
            all: null,
            header: null,
            answers: null,
            answersnoline: null,
            korppi: null,
        }),
        sort: t.string,
        age: t.string,
        consent: t.string,
        format: t.keyof({text: null, json: null}),
        salt: maybeUndefined(t.string),
    }),
    CommonDialogOptions,
]);

interface IOptions extends t.TypeOf<typeof AnswersDialogOptions> {}

export interface IAllAnswersParams {
    identifier: string;
    url: string;
    allTasks: boolean;
}

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "tim-all-answers-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <form class="form-horizontal">
                    <div class="form-group">
                        <div class="col-sm-3">
                            <label class="radio-inline" i18n>Answer age</label>
                        </div>
                        <div class="col-sm-9">
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.age" name="age" value="max">
                                    <ng-container i18n>Newest of each user</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.age" name="age" value="min">
                                    <ng-container i18n>Oldest of each user</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.age" name="age" value="all">
                                    <ng-container i18n>All</ng-container>
                                </label>
                            </div>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-3">
                            <label class="radio-inline" i18n>Period</label>
                        </div>
                        <div class="col-sm-9">
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="whenever">
                                    <ng-container i18n>Whenever</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="sincelast">
                                    <ng-container i18n>Since last answer fetch</ng-container>
                                    (
                                    <ng-container *ngIf="lastFetch">{{ lastFetch | timdate }}</ng-container>
                                    <ng-container *ngIf="!lastFetch" i18n>no fetches yet</ng-container>
                                    )
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="day">
                                    <ng-container i18n>Past day</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="week">
                                    <ng-container i18n>Past week</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="month">
                                    <ng-container i18n>Past month</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="other">
                                    <ng-container i18n>Other...</ng-container>
                                </label>
                                <div *ngIf="options.period === 'other'">
                                    <ng-container i18n>from</ng-container>
                                    <tim-datetime-picker [(time)]="options.periodFrom"></tim-datetime-picker>
                                    <ng-container i18n>to</ng-container>
                                    <tim-datetime-picker [(time)]="options.periodTo"></tim-datetime-picker>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-3">
                            <label class="radio-inline" i18n>Validity</label>
                        </div>
                        <div class="col-sm-9">
                            <label class="radio-inline">
                                <input type="radio" [(ngModel)]="options.valid" name="valid" value="1">
                                <ng-container i18n>Valid</ng-container>
                            </label>
                            <label class="radio-inline">
                                <input type="radio" [(ngModel)]="options.valid" name="valid" value="0">
                                <ng-container i18n>Invalid</ng-container>
                            </label>
                            <label class="radio-inline">
                                <input type="radio" [(ngModel)]="options.valid" name="valid" value="all">
                                <ng-container i18n>All</ng-container>
                            </label>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-3">
                            <label class="radio-inline" i18n>Names</label>
                        </div>
                        <div class="col-sm-9">
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.name" name="name" value="both">
                                    <ng-container i18n>Username and full name</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.name" name="name" value="username">
                                    <ng-container i18n>Username only</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.name" name="name" value="anonymous">
                                    <ng-container i18n>Anonymous username</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.name" name="name" value="pseudonym">
                                    <ng-container i18n>Pseudonymous username</ng-container>
                                </label>
                                <ng-container *ngIf="options.name === 'pseudonym'">
                                    <div class="input-group pseudonym-input">
                                        <input #salt="ngModel" minlength="10" [(ngModel)]="options.salt" name="salt" type="text" class="form-control" placeholder="Key for generating pseudonyms" i18n-placeholder>
                                        <span class="input-group-btn">
                                            <button class="btn btn-default" type="button" title="Generate a random key" i18n-title (click)="setRandomSalt()"><i class="glyphicon glyphicon-refresh"></i></button>
                                        </span>
                                    </div>
                                    <div *ngIf="salt.value === undefined || salt.value === null || salt.value.length == 0" class="pseudo-info" i18n>Specify a key to generate pseudonyms. A user will always get the same pseudonym for a key.</div>
                                    <div *ngIf="salt.invalid" class="validation-error" i18n>The key should be at least 10 characters long</div>
                                </ng-container>
                            </div>
                        </div>
                    </div>
                    <div class="form-group" *ngIf="showSort">
                        <div class="col-sm-3">
                            <label class="radio-inline" i18n>Sort by</label>
                        </div>
                        <div class="col-sm-9">
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.sort" name="sort" value="task">
                                    <ng-container i18n>Task, then username</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.sort" name="sort" value="username">
                                    <ng-container i18n>Username, then task</ng-container>
                                </label>
                            </div>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-3">
                            <label class="radio-inline" i18n>Print</label>
                        </div>
                        <div class="col-sm-9">
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.print" name="print" value="all">
                                    <ng-container i18n>Headers and answers</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.print" name="print" value="header">
                                    <ng-container i18n>Headers only</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.print" name="print" value="answers">
                                    <ng-container i18n>Answers only</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.print" name="print" value="answersnoline">
                                    <ng-container i18n>Answers only without separator line</ng-container>
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.print" name="print" value="korppi">
                                    <ng-container i18n>Korppi export</ng-container>
                                </label>
                            </div>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-3">
                            <label class="radio-inline" title="Format for output" i18n>Output fmt</label>
                        </div>
                        <div class="col-sm-9">
                            <label class="radio-inline">
                                <input type="radio" [(ngModel)]="options.format" name="format" value="text">
                                <ng-container i18n>Text</ng-container>
                            </label>
                            <label class="radio-inline">
                                <input type="radio" [(ngModel)]="options.format" name="format" value="json">
                                <ng-container i18n>JSON</ng-container>
                            </label>
                        </div>
                    </div>
                </form>
            </ng-container>
            <ng-container footer>
                <button class="timButton" type="button" (click)="ok()" i18n>Get answers</button>
                <button class="btn btn-default" type="button" (click)="cancel()" i18n>Cancel</button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["all-answers-dialog.component.scss"],
})
export class AllAnswersDialogComponent extends AngularDialogComponent<
    IAllAnswersParams,
    void
> {
    protected dialogName = "AllAnswers";
    showSort: boolean = false;
    options!: IOptions;
    private storage = new TimStorage("allAnswersOptions", AnswersDialogOptions);
    lastFetch?: ReadonlyMoment;

    constructor(private http: HttpClient) {
        super();
    }

    getTitle() {
        return $localize`Get answers`;
    }

    setRandomSalt() {
        const salt = new Uint8Array(16);
        window.crypto.getRandomValues(salt);
        // Convert to base64
        this.options.salt = String.fromCharCode(
            ...salt.map((b) => (b % 62) + (b < 62 ? 48 : 55))
        );
    }

    ngOnInit() {
        const options = this.data;
        this.showSort = options.allTasks;

        const defs = {
            age: "max",
            valid: "1",
            name: "both",
            sort: "username",
            periodFrom: undefined,
            periodTo: undefined,
            consent: "any",
            period: "whenever",
            print: "all",
            format: "text",
            salt: undefined,
        } as const;

        this.options = this.storage.get() ?? defs;

        (async () => {
            const r = await toPromise(
                this.http.get<{last_answer_fetch: Record<string, string>}>(
                    "/settings/get/last_answer_fetch"
                )
            );
            if (r.ok && r.result.last_answer_fetch) {
                this.lastFetch = moment(
                    r.result.last_answer_fetch[options.identifier.toString()]
                );
            }
        })();
    }

    ok() {
        if (!this.options || !this.storage) {
            return;
        }
        const toSerialize: IOptions = {
            ...this.options,
            periodFrom: this.options.periodFrom,
            periodTo: this.options.periodTo,
        };
        this.storage.set(this.options);
        // Don't include the salt in the serialized data if it's not used
        if (toSerialize.name !== "pseudonym") {
            toSerialize.salt = undefined;
        }
        window.open(
            this.data.url + "?" + $httpParamSerializer(toSerialize),
            "_blank"
        );
        this.close();
    }

    cancel() {
        this.dismiss();
    }
}

@NgModule({
    declarations: [AllAnswersDialogComponent],
    imports: [
        BrowserModule,
        DialogModule,
        FormsModule,
        HttpClientModule,
        TimUtilityModule,
        DatetimePickerModule,
    ],
})
export class AllAnswersDialogModule {}
