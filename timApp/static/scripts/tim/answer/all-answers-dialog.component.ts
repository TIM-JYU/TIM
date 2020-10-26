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
import {either} from "fp-ts/Either";
import {maybeUndefined} from "tim/plugin/attributes";
import {$httpParamSerializer} from "../util/ngimport";
import {ReadonlyMoment, TimStorage, to2} from "../util/utils";

// from https://github.com/gcanti/io-ts/blob/master/index.md#custom-types
const DateFromString = new t.Type<Date, string, unknown>(
    "DateFromString",
    (u): u is Date => u instanceof Date,
    (u, c) =>
        either.chain(t.string.validate(u, c), (s) => {
            const d = new Date(s);
            return isNaN(d.getTime()) ? t.failure(u, c) : t.success(d);
        }),
    (a) => a.toISOString()
);

const AnswersDialogOptions = t.type({
    period: t.keyof({
        whenever: null,
        day: null,
        week: null,
        month: null,
        other: null,
    }),
    print: t.keyof({
        all: null,
        header: null,
        answers: null,
        answersnoline: null,
        korppi: null,
    }),
    age: t.string,
    valid: t.string,
    name: t.string,
    sort: t.string,
    consent: t.string,
    format: t.keyof({text: null, json: null}),
    periodFrom: maybeUndefined(DateFromString),
    periodTo: maybeUndefined(DateFromString),
});

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
                            <label class="radio-inline">Answer age</label>
                        </div>
                        <div class="col-sm-9">
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.age" name="age" value="max">
                                    Newest of each user
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.age" name="age" value="min">
                                    Oldest of each user
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.age" name="age" value="all">
                                    All
                                </label>
                            </div>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-3">
                            <label class="radio-inline">Period</label>
                        </div>
                        <div class="col-sm-9">
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="whenever">
                                    Whenever
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="sincelast">
                                    Since last answer fetch
                                    (
                                    <ng-container *ngIf="lastFetch">{{ lastFetch | timdate }}</ng-container>
                                    <ng-container *ngIf="!lastFetch">no fetches yet</ng-container>
                                    )
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="day">
                                    Past day
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="week">
                                    Past week
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="month">
                                    Past month
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.period" name="period" value="other">
                                    Other...
                                </label>
                                <div *ngIf="options.period === 'other'">
                                    <tim-datetime-picker [(time)]="options.periodFrom"></tim-datetime-picker>
                                    to
                                    <tim-datetime-picker [(time)]="options.periodTo"></tim-datetime-picker>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-3">
                            <label class="radio-inline">Validity</label>
                        </div>
                        <div class="col-sm-9">
                            <label class="radio-inline">
                                <input type="radio" [(ngModel)]="options.valid" name="valid" value="1"> Valid
                            </label>
                            <label class="radio-inline">
                                <input type="radio" [(ngModel)]="options.valid" name="valid" value="0"> Invalid
                            </label>
                            <label class="radio-inline">
                                <input type="radio" [(ngModel)]="options.valid" name="valid" value="all"> All
                            </label>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-3">
                            <label class="radio-inline">Names</label>
                        </div>
                        <div class="col-sm-9">
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.name" name="name" value="both">
                                    Username and full name
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.name" name="name" value="username">
                                    Username only
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.name" name="name" value="anonymous">
                                    Anonymous username
                                </label>
                            </div>
                        </div>
                    </div>
                    <div class="form-group" *ngIf="showSort">
                        <div class="col-sm-3">
                            <label class="radio-inline">Sort by</label>
                        </div>
                        <div class="col-sm-9">
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.sort" name="sort" value="task">
                                    Task, then username
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.sort" name="sort" value="username">
                                    Username, then task
                                </label>
                            </div>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-3">
                            <label class="radio-inline">Print</label>
                        </div>
                        <div class="col-sm-9">
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.print" name="print" value="all">
                                    Headers and answers
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.print" name="print" value="header">
                                    Headers only
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.print" name="print" value="answers">
                                    Answers only
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.print" name="print" value="answersnoline">
                                    Answers only without separator line
                                </label>
                            </div>
                            <div class="radio">
                                <label>
                                    <input type="radio" [(ngModel)]="options.print" name="print" value="korppi">
                                    Korppi export
                                </label>
                            </div>
                        </div>
                    </div>
                    <div class="form-group">
                        <div class="col-sm-3">
                            <label class="radio-inline" title="Format for output">Output fmt</label>
                        </div>
                        <div class="col-sm-9">
                            <label class="radio-inline">
                                <input type="radio" [(ngModel)]="options.format" name="format" value="text">
                                Text
                            </label>
                            <label class="radio-inline">
                                <input type="radio" [(ngModel)]="options.format" name="format" value="json">
                                JSON
                            </label>
                        </div>
                    </div>
                </form>
            </ng-container>
            <ng-container footer>
                <button class="timButton" type="button" (click)="ok()">Get answers
                </button>
                <button class="btn btn-default" type="button" (click)="cancel()">Cancel</button>
            </ng-container>
        </tim-dialog-frame>
    `,
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
        return "Get answers";
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
        } as const;

        this.options = this.storage.get() ?? defs;

        (async () => {
            const r = await to2(
                this.http
                    .get<{last_answer_fetch: {[index: string]: string}}>(
                        "/settings/get/last_answer_fetch"
                    )
                    .toPromise()
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
