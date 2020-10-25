import moment, {Moment} from "moment";
import {ngStorage} from "ngstorage";
import {DialogController} from "tim/ui/dialogController";
import {registerDialogComponent, showDialog} from "../ui/dialog";
import {IUser} from "../user/IUser";
import {$http, $httpParamSerializer, $localStorage} from "../util/ngimport";
import {dateFormat, to} from "../util/utils";

interface IFBOptions<T> {
    period: "whenever" | "sincelast" | "day" | "week" | "month" | "other";
    valid: string;
    name: string;
    periodFrom: T;
    periodTo: T;
    scope: string;
    answers: string;
    format: string;
    users: string;
    decimal: string;
}

export interface IFeedbackAnswersParams {
    identifier: string;
    users: IUser[];
    url: string;
    allTasks: boolean;
}

export class FeedbackAnswersCtrl extends DialogController<
    {params: IFeedbackAnswersParams},
    void
> {
    static component = "timFeedbackAnswers";
    static $inject = ["$element", "$scope"] as const;
    private options?: IFBOptions<Moment>;
    private storage?: ngStorage.StorageService & {
        feedbackAnswersOptions: IFBOptions<number | null>;
    };
    private showSort: boolean = false;
    private datePickerOptionsFrom?: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsTo?: EonasdanBootstrapDatetimepicker.SetOptions;
    private lastFetch: unknown;

    protected getTitle() {
        return "Export to csv";
    }

    $onInit() {
        super.$onInit();
        const options = this.resolve.params;
        this.showSort = options.allTasks;
        const defs = {
            period: "whenever",
            valid: "1",
            name: "both",
            periodFrom: null,
            periodTo: null,
            scope: "task",
            answers: "all",
            format: "semicolon",
            users: "",
            decimal: "point",
        } as const;
        this.storage = $localStorage.$default({
            feedbackAnswersOptions: defs,
        });

        this.options = {
            ...this.storage.feedbackAnswersOptions,
            periodFrom: moment(
                this.storage.feedbackAnswersOptions.periodFrom ?? Date.now()
            ),
            periodTo: moment(
                this.storage.feedbackAnswersOptions.periodFrom ?? Date.now()
            ),
        };
        this.datePickerOptionsFrom = {
            format: dateFormat,
            defaultDate: moment(this.options.periodFrom),
            showTodayButton: true,
        };
        this.datePickerOptionsTo = {
            format: dateFormat,
            defaultDate: moment(this.options.periodTo),
            showTodayButton: true,
        };

        this.lastFetch = null;

        (async () => {
            const r = await to(
                $http.get<{last_answer_fetch: {[index: string]: string}}>(
                    "/settings/get/last_answer_fetch"
                )
            );
            if (r.ok && r.result.data.last_answer_fetch) {
                this.lastFetch =
                    r.result.data.last_answer_fetch[options.identifier];
                if (!this.lastFetch) {
                    this.lastFetch = "no fetches yet";
                }
            }
        })();
        this.options.users = "";
        for (const user of options.users) {
            this.options.users += user.name + ",";
        }
    }

    ok() {
        if (!this.options || !this.storage) {
            return;
        }
        const toSerialize: IFBOptions<Date | null> = {
            ...this.options,
            periodFrom: this.options.periodFrom.toDate(),
            periodTo: this.options.periodTo.toDate(),
        };
        this.storage.feedbackAnswersOptions = {
            ...this.options,
            periodFrom: this.options.periodFrom.valueOf(),
            periodTo: this.options.periodTo.valueOf(),
        };
        window.open(
            this.resolve.params.url + "?" + $httpParamSerializer(toSerialize),
            "_blank"
        );
        this.close();
    }
    cancel() {
        this.dismiss();
    }
}

registerDialogComponent(FeedbackAnswersCtrl, {
    templateUrl: "/static/templates/allFeedbackAnswersOptions.html",
});

export function showFeedbackAnswers(p: IFeedbackAnswersParams) {
    return showDialog(FeedbackAnswersCtrl, {params: () => p}).result;
}
