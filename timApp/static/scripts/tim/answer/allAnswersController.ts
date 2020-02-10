import moment, {Moment} from "moment";
import {ngStorage} from "ngstorage";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {$http, $httpParamSerializer, $localStorage} from "../util/ngimport";
import {dateFormat, to} from "../util/utils";

interface IOptions<T> {
    age: string;
    valid: string;
    name: string;
    sort: string;
    periodFrom: T;
    periodTo: T;
    consent: string;
}

export interface IAllAnswersParams {
    identifier: string;
    url: string;
    allTasks: boolean;
}

export class AllAnswersCtrl extends DialogController<{params: IAllAnswersParams}, {}> {
    static component = "timAllAnswers";
    static $inject = ["$element", "$scope"] as const;
    private showSort: boolean = false;
    private options?: IOptions<Moment>;
    private storage?: ngStorage.StorageService & {allAnswersOptions: IOptions<number | null>};
    private datePickerOptionsFrom?: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsTo?: EonasdanBootstrapDatetimepicker.SetOptions;
    private lastFetch: unknown;

    protected getTitle() {
        return "Get answers";
    }

    $onInit() {
        super.$onInit();
        const options = this.resolve.params;
        this.showSort = options.allTasks;

        const defs = {
            age: "max",
            valid: "1",
            name: "both",
            sort: "username",
            periodFrom: null,
            periodTo: null,
            consent: "any",
        };
        this.storage = $localStorage.$default({
            allAnswersOptions: defs,
        });

        this.options = {
            ...this.storage.allAnswersOptions,
            periodFrom: moment(this.storage.allAnswersOptions.periodFrom ?? Date.now()),
            periodTo: moment(this.storage.allAnswersOptions.periodFrom ?? Date.now()),
            // The consent option was removed from the dialog (because the consent dialog was disabled),
            // so make sure that it does not restrict the search in case in is in local storage.
            consent: "any",
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
            const r = await to($http.get<{last_answer_fetch: {[index: string]: string}}>("/settings/get/last_answer_fetch"));
            if (r.ok && r.result.data.last_answer_fetch) {
                this.lastFetch = r.result.data.last_answer_fetch[options.identifier];
                if (!this.lastFetch) {
                    this.lastFetch = "no fetches yet";
                }
            }
        })();
    }

    ok() {
        if (!this.options || !this.storage) {
            return;
        }
        const toSerialize: IOptions<Date | null> = {
            ...this.options,
            periodFrom: this.options.periodFrom.toDate(),
            periodTo: this.options.periodTo.toDate(),
        };
        this.storage.allAnswersOptions = {
            ...this.options,
            periodFrom: this.options.periodFrom.valueOf(),
            periodTo: this.options.periodTo.valueOf(),
        };
        window.open(this.resolve.params.url + "?" + $httpParamSerializer(toSerialize), "_blank");
        this.close({});
    }

    cancel() {
        this.dismiss();
    }
}

registerDialogComponent(AllAnswersCtrl, {templateUrl: "/static/templates/allAnswersOptions.html"});

export function showAllAnswers(p: IAllAnswersParams) {
    return showDialog(AllAnswersCtrl, {params: () => p}).result;
}
