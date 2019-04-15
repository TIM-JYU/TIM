import {IPromise} from "angular";
import moment from "moment";
import {ngStorage} from "ngstorage";
import {DialogController, registerDialogComponent, showDialog} from "../ui/dialog";
import {$http, $httpParamSerializer, $localStorage} from "../util/ngimport";
import {to} from "../util/utils";

interface IOptions {
    age: string;
    valid: string;
    name: string;
    sort: string;
    periodFrom: any;
    periodTo: any;
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
    private options!: IOptions; // $onInit
    private $storage!: ngStorage.StorageService & {allAnswersOptions: IOptions}; // $onInit
    private datePickerOptionsFrom!: EonasdanBootstrapDatetimepicker.SetOptions; // $onInit
    private datePickerOptionsTo!: EonasdanBootstrapDatetimepicker.SetOptions; // $onInit
    private lastFetch: any;

    protected getTitle() {
        return "Get answers";
    }

    async $onInit() {
        super.$onInit();
        const options = this.resolve.params;
        moment.locale("en", {
            week: {dow: 1, doy: 4}, // set Monday as the first day of the week
        });
        this.showSort = options.allTasks;
        this.options = {
            age: "max",
            valid: "1",
            name: "both",
            sort: "username",
            periodFrom: null,
            periodTo: null,
            consent: "any",
        };
        this.$storage = $localStorage.$default({
            allAnswersOptions: this.options,
        });

        this.options = this.$storage.allAnswersOptions;
        this.options.periodFrom = this.options.periodFrom || Date.now();
        this.options.periodTo = this.options.periodTo || Date.now();
        this.datePickerOptionsFrom = {
            format: "D.M.YYYY HH:mm:ss",
            defaultDate: moment(this.options.periodFrom),
            showTodayButton: true,
        };
        this.datePickerOptionsTo = {
            format: "D.M.YYYY HH:mm:ss",
            defaultDate: moment(this.options.periodTo),
            showTodayButton: true,
        };

        this.lastFetch = null;
        const r = await to($http.get<{last_answer_fetch: {[index: string]: string}}>("/settings/get/last_answer_fetch"));
        if (r.ok && r.result.data.last_answer_fetch) {
            this.lastFetch = r.result.data.last_answer_fetch[options.identifier];
            if (!this.lastFetch) {
                this.lastFetch = "no fetches yet";
            }
        }
    }

    ok() {
        if (this.options.periodFrom) {
            this.options.periodFrom = this.options.periodFrom.toDate();
        }
        if (this.options.periodTo) {
            this.options.periodTo = this.options.periodTo.toDate();
        }
        window.open(this.resolve.params.url + "?" + $httpParamSerializer(this.options), "_blank");
        this.close({});
    }

    cancel() {
        this.dismiss();
    }
}

registerDialogComponent(AllAnswersCtrl, {templateUrl: "/static/templates/allAnswersOptions.html"});

export function showAllAnswers(p: IAllAnswersParams): IPromise<AllAnswersCtrl["ret"]> {
    return showDialog(AllAnswersCtrl, {params: () => p}).result;
}
