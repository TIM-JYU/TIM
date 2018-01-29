import {IController} from "angular";
import moment from "moment";
import {ngStorage} from "ngstorage";
import {timApp} from "tim/app";
import {$http, $httpParamSerializer, $localStorage, $log, $window} from "../ngimport";

type Options = {age: string, valid: string, name: string, sort: string; periodFrom: any; periodTo: any;};

export interface IAllAnswersParams {
    identifier: string;
    url: string;
    allTasks: boolean;
}

export class AllAnswersCtrl implements IController {
    private static $inject = ["$uibModalInstance", "options"];
    private showSort: boolean;
    private options: Options;
    private $storage: ngStorage.StorageService & {allAnswersOptions: Options};
    private datePickerOptionsFrom: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsTo: EonasdanBootstrapDatetimepicker.SetOptions;
    private lastFetch: any;
    private uibModalInstance: angular.ui.bootstrap.IModalInstanceService;
    private url: string;

    constructor(uibModalInstance: angular.ui.bootstrap.IModalInstanceService, options: IAllAnswersParams) {
        this.uibModalInstance = uibModalInstance;
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
        };
        this.url = options.url;
        this.options.age = "max";
        this.options.valid = "1";
        this.options.name = "both";
        this.options.sort = "username";
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
        $http.get<{last_answer_fetch: {[index: string]: string}}>("/settings/get/last_answer_fetch").then((response) => {
            if (response.data.last_answer_fetch) {
                this.lastFetch = response.data.last_answer_fetch[options.identifier];
                if (!this.lastFetch) {
                    this.lastFetch = "no fetches yet";
                }
            }
        }, (response) => {
            $log.error("Failed to fetch last answer time");
        });
    }

    $onInit() {

    }

    ok() {
        if (this.options.periodFrom) {
            this.options.periodFrom = this.options.periodFrom.toDate();
        }
        if (this.options.periodTo) {
            this.options.periodTo = this.options.periodTo.toDate();
        }
        $window.open(this.url + "?" + $httpParamSerializer(this.options), "_blank");
        this.uibModalInstance.close("close");
    }

    cancel() {
        this.uibModalInstance.dismiss("cancel");
    }
}

timApp.component("timAllAnswers", {
    controller: AllAnswersCtrl,
    templateUrl: "/static/templates/allAnswersOptions.html",
});
