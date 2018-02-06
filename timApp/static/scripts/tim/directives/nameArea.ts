import $ from "jquery";
import {timApp} from "tim/app";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";

timApp.directive("noPeriod", () => (scope, element, attrs) => {
    const keyCode = [190, 188, 110];
    element.bind("keydown", (event) => {
        if ($.inArray(event.which, keyCode) !== -1) {
            scope.$apply(() => {
                scope.$eval(attrs.noPeriod);
                event.preventDefault();
            });
            event.preventDefault();
        }
    });
});

export interface INameAreaOptions {
    alttext?: string;
    collapse: boolean;
    collapsible: boolean;
    endtime?: string;
    hlevel: number;
    starttime?: string;
    timed?: boolean;
    title?: string;
}

class NameAreaController extends DialogController<{}, {areaName: string, options: INameAreaOptions}, "timNameArea"> {
    private areaName: string;
    private options: INameAreaOptions;
    private datePickerOptions: { format: string; showTodayButton: boolean };

    constructor() {
        super();
        this.areaName = "";
        this.options = {
            collapse: true,
            collapsible: false,
            hlevel: 0,
        };
        this.datePickerOptions = {
            format: "D.M.YYYY HH:mm:ss",
            showTodayButton: true,
        };
    }

    private $onInit() {

    }

    private addArea() {
        if (!this.areaName) {
            return;
        }
        this.close({areaName: this.areaName, options: this.options});
    }
}

registerDialogComponent("timNameArea",
    NameAreaController,
    {templateUrl: "/static/templates/nameArea.html"});

export async function showNameAreaDialog() {
    return await showDialog<NameAreaController>("timNameArea", {});
}
