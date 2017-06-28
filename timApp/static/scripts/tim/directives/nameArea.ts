import {IRootElementService} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";

class NameAreaController {
    private static $inject = ["$element"];
    private $area: JQuery;
    private areaName: string;
    private options: {collapse: boolean; hlevel: number};
    private datePickerOptions: {format: string; showTodayButton: boolean};
    private element: IRootElementService;
    private onClose: (e: JQuery) => void;
    private onCancel: (e: JQuery) => void;
    private onOk: (area: JQuery, name: string, options) => void;

    constructor(element: IRootElementService) {
        this.element = element;
        this.$area = element.parents(".area").first();

        const area = $("#areaname");
        area.keypress(function(e) {
            if (e.which == 13) {
                this.addArea();
            }
        });

        this.areaName = "";
        this.options = {collapse: true, hlevel: 0};
        element.css("position", "absolute"); // IE needs this
        this.datePickerOptions = {
            format: "D.M.YYYY HH:mm:ss",
            showTodayButton: true,
        };

        area.focus();
    }

    closePopup() {
        this.element.remove();

        if (this.onClose) {
            this.onClose(this.$area);
        }
    }

    addArea() {
        this.closePopup();

        if (this.onOk) {
            this.onOk(this.$area, this.areaName, this.options);
        }
    }

    cancelAdd() {
        this.closePopup();

        if (this.onCancel) {
            this.onCancel(this.$area);
        }
    }
}

/**
 * A popup window directive that is used in the document view
 * when creating a new area.
 */
timApp.component("nameArea", {
    bindings: {
        onCancel: "&",
        onClose: "&",
        onOk: "&",
    },
    controller: NameAreaController,
    templateUrl: "/static/templates/nameArea.html",
});
