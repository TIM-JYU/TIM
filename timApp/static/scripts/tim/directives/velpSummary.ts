/**
 * The directive handles velp summary. Requires reviewController.
 *
 * @module velpSummary
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
 */

import angular from "angular";
import {timApp} from "tim/app";

type Annotation = {points, selected: boolean};

export class VelpSummaryController {
    private settings: {selectedAll: boolean};
    private annotations: Annotation[];
    private toggleAnnotation: (annotation) => void;

    constructor() {
        this.settings = {selectedAll: false};
    }

    /**
     * Gets total number of points.
     * @method getTotalPoints
     * @returns {number} Total number of points
     */
    getTotalPoints(annotations) {
        let p = 0;
        if (annotations === undefined) {
            return p;
        }

        for (let i = 0; i < annotations.length; i++) {

            p += this.annotations[i].points;

        }
        //cast back to a number, the string has trailing zeros.
        return Number(p.toPrecision(4));
    }

    /**
     * Checks all checkboxes linked to the annotations in the velp summary.
     * @method checkAll
     */
    checkAll() {
        angular.forEach(this.annotations, (a) => {
            a.selected = this.settings.selectedAll;
        });
    }
}

/**
 * Angular directive for phrase selection
 * @lends module:velpSummary
 */
timApp.component("velpSummary", {
        bindings: {annotations: "=", toggleAnnotation: "&", selectedUser: "<"},
        controller: VelpSummaryController,
        controllerAs: "vsumctrl",
        templateUrl: "/static/templates/velpSummary.html",
    },
);
