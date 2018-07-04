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
import {IController} from "angular";
import {timApp} from "tim/app";
import {Binding} from "../util/utils";
import {IAnnotation, IUIFields} from "./velptypes";

export class VelpSummaryController implements IController {
    private settings: {selectedAll: boolean};
    private annotations!: Binding<Array<IAnnotation & IUIFields>, "<">;
    private toggleAnnotation!: Binding<(params: {$ANNOTATION: IAnnotation, $SCROLL: boolean}) => void, "&">;

    constructor() {
        this.settings = {selectedAll: false};
    }

    $onInit() {
    }

    /**
     * Gets total number of points.
     * @method getTotalPoints
     * @returns {number} Total number of points
     */
    getTotalPoints(annotations: IAnnotation[]) {
        let p = 0;
        if (annotations == null) {
            return p;
        }

        for (let i = 0; i < annotations.length; i++) {

            p += this.annotations[i].points || 0;

        }
        // cast back to a number, the string has trailing zeros.
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
        bindings: {
            annotations: "<",
            selectedUser: "<",
            toggleAnnotation: "&",
        },
        controller: VelpSummaryController,
        controllerAs: "vsumctrl",
        templateUrl: "/static/templates/velpSummary.html",
    },
);
