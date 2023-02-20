/**
 * Filter pipes for filtering and ordering velps
 */
import type {PipeTransform} from "@angular/core";
import {Injectable, Pipe} from "@angular/core";
import type {ILabelUI} from "tim/velp/velptypes";

@Pipe({
    name: "filterByLabelContent",
})
@Injectable()
export class LabelContentFilter implements PipeTransform {
    transform(
        labels?: ILabelUI[],
        advancedOn?: boolean
    ): ILabelUI[] | undefined {
        // TODO for now a copy of velp filtering by label
        const selectedLabels = [];

        if (!advancedOn) {
            return labels;
        }

        if (labels != null) {
            for (const l of labels) {
                if (l.selected) {
                    // selectedLabels.push(l.id);
                    selectedLabels.push(l);
                }
            }
        }

        // const selectedArray = [];
        // const returnVelps = [];

        // for (const sv in selectedVelps) {
        //     if (selectedVelps.hasOwnProperty(sv)) {
        //         selectedArray.push(selectedVelps[sv]);
        //     }
        // }

        // selectedArray.sort((a, b) => b[1] - a[1]);

        // for (const s of selectedArray) {
        //     returnVelps.push(s[0]);
        // }

        // return returnVelps;
        return selectedLabels;
    }
}
