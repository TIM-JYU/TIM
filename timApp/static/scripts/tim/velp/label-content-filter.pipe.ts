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
        labels: ILabelUI[],
        searchString?: string,
        advancedOn?: boolean
    ): ILabelUI[] {
        const returnLabels = [];

        if (!advancedOn || !searchString) {
            return labels;
        }

        for (const l of labels) {
            if (l.content.includes(searchString)) {
                returnLabels.push(l);
            }
        }

        return returnLabels;
    }
}
