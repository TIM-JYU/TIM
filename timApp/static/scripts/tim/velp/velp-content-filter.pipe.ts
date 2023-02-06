/**
 * Filter pipes for filtering and ordering velps
 */
import type {PipeTransform} from "@angular/core";
import {Injectable, Pipe} from "@angular/core";
import type {ILabelUI, IVelp} from "tim/velp/velptypes";

@Pipe({
    name: "filterByContent",
})
@Injectable()
export class VelpContentFilter implements PipeTransform {
    transform(
        velps?: IVelp[],
        labels?: ILabelUI[],
        advancedOn?: boolean
    ): IVelp[] | undefined {
        // TODO for now a copy of label filtering, convert to velp content filter
        const selectedVelps: Record<number, [IVelp, number]> = {};
        const selectedLabels = [];

        if (!advancedOn) {
            return velps;
        }

        if (labels != null) {
            for (const l of labels) {
                if (l.selected) {
                    selectedLabels.push(l.id);
                }
            }
        }

        if (velps != null) {
            for (let j = 0; j < velps.length; j++) {
                for (const s of selectedLabels) {
                    if (velps[j].labels?.includes(s)) {
                        if (!(j in selectedVelps)) {
                            selectedVelps[j] = [velps[j], 1];
                        } else {
                            selectedVelps[j][1] += 1;
                        }
                    }
                }
            }
        }

        // return all velps if no labels selected
        // TODO can we do an early(-ier) return on this?
        if (selectedLabels.length === 0) {
            return velps;
        }

        const selectedArray = [];
        const returnVelps = [];

        for (const sv in selectedVelps) {
            if (selectedVelps.hasOwnProperty(sv)) {
                selectedArray.push(selectedVelps[sv]);
            }
        }

        selectedArray.sort((a, b) => b[1] - a[1]);

        for (const s of selectedArray) {
            returnVelps.push(s[0]);
        }

        return returnVelps;
    }
}
