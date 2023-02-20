/**
 * Filter pipes for filtering and ordering velps
 */
import type {PipeTransform} from "@angular/core";
import {Injectable, Pipe} from "@angular/core";
import type {IVelp} from "tim/velp/velptypes";

@Pipe({
    name: "filterByContent",
})
@Injectable()
export class VelpContentFilter implements PipeTransform {
    transform(
        velps: IVelp[],
        searchString?: string,
        advancedOn?: boolean
    ): IVelp[] {
        // TODO advancedOn is probably redundant here:
        //   search/filter options are not visible to user if advancedOn is false
        if (!advancedOn || !searchString) {
            return velps;
        }

        const returnVelps = [];
        if (velps != null) {
            for (const v of velps) {
                if (v.content.includes(searchString)) {
                    returnVelps.push(v);
                }
            }
        }
        return returnVelps;
    }
}
