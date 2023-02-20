/**
 * Filter pipes for filtering and ordering velps
 */
import type {PipeTransform} from "@angular/core";
import {Injectable, Pipe} from "@angular/core";
import type {IVelp, IVelpGroupUI} from "tim/velp/velptypes";

@Pipe({
    name: "filterByVelpGroups",
})
@Injectable()
export class VelpGroupFilter implements PipeTransform {
    transform(
        velps: IVelp[],
        groups?: IVelpGroupUI[],
        advancedOn?: boolean
    ): IVelp[] {
        const selected: IVelp[] = [];
        const checkedGroups = [];

        // TODO advancedOn is probably redundant here:
        //   search/filter options are not visible to user if advancedOn is false
        if (groups == null || velps == null || !advancedOn) {
            return velps;
        }

        for (const g of groups) {
            if (g.show) {
                checkedGroups.push(g.id);
            }
        }

        for (const v of velps) {
            // always include velp that is being edited

            // TODO find a way to include velp that is being edited (maybe use velpToEdit from VelpMenuComponent)
            // if (v.edit) {
            //     selected.push(v);
            //     continue;
            // }

            for (const c of checkedGroups) {
                if (v.velp_groups.includes(c) && !selected.includes(v)) {
                    selected.push(v);
                }
            }
        }
        return selected;
    }
}
