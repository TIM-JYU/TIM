/**
 * Filter pipes for filtering and ordering velps
 */
import type {PipeTransform} from "@angular/core";
import {Injectable, Pipe} from "@angular/core";
import type {INewVelp, IVelpGroupUI} from "tim/velp/velptypes";

@Pipe({
    name: "filterByVelpGroups",
})
@Injectable()
export class VelpGroupFilter implements PipeTransform {
    transform(
        velps?: INewVelp[],
        groups?: IVelpGroupUI[]
    ): INewVelp[] | undefined {
        const selected: INewVelp[] = [];
        const checkedGroups = [];

        if (groups == null || velps == null) {
            return velps;
        }

        for (const g of groups) {
            if (g.show) {
                checkedGroups.push(g.id);
            }
        }

        for (const v of velps) {
            // always include velp that is being edited
            if (v.edit) {
                selected.push(v);
                continue;
            }

            for (const c of checkedGroups) {
                if (v.velp_groups.includes(c) && !selected.includes(v)) {
                    selected.push(v);
                }
            }
        }
        return selected;
    }
}
