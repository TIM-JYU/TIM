/**
 * Filter pipes for filtering and ordering velps
 */
import type {PipeTransform} from "@angular/core";
import {Injectable, Pipe} from "@angular/core";
import type {INewVelp, IVelp} from "tim/velp/velptypes";

@Pipe({
    name: "orderByWhenNotEditing",
})
@Injectable()
export class VelpOrder implements PipeTransform {
    transform(
        velps: INewVelp[],
        orderStr: string,
        filteredVelps: IVelp[],
        sortLang?: string
    ): INewVelp[] {
        for (const v of velps) {
            if (v.edit) {
                return filteredVelps;
            }
        }

        let list;
        let reverse = false;
        let order: keyof IVelp = orderStr as keyof IVelp;
        if (orderStr.startsWith("-")) {
            reverse = true;
            order = order.substring(1) as keyof IVelp;
        }

        if (order === "labels") {
            list = velps;
        } else if (order === "content") {
            list = velps.sort((v1, v2) =>
                v1.content.localeCompare(v2.content, sortLang)
            );
        } else {
            list = velps.sort((v1, v2) => {
                const v1o = v1[order];
                const v2o = v2[order];
                if (v1o == null) {
                    return v2o != null ? -1 : 0;
                } else if (v2o == null) {
                    return 1;
                } else if (v1o < v2o) {
                    return -1;
                } else if (v1o > v2o) {
                    return 1;
                }
                return 0;
            });
        }

        if (reverse) {
            list = list.reverse();
        }

        return list;
    }
}
