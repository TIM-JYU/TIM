import {PointsDisplayComponent} from "./points-display.component";
import {angularDialog} from "./angulardialog/dialog.service";

export async function showPointsDisplay() {
    const {PointsDisplayComponent} = await import("./points-display.component");
    return await (
        await angularDialog.open(PointsDisplayComponent, {})
    ).result;
}
