import {PointsDisplayComponent} from "./points-display.component";

export let pointsDisplayInstance: PointsDisplayComponent | undefined;

export function setPointsDisplay(p: PointsDisplayComponent): void {
    pointsDisplayInstance = p;
}
