import type {PointsDisplayComponent} from "tim/ui/points-display.component";

export let pointsDisplayInstance: PointsDisplayComponent | undefined;

export function setPointsDisplay(p: PointsDisplayComponent): void {
    pointsDisplayInstance = p;
}
