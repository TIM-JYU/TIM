export class PointService {
    getPoints(group: string): Record<string, number> | null {
        const item = localStorage.getItem(`points-${group}`);
        return item ? JSON.parse(item) : null;
    }

    getTotalPoints(group: string): number {
        const points = this.getPoints(group);
        return points ? Object.values(points).reduce((a, b) => a + b, 0) : 0;
    }

    setPoints(group: string, points: Record<string, number>) {
        localStorage.setItem(`points-${group}`, JSON.stringify(points));
    }
}
