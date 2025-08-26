import type {OnDestroy, OnInit} from "@angular/core";
import {Component, HostListener, NgModule} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {toPromise} from "tim/util/utils";
import {documentglobals} from "tim/util/globals";
import type {ITaskScoreInfo} from "tim/sidebarmenu/services/scoreboard.service";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {from, interval, Subject, switchMap, takeUntil} from "rxjs";

export interface ITaskInfo {
    total_points: number;
    tasks_done: number;
    groups: Groups;
    point_dict: ITaskScoreInfo;
    total_maximum: number;
    group_max_points: GroupMaxPoints;
}

interface IGroupCircle {
    name: string;
    percent: number;
    cx: number;
    cy: number;
    text: string;
}

interface IGroup {
    name: string;
    task_count: number;
    task_sum: number;
    text: string;
    total_sum: number;
}

type Groups = Record<string, IGroup>;
type GroupMaxPoints = Record<string, number>;

@Component({
    selector: "points-display",
    template: `
        <div class="task-progress">
            <svg xmlns="http://www.w3.org/2000/svg" [attr.viewBox]="'0 ' + '0 ' + imgSize + ' ' +  imgSize" [attr.height]="pixelSize" [attr.width]="pixelSize" role="img">
                <defs>
                    <filter id="shadow">
                        <feDropShadow dx="0.7" dy="0.8" stdDeviation="1.5"></feDropShadow>
                    </filter>
                </defs>
                <g [tooltip]="'Total tasks done'" container="body" triggers="hover click">
                    <circle class="progress-circle-bg" [attr.r]="mainCircleRadius" [attr.cx]="centerX"
                            [attr.cy]="centerY" 
                            [attr.stroke-width]="progressStrokeWidth" 
                            filter="url(#shadow)"></circle>
                    <g [attr.transform]="'rotate(' + '-90,' + centerX + ',' + centerY + ')'">
                        <circle [attr.r]="mainCircleRadius" [attr.cx]="centerX" [attr.cy]="centerY" [attr.stroke-width]="progressStrokeWidth"
                                class="progress-circle-fg"
                                [attr.stroke]="mainCircleStrokeColor"
                                [attr.stroke-dasharray]="circumference"
                                [attr.stroke-dashoffset]="mainDashOffset"
                                (click)="showSatellites()"></circle>
                    </g>
                    <text [attr.x]="centerX" [attr.y]="textCenterY" class="progress-text" text-anchor="middle">{{donePercentage}}%</text>
                </g>
                <g>
                    <g *ngFor="let sat of satellites; index as i" class="sat-initial" [ngClass]="{'visible-sat': satellitesVisible}" [tooltip]="sat.text" container="body" triggers="hover click">
                        <circle [attr.r]="satelliteR" [attr.cx]="sat.cx" [attr.cy]="sat.cy" 
                                fill="none" [attr.stroke-width]="satelliteStrokeWidth" 
                                class="sat-bg" filter="url(#shadow)">
                        </circle>
                        <g [attr.transform]="'rotate(' + '-90,' + sat.cx + ',' + sat.cy + ')'">
                            <circle [attr.r]="satelliteR" [attr.cx]="sat.cx" [attr.cy]="sat.cy" 
                                    [attr.stroke-width]="satelliteStrokeWidth" fill="white"
                                    class="sat-progress-initial"
                                    [attr.stroke]="pickSatelliteStrokeColor(i)"
                                    [ngClass]="{'visible-sat': satellitesVisible}"
                                    [attr.stroke-dasharray]="satelliteCircumference"
                                    [attr.stroke-dashoffset]="satelliteDashOffset(sat)"></circle>
                        </g>
                        <text [attr.x]="sat.cx"  [attr.y]="sat.cy + 4" text-anchor="middle" class="sat-text">{{sat.percent}}%</text>
                    </g>
                </g>
            </svg>
        </div>
    `,
    styleUrls: ["./points-display.component.scss"],
})
export class PointsDisplayComponent implements OnInit, OnDestroy {
    protected dialogName = "Points display";
    tasksDone: number;
    totalPoints: number;
    docId: number;
    groups: Groups;
    satellites: IGroupCircle[] = [];
    processedGroups: IGroup[] = [];
    satellitesVisible = false;
    colorList: string[];
    mainCircleStrokeColor;
    satelliteR = 25;
    totalMaximum: number;
    private groupMaxPoints: GroupMaxPoints;

    readonly imgSize = 220;
    readonly pixelSize = 120;
    readonly mainCircleRadius = 40;
    readonly progressStrokeWidth = 5;
    readonly circumference = 2 * Math.PI * this.mainCircleRadius;

    readonly centerX = this.imgSize / 2;
    readonly centerY = this.imgSize / 2;
    readonly textCenterY = this.centerY + 7;

    readonly satelliteStrokeWidth = 3;
    readonly gap = 5 + this.progressStrokeWidth + this.satelliteStrokeWidth;
    readonly satelliteCircumference = 2 * Math.PI * this.satelliteR;
    readonly defaultMainCircleStrokeColor = "#004494";
    readonly defaultColorList = [
        "#c33f33",
        "#01d375",
        "#1bc5fa",
        "#f981ca",
        "#f3e725",
        "#5243af",
        "#eeb46c",
        "#0c5b8e",
        "#b5cb75",
        "#f3a122",
        "#f469a4",
        "#01affe",
    ];

    readonly satScaleNumThreshold = 9;
    readonly satReductionFactor = 0.9;

    private destroy$ = new Subject<void>();
    private pollingStop$ = new Subject<void>();

    constructor(private http: HttpClient) {
        this.docId = documentglobals().curr_item.id;
        this.tasksDone = 0;
        this.totalPoints = 0;
        this.totalMaximum = 0;
        this.groups = {};
        this.groupMaxPoints = {};

        const settingsPalette =
            documentglobals().docSettings.progressCirclePalette;
        if (settingsPalette) {
            this.mainCircleStrokeColor = settingsPalette[0];
            if (settingsPalette.length > 1) {
                this.colorList = settingsPalette.slice(1);
            } else {
                this.colorList = settingsPalette;
            }
        } else {
            this.mainCircleStrokeColor = this.defaultMainCircleStrokeColor;
            this.colorList = this.defaultColorList;
        }
    }

    ngOnInit() {
        this.getSatellitesAndGroups();

        this.initiatePolling();
    }

    ngOnDestroy() {
        this.stopPolling();
        this.destroy$.next();
        this.destroy$.complete();
    }

    stopPolling() {
        this.pollingStop$.next();
    }

    initiatePolling() {
        this.stopPolling();

        interval(30000)
            .pipe(
                switchMap(() => from(this.updateSatellites())),
                takeUntil(this.destroy$),
                takeUntil(this.pollingStop$)
            )
            .subscribe();
    }

    @HostListener("document:visibilitychange", [])
    onVisibilityChange() {
        if (document.hidden) {
            this.stopPolling();
        } else {
            this.initiatePolling();
        }
    }

    pickSatelliteStrokeColor(i: number) {
        const cll = this.colorList.length;
        if (cll == 1) {
            return this.colorList[0];
        } else if (i >= cll) {
            i = i % cll;
        }
        return this.colorList[i];
    }

    calculateSatellites(
        groups: IGroup[],
        mainR: number,
        satR: number,
        gap: number
    ) {
        const n = groups.length;
        if (n == 0) {
            return [];
        }
        const arcSpanDeg = this.calculateArcspan(n);
        const arcSpandRad = (arcSpanDeg * Math.PI) / 180;
        const startAngle = -arcSpandRad / 2;
        const step = n > 1 ? arcSpandRad / (n - 1) : 0;
        // R is the distance between circle centers plus the gap
        const R = mainR + gap + satR;
        this.satellites = groups.map((g, i) => {
            const angle = startAngle + i * step;
            const cx = this.centerX + R * Math.sin(angle);
            const cy = this.centerY + -R * Math.cos(angle);
            const done = g.total_sum;
            const total = this.groupMaxPoints[g.name];
            const percent = this.calculatePercentage(done, total);
            return {...g, cx, cy, percent};
        });
        // When sufficiently many satellites decrease the size of each
        if (n >= this.satScaleNumThreshold) {
            this.satelliteR = this.satelliteR * this.satReductionFactor;
        }
    }

    async updateSatellites() {
        this.processedGroups = [];
        await this.getTasksInfo();
        const updateMap = new Map(
            this.processedGroups.map((g) => {
                const percent = this.calculatePercentage(
                    g.total_sum,
                    this.groupMaxPoints[g.name]
                );
                return [g.name, percent];
            })
        );
        for (const sat of this.satellites) {
            const name = sat.name;
            const updatePercent = updateMap.get(name);
            if (updatePercent) {
                sat.percent = updatePercent;
            }
        }
    }

    get mainDashOffset() {
        return this.circumference * (1 - this.donePercentage / 100);
    }

    get donePercentage() {
        if (this.tasksDone === 0 || !this.totalMaximum) {
            return 0;
        }
        return this.calculatePercentage(this.totalPoints, this.totalMaximum);
    }

    calculatePercentage(done: number, total: number) {
        if (!(total > 0)) {
            return 0;
        }
        return +((done / total) * 100).toFixed(0);
    }

    satelliteDashOffset(sat: IGroupCircle) {
        return this.satelliteCircumference * (1 - sat.percent / 100);
    }

    showSatellites() {
        this.satellitesVisible = !this.satellitesVisible;
    }

    async getSatellitesAndGroups() {
        await this.getTasksInfo();
        this.calculateSatellites(
            this.processedGroups,
            this.mainCircleRadius,
            this.satelliteR,
            this.gap
        );
    }

    calculateArcspan(n: number) {
        const threshold = 5;
        const largeAngleFactor = 35;
        const smallAngleFactor = 25;
        const maximumAngle = 280;
        const baseAngle = 60;
        const incr = n > threshold ? largeAngleFactor : smallAngleFactor;
        return Math.min(baseAngle + n * incr, maximumAngle);
    }

    async getTasksInfo() {
        const params = {doc_id: this.docId};
        const r = await toPromise(
            this.http.get<ITaskInfo>("/pointsrulepoints", {
                params: params,
            })
        );
        if (!r.ok) {
            return;
        } else {
            this.tasksDone = r.result.tasks_done;
            this.totalPoints = r.result.total_points;
            this.groups = r.result.groups;
            this.groupMaxPoints = r.result.group_max_points;
            this.totalMaximum = r.result.total_maximum;
            const names = Object.keys(this.groups);
            for (const name of names) {
                const {task_count, task_sum, text, total_sum} =
                    this.groups[name];
                const newGroup: IGroup = {
                    name: name,
                    task_count: task_count,
                    task_sum: task_sum,
                    text: text,
                    total_sum: total_sum,
                };
                this.processedGroups.push(newGroup);
            }
        }
    }
}
@NgModule({
    declarations: [PointsDisplayComponent],
    exports: [PointsDisplayComponent],
    imports: [TimUtilityModule, CommonModule, TooltipModule],
})
export class PointsDisplayModule {}
