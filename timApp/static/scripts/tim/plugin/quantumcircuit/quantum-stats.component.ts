import type {
    AfterViewInit,
    OnChanges,
    OnInit,
    SimpleChanges,
} from "@angular/core";
import {
    Component,
    EventEmitter,
    Input,
    Output,
    ViewChild,
    ElementRef,
} from "@angular/core";
import type {Measurement} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {ChartData, ChartOptions} from "chart.js";

/**
 * Data for the chart.
 * labels are on y-axis and probabilities are on x-axis.
 */
export interface QuantumChartData {
    labels: string[];
    probabilities: number[];
}

@Component({
    selector: "tim-quantum-stats",
    template: `
        <div class="stats-container" [hidden]="!showChart && !showPrintField">
            <div>
                <div class="stats-description">
                    <span *ngIf="showChart">{{statsDescription}}</span>
                    <tim-loading *ngIf="isSimulatorRunning"></tim-loading>
                </div>

                <div class="chart" *ngIf="showChart">
                    <div class="chart-inner" #chartInnerElement>
                        <canvas baseChart #chartCanvas [type]="'bar'" [options]="chartOptions"
                                [data]="chartData"></canvas>
                    </div>
                </div>
            </div>

            <div class="output-container">

                <p *ngIf="showPrintField" class="measurements-description"><ng-container i18n>Measurements</ng-container></p>
                <div class="output-print" *ngIf="showPrintField">
                    <table #outputTable class="output-table">
                        <thead>
                        <tr>
                            <th>#&nbsp;</th>
                            <th>Input</th>
                            <th>Output</th>
                        </tr>
                        </thead>
                        <tbody>
                        <tr *ngFor="let measurement of measurements; even as isEven; first as isFirst; index as i"
                            [class.even-row]="isEven">
                            <td>{{measurements.length - i}}</td>
                            <td>{{measurement.input}}</td>
                            <td>{{measurement.output}}</td>
                        </tr>
                        </tbody>
                    </table>
                </div>

                <div class="buttons" *ngIf="showPrintField || samplingMode === 'sample'">
                    <button class="timButton" (click)="handleMeasure()"><ng-container i18n>Measure</ng-container></button>
                    <button class="timButton" (click)="handleClear()"><ng-container i18n>Clear</ng-container></button>
                </div>

                <label class="font-weight-normal" *ngIf="showChart">
                    <input type="checkbox" [(ngModel)]="hideZeroRows" (ngModelChange)="updateChart()"/>
                    <ng-container i18n>Hide 0% rows</ng-container>
                </label>

            </div>
        </div>
    `,
    styleUrls: ["./quantum-stats.component.scss"],
})
export class QuantumStatsComponent implements OnInit, AfterViewInit, OnChanges {
    hideZeroRows: boolean = false;

    @ViewChild("chartInnerElement")
    chartInner?: ElementRef<HTMLDivElement>;

    @ViewChild("outputTable")
    table?: ElementRef<HTMLTableElement>;

    chartData!: ChartData<"bar", number[]>;

    chartOptions: ChartOptions = {
        indexAxis: "y",
        scales: {
            y: {
                ticks: {
                    autoSkip: false,
                    font: {family: "'courier new', courier, monospace"},
                },
            },
            x: {ticks: {stepSize: 10}, min: 0, max: 100},
        },
        maintainAspectRatio: false,
        responsive: true,
        plugins: {
            legend: {
                display: false,
            },
        },
        backgroundColor: "#004494",
    };

    statsDescription: string = "";

    @Input()
    isSimulatorRunning: boolean = false;

    @Input()
    measurements!: Measurement[];

    @Input()
    quantumChartData: QuantumChartData = {
        labels: [],
        probabilities: [],
    };

    @Input()
    showChart: boolean = true;

    @Input()
    showPrintField: boolean = true;

    @Input()
    samplingMode!: "matrix" | "sample" | "autoSample";

    @Input()
    nSamples!: number;

    @Output()
    clear = new EventEmitter<void>();

    @Output()
    measure = new EventEmitter<void>();

    constructor() {}

    updateStatsDescription() {
        switch (this.samplingMode) {
            case "autoSample":
                this.statsDescription = $localize`Output probabilities based on ${this.nSamples} measurements`;
                break;
            case "sample":
                this.statsDescription = $localize`Output probabilities based on ${this.measurements.length} measurements`;
                break;
            case "matrix":
                this.statsDescription = $localize`Output probabilities based on matrix computation`;
                break;
        }
    }

    ngOnInit(): void {
        this.updateStatsDescription();
    }

    /**
     * Clear measurements.
     */
    handleClear() {
        this.clear.emit();
    }

    /**
     * Measure a new sample.
     */
    handleMeasure() {
        this.measure.emit();
    }

    /**
     * Sets data of chart to current data.
     */
    updateChart() {
        if (!this.quantumChartData) {
            return;
        }
        let labels = this.quantumChartData.labels;
        let probabilities = this.quantumChartData.probabilities;

        if (this.hideZeroRows) {
            labels = [];
            probabilities = [];
            for (
                let i = 0;
                i < this.quantumChartData.probabilities.length;
                i++
            ) {
                const p = this.quantumChartData.probabilities[i];
                if (p > 0) {
                    probabilities.push(p);
                    labels.push(this.quantumChartData.labels[i]);
                }
            }
        }
        this.chartData = {
            labels: labels,
            datasets: [
                {
                    data: probabilities,
                },
            ],
        };

        if (this.chartInner) {
            // set height so there's 10px of space for each row in chart
            const h = labels.length * 10 + 40;
            this.chartInner.nativeElement.style.height = `${h}px`;
        }
    }

    ngAfterViewInit(): void {
        if (this.showChart) {
            this.updateChart();
        }
        const charWidth = 12;

        if (
            this.table &&
            this.quantumChartData &&
            this.quantumChartData.labels.length > 0
        ) {
            const nQubits = this.quantumChartData.labels[0].length;
            // | "# " | nQubits chars | nQubits chars |
            const tableWidth = 2 * charWidth + 2 * charWidth * nQubits;
            this.table.nativeElement.style.width = `${tableWidth}px`;
        }
    }

    /**
     * Update chart if data in it changed.
     * @param changes possible change in chart data
     */
    ngOnChanges(changes: SimpleChanges): void {
        const chartChange = changes.quantumChartData;
        if (chartChange && this.showChart) {
            this.updateChart();
        }
        if (changes.measurements) {
            this.updateStatsDescription();
        }
    }
}
