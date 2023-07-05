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
        <div class="stats-container">
            <div class="chart" *ngIf="showChart">
                <div class="chart-inner" #chartInnerElement>
                    <canvas baseChart #chartCanvas [type]="'bar'" [options]="chartOptions" [data]="chartData"></canvas>
                </div>
            </div>

            <div class="output-container">

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

                <div class="buttons" *ngIf="showPrintField">
                    <button class="timButton" (click)="handleMeasure()">Mittaa</button>
                    <button class="timButton" (click)="handleClear()">Tyhjennä</button>
                </div>

                <label class="font-weight-normal" *ngIf="showChart">
                    <input type="checkbox" [(ngModel)]="hideZeroRows" (ngModelChange)="updateChart()"/>
                    Piilota 0% rivit
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
    nQubits!: number;

    @Output()
    clear = new EventEmitter<void>();

    @Output()
    measure = new EventEmitter<void>();

    constructor() {}

    ngOnInit(): void {}

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

        if (this.table) {
            // | "# " | nQubits chars | nQubits chars |
            const tableWidth = 2 * charWidth + 2 * charWidth * this.nQubits;
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
    }
}
