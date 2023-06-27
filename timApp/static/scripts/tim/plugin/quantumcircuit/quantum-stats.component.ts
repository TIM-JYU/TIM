import type {
    AfterViewInit,
    OnChanges,
    OnInit,
    PipeTransform,
    SimpleChanges,
} from "@angular/core";
import {
    Component,
    EventEmitter,
    Input,
    Output,
    ViewChild,
    ElementRef,
    Pipe,
} from "@angular/core";
import type {Measurement} from "tim/plugin/quantumcircuit/quantum-circuit.component";
import type {ChartData, ChartOptions} from "chart.js";

/**
 * Turns a list of measurement objects into multiline string format to be added into a textarea.
 */
@Pipe({name: "measurementsFormat"})
export class MeasurementsPipe implements PipeTransform {
    transform(measurements: Measurement[]) {
        return measurements
            .map((m) => `Input: ${m.input}; Output: ${m.output}`)
            .join("\n");
    }
}

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
            <div class="chart">
                <div class="chart-inner" #chartInnerElement>
                    <canvas baseChart #chartCanvas [type]="'bar'" [options]="chartOptions" [data]="chartData"></canvas>
                </div>
            </div>

            <div class="output-container">
                <div class="output-print">
                    <table #outputTable class="output-table">
                        <thead>
                        <tr>
                            <th>Input</th>
                            <th>Output</th>
                        </tr>
                        </thead>
                        <tbody>
                        <tr *ngFor="let measurement of measurements; even as isEven; first as isFirst" [class.even-row]="isEven">
                            <td>{{measurement.input}}</td>
                            <td>{{measurement.output}}</td>
                        </tr>
                        </tbody>
                    </table>
                </div>

                <div class="buttons">
                    <button class="timButton" (click)="handleMeasure()">Mittaa</button>
                    <button class="timButton" (click)="handleClear()">Tyhjennä</button>
                </div>

            </div>
        </div>
    `,
    styleUrls: ["./quantum-stats.component.scss"],
})
export class QuantumStatsComponent implements OnInit, AfterViewInit, OnChanges {
    @ViewChild("chartCanvas")
    chartCanvas!: ElementRef<HTMLCanvasElement>;

    @ViewChild("chartInnerElement")
    chartInner!: ElementRef<HTMLDivElement>;

    @ViewChild("outputTable")
    table!: ElementRef<HTMLTableElement>;

    chartData!: ChartData<"bar", number[]>;

    chartOptions: ChartOptions = {
        indexAxis: "y",
        scales: {
            y: {ticks: {autoSkip: false}},
            x: {ticks: {stepSize: 10}, min: 0, max: 100},
        },
        maintainAspectRatio: false,
        responsive: true,
        plugins: {legend: {display: false}},
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
        this.chartData = {
            labels: this.quantumChartData.labels,
            datasets: [
                {
                    data: this.quantumChartData.probabilities,
                },
            ],
        };
    }

    ngAfterViewInit(): void {
        this.updateChart();
        // leave room for both columns. About 10px is needed for each character.
        const w = this.nQubits * 10 * 2;
        this.table.nativeElement.style.width = `${w}px`;

        if (this.chartInner) {
            // set height so there's 10px of space for each row in chart
            const h = 2 ** this.nQubits * 10 + 40;
            this.chartInner.nativeElement.style.height = `${h}px`;
        }
    }

    /**
     * Update chart if data in it changed.
     * @param changes possible change in chart data
     */
    ngOnChanges(changes: SimpleChanges): void {
        const chartChange = changes.quantumChartData;
        if (chartChange) {
            this.updateChart();
        }
    }
}
