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
                <canvas baseChart #chartCanvas [type]="'bar'" [options]="chartOptions" [data]="chartData"></canvas>
            </div>

            <div class="buttons">
                <button class="timButton" (click)="handleMeasure()">Mittaa</button>
                <button class="timButton" (click)="handleClear()">Tyhjennä</button>
            </div>

            <textarea readonly rows="5">{{measurements | measurementsFormat}}</textarea>
        </div>
    `,
    styleUrls: ["./quantum-stats.component.scss"],
})
export class QuantumStatsComponent implements OnInit, AfterViewInit, OnChanges {
    @ViewChild("chartCanvas")
    chartCanvas!: ElementRef<HTMLCanvasElement>;

    chartData!: ChartData<"bar", number[]>;

    chartOptions: ChartOptions = {
        indexAxis: "y",
        scales: {
            x: {min: 0, max: 100, ticks: {stepSize: 10}},
            y: {ticks: {autoSkip: false}},
        },
        plugins: {legend: {display: false}},
    };

    @Input()
    measurements!: Measurement[];

    @Input()
    quantumChartData!: QuantumChartData;

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
                    backgroundColor: "#004494",
                },
            ],
        };
    }

    ngAfterViewInit(): void {
        this.updateChart();
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
