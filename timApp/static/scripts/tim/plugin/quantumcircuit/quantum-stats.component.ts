import type {AfterViewInit, OnInit, PipeTransform} from "@angular/core";
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

@Pipe({name: "measurementsFormat"})
export class MeasurementsPipe implements PipeTransform {
    transform(measurements: Measurement[]) {
        return measurements
            .map((m) => `Input: ${m.input}; Output: ${m.output}`)
            .join("\n");
    }
}

@Component({
    selector: "tim-quantum-stats",
    template: `
        <div class="stats-container">
            <div class="chart">
                <canvas baseChart #chartCanvas [type]="'bar'" [options]="chartOptions" [data]="chartData"></canvas>
            </div>

            <div class="buttons">
                <button class="timButton">Mittaa</button>
                <button class="timButton" (click)="handleClear()">Tyhjennä</button>
            </div>

            <textarea readonly rows="5">{{measurements | measurementsFormat}}</textarea>
        </div>
    `,
    styleUrls: ["./quantum-stats.component.scss"],
})
export class QuantumStatsComponent implements OnInit, AfterViewInit {
    @ViewChild("chartCanvas")
    chartCanvas!: ElementRef<HTMLCanvasElement>;

    chartData!: ChartData<"bar", number[]>;

    chartOptions: ChartOptions = {
        indexAxis: "y",
        scales: {x: {min: 0, max: 100, ticks: {stepSize: 10}}},
        plugins: {legend: {display: false}},
    };

    @Input()
    measurements: Measurement[] = [];

    @Output()
    clear = new EventEmitter<void>();

    constructor() {}

    ngOnInit(): void {}

    /**
     * Clear measurements.
     */
    handleClear() {
        this.clear.emit();
    }

    initChart() {
        const probabilities = [50, 25, 25, 0];
        const labels = ["00", "01", "10", "11"];
        this.chartData = {
            labels: labels,
            datasets: [
                {
                    data: probabilities,
                    backgroundColor: "#004494",
                },
            ],
        };
    }

    ngAfterViewInit(): void {
        this.initChart();
    }
}
