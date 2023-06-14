import type {AfterViewInit, OnInit} from "@angular/core";
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

@Component({
    selector: "tim-quantum-stats",
    template: `
        <div class="stats-container">
            <div class="chart">
                <canvas baseChart #chartCanvas [type]="'bar'" [options]="chartOptions" [data]="chartData"></canvas>
            </div>

            <button>Mittaa</button>

            <div class="measurements">
                <p *ngFor="let measurement of measurements">
                    Input: {{measurement.input}}; Output: {{measurement.output}}
                </p>
                
                <button (click)="handleClear()">Tyhjennä</button>
            </div>
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
        scales: {x: {min: 0, max: 100}},
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
