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
    ChangeDetectionStrategy,
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
        <div class="stats-container" [hidden]="!showChart && !showPrintField && !showExport">
          <div>
            <div class="stats-description">
              @if (showChart) {
                <span>{{statsDescription}}</span>
              }
        
              @if (showExport) {
                <div class="btn-group export-container" dropdown>
                  <button type="button" class="btn btn-default dropdown-toggle" dropdownToggle>
                    <ng-container i18n>Export</ng-container>
                    <span class="caret"></span>
                  </button>
                  <ul class="dropdown-menu" *dropdownMenu role="menu">
                    <li (click)="handleExport($event, 'probabilities')"><a>
                      <ng-container i18n>Probabilities</ng-container>
                    </a></li>
                    <li (click)="handleExport($event, 'stateVector')"><a>
                      <ng-container i18n>State vector</ng-container>
                    </a></li>
                  </ul>
                </div>
              }
        
              @if (isSimulatorRunning) {
                <tim-loading></tim-loading>
              }
        
            </div>
        
            @if (showChart) {
              <div class="chart">
                <div class="chart-inner" #chartInnerElement>
                  <canvas baseChart #chartCanvas [type]="'bar'" [options]="chartOptions"
                  [data]="chartData"></canvas>
                </div>
              </div>
            }
          </div>
        
          <div class="output-container">
        
            @if (showPrintField) {
              <p class="measurements-description">
                <ng-container i18n>Measurements</ng-container>
              </p>
            }
            @if (showPrintField) {
              <div class="output-print">
                <table #outputTable class="output-table">
                  <thead>
                    <tr>
                      <th>#&nbsp;</th>
                      <th>Input</th>
                      <th>Output</th>
                    </tr>
                  </thead>
                  <tbody>
                    @for (measurement of measurements; track measurement; let isEven = $even; let isFirst = $first; let i = $index) {
                      <tr
                        [class.even-row]="isEven">
                        <td>{{measurements.length - i}}</td>
                        <td>{{measurement.input}}</td>
                        <td>{{measurement.output}}</td>
                      </tr>
                    }
                  </tbody>
                </table>
              </div>
            }
        
            @if (showPrintField || samplingMode === 'sample') {
              <div class="buttons">
                <button class="timButton" (click)="handleMeasure()">
                  <ng-container i18n>Measure</ng-container>
                </button>
                <button class="timButton" (click)="handleClear()">
                  <ng-container i18n>Clear</ng-container>
                </button>
              </div>
            }
        
            @if (showChart) {
              <label class="font-weight-normal">
                <input type="checkbox" [(ngModel)]="hideZeroRows" (ngModelChange)="updateChart()"/>
                <ng-container i18n>Hide 0% rows</ng-container>
              </label>
            }
        
          </div>
        </div>
        `,
    styleUrls: ["./quantum-stats.component.scss"],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class QuantumStatsComponent implements OnInit, AfterViewInit, OnChanges {
    hideZeroRows: boolean = false;
    hideZeroRowsLimit: number = 2000;
    hideZeroRowsHasChanged: boolean = false;

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
        animation: false,
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
    showExport: boolean = false;

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

    @Output()
    export = new EventEmitter<string>();

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
                this.statsDescription = $localize`Output probabilities`;
                break;
        }
    }

    ngOnInit(): void {
        this.updateStatsDescription();
    }

    handleExport(event: MouseEvent, type: string) {
        event.preventDefault();
        this.export.emit(type);
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

        // hide zero rows by default when there is a lot of data
        if (
            !this.hideZeroRowsHasChanged &&
            labels.length > this.hideZeroRowsLimit
        ) {
            this.hideZeroRows = true;
            this.hideZeroRowsHasChanged = true;
        }

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
