import type {OnInit} from "@angular/core";
import {Component, EventEmitter, Input, Output} from "@angular/core";
import type {JsonValue} from "tim/util/jsonvalue";

export interface ChatModel {
    label: string;
    value: string;
}

export interface ControlPanelData extends Record<string, JsonValue> {
    model_id: string;
    llm_mode: string;
    max_tokens: number;
    tim_paths: string;
}

@Component({
    selector: "chattim-control-panel",
    template: `
        <button class="btn btn-link settings-btn"
                (click)="settingsOpen = !settingsOpen"
                [attr.aria-expanded]="settingsOpen"
                title="Open control panel"
                style="float: right">
            <span class="glyphicon glyphicon-cog" style="font-size: 1.5em;"></span>
        </button>


        <div class="settings-panel" *ngIf="settingsOpen">

            <!-- Choose the LLM -->
            <div class="settings-row">
                <button class="btn btn-link settings-section-btn"
                        (click)="modelOpen = !modelOpen">
                    <span class="glyphicon"
                          [class.glyphicon-chevron-right]="!modelOpen"
                          [class.glyphicon-chevron-down]="modelOpen">
                    </span>
                    Model: <strong>{{ selectedModelLabel }}</strong>
                </button>
                <div *ngIf="modelOpen" class="settings-section-body">
                    <select class="form-control"
                            [(ngModel)]="selectedModel">
                        <option *ngFor="let m of availableModels" [value]="m.value">{{ m.label }}</option>
                    </select>
                </div>
            </div>

            <!-- Switch between summarizing, (balanced) and creative -->
            <div class="settings-row">
                <button class="btn btn-link settings-section-btn"
                        (click)="modeOpen = !modeOpen">
        <span class="glyphicon"
              [class.glyphicon-chevron-right]="!modeOpen"
              [class.glyphicon-chevron-down]="modeOpen">
        </span>
                    Mode: <strong>{{ selectedMode }}</strong>
                </button>
                <div *ngIf="modeOpen" class="settings-section-body">
                    <div class="radio" *ngFor="let mode of modes">
                        <label>
                            <input type="radio"
                                   name="modeRadio"
                                   [value]="mode"
                                   [(ngModel)]="selectedMode">
                            {{ mode }}
                        </label>
                    </div>
                </div>
            </div>

            <!-- Set max tokens per user -->
            <div class="settings-row">
                <button class="btn btn-link settings-section-btn"
                        (click)="tokensOpen = !tokensOpen">
                    <span class="glyphicon"
                          [class.glyphicon-chevron-right]="!tokensOpen"
                          [class.glyphicon-chevron-down]="tokensOpen">
                    </span>
                    Max tokens: <strong>{{ maxTokens }}</strong>
                </button>
                <div *ngIf="tokensOpen" class="settings-section-body">
                    <input type="range"
                           class="form-control"
                           min="100" max="10000" step="100"
                           [(ngModel)]="maxTokens" >
                </div>
            </div>

            <!-- Add more documents to the model (TIM - filepath) -->
            <div class="settings-row">
                <button class="btn btn-link settings-section-btn"
                        (click)="filesOpen = !filesOpen">
                    <span class="glyphicon"
                          [class.glyphicon-chevron-right]="!filesOpen"
                          [class.glyphicon-chevron-down]="filesOpen">
                    </span>
                    Add TIM-documents:
                </button>
                <div *ngIf="filesOpen" class="settings-section-body">
                    <textarea class="form-control"
                              style="width: 100%"
                              placeholder="kurssit/tie/proj/2026/chattim"
                              [(ngModel)]="localFilePaths">
                    </textarea>
                </div>
            </div>
            
            <!-- Save button that sends the chosen stuff -->
            <div class="settings-row">
                <button class="btn btn-primary" style="margin: 2px;"
                        (click)="saveSettingsClicked()">
                    Save
                </button>
            </div>
            <div *ngIf="error && !response" [innerHTML]="error | purify"></div>
            <div *ngIf="response && !error" [innerHTML]="response | purify"></div>

        </div>
    `,
})
export class ChatControlPanelComponent implements OnInit {
    settingsOpen = false;
    modelOpen = false;
    modeOpen = false;
    tokensOpen = false;
    filesOpen = false;

    @Input() localFilePaths!: string;
    @Input() error?: string;
    @Input() response?: string;
    @Input() selectedModel!: string;
    @Input() selectedMode!: string;
    @Input() maxTokens!: number;

    @Output() saveSettingsClick = new EventEmitter<ControlPanelData>();
    @Output() initControlPanelDataFetch = new EventEmitter<void>();

    availableModels: ChatModel[] = [
        {label: "GPT-4o-Mini", value: "gpt-4.1-mini"},
        {label: "Dummy", value: "dummy-model-1"},
    ];

    modes = ["Summarizing", "Creative"];

    ngOnInit(): void {
        this.initControlPanelDataFetch.emit();
    }

    saveSettingsClicked() {
        const data: ControlPanelData = {
            model_id: this.selectedModel,
            llm_mode: this.selectedMode,
            max_tokens: this.maxTokens,
            tim_paths: this.localFilePaths,
        };
        console.log("sending: ", data);
        this.saveSettingsClick.emit(data);
    }

    get selectedModelLabel(): string {
        const model = this.availableModels.find(
            (m) => m.value === this.selectedModel
        );
        return model ? model.label : "";
    }
}
