import {
    Component,
    ElementRef,
    EventEmitter,
    Input,
    Output,
} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {DomSanitizer} from "@angular/platform-browser";
import type {JsonValue} from "tim/util/jsonvalue";

export interface ChatModel {
    label: string;
    value: string;
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
                    Model: <strong>{{ selectedModel }}</strong>
                </button>
                <div *ngIf="modelOpen" class="settings-section-body">
                    <select class="form-control"
                            [ngModel]="selectedModel"
                            (ngModelChange)="selectedModelChange.emit($event)">
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
                                   [ngModel]="selectedMode"
                                   (ngModelChange)="selectedModeChange.emit($event)">
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
                           [ngModel]="maxTokens"
                           (ngModelChange)="maxTokensChange.emit($event)">
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
                    <strong *ngIf="filePaths">{{ filePathCount() }} path(s)</strong>
                </button>
                <div *ngIf="filesOpen" class="settings-section-body">
                    <textarea class="form-control"
                              style="width: 100%"
                              placeholder="kurssit/tie/proj/2026/chattim"
                              [(ngModel)]="localFilePaths">
                    </textarea>
                    <button class="btn btn-primary" style="margin: 2px;"
                            (click)="submitFilePaths()">
                        Add file
                    </button>
                </div>
            </div>
            
            <!-- Save button that sends the chosen stuff -->
            <div class="settings-row">
                <button class="btn btn-primary" style="margin: 2px;"
                        (click)="saveSettingsClicked()">
                    Save
                </button>
            </div>
            <div *ngIf="error" [innerHTML]="error | purify"></div>

        </div>
    `,
})
export class ChatControlPanelComponent {
    settingsOpen = false;
    modelOpen = false;
    modeOpen = false;
    tokensOpen = false;
    filesOpen = false;
    localFilePaths: string = "";

    @Input() error?: string;

    @Input() selectedModel: string = "gpt-4o";
    @Output() selectedModelChange = new EventEmitter<string>();

    @Input() selectedMode: string = "Summarizing";
    @Output() selectedModeChange = new EventEmitter<number>();

    @Input() maxTokens: number = 1000;
    @Output() maxTokensChange = new EventEmitter<number>();

    @Input() filePaths: string = "";
    @Output() filePathsChange = new EventEmitter<string>();

    @Output() saveSettingsClick = new EventEmitter<CtrlPanelData>();

    availableModels: ChatModel[] = [
        {label: "GPT-4o", value: "gpt-4o"},
        {label: "Dummy", value: "Dummy"},
    ];

    modes = ["Summarizing", "Creative"];

    filePathCount(): number {
        return this.filePaths.split("\n").filter((l) => l.trim().length > 0)
            .length;
    }

    submitFilePaths() {
        console.log("Tried to add :", this.localFilePaths);
        this.filePathsChange.emit(this.localFilePaths);
        this.localFilePaths = "";
    }

    saveSettingsClicked() {
        const data: CtrlPanelData = {
            model_id: this.selectedModel,
            mode: this.selectedMode,
            max_tokens: this.maxTokens,
            tim_paths: this.filePaths,
        };
        this.saveSettingsClick.emit(data);
    }
}

export interface CtrlPanelData extends Record<string, JsonValue> {
    model_id: string;
    mode: string;
    max_tokens: number;
    tim_paths: string;
}
