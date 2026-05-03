import {Component, EventEmitter, Input, Output} from "@angular/core";
import type {JsonValue} from "tim/util/jsonvalue";

export interface ChatModel extends Record<string, JsonValue> {
    label: string;
    value: string;
}

export interface ControlPanelSettings extends Record<string, JsonValue> {
    model_id: string;
    llm_mode: string;
    max_tokens: number;
    tim_paths: string;
    system_prompt_path: string;
    global_policy: TokenLimitForUser;
}

export interface TokenLimitForUser extends Record<string, JsonValue> {
    token_cap_enabled: boolean;
    token_cap: number;
    time_window_enabled: boolean;
    window_unit: string;
    window_value: number;
    token_cap_for_window: number;
}

@Component({
    selector: "chattim-control-panel",
    template: `
        <button class="btn btn-link settings-btn"
                (click)="togglePanel()"
                [attr.aria-expanded]="settingsOpen"
                title="Avaa asetukset">
            <span class="glyphicon glyphicon-cog" style="font-size: 1.8em;"></span>
        </button>


        <div class="settings-panel" [style.display]="settingsOpen ? 'block' : 'none'">


            <ng-container *ngIf="!isTeacher">
                <div class="settings-row">
                    <p>Tämä näkymä on opiskelijalle</p>
                    <span>Käytetyt tokenit: <strong>TODO!</strong></span>
                </div>
                <div class="settings-row">
                    <button class="btn btn-warning">
                        TODO: Tyhjennä keskustelu
                    </button>
                </div>
            </ng-container>
            <ng-container *ngIf="isTeacher">
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
                            <option *ngFor="let m of availableModels" [ngValue]="m.value">{{ m.label }}</option>
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
                        <div class="radio" *ngFor="let mode of availableModes">
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

                <!-- Set max tokens for the instance -->
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
                               [(ngModel)]="maxTokens">
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

                <!-- Add a custom system prompt -->
                <div class="settings-row">
                    <button class="btn btn-link settings-section-btn"
                            (click)="promptOpen = !promptOpen">
                    <span class="glyphicon"
                          [class.glyphicon-chevron-right]="!promptOpen"
                          [class.glyphicon-chevron-down]="promptOpen">
                    </span>
                        Set system prompt path:
                    </button>
                    <div *ngIf="promptOpen" class="settings-section-body">
                        <input type="text" class="form-control"
                               style="width: 100%"
                               placeholder="users/user/prompt"
                               [(ngModel)]="systemPromptPath"
                        >
                    </div>
                </div>


                <!-- Add time window restrictions for users -->
                <div class="settings-row">
                    <button class="btn btn-link settings-section-btn"
                            (click)="globalPolicyOpen = !globalPolicyOpen">
                    <span class="glyphicon"
                          [class.glyphicon-chevron-right]="!globalPolicyOpen"
                          [class.glyphicon-chevron-down]="globalPolicyOpen">
                    </span>
                        Restrictions for all users:
                    </button>

                    <ng-container *ngIf="globalPolicyOpen">
                        <div class="checkbox">
                            <label>
                                <input type="checkbox" 
                                       [(ngModel)]="tokenLimitAllUsers.token_cap_enabled" >
                                Enable token limits per user
                            </label>
                        </div>

                        <div *ngIf="tokenLimitAllUsers.token_cap_enabled" class="settings-section-body">
                            <input type="range"
                                   class="form-control"
                                   min="0" max="20000" step="200"
                                   [(ngModel)]="tokenLimitAllUsers.token_cap">
                              <span>
                                {{ tokenLimitAllUsers.token_cap }}
                              </span>
                        </div>

                        <div class="checkbox">
                            <label>
                                <input type="checkbox" [(ngModel)]="tokenLimitAllUsers.time_window_enabled">
                                Enable time window token limits
                            </label>
                        </div>

                        <div *ngIf="tokenLimitAllUsers.time_window_enabled">
                            <div class="settings-section-body">
                                    <input type="range"
                                           min="100" max="20000" step="200"
                                           [(ngModel)]="tokenLimitAllUsers.token_cap_for_window">
                                    <span> {{ tokenLimitAllUsers.token_cap_for_window }} </span>
                                <div class="control-panel-input-and-value">
                                    <input type="number" class="form-control"
                                           min="1"
                                           step="1"
                                           placeholder="5"
                                           [(ngModel)]="tokenLimitAllUsers.window_value"
                                           #numericControl="ngModel"
                                    >
                                    <select class="form-control"
                                            [(ngModel)]="tokenLimitAllUsers.window_unit">
                                        <option *ngFor="let timeUnit of timeUnitOptions" 
                                                [ngValue]="timeUnit.value">{{ timeUnit.label }}
                                        </option>
                                    </select>
                                </div>
                                <div *ngIf="numericControl.invalid && numericControl.touched">
                                    Must be a positive number
                                </div>
                            </div>
                        </div>
                    </ng-container>
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
            </ng-container>
        </div>
    `,
})
export class ChatControlPanelComponent {
    settingsOpen = false;
    modelOpen = false;
    modeOpen = false;
    tokensOpen = false;
    filesOpen = false;
    promptOpen = false;
    globalPolicyOpen = false;

    @Input() localFilePaths!: string;
    @Input() error?: string;
    @Input() response?: string;
    @Input() selectedModel!: string;
    @Input() selectedMode!: string;
    @Input() maxTokens!: number;
    @Input() systemPromptPath!: string;
    @Input() isTeacher: boolean = false;

    timeUnitOptions: {value: string; label: string}[] = [
        {value: "min", label: "Minutes"},
        {value: "h", label: "Hours"},
        {value: "d", label: "Days"},
    ];
    @Input() tokenLimitAllUsers!: TokenLimitForUser;

    @Input() availableModels?: ChatModel[];
    @Input() availableModes?: string[];

    @Output() saveSettingsClick = new EventEmitter<ControlPanelSettings>();
    @Output() panelToggled = new EventEmitter<boolean>();

    togglePanel() {
        this.settingsOpen = !this.settingsOpen;
        this.panelToggled.emit(this.settingsOpen);
    }

    saveSettingsClicked() {
        const data: ControlPanelSettings = {
            model_id: this.selectedModel,
            llm_mode: this.selectedMode,
            max_tokens: this.maxTokens,
            tim_paths: this.localFilePaths,
            system_prompt_path: this.systemPromptPath,
            global_policy: this.tokenLimitAllUsers,
        };
        console.log("sending: ", data);
        this.saveSettingsClick.emit(data);
    }

    get selectedModelLabel(): string {
        const model = this.availableModels?.find(
            (m) => m.value === this.selectedModel
        );
        return model ? model.label : "";
    }
}
