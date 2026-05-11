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
    token_cap: number | null;
    time_window_enabled: boolean;
    window_unit: string;
    window_value: number | null;
    token_cap_for_window: number | null;
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
                        <input type="number"
                               class="form-control"
                               min="0" 
                               [(ngModel)]="maxTokens"
                               >
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
                            <input type="number"
                                   class="form-control"
                                   [(ngModel)]="tokenLimitAllUsers.token_cap"
                            >
                        </div>
                        <div class="error" *ngIf="isInvalidTokenCap">
                            Input invalid or empty while token limit is enabled! 
                        </div>

                        <div class="checkbox">
                            <label>
                                <input type="checkbox" [(ngModel)]="tokenLimitAllUsers.time_window_enabled">
                                Enable time window token limits
                            </label>
                        </div>

                        <div *ngIf="tokenLimitAllUsers.time_window_enabled">
                            <div class="form-group">
                                <label class="col-sm-2 control-label time-window-label">
                                    <span class="time-window-label-span">Tokens</span>
                                    <input type="number" class="form-control restriction-inputs"
                                           placeholder="5000"
                                           [(ngModel)]="tokenLimitAllUsers.token_cap_for_window">
                                </label>
                                <div class="error" *ngIf="isInvalidWindowTokens">
                                    Input invalid or empty while window is enabled!
                                </div>
                                <div>
                                    <label class="col-sm-2 control-label time-window-label">
                                        <span class="time-window-label-span">Window</span>
                                        <input type="number" class="form-control restriction-inputs"
                                               placeholder="5"
                                               [(ngModel)]="tokenLimitAllUsers.window_value"
                                               >
                                    <select class="form-control restriction-inputs"
                                            [(ngModel)]="tokenLimitAllUsers.window_unit">
                                        <option *ngFor="let timeUnit of timeUnitOptions" 
                                                [ngValue]="timeUnit.value">{{ timeUnit.label }}
                                        </option>
                                    </select>
                                    </label>
                                </div>
                                <div class="error" *ngIf="isInvalidWindowTime">
                                    Input invalid or empty while window is enabled!
                                </div>
                            </div>
                        </div>
                    </ng-container>
                </div>

                <!-- Save button that sends the chosen stuff -->
                <div class="settings-row">
                    <button class="btn btn-primary" style="margin: 2px;"
                            [disabled]="invalidInputState"
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

    isValidNonNegativeNumber(value: number | null): boolean {
        return !(value === null || value < 0 || !Number.isFinite(value));
    }

    isValidNumberInput(enabled: boolean, value: number | null): boolean {
        return enabled && !this.isValidNonNegativeNumber(value);
    }

    get isInvalidTokenCap(): boolean {
        return this.isValidNumberInput(
            this.tokenLimitAllUsers.token_cap_enabled,
            this.tokenLimitAllUsers.token_cap
        );
    }

    get isInvalidWindowTokens(): boolean {
        return this.isValidNumberInput(
            this.tokenLimitAllUsers.time_window_enabled,
            this.tokenLimitAllUsers.token_cap_for_window
        );
    }

    get isInvalidWindowTime(): boolean {
        return this.isValidNumberInput(
            this.tokenLimitAllUsers.time_window_enabled,
            this.tokenLimitAllUsers.window_value
        );
    }

    get invalidInputState(): boolean {
        return (
            this.isInvalidTokenCap ||
            this.isInvalidWindowTime ||
            this.isInvalidWindowTokens
        );
    }
}
