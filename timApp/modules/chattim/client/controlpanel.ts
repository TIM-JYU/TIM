import {Component, EventEmitter, Input, Output} from "@angular/core";
import type {JsonValue} from "tim/util/jsonvalue";
import {FormsModule} from "@angular/forms";
import {NgForOf, NgIf} from "@angular/common";
import {PurifyModule} from "tim/util/purify.module";
import {TokenLimitForUser, UserPolicyComponent} from "./userpolicy";
import {UserControlComponent} from "./usercontrol";
import type {UserData} from "./usercontrol";

export interface ChatModel extends Record<string, JsonValue> {
    label: string;
    value: string;
}

export interface ControlPanelSettings extends Record<string, JsonValue> {
    model_id: string;
    llm_mode: string;
    embedder_provider: string;

    max_tokens: number | null;
    tim_paths: string;
    system_prompt_path: string;
    global_policy: TokenLimitForUser;
    use_streaming: boolean;
    model_temperature: number | null;
}

@Component({
    selector: "chattim-control-panel",
    standalone: true,
    imports: [
        UserPolicyComponent,
        FormsModule,
        NgIf,
        NgForOf,
        PurifyModule,
        UserControlComponent,
    ],
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

                        <!-- Model parameters -->
                        <div class="settings-section-body">
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox"
                                           [(ngModel)]="useStreaming">
                                    Use streaming responses
                                </label>
                            </div>
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox"
                                           [(ngModel)]="enabledTemperature">
                                    Enable temperature parameter
                                </label>
                            </div>
                            <div *ngIf="enabledTemperature" class="settings-section-body">
                                <input type="number"
                                       class="form-control"
                                       [(ngModel)]="modelTemperature"
                                       [min]="0"
                                       [max]="2"
                                       [step]="0.1"
                                >
                            </div>
                            <div class="error" *ngIf="isInvalidModelTemperature">
                                Input should be between 0 and 2
                            </div>
                        </div>

                        Provider for embedding creation: <strong>{{ selectedEmbedderProvider }}</strong> 
                        <div class="embedder-buttons">
                            <div class="form-check" *ngFor="let provider of allEmbedderProviders">
                                <label class="form-check-label" [class.disabled-label]="!embedderAvailable(provider)"
                                       [title]="embedderAvailable(provider) ? '' : 'No API key for ' + provider">
                                    <input type="radio"
                                           class="form-check-input"
                                           name="embedderProviderRadio"
                                           [disabled]="!embedderAvailable(provider)"
                                           [value]="provider" 
                                           [(ngModel)]="selectedEmbedderProvider">
                                    {{ provider }}
                                </label>
                            </div>
                        </div>
                        
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
                        Max tokens: <strong>{{ maxTokensValue }}</strong>
                    </button>
                    <div *ngIf="tokensOpen" class="settings-section-body">
                        <input type="text"
                               class="form-control"
                               [(ngModel)]="maxTokensLocal"
                               (ngModelChange)="onMaxTokensChanged()"
                        >
                    </div>

                    <div class="error" *ngIf="isInvalidMaxTokens">
                        Input should be a positive integer
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
                            <userpolicy [userLimits]="tokenLimitAllUsers"
                                        (isInInvalidState)="invalidUserPolicyState = $event"
                                        >
                            </userpolicy>
                    </ng-container>
                </div>

                <!-- Open dialog to view user token usage and policy modif -->
                <div class="settings-row"  >
                    <button class="btn btn-link settings-section-btn"
                            (click)="userControlOpen = !userControlOpen">
                        <span class="glyphicon"
                              [class.glyphicon-chevron-right]="!userControlOpen"
                              [class.glyphicon-chevron-down]="userControlOpen">
                        </span>
                        Token consumption & per user policies:
                    </button>
                    <usercontrol *ngIf="userControlOpen"
                                 [setUserData]="userUsageAndPolicyData"
                                 (userDataRequest)="userDataRequest.emit($event)"
                                 (policySaveRequest)="policySaveRequest.emit($event)"
                                 >
                    </usercontrol>
                </div>

                <!-- Save button that sends the chosen stuff -->
                <div class="settings-row">
                    <button class="btn btn-primary" style="margin: 2px;"
                            [disabled]="invalidInputState"
                            (click)="saveSettingsClicked()">
                        Save
                    </button>
                </div>
                <div class="error" *ngIf="error && !response" [innerHTML]="error | purify"></div>
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
    userControlOpen = false;

    @Input() policyDataSaveResp: undefined | string;
    @Input() userUsageAndPolicyData: undefined | UserData[];
    @Output() userDataRequest = new EventEmitter<number>();
    @Output() policySaveRequest = new EventEmitter<UserData>();

    isInvalidMaxTokens = false;
    maxTokensLocal = "";
    maxTokensValue: number | null = null;
    @Input() set maxTokens(value: number | null) {
        this.maxTokensValue = value;
        this.maxTokensLocal = value != null ? String(value) : "";
    }

    invalidUserPolicyState: boolean = false;

    enabledTemperature: boolean = false;
    modelTemperature: number | null = 0.2;
    @Input() set setModelTemperature(value: number | null) {
        if (!value) {
            this.enabledTemperature = false;
            return;
        }
        this.modelTemperature = value;
    }
    @Input() useStreaming: boolean = false;

    @Input() localFilePaths!: string;
    @Input() error?: string;
    @Input() response?: string;
    @Input() selectedModel!: string;

    @Input() selectedMode!: string;
    @Input() selectedEmbedderProvider!: string;

    @Input() systemPromptPath!: string;
    @Input() isTeacher: boolean = false;

    @Input() tokenLimitAllUsers!: TokenLimitForUser;

    @Input() availableModels?: ChatModel[];
    @Input() availableModes?: string[];
    allEmbedderProviders: string[] = ["OpenAI", "Google"];
    @Input() availableEmbedderProviders: string[] = [];
    embedderAvailable(provider: string): boolean {
        return this.availableEmbedderProviders.includes(provider.toLowerCase());
    }
    @Output() saveSettingsClick = new EventEmitter<ControlPanelSettings>();
    @Output() panelToggled = new EventEmitter<boolean>();

    togglePanel() {
        this.settingsOpen = !this.settingsOpen;
        this.panelToggled.emit(this.settingsOpen);
    }

    openUsageViewClicked() {
        this.userControlOpen = !this.userControlOpen;
    }

    saveSettingsClicked() {
        const data: ControlPanelSettings = {
            model_id: this.selectedModel,
            llm_mode: this.selectedMode,
            embedder_provider: this.selectedEmbedderProvider,

            max_tokens: this.maxTokensValue,
            tim_paths: this.localFilePaths,
            system_prompt_path: this.systemPromptPath,
            global_policy: this.tokenLimitAllUsers,
            use_streaming: this.useStreaming,
            model_temperature: this.enabledTemperature
                ? this.modelTemperature
                : null,
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

    isValidFloatBetween(
        value: number | null,
        min: number,
        max: number
    ): boolean {
        return value !== null && value >= min && value <= max;
    }

    get isInvalidModelTemperature(): boolean {
        return (
            this.enabledTemperature &&
            !this.isValidFloatBetween(this.modelTemperature, 0, 2)
        );
    }

    get invalidInputState(): boolean {
        return (
            this.isInvalidMaxTokens ||
            this.invalidUserPolicyState ||
            this.isInvalidModelTemperature
        );
    }

    onMaxTokensChanged(): void {
        const trimmed = this.maxTokensLocal.trim();

        if (trimmed === "") {
            this.maxTokensValue = null;
            this.isInvalidMaxTokens = false;
            return;
        }

        const parsed = Number(trimmed);

        this.isInvalidMaxTokens =
            Number.isNaN(parsed) || parsed < 0 || !Number.isInteger(parsed);

        if (!this.isInvalidMaxTokens) {
            this.maxTokensValue = parsed;
        }
    }
}
