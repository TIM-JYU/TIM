import type {OnChanges, SimpleChanges} from "@angular/core";
import {Component, EventEmitter, Input, Output} from "@angular/core";
import {showConfirm} from "tim/ui/showConfirmDialog";
import type {JsonValue} from "tim/util/jsonvalue";
import {DirectoryPickerRestrictions} from "tim/folder/directory-picker.component";

export interface ChatModel extends Record<string, JsonValue> {
    label: string;
    value: string;
}

export interface UserKey extends Record<string, JsonValue> {
    provider: string;
    public_key: string;
    is_selected: boolean;
    is_shared: boolean;
    shared_by: string;
}

export interface ControlPanelSettings extends Record<string, JsonValue> {
    public_key: string;
    model_id: string;
    llm_mode: string;
    embedder_provider: string;

    max_tokens: number | null;
    tim_paths: string[];
    system_prompt_path: string;
    global_policy: TokenLimitForUser;
    use_streaming: boolean;
    model_temperature: number | null;
    top_k_chunks: number;
    include_citations: boolean;
    similarity_threshold: number | null;
}

export interface TokenLimitForUser extends Record<string, JsonValue> {
    token_cap_enabled: boolean;
    token_cap: number | null;
    time_window_enabled: boolean;
    window_unit: string;
    window_value: number | null;
    token_cap_for_window: number | null;
}

type SimilarityMode = "none" | "loose" | "balanced" | "strict" | "custom";

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
                    <button class="btn btn-warning"
                            (click)="clearConversationClicked()">
                        Clear conversation
                    </button>
                </div>
            </ng-container>
            <ng-container *ngIf="isTeacher">

                <!-- Choose API-key -->
                <div class="settings-row">
                    <button class="btn btn-link settings-section-btn"
                            (click)="keyOpen = !keyOpen">
                    <span class="glyphicon"
                          [class.glyphicon-chevron-right]="!keyOpen"
                          [class.glyphicon-chevron-down]="keyOpen">
                    </span>
                        API-alias: <strong>{{ selectedPublicKey }}</strong>
                    </button>
                    <div *ngIf="keyOpen" class="settings-section-body">
                        <select class="form-control"
                                [(ngModel)]="selectedPublicKey">
                            <option *ngFor="let key of availablePublicKeys"
                                    [ngValue]="key.public_key">{{
                                    key.public_key + " - " + key.provider + (key.is_shared ? " " +
                                        "(shared by " + key.shared_by + ")" : "")
                                }}
                            </option>
                            z§
                        </select>
                        <button class="btn btn-default"
                                style="margin-top: 6px;"
                                [disabled]="!selectedPublicKey"
                                (click)="fetchModelsClicked()">
                            Fetch models
                        </button>
                    </div>
                </div>


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
                        <div class="checkbox">
                            <label>
                                <input type="checkbox" [(ngModel)]="filterModels">
                                Filter non-chat models
                            </label>
                        </div>
                        <select class="form-control" [(ngModel)]="selectedModel">
                            <option *ngFor="let m of filteredModels" [ngValue]="m.value">{{ m.label }}</option>
                        </select>

                        <!-- Model parameters -->
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
                            Temperature input should be between 0 and 2
                        </div>

                        <div class="checkbox">
                            <label>
                                <input type="checkbox"
                                       [(ngModel)]="includeCitations">
                                Include citations
                            </label>
                        </div>

                        <div class="form-group">
                            <div class="inline-field">
                                <label class="inline-field-label">Similarity threshold</label>
                                <select class="form-control inline-field-control"
                                        [(ngModel)]="similarityThresholdMode">
                                    <option *ngFor="let o of SIMILARITY_OPTION_LIST" [ngValue]="o.mode">
                                        {{ o.label }}
                                    </option>
                                </select>
                            </div>

                            <div *ngIf="similarityThresholdMode === 'custom'" style="margin-top: 6px;">
                                <input
                                    type="number"
                                    class="form-control"
                                    [(ngModel)]="similarityThreshold"
                                    min="-1"
                                    max="1"
                                    step="0.05"
                                />
                            </div>
                            <div class="error" *ngIf="isInvalidSimilarityThreshold">
                                Similarity input should be between -1 and 1
                            </div>
                        </div>
                        
                        <div class="inline-field">
                            <label class="inline-field-label">Top-K chunks</label>
                            <input
                                type="number"
                                class="form-control inline-field-control"
                                [(ngModel)]="topKChunks"
                                min="1"
                                max="20"
                            />
                        </div>
                        <div class="error" *ngIf="isInvalidTopChunks">
                            Top-K should be between 1 and 20
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
                        Add TIM-documents: {{ selectedItemPaths.length }}
                    </button>
                    <div *ngIf="filesOpen" class="settings-section-body">
                        <tim-directory-picker
                            [startItem]="currentFolder 
                                        ? {itemPath: currentFolder, isFolder: true} 
                                        : undefined"
                            [(selection)]="selectedItemPaths"
                            [restrictions]="pathRestrictions"
                        ></tim-directory-picker>
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
                        Set system prompt path: {{ systemPrompt }}
                    </button>
                    <div *ngIf="promptOpen" class="settings-section-body">
                        <tim-directory-picker
                            [startItem]="systemPrompt 
                                        ? {itemPath: systemPrompt, isFolder: false} 
                                        : undefined"
                            [(selection)]="systemPromptSelection"
                            [restrictions]="{maxSelectedCount: 1, selectable: 'documents'}"
                        ></tim-directory-picker>
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
                                       [(ngModel)]="tokenLimitAllUsers.token_cap_enabled">
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
                            Input should be a positive integer
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
                                    Input should be a positive integer
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
                                    Input should be a positive integer
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
                <div class="error" *ngIf="error && !response" [innerHTML]="error | purify"></div>
                <div *ngIf="response && !error" [innerHTML]="response | purify"></div>
            </ng-container>
        </div>
    `,
})
export class ChatControlPanelComponent implements OnChanges {
    settingsOpen = false;
    modelOpen = false;
    modeOpen = false;
    keyOpen = false;

    tokensOpen = false;
    filesOpen = false;
    promptOpen = false;
    globalPolicyOpen = false;

    isInvalidMaxTokens = false;
    maxTokensLocal = "";
    maxTokensValue: number | null = null;

    enabledTemperature: boolean = false;
    modelTemperature: number | null = 0.2;
    filterModels = true;
    similarityThreshold: number | null = 0.3;
    similarityThresholdMode: SimilarityMode = "none";

    systemPromptSelection: string[] = [];

    readonly NON_CHAT_PREFIXES = [
        "image",
        "embedding",
        "whisper",
        "tts",
        "audio",
        "transcribe",
        "realtime",
        "sora",
        "dall-e",
        "moderation",
    ];

    readonly SIMILARITY_OPTIONS: Record<
        SimilarityMode,
        {label: string; value: number | null}
    > = {
        none: {label: "None", value: null},
        loose: {label: "Loose (0.3)", value: 0.3},
        balanced: {label: "Balanced (0.5)", value: 0.5},
        strict: {label: "Strict (0.7)", value: 0.7},
        custom: {label: "Custom…", value: null},
    };

    readonly SIMILARITY_OPTION_LIST = Object.entries(
        this.SIMILARITY_OPTIONS
    ).map(([mode, opt]) => ({mode: mode as SimilarityMode, ...opt}));

    timeUnitOptions: {value: string; label: string}[] = [
        {value: "min", label: "Minutes"},
        {value: "h", label: "Hours"},
        {value: "d", label: "Days"},
    ];

    allEmbedderProviders: string[] = ["OpenAI", "Google"];

    @Input() set maxTokens(value: number | null) {
        this.maxTokensValue = value;
        this.maxTokensLocal = value != null ? String(value) : "";
    }

    @Input() set setModelTemperature(value: number | null) {
        if (!value) {
            this.enabledTemperature = false;
            return;
        }
        this.modelTemperature = value;
    }

    @Input() set setSimilarityThreshold(value: number | null) {
        if (value == null) {
            this.similarityThresholdMode = "none";
            this.similarityThreshold = 0.3;
            return;
        }
        for (const entry of this.SIMILARITY_OPTION_LIST) {
            if (entry.value == value) {
                this.similarityThresholdMode = entry.mode;
                this.similarityThreshold = value;
                return;
            }
        }
        this.similarityThresholdMode = "custom";
        this.similarityThreshold = value;
    }

    @Input() set systemPromptPath(path: string) {
        if (path) {
            this.systemPromptSelection = [path.trim()];
        }
    }

    @Input() useStreaming: boolean = false;
    @Input() includeCitations: boolean = false;
    @Input() topKChunks: number = 3;
    @Input() selectedItemPaths!: string[];
    @Input() pathRestrictions?: DirectoryPickerRestrictions;
    @Input() currentFolder?: string;
    @Input() error?: string;
    @Input() response?: string;
    @Input() selectedModel!: string;
    @Input() selectedMode!: string;
    @Input() selectedEmbedderProvider!: string;
    @Input() isTeacher: boolean = false;
    @Input() tokenLimitAllUsers!: TokenLimitForUser;
    @Input() availableModels?: ChatModel[];
    @Input() availableModes?: string[];
    @Input() selectedPublicKey: string = "";
    @Input() availablePublicKeys: UserKey[] = [];
    @Input() availableEmbedderProviders: string[] = [];

    @Output() fetchModelsClick = new EventEmitter<string>();
    @Output() saveSettingsClick = new EventEmitter<ControlPanelSettings>();
    @Output() panelToggled = new EventEmitter<boolean>();
    @Output() clearConversationClick = new EventEmitter<void>();

    ngOnChanges(changes: SimpleChanges): void {
        if (changes.availableModels) {
            const prev = changes.availableModels.previousValue as
                | ChatModel[]
                | undefined;
            const curr = changes.availableModels.currentValue as
                | ChatModel[]
                | undefined;
            if (curr && curr.length > 0 && curr !== prev) {
                this.selectFirstFilteredModel();
            }
        }
    }

    selectFirstFilteredModel(): void {
        const first = this.filteredModels[0];
        if (first) {
            this.selectedModel = first.value;
        }
    }

    onFilterModelsChanged(): void {
        const stillVisible = this.filteredModels.some(
            (m) => m.value === this.selectedModel
        );
        if (!stillVisible) {
            this.selectFirstFilteredModel();
        }
    }

    embedderAvailable(provider: string): boolean {
        return this.availableEmbedderProviders.includes(provider.toLowerCase());
    }

    fetchModelsClicked() {
        this.fetchModelsClick.emit(this.selectedPublicKey);
    }

    togglePanel() {
        this.settingsOpen = !this.settingsOpen;
        this.panelToggled.emit(this.settingsOpen);
    }

    async clearConversationClicked() {
        if (
            !(await showConfirm(
                "Clear conversation?",
                "Are you sure you want to clear the conversation?"
            ))
        ) {
            return;
        }
        this.clearConversationClick.emit();
    }

    saveSettingsClicked() {
        const data: ControlPanelSettings = {
            public_key: this.selectedPublicKey,
            model_id: this.selectedModel,
            llm_mode: this.selectedMode,
            embedder_provider: this.selectedEmbedderProvider,
            max_tokens: this.maxTokensValue,
            tim_paths: this.selectedItemPaths,
            system_prompt_path: this.systemPrompt ?? "",
            global_policy: this.tokenLimitAllUsers,
            use_streaming: this.useStreaming,
            include_citations: this.includeCitations,
            similarity_threshold: this.similarityThresholdValue,
            model_temperature: this.enabledTemperature
                ? this.modelTemperature
                : null,
            top_k_chunks: this.topKChunks,
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

    get systemPrompt(): string | undefined {
        if (!this.systemPromptSelection) {
            return undefined;
        }
        return this.systemPromptSelection[0];
    }

    get similarityThresholdValue(): number | null {
        const preset: number | null =
            this.SIMILARITY_OPTIONS[this.similarityThresholdMode].value;
        return this.similarityThresholdMode === "custom"
            ? this.similarityThreshold
            : preset;
    }

    get filteredModels(): ChatModel[] {
        if (!this.availableModels) {
            return [];
        }
        if (!this.filterModels) {
            return this.availableModels;
        }
        return this.availableModels.filter(
            (m) => !this.NON_CHAT_PREFIXES.some((p) => m.value.includes(p))
        );
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

    get isInvalidModelTemperature(): boolean {
        return (
            this.enabledTemperature &&
            !this.isValidFloatBetween(this.modelTemperature, 0, 2)
        );
    }

    get isInvalidSimilarityThreshold(): boolean {
        if (this.similarityThresholdMode == "none") {
            return false;
        }
        return !this.isValidFloatBetween(this.similarityThresholdValue, -1, 1);
    }

    get isInvalidTopChunks(): boolean {
        return !this.isValidFloatBetween(this.topKChunks, 1, 20);
    }

    get invalidInputState(): boolean {
        return (
            this.isInvalidMaxTokens ||
            this.isInvalidTokenCap ||
            this.isInvalidWindowTime ||
            this.isInvalidWindowTokens ||
            this.isInvalidModelTemperature ||
            this.isInvalidSimilarityThreshold ||
            this.isInvalidTopChunks
        );
    }

    isValidNonNegativeInt(value: number | null): boolean {
        return value !== null && Number.isInteger(value) && value >= 0;
    }

    isValidNumberInput(enabled: boolean, value: number | null): boolean {
        return enabled && !this.isValidNonNegativeInt(value);
    }

    isValidFloatBetween(
        value: number | null,
        min: number,
        max: number
    ): boolean {
        return value !== null && value >= min && value <= max;
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
