import {Component, EventEmitter, Input, Output} from "@angular/core";
import type {JsonValue} from "tim/util/jsonvalue";
import {FormsModule} from "@angular/forms";
import {NgForOf, NgIf} from "@angular/common";
import {PurifyModule} from "tim/util/purify.module";
import moment from "moment";
import {
    DirectoryPickerComponent,
    DirectoryPickerRestrictions,
} from "tim/folder/directory-picker.component";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {TokenLimitForUser, UserPolicyComponent} from "./userpolicy";
import {UserControlComponent} from "./usercontrol";
import type {UserData} from "./usercontrol";

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

    max_tokens: number | null;
    conv_time_window: number;
    conv_messages_max: number;
    tim_paths: string[];
    system_prompt_path: string;
    global_policy: TokenLimitForUser;
    use_streaming: boolean;
    model_temperature: number | null;
    top_k_chunks: number;
    include_citations: boolean;
    similarity_threshold: number | null;
}

type SimilarityMode = "none" | "loose" | "balanced" | "strict" | "custom";
type TimeUnit = "seconds" | "minutes" | "hours" | "days";

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
        TooltipModule,
        DirectoryPickerComponent,
    ],
    template: `
        <button class="btn btn-link settings-btn"
                (click)="togglePanel()"
                [attr.aria-expanded]="settingsOpen"
                title="Avaa asetukset">
            <span class="glyphicon glyphicon-cog" style="font-size: 1.8em;"></span>
        </button>


        <div class="settings-panel" [style.display]="settingsOpen ? 'block' : 'none'">
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
                            [ngModel]="selectedPublicKey"
                            (ngModelChange)="onPublicKeyChange($event)">
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
                            <input type="checkbox"
                                   [(ngModel)]="filterModels"
                                   (change)="onFilterModelsChanged()"
                            >
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
                            <a tooltip="{{getStreamTooltip}}"><i class="glyphicon glyphicon-info-sign"></i></a>
                        </label>
                    </div>

                    <div class="checkbox">
                        <label>
                            <input type="checkbox"
                                   [(ngModel)]="includeCitations">
                            Include citations
                            <a tooltip="{{getCitationTooltip}}"><i class="glyphicon glyphicon-info-sign"></i></a>
                        </label>
                    </div>

                    <div class="checkbox">
                        <label>
                            <input type="checkbox"
                                   [(ngModel)]="enabledTemperature">
                            Enable temperature parameter
                            <a tooltip="{{getTemperatureTooltip}}"><i class="glyphicon glyphicon-info-sign"></i></a>
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

                    <div class="form-group">
                        <div class="inline-field">
                            <label class="inline-field-label">Similarity threshold
                                <a tooltip="{{getSimilarityTooltip}}"><i class="glyphicon glyphicon-info-sign"></i></a>
                            </label>
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
                        <label class="inline-field-label">Top-K chunks
                            <a tooltip="{{getTopKTooltip}}"><i class="glyphicon glyphicon-info-sign"></i></a>
                        </label>
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
                    <span *ngIf="isAnthropicKeySelected" style="margin-left: 6px; font-size: 0.85em; color: #888;">
                            (Summarizing not available with Anthropic)
                        </span>
                </button>
                <div *ngIf="modeOpen" class="settings-section-body">
                    <div class="radio" *ngFor="let mode of availableModes">
                        <label [class.disabled-label]="isAnthropicKeySelected && mode !== 'Creative'">
                            <input type="radio"
                                   name="modeRadio"
                                   [value]="mode"
                                   [(ngModel)]="selectedMode"
                                   [disabled]="isAnthropicKeySelected && mode !== 'Creative'">
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
                    Input should be an integer within [0, {{MAX_NUMBER_INPUT}}]
                </div>
            </div>

            <!-- Conversation context window -->
            <div class="settings-row">
                <button class="btn btn-link settings-section-btn"
                        (click)="convWindowOpen = !convWindowOpen">
                      <span class="glyphicon"
                            [class.glyphicon-chevron-right]="!convWindowOpen"
                            [class.glyphicon-chevron-down]="convWindowOpen">
                      </span>
                    Conversation window:
                    <strong>{{ convTimeWindowLabel }}</strong>
                </button>

                <div *ngIf="convWindowOpen" class="settings-section-body">
                    <div>
                        <label class="col-sm-2 control-label time-window-label">
                            <span class="time-window-label-span">Max messages</span>
                            <input type="number" class="form-control restriction-inputs"
                                   [(ngModel)]="convMessagesMax"
                                   min="0"
                                   step="1"
                            >
                        </label>
                    </div>
                    <div class="error" *ngIf="isInvalidMaxMessages">
                        Input should be a positive integer under 256
                    </div>
                    
                    <div>
                        <label class="col-sm-2 control-label time-window-label">
                            <span class="time-window-label-span">Time window</span>
                            <input type="number" class="form-control restriction-inputs"
                                   [(ngModel)]="convTimeWindowAmount"
                                   min="0"
                                   step="1"
                            >
                            <select class="form-control restriction-inputs" [(ngModel)]="convTimeWindowUnit">
                                <option *ngFor="let timeUnit of timeUnitOptions" [ngValue]="timeUnit.value">
                                    {{ timeUnit.label }}
                                </option>
                            </select>
                        </label>
                    </div>
                    <div class="error" *ngIf="isInvalidConvTimeWindow">
                        Input should be an integer within [0, {{MAX_NUMBER_INPUT}}]
                    </div> 
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
                    <userpolicy [userLimits]="tokenLimitAllUsers"
                                (isInInvalidState)="invalidUserPolicyState = $event"
                    >
                    </userpolicy>
                </ng-container>
            </div>

            <!-- Open dialog to view user token usage and policy modifications -->
            <div class="settings-row">
                <button class="btn btn-link settings-section-btn"
                        (click)="toggleUserControl()">
                        <span class="glyphicon"
                              [class.glyphicon-chevron-right]="!userControlOpen"
                              [class.glyphicon-chevron-down]="userControlOpen">
                        </span>
                    Token consumption & per-user policies:
                </button>
                <usercontrol *ngIf="userControlOpen"
                             [setUserData]="userUsageAndPolicyData"
                             (userDataRequest)="userDataRequest.emit()"
                             (policySaveRequest)="policySaveRequest.emit($event)"
                             [policySaveResponse]="policySaveResponse"
                >
                </usercontrol>
            </div>

                <div class="settings-row">
                    <!-- Save button that sends the chosen stuff -->
                    <button class="btn btn-primary" style="margin: 2px;"
                            [disabled]="invalidInputState"
                            (click)="saveSettingsClicked()">
                        Save
                    </button>
                    <!-- Delete button that deletes the plugin instance -->
                    <button class="btn btn-danger" style="margin: 2px;"
                            (click)="deletePluginClicked()">
                        Delete
                    </button>
                </div>
                 <div class="error" *ngIf="error && !response" [innerHTML]="error | purify"></div> 
                <div *ngIf="response && !error" [innerHTML]="response | purify"></div>
        </div>
    `,
})
export class ChatControlPanelComponent {
    MAX_NUMBER_INPUT = 1000_000;

    settingsOpen = false;
    modelOpen = false;
    modeOpen = false;
    keyOpen = false;

    tokensOpen = false;
    convWindowOpen = false;
    filesOpen = false;
    promptOpen = false;
    globalPolicyOpen = false;
    userControlOpen = false;

    @Input() policySaveResponse!: {
        result: string;
        error: string;
    };
    @Input() userUsageAndPolicyData: undefined | UserData[];
    @Output() userDataRequest = new EventEmitter<number>();
    @Output() policySaveRequest = new EventEmitter<UserData>();

    @Output() fetchControlPanelData = new EventEmitter<void>();

    isInvalidMaxTokens = false;
    invalidUserPolicyState: boolean = false;
    maxTokensLocal = "";
    maxTokensValue: number | null = null;

    convTimeWindowAmount: number | null = 0;
    convTimeWindowUnit: TimeUnit = "minutes";

    @Input() convMessagesMax: number = 32;

    timeUnitOptions: {value: TimeUnit; label: string}[] = [
        {value: "seconds", label: "Seconds"},
        {value: "minutes", label: "Minutes"},
        {value: "hours", label: "Hours"},
        {value: "days", label: "Days"},
    ];

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

    allEmbedderProviders: string[] = ["OpenAI", "Google"];

    @Input() set maxTokens(value: number | null) {
        this.maxTokensValue = value;
        this.maxTokensLocal = value != null ? String(value) : "";
    }

    @Input() set convTimeWindow(value: number | null) {
        this.setConvTimeWindowFromSeconds(value ?? 0);
    }

    @Input() set setModelTemperature(value: number | null) {
        if (value == null) {
            this.enabledTemperature = false;
            return;
        }
        this.enabledTemperature = true;
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
    @Output() deletePluginClick = new EventEmitter<void>();

    ngOnInit(): void {
        this.fetchControlPanelData.emit();
    }

    selectFirstFilteredModel(): void {
        const first = this.filteredModels[0];
        if (first) {
            this.selectedModel = first.value;
        }
    }

    onFilterModelsChanged(): void {
        this.filterModels = !this.filterModels;
        if (!this.filterModels) {
            return;
        }
        const stillVisible = this.filteredModels.some(
            (m) => m.value === this.selectedModel
        );
        if (!stillVisible) {
            this.selectFirstFilteredModel();
        }
    }

    fetchModelsClicked() {
        this.fetchModelsClick.emit(this.selectedPublicKey);
    }

    togglePanel() {
        this.settingsOpen = !this.settingsOpen;
        this.panelToggled.emit(this.settingsOpen);
    }

    toggleUserControl() {
        this.userControlOpen = !this.userControlOpen;
        if (this.userControlOpen) {
            this.userDataRequest.emit();
        }
    }

    saveSettingsClicked() {
        const data: ControlPanelSettings = {
            public_key: this.selectedPublicKey,
            model_id: this.selectedModel,
            llm_mode: this.selectedMode,
            max_tokens: this.maxTokensValue,
            conv_time_window: this.convTimeWindowValue,
            conv_messages_max: this.convMessagesMax,
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
            this.isInvalidMaxMessages ||
            this.isInvalidConvTimeWindow ||
            this.isInvalidModelTemperature ||
            this.isInvalidSimilarityThreshold ||
            this.isInvalidTopChunks ||
            this.invalidUserPolicyState
        );
    }

    get isInvalidMaxMessages(): boolean {
        return !this.isValidFloatBetween(this.convMessagesMax, 0, 256);
    }

    get isInvalidConvTimeWindow(): boolean {
        if (this.convTimeWindowAmount == null) {
            return false;
        }
        return (
            !Number.isInteger(this.convTimeWindowAmount) ||
            this.convTimeWindowAmount < 0 ||
            this.convTimeWindowAmount > this.MAX_NUMBER_INPUT
        );
    }

    /* Get the current conversation time window value in seconds. */
    get convTimeWindowValue(): number {
        const v = this.convTimeWindowAmount;
        if (v == null || Number.isNaN(v)) {
            return 0;
        }

        const amount = Math.trunc(v);
        if (amount <= 0) {
            return 0;
        }
        return Math.trunc(
            moment.duration(amount, this.convTimeWindowUnit).asSeconds()
        );
    }

    /* Get the label for the selected conversation time window. */
    get convTimeWindowLabel(): string {
        if (this.convTimeWindowValue === 0 || this.convMessagesMax === 0) {
            return "Disabled";
        }
        const amount: number = Math.trunc(this.convTimeWindowAmount ?? 0);
        const unitLabel: string =
            this.timeUnitOptions.find(
                (o) => o.value === this.convTimeWindowUnit
            )?.label ?? this.convTimeWindowUnit;
        return `${this.convMessagesMax} in ${amount} ${unitLabel}`;
    }

    /* Set the closest time window unit and value from seconds. */
    private setConvTimeWindowFromSeconds(seconds: number): void {
        if (!seconds || Number.isNaN(seconds) || seconds <= 0) {
            this.convTimeWindowAmount = 0;
            this.convTimeWindowUnit = "minutes";
            return;
        }

        const units: TimeUnit[] = this.timeUnitOptions.map((u) => u.value);
        let bestUnit: TimeUnit = "seconds";
        let bestAmount: number = Math.max(1, Math.round(seconds));
        let bestDiff: number = Math.abs(bestAmount - seconds);

        for (const unit of units) {
            const mult: number = moment.duration(1, unit).asSeconds();
            const amount: number = Math.max(1, Math.round(seconds / mult));
            const candidateSeconds: number = amount * mult;
            const diff: number = Math.abs(candidateSeconds - seconds);
            if (
                diff < bestDiff ||
                (diff === bestDiff &&
                    mult > moment.duration(1, bestUnit).asSeconds())
            ) {
                bestDiff = diff;
                bestUnit = unit;
                bestAmount = amount;
            }
        }

        this.convTimeWindowUnit = bestUnit;
        this.convTimeWindowAmount = bestAmount;
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
            Number.isNaN(parsed) ||
            parsed < 0 ||
            parsed > this.MAX_NUMBER_INPUT ||
            !Number.isInteger(parsed);

        if (!this.isInvalidMaxTokens) {
            this.maxTokensValue = parsed;
        }
    }

    get getStreamTooltip(): string {
        return "Enable streaming to receive responses incrementally.";
    }

    get getTemperatureTooltip(): string {
        return (
            "Adjust the randomness of the model's responses." +
            " Higher values result in more random outputs."
        );
    }

    get getCitationTooltip(): string {
        return "Include citations in the responses to provide references for the used context.";
    }

    get getSimilarityTooltip(): string {
        return (
            "Affects the selection of found context." +
            " The higher the value, the stricter the similarity requirement for context selection."
        );
    }

    get getTopKTooltip(): string {
        return "Specify the number of top chunks to consider for context selection.";
    }

    get isAnthropicKeySelected() {
        const key = this.availablePublicKeys.find(
            (k) => k.public_key === this.selectedPublicKey
        );
        return key?.provider.toLowerCase() === "anthropic";
    }

    onPublicKeyChange(key: string): void {
        this.selectedPublicKey = key;
        if (this.isAnthropicKeySelected) {
            this.selectedMode = "Creative";
        }
    }

    async deletePluginClicked() {
        if (
            !(await showConfirm(
                $localize`Delete plugin?`,
                $localize`Are you sure you want to permanently delete this plugin from the document?`
            ))
        ) {
            return;
        }
        this.deletePluginClick.emit();
    }
}
