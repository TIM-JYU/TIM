/**
 * Defines the client-side implementation chattim-plugin.
 */
import * as t from "io-ts";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
} from "tim/plugin/attributes";
import type {AfterViewInit, ApplicationRef, DoBootstrap} from "@angular/core";
import {ViewChild} from "@angular/core";
import {
    Component,
    ElementRef,
    NgModule,
    ViewEncapsulation,
} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {PurifyModule} from "tim/util/purify.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import type {HttpDownloadProgressEvent, HttpEvent} from "@angular/common/http";
import {
    HttpClient,
    HttpClientModule,
    HttpEventType,
} from "@angular/common/http";
import {DomSanitizer} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";
import {Users} from "tim/user/userService";
import type {DirectoryPickerRestrictions} from "tim/folder/directory-picker.component";
import {DirectoryPickerComponent} from "tim/folder/directory-picker.component";
import {itemglobals} from "tim/util/globals";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import type {AngularError, Result} from "tim/util/utils";
import type {ChatModel, ControlPanelSettings, UserKey} from "./controlpanel";
import {ChatControlPanelComponent} from "./controlpanel";
import type {TokenLimitForUser} from "./userpolicy";
import type {UserData} from "./usercontrol";

const PluginMarkupFields = t.intersection([
    t.partial({
        welcomeText: t.string,
        apiAlias: t.string,
        defaultWindowSize: t.union([
            t.literal("sm"),
            t.literal("md"),
            t.literal("lg"),
            t.literal("xs"),
        ]),
        blockContent: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
    }),
]);
const PluginFields = t.intersection([
    getTopLevelFields(PluginMarkupFields),
    t.type({
        state: nullable(t.type({userinput: t.string})),
    }),
]);

export interface Message {
    content: string;
    role: string;
    timestamp_ms?: number;
    citations?: string[];
}

export interface ChatEntry {
    user: Message;
    agent: Message;
}

export interface AskResponse {
    answer?: string;
    citations?: string[];
    error?: string;
}

export interface AskParams {
    input: string;
    document_id: number;
}

export interface ControlPanelData extends ControlPanelSettings {
    availableModels: ChatModel[];
    availableModes: string[];
    availableEmbedderProviders: string[];
    availableKeys: UserKey[];
    chosenKey: string;
    selectedModel: string | null;
    allowedItemPaths?: string[];
}

@Component({
    selector: "chattim-runner",
    encapsulation: ViewEncapsulation.None,
    template: `
        <ng-container *ngIf="!pluginDeleted">
            <div class="chattim-block-anchor"
                 *ngIf="(markup.blockContent ?? '').trim().length > 0"
                 [innerHTML]="markup.blockContent | purify"
                 [hidden]="!hasManageRights"
            >
            </div>
            <tim-dialog-frame class="chattim-dialog-frame" [size]="windowSize">
                <ng-container header> {{ header }}</ng-container>
                <ng-container body>
                    <div class="chattim-body scroll-box" #conversationScroll>
                        <div class="upper-area">
                            <div>
                                <div *ngIf="conversation.length === 0" class="chat-welcome">
                                    <ng-container *ngIf="markup.welcomeText; else localizedWelcome">
                                        {{ markup.welcomeText }}
                                    </ng-container>
                                    <ng-template #localizedWelcome>
                                        <span i18n>"Welcome to use TIM's helper chatbot!"</span>
                                    </ng-template>
                                </div>
                                <div *ngFor="let entry of conversation">
                                    <div class="chat-bubble-wrapper">
                                        <div class="chat-user">{{ entry.user.content }}</div>
                                    </div>
                                    <div class="chat-bubble-wrapper">
                                        <div class="chat-bot">
                                            <div [innerHTML]="entry.agent.content | purify"></div>
                                            <div class="answer-footer">
                                            <span *ngIf="entry.agent.citations && entry.agent.citations.length > 0">
                                            <ng-container *ngFor="let citation of entry.agent.citations; let i = index">
                                                <a [href]="citation" target="_blank">[{{ i + 1 }}]</a>
                                                <ng-container *ngIf="i < entry.agent.citations.length - 1">, </ng-container>
                                            </ng-container>
                                        </span><span
                                                class="chat-timestamp">{{ dateString(entry.agent.timestamp_ms) }}</span>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </div>
    
                            <div>
                                <tim-loading *ngIf="isRunning"></tim-loading>
                                <div *ngIf="error" [innerHTML]="error | purify"></div>
                            </div>
                            <label class="justify-center w-100">{{ inputStem }} </label>
                            <div class="sticky-chat-row">
                                <div class="d-flex flex-row w-100 justify-content-center chat-row">
                        <textarea i18n-placeholder class="form-control chat-textarea"
                                  rows="2"
                                  placeholder="Ask me about TIM related things"
                                  style="resize: none; overflow: hidden; min-width: 0;"
                                  [(ngModel)]="userInput"
                                  (keydown.enter)="onEnter($event)"
                                  onkeyup="this.style.height='auto'; this.style.height=this.scrollHeight+'px'"
                                  oninput="this.style.height='auto'; this.style.height=this.scrollHeight+'px'">
                        </textarea>
    
                                    <button class="timButton flex-shrink-0 ms-2"
                                            *ngIf="buttonText()"
                                            [disabled]="!canSendInput()"
                                            (click)="sendUserInput()"
                                            [innerHTML]="buttonText() | purify">
                                    </button>
                                </div>
                            </div>
                        </div>
    
                        <div class="control-panel-container">
                            
                            <ng-container *ngIf="!isTeacher">
                                <button class="btn btn-link settings-btn"
                                        (click)="onControlPanelToggle(!controlPanelOpen)"
                                        [attr.aria-expanded]="controlPanelOpen"
                                        title="Avaa asetukset">
                                    <span class="glyphicon glyphicon-cog" style="font-size: 1.8em;"></span>
                                </button>
                                
                                <div class="settings-panel" [style.display]="controlPanelOpen ? 'block' : 'none'">
                                    <button class="btn btn-warning"
                                            (click)="clearConversationClicked()">
                                        Clear conversation
                                    </button>
                                </div>
                            </ng-container>
                                
                            <chattim-control-panel *ngIf="isTeacher"
                                [isTeacher]="isTeacher"
                                (saveSettingsClick)="onSaveSettings($event)"
                                (deletePluginClick)="onDeletePlugin()"
                                (panelToggled)="onControlPanelToggle($event)"
                                (fetchModelsClick)="onFetchModels($event)"
                                [selectedModel]="selectedModel"
                                [setModelTemperature]="modelTemperature"
                                [useStreaming]="useStreaming"
                                [includeCitations]="includeCitations"
                                [setSimilarityThreshold]="similarityThreshold"
                                [topKChunks]="topKChunks"
                                [systemPromptPath]="systemPromptPath"
                                [selectedMode]="selectedMode"
                                [maxTokens]="maxTokens"
                                [response]="controlpanelResponse"
                                [error]="controlpanelError"
                                [selectedItemPaths]="selectedItemPaths"
                                [pathRestrictions]="pathRestrictions"
                                [currentFolder]="getCurrentFolder"
                                [availableModels]="availableModels"
                                [availableEmbedderProviders]="availableEmbedderProviders"
                                [availableModes]="availableModes"
                                [tokenLimitAllUsers]="globalPolicy"
                                [userUsageAndPolicyData]="userUsageAndPolicyData"
                                (userDataRequest)="getUserData()"
                                (policySaveRequest)="handleUserPolicySave($event)" 
                                [policySaveResponse]="policySaveResponse" 
                                [tokenLimitAllUsers]="globalPolicy"
                                [selectedPublicKey]="selectedPublicKey"
                                [availablePublicKeys]="availablePublicKeys">
                            </chattim-control-panel>
                        </div>
    
                    </div>
                </ng-container>
            </tim-dialog-frame>
        </ng-container> 
    `,
    styleUrls: ["./chattim.scss"],
})
export class ChatTIMComponent
    extends AngularPluginBase<
        t.TypeOf<typeof PluginMarkupFields>,
        t.TypeOf<typeof PluginFields>,
        typeof PluginFields
    >
    implements AfterViewInit
{
    @ViewChild(ChatControlPanelComponent)
    controlPanel!: ChatControlPanelComponent;
    controlPanelOpen: boolean = false;
    private hostElement!: ElementRef<HTMLElement>;

    onControlPanelToggle(isOpen: boolean) {
        const dialog = this.getModalDialog();
        if (!dialog) {
            return;
        }
        this.controlPanelOpen = isOpen;
        const panelEl: HTMLElement | null =
            this.hostElement.nativeElement.querySelector(".settings-panel");

        if (isOpen) {
            if (panelEl) {
                panelEl.style.position = "absolute";
                panelEl.style.visibility = "hidden";
                panelEl.style.display = "block";
                const panelHeight = panelEl.scrollHeight;
                panelEl.style.position = "";
                panelEl.style.visibility = "";
                panelEl.style.display = "";
                requestAnimationFrame(() => {
                    dialog.style.height = `${
                        dialog.offsetHeight + panelHeight
                    }px`;
                });
            }
        } else {
            if (!panelEl) {
                return;
            }
            const panelHeight = panelEl.scrollHeight;
            requestAnimationFrame(() => {
                dialog.style.height = `${dialog.offsetHeight - panelHeight}px`;
            });
        }
    }

    @ViewChild("conversationScroll", {static: false}) scrollFrame!: ElementRef;
    private scrollContainer?: HTMLElement;
    private scrollScheduled: boolean = false;
    private scrollWanted: boolean = false;

    private loadingOlder: boolean = false;
    private hasMoreHistory: boolean = true;
    private readonly historyPageSize: number = 20;
    private readonly scrollUpThreshold: number = 50;

    conversation: ChatEntry[] = [];

    answer?: string;
    error?: string;
    isRunning: boolean = false;
    userInput = "";
    inputStem = ""; // TODO: do something with this or remove?
    document_id = -1;
    isTeacher: boolean = false;
    hasManageRights: boolean = false;
    selectedItemPaths: string[] = [];
    pathRestrictions?: DirectoryPickerRestrictions;
    selectedPublicKey: string = "";
    availablePublicKeys: UserKey[] = [];
    selectedMode = "Creative";
    selectedModel = "";
    pluginDeleted = false;

    policySaveResponse = {
        result: "",
        error: "",
    };
    userUsageAndPolicyData: undefined | UserData[] = [];

    maxTokens: number | null = 1000;
    controlpanelError?: string;
    controlpanelResponse?: string;
    availableModels?: ChatModel[];
    availableModes?: string[];
    availableEmbedderProviders: string[] = [];

    useStreaming: boolean = false;
    modelTemperature: number | null = null;
    systemPromptPath: string = "";
    includeCitations: boolean = false;
    similarityThreshold: number | null = null;
    topKChunks: number = 3;

    globalPolicy: TokenLimitForUser = {
        token_cap_enabled: false,
        token_cap: 1000,
        time_window_enabled: false,
        window_unit: "h",
        window_value: 5,
        token_cap_for_window: 5000,
    };

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer
    ) {
        super(el, http, domSanitizer);
        this.hostElement = el;
    }
    private getModalDialog(): HTMLElement | null {
        const dialogFrame =
            this.hostElement.nativeElement.querySelector("tim-dialog-frame");
        if (!dialogFrame) {
            return null;
        }
        return dialogFrame.querySelector(".modal-dialog");
    }

    async ngAfterViewInit() {
        /* calling this.pluginMeta.getTaskIdUrl() too
         early crashes thus we call in ngAfterViewInit */
        this.initDocId();
        await this.initScrollContainer();
        // TODO: separate control panel data and client data fetch
        await this.getControlPanelData();
        await this.fetchRights();
        console.log(this.isTeacher);
    }
    async fetchRights(): Promise<void> {
        if (this.document_id <= 0) {
            return;
        }
        this.hasManageRights = itemglobals().curr_item.rights.manage;
        const user_id: number = Users.getCurrent().id;
        const response = await this.httpPost<{is_teacher: boolean}>(
            this.route("getRights"),
            {
                user_id: user_id,
                document_id: this.document_id,
            }
        );
        if (response.ok) {
            this.isTeacher = response.result.is_teacher;
        }
    }

    async onEnter(e: Event) {
        e.preventDefault();
        await this.sendUserInput();
    }

    onScroll = () => {
        const el = this.scrollContainer;
        if (!el) {
            return;
        }
        if (el.scrollTop <= this.scrollUpThreshold) {
            void this.loadOlderHistory();
        }
    };

    buttonText() {
        return super.buttonText() ?? $localize`Send`;
    }

    getDefaultMarkup() {
        return {};
    }

    async sendUserInput() {
        if (!this.canSendInput()) {
            return;
        }
        await this.doSendUserInput();
    }

    /* used to set the default window size from markup */
    get windowSize(): "sm" | "md" | "lg" | "xs" {
        return this.markup.defaultWindowSize ?? "md";
    }

    getAttributeType() {
        return PluginFields;
    }

    initDocId() {
        this.document_id = this.pluginMeta.getDocumentId() ?? -1;
    }

    get getCurrentFolder(): string {
        const it = itemglobals().curr_item;
        return it.isFolder ? it.path : it.location;
    }

    /* Initialize the scrollable chat box. */
    async initScrollContainer(): Promise<void> {
        this.scrollContainer = this.scrollFrame.nativeElement as HTMLElement;
        this.scrollContainer.addEventListener("scroll", this.onScroll, {
            passive: true,
        });

        await this.initConversation();
        await this.nextAnimationFrame();

        while (!this.isScrollable() && this.hasMoreHistory) {
            const beforeLen: number = this.conversation.length;
            await this.loadOlderHistory();
            await this.nextAnimationFrame();

            if (this.conversation.length === beforeLen) {
                break; // Fallback
            }
        }

        // Start pinned to bottom on reload
        requestAnimationFrame(() => {
            const el = this.scrollContainer;
            if (!el) {
                return;
            }
            el.scrollTo({top: el.scrollHeight, left: 0, behavior: "auto"});
        });
    }

    /* Fetch and initialize the last messages from the conversation. */
    async initConversation(): Promise<void> {
        if (this.document_id <= 0) {
            return;
        }

        const messages: Message[] = await this.fetchMessages(
            this.historyPageSize
        );
        this.hasMoreHistory = messages.length >= this.historyPageSize;
        this.conversation = this.messagesToEntries(messages);
    }

    /**
     * Fetch earlier conversation messages from the plugin server.
     * @param n Amount of messages to fetch.
     * @param ts_end Timestamp in milliseconds to fetch older messages from.
     */
    async fetchMessages(n: number, ts_end?: number): Promise<Message[]> {
        const url = this.route("getMessages");
        const document_id: number = this.document_id;

        const response = await this.httpPost<{messages: Message[]}>(url, {
            document_id: document_id,
            amount: n,
            timestamp_end_ms: ts_end,
        });
        if (response.ok && Array.isArray(response.result.messages)) {
            return response.result.messages;
        }
        return [];
    }

    /**
     * Convert the message list to the conversation chat entries.
     * @param messages Message list to convert.
     */
    messagesToEntries(messages: Message[]): ChatEntry[] {
        const out: ChatEntry[] = [];
        let current: ChatEntry | undefined;
        for (const m of messages) {
            if (m.role === "user") {
                current = {
                    user: m,
                    agent: {role: "assistant", content: ""},
                };
                out.push(current);
                continue;
            }

            if (!current) {
                current = {
                    user: {role: "user", content: ""},
                    agent: m,
                };
                out.push(current);
                continue;
            }
            current.agent = m;
            current = undefined;
        }
        return out;
    }

    /** Load older messages in the conversation and prepend them. */
    async loadOlderHistory(): Promise<void> {
        const el = this.scrollContainer;
        if (
            !el ||
            this.document_id <= 0 ||
            this.loadingOlder ||
            !this.hasMoreHistory
        ) {
            return;
        }

        this.loadingOlder = true;
        const prevScrollHeight = el.scrollHeight;
        const prevScrollTop = el.scrollTop;
        const oldestTs = this.getOldestMessageTs();

        const messages: Message[] = await this.fetchMessages(
            this.historyPageSize,
            oldestTs - 1 // Don't include the one we have
        );
        if (messages.length < this.historyPageSize) {
            this.hasMoreHistory = false;
        }
        if (messages.length === 0) {
            this.loadingOlder = false;
            return;
        }

        const olderEntries: ChatEntry[] = this.messagesToEntries(messages);
        const existing: ChatEntry[] = this.conversation;

        // Prepend the entries
        if (existing.length && olderEntries.length) {
            const lastOlder: ChatEntry = olderEntries[olderEntries.length - 1];
            const firstExisting: ChatEntry = existing[0];
            // Merge the user and assistant messages if boundary is split
            if (
                lastOlder.agent.content === "" &&
                firstExisting.user.content === "" &&
                firstExisting.agent.content !== ""
            ) {
                lastOlder.agent = firstExisting.agent;
                this.conversation = olderEntries.concat(existing.slice(1));
            } else {
                this.conversation = olderEntries.concat(existing);
            }
        } else {
            this.conversation = olderEntries.concat(existing);
        }

        await this.nextAnimationFrame(() => {
            const delta: number = el.scrollHeight - prevScrollHeight;
            el.scrollTop = prevScrollTop + delta;
            this.loadingOlder = false;
        });
    }

    canSendInput(): boolean {
        const trimmed: string = this.userInput.trim();
        return !this.isRunning && trimmed != "";
    }

    async doSendUserInput(): Promise<void> {
        this.isRunning = true;
        this.error = undefined;
        this.answer = undefined;

        const input: string = this.userInput;
        this.userInput = "";
        const document_id: number = this.document_id;
        const body: AskParams = {input, document_id};

        const entry: ChatEntry = {
            user: {content: input, role: "user", timestamp_ms: Date.now()},
            agent: {content: "", role: "assistant"},
        };
        const len: number = this.conversation.push(entry);
        const index: number = len - 1;
        this.scheduleAutoScroll(true);

        if (this.useStreaming) {
            await this.askPostStream(body, index);
        } else {
            await this.askPost(body, index);
        }
        this.isRunning = false;
    }

    /**
     * Fetch an answer for the user input from the plugin server.
     * @param body The body to send with the post request.
     * @param entry_index The index of the chat entry.
     */
    async askPost(body: AskParams, entry_index: number): Promise<void> {
        const response = await this.httpPost<AskResponse>(
            this.route("ask"),
            Object.fromEntries(Object.entries(body))
        );

        if (response.ok) {
            const pinned = this.isNearBottom();
            const data: AskResponse = response.result;

            // Don't add new chat entry if no answer
            if (data.error) {
                this.error = data.error;
                this.conversation.splice(entry_index, 1);
                return;
            }

            this.answer = data.answer;
            const message: Message = this.conversation[entry_index].agent;
            message.content = this.answer ?? "";
            message.timestamp_ms = Date.now();
            message.citations = data.citations;
            this.scheduleAutoScroll(false, pinned);
        } else {
            this.handleError(response.result.error.error, "http");
        }
    }

    /**
     * Fetch an answer for the user input from the plugin server. Uses streaming.
     * @param body The body to send with the post request.
     * @param entry_index The index of the chat entry.
     */
    async askPostStream(body: AskParams, entry_index: number): Promise<void> {
        const url: string = this.route("askStream");
        const observable = this.http.post(url, body, {
            observe: "events",
            responseType: "text",
            reportProgress: true,
        });

        const entry: ChatEntry = this.conversation[entry_index];
        let buffer: string = "";
        let processedIdx: number = 0; // Index in the buffer

        // Function to handle the stream events
        const handleNextEvent = (event: HttpEvent<string>): void => {
            if (event.type != HttpEventType.DownloadProgress) {
                return;
            }
            let didAppend = false;
            const pinned = this.isNearBottom();
            const partial: string =
                (event as HttpDownloadProgressEvent).partialText ?? "";

            const chunk: string = partial.slice(buffer.length);
            buffer += chunk;

            // Drain the response
            while (true) {
                const remaining: string = buffer.slice(processedIdx);
                const idx: number = remaining.indexOf("\n");
                if (idx < 0) {
                    break;
                }
                const json_str: string = remaining.slice(0, idx);

                const res = this.tryParseAskResponse(json_str);
                if (!res) {
                    break;
                }
                // Don't add new chat entry if no answer
                if (res.error) {
                    this.error = res.error;
                    this.conversation.splice(entry_index, 1);
                    break;
                }

                if (res.citations) {
                    entry.agent.citations = res.citations;
                }

                entry.agent.content += res.answer ?? "";
                processedIdx += idx + 1;
                didAppend = true;
                this.handleError(res.error, "server");
            }
            if (didAppend) {
                this.scheduleAutoScroll(false, pinned);
            }
        };

        return new Promise((resolve, reject): void => {
            const sub = observable.subscribe({
                next: (event: HttpEvent<string>) => handleNextEvent(event),
                error: (err) => {
                    this.handleError(err, "stream");
                    sub.unsubscribe();
                    reject();
                },
                complete: () => {
                    entry.agent.timestamp_ms = Date.now();
                    sub.unsubscribe();
                    resolve();
                },
            });
        });
    }

    /**
     * To get the stored/default instance values for control panel. Gets also
     * supported models and modes
     */
    async getControlPanelData() {
        this.isRunning = true;

        const getRequest = {
            document_id: this.document_id,
        };
        // Note: this is POST due to GET not being able to transfer int
        const response = await this.httpPost<{
            result: ControlPanelData;
            error?: string;
        }>(this.route("getSettings"), getRequest);
        this.isRunning = false;
        this.handleControlPanelResponse(response);
    }

    /**
     * To save the settings given by user
     * @param controlPanelSettings holds the values set by user
     */
    async onSaveSettings(controlPanelSettings: ControlPanelSettings) {
        this.isRunning = true;

        const save_request = {
            document_id: this.document_id,
            control_panel_settings: controlPanelSettings,
        };

        const response = await this.httpPost<{
            result: ControlPanelData;
            error?: string;
        }>(this.route("saveSettings"), save_request);
        this.isRunning = false;
        this.handleControlPanelResponse(response);
    }

    handleControlPanelResponse(
        response: Result<
            {result: ControlPanelData; error?: string},
            AngularError
        >
    ) {
        if (response.ok) {
            const data = response.result;
            const result = data.result;
            this.controlpanelError = data.error;

            if (
                this.controlpanelError === undefined ||
                this.controlpanelError === ""
            ) {
                this.error = undefined;
                this.setControlPanelData(result);
            }
        } else {
            this.controlpanelError = response.result.error.error;
        }

        if (this.controlpanelError) {
            const el = this.scrollContainer;
            if (!el) {
                return;
            }
            requestAnimationFrame(() => {
                el.scrollTo({
                    top: el.scrollHeight,
                    behavior: "smooth",
                });
            });
        }
    }

    private setControlPanelData(data: ControlPanelData) {
        this.selectedModel = data.selectedModel ?? data.model_id;
        this.selectedMode = data.llm_mode;
        this.maxTokens = data.max_tokens;
        this.selectedItemPaths = data.tim_paths;
        this.pathRestrictions = {
            allowedPaths: data.allowedItemPaths,
            maxDepth: 1,
            selectable: "both",
            behavior: "disable",
        };
        this.availableModels = data.availableModels;
        this.availableEmbedderProviders = data.availableEmbedderProviders;
        this.availableModes = data.availableModes;
        this.availablePublicKeys = data.availableKeys;
        this.selectedPublicKey =
            data.availableKeys.find((k) => k.is_selected)?.public_key ?? "";

        this.globalPolicy = data.global_policy;
        this.useStreaming = data.use_streaming;
        this.modelTemperature = data.model_temperature;
        this.systemPromptPath = data.system_prompt_path;
        this.includeCitations = data.include_citations;
        this.similarityThreshold = data.similarity_threshold;
        this.topKChunks = data.top_k_chunks;
    }

    async onClearConversation(): Promise<void> {
        if (this.isRunning || this.document_id <= 0) {
            return;
        }
        if (this.conversation.length == 0) {
            return;
        }
        this.isRunning = true;
        const response = await this.httpPost(this.route("clearMessages"), {
            document_id: this.document_id,
        });
        this.isRunning = false;

        if (response.ok) {
            this.conversation = [];
            this.hasMoreHistory = false;
            this.answer = undefined;
            this.error = undefined;
            return;
        }

        this.handleError(response.result.error.error, "http");
    }

    async onDeletePlugin(): Promise<void> {
        if (this.isRunning || this.document_id <= 0) {
            return;
        }
        this.isRunning = true;
        const response = await this.httpPost(this.route("deletePlugin"), {
            document_id: this.document_id,
        });

        this.isRunning = false;

        if (response.ok) {
            this.pluginDeleted = true;
            this.answer = undefined;
            this.error = undefined;
            return;
        }

        this.handleError(response.result.error.error, "http");
    }

    /**
     * Try to parse a `AskResponse` from a string.
     * @param data The string to parse.
     * @returns AskResponse if valid or undefined.
     */
    tryParseAskResponse(data: string): AskResponse | undefined {
        const trimmed = data.trim();
        if (!trimmed) {
            return undefined;
        }
        try {
            return JSON.parse(trimmed);
        } catch {
            return undefined;
        }
    }

    /**
     * Create a full URL.
     * @param endpoint The endpoint to append to the base URL.
     * @returns The full URL for the given endpoint.
     */
    route(endpoint: string): string {
        return "/chattim/" + endpoint;
    }

    /**
     * Handle the error if needed.
     * @param err The error.
     * @param scope Optional scope of the error.
     */
    handleError(err: string | undefined, scope?: string) {
        if (!err) {
            return;
        }
        this.error = err;
        if (scope) {
            console.error(`error(${scope}):`, err);
            return;
        }
        console.error(err);
    }

    /**
     * Determine if the chat box scroll position is near the end.
     * @param threshold Threshold for being near the end.
     */
    private isNearBottom(threshold = 150): boolean {
        const el = this.scrollContainer;
        if (!el) {
            return false;
        }
        const position: number = el.scrollTop + el.clientHeight;
        return position >= el.scrollHeight - threshold;
    }

    private getOldestMessageTs(): number {
        if (this.conversation.length === 0) {
            return Date.now();
        }
        const oldest: ChatEntry = this.conversation[0];
        return oldest.user.timestamp_ms ?? oldest.agent.timestamp_ms ?? 0;
    }

    private isScrollable(): boolean {
        const el = this.scrollContainer;
        return el ? el.scrollHeight > el.clientHeight + 1 : false;
    }

    /**
     * Wait for the next animation frame.
     * @param callback Optional function to call when it's time to update.
     */
    private nextAnimationFrame(callback?: FrameRequestCallback): Promise<void> {
        return new Promise((resolve, reject) => {
            requestAnimationFrame((ts) => {
                try {
                    callback?.(ts);
                    resolve();
                } catch (e) {
                    reject(e);
                }
            });
        });
    }

    /**
     * Auto scroll the chat box when new content is appended.
     * @param forceNewEntry Force scroll to the bottom.
     * @param pinnedBeforeUpdate Is the scroll position at the end already.
     */
    private scheduleAutoScroll(
        forceNewEntry = false,
        pinnedBeforeUpdate?: boolean
    ) {
        const el = this.scrollContainer;
        if (!el) {
            return;
        }
        if (this.controlPanelOpen) {
            return;
        }

        const shouldScroll = forceNewEntry
            ? true
            : pinnedBeforeUpdate ?? this.isNearBottom();
        if (!shouldScroll) {
            return;
        }

        this.scrollWanted = true;
        if (this.scrollScheduled) {
            return;
        }
        this.scrollScheduled = true;

        const behavior = forceNewEntry ? "smooth" : "auto";
        requestAnimationFrame(() => {
            this.scrollScheduled = false;
            if (!this.scrollWanted || !this.scrollContainer) {
                this.scrollWanted = false;
                return;
            }
            this.scrollWanted = false;
            this.scrollContainer.scrollTo({
                top: this.scrollContainer.scrollHeight,
                left: 0,
                behavior,
            });
        });
    }

    /**
     * Return the date string of the timestamp or empty string if undefined.
     * @param ts Timestamp in milliseconds.
     */
    dateString(ts: number | undefined): string {
        return ts ? new Date(ts).toLocaleString() : "";
    }

    async onFetchModels(publicKey: string) {
        this.isRunning = true;
        this.controlpanelError = undefined;

        const response = await this.httpPost<{
            models: ChatModel[];
            error?: string;
        }>(this.route("getModels"), {public_key: publicKey});

        this.isRunning = false;
        if (response.ok) {
            const data = response.result;
            if (data.error) {
                this.controlpanelError = data.error;
                return;
            }
            this.availableModels = data.models;
            if (this.availableModels.length > 0) {
                this.controlPanel.selectFirstFilteredModel();
            } else {
                this.controlpanelError = "No models found for this API key.";
            }
        } else {
            this.controlpanelError = response.result.error.error;
        }
    }

    async handleUserPolicySave(userData: UserData) {
        this.isRunning = true;
        const save_request = {
            document_id: this.document_id,
            user_data: userData,
        };
        const response = await this.httpPost<{
            result: string;
            error: string;
        }>(this.route("saveUserPolicy"), save_request);
        this.isRunning = false;
        if (response.ok) {
            const data = response.result;
            this.policySaveResponse = {
                error: data.error,
                result: data.result,
            };
        } else {
            this.controlpanelError = response.result.error.error;
        }
    }

    async getUserData() {
        this.isRunning = true;
        const data_request = {
            document_id: this.document_id,
        };
        const response = await this.httpPost<{
            result: UserData[];
            error?: string;
        }>(this.route("getUserPolicyData"), data_request);
        this.isRunning = false;
        if (response.ok) {
            const data = response.result;
            this.controlpanelError = data.error;
            this.userUsageAndPolicyData = data.result;
        } else {
            this.controlpanelError = response.result.error.error;
        }
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
        await this.onClearConversation();
    }
}

@NgModule({
    declarations: [ChatTIMComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        TimUtilityModule,
        PurifyModule,
        DialogModule,
        FormsModule,
        ChatControlPanelComponent,
        DirectoryPickerComponent,
        TooltipModule,
    ],
})
export class ChatTIMModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("chattim-runner", ChatTIMModule, ChatTIMComponent);
