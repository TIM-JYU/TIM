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
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
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
import {Users} from "tim/user/userService";
import type {ControlPanelData} from "./controlpanel";
import {ChatControlPanelComponent} from "./controlpanel";

const PluginMarkupFields = t.intersection([
    t.partial({
        // ei tarvita mitään ainakaan toistaiseksi
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
}

export interface ChatEntry {
    user: Message;
    agent: Message;
}

export interface AskResponse {
    answer?: string;
    usage?: number;
    error?: string;
}

export interface AskParams {
    input: string;
    user_id: number;
    document_id: number;
}

// Huom: <tim-dialog-frame ei sisällä markupError attribuuttia
// joten täytyy joka tehdä oma versio tai muuten markupErroria ei nähdä
@Component({
    selector: "chattim-runner",
    encapsulation: ViewEncapsulation.None,
    // TODO: Display message datetime from the timestamp with `dateString()`
    template: `
        <tim-dialog-frame class="chattim-dialog-frame" [size]="'md'">
            <ng-container body>
                <div class="scroll-box" #conversationScroll>
                    <div *ngFor="let entry of conversation">
                        <div class="chat-user">{{ entry.user.content }}</div>
                        <div class="chat-bot" [innerHTML]="entry.agent.content | purify"></div>
                    </div>
                </div>

                <div>
                    <tim-loading *ngIf="isRunning"></tim-loading>
                    <div *ngIf="error" [innerHTML]="error | purify"></div>
                </div>

                <div class="form-inline">
                    <label>{{ inputStem }}
                        <input type="text"
                               class="form-control"
                               [(ngModel)]="userInput"
                               (keyup.enter)="onEnter()"
                        >
                    </label>
                    <button class="timButton"
                            *ngIf="buttonText()"
                            [disabled]="!canSendInput()"
                            (click)="sendUserInput()"
                            [innerHTML]="buttonText() | purify">
                    </button>
                    <chattim-control-panel
                        (saveSettingsClick)="onSaveSettings($event)"
                        (initControlPanelDataFetch)="getControlPanelData()"
                        [selectedModel]="selectedModel"
                        [selectedMode]="selectedMode"
                        [maxTokens]="maxTokens"
                        [response]="controlpanelResponse"
                        [error]="controlpanelError"
                        [localFilePaths]="localFilePaths">
                    </chattim-control-panel>
                </div>
            </ng-container>
        </tim-dialog-frame>
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
    @ViewChild("conversationScroll", {static: false}) scrollFrame!: ElementRef;
    private scrollContainer?: HTMLElement;
    private scrollScheduled: boolean = false;
    private scrollWanted: boolean = false;

    private loadingOlder: boolean = false;
    private hasMoreHistory: boolean = true;
    private readonly historyPageSize: number = 20;
    private readonly scrollUpThreshold: number = 50;

    conversation: ChatEntry[] = [];

    answer?: string; // TODO: needed?
    error?: string;
    isRunning: boolean = false;
    userInput = "";
    inputStem = ""; // TODO: do something with this or remove?
    document_id = -1;

    localFilePaths = "";
    selectedMode = "Creative";
    selectedModel = "gpt-4.1-mini";
    maxTokens = 1000;
    controlpanelError?: string;
    controlpanelResponse?: string;

    // TODO: make a configurable option for user in settings?
    useStreaming: boolean = true;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer
    ) {
        super(el, http, domSanitizer);
    }

    async ngAfterViewInit() {
        /* calling this.pluginMeta.getTaskIdUrl() too
         early crashes thus we call in ngAfterViewInit */
        this.initDocId();
        await this.initScrollContainer();
    }

    async onEnter() {
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
        return super.buttonText() ?? "Send";
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

    getAttributeType() {
        return PluginFields;
    }

    /* Extracts the tim-document id from the taskidurl. */
    initDocId() {
        const task_id_url: string = String(this.pluginMeta.getTaskIdUrl());
        const id_str: string | undefined = task_id_url
            .split("/")
            .pop()
            ?.split(".")[0];

        this.document_id = Number(id_str);

        if (this.document_id === 0) {
            console.error(
                "Warning: could not parse document_id from task_id_url: ${task_id_url}"
            );

            this.document_id = -1;
        }
    }

    /* Initialize the scrollable chat box. */
    async initScrollContainer(): Promise<void> {
        this.scrollContainer = this.scrollFrame.nativeElement as HTMLElement;
        this.scrollContainer.addEventListener("scroll", this.onScroll, {
            passive: true,
        });

        await this.initConversation();

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
        const user_id: number = Users.getCurrent().id;
        const document_id: number = this.document_id;

        const response = await this.httpPost<{messages: Message[]}>(url, {
            user_id: user_id,
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

        requestAnimationFrame(() => {
            const delta = el.scrollHeight - prevScrollHeight;
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
        this.answer = undefined;

        const input: string = this.userInput;
        this.userInput = "";
        // TODO: ei tarvita user id?
        const user_id: number = Users.getCurrent().id;
        const document_id: number = this.document_id;
        const body: AskParams = {input, user_id, document_id};

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
            // TODO: For testing purposes
            if (data.usage != undefined) {
                this.conversation[entry_index].agent.content +=
                    "\nTokens used: " + data.usage ?? "";
            }
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

                entry.agent.content += res.answer ?? "";
                processedIdx += idx + 1;
                didAppend = true;
                // TODO: For dev purposes. Handle tokens somehow else
                if (res.usage) {
                    entry.agent.content += "\nTokens used: " + res.usage;
                }
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
                    console.log("Answer completed");
                    entry.agent.timestamp_ms = Date.now();
                    sub.unsubscribe();
                    resolve();
                },
            });
        });
    }

    async getControlPanelData() {
        this.isRunning = true;

        const getRequest = {
            document_id: this.document_id,
        };

        const response = await this.httpPost<{
            web: {result: ControlPanelData; error?: string};
        }>("/chattim/get_controlpanel_data", getRequest);

        this.isRunning = false;
        if (response.ok) {
            const data = response.result;
            this.controlpanelError = data.web.error;
            const result = data.web.result;
            if (this.controlpanelError === "") {
                this.selectedModel = result.model_id;
                this.selectedMode = result.llm_mode;
                this.maxTokens = result.max_tokens;
                this.localFilePaths = result.tim_paths;
            }
        } else {
            this.controlpanelError = response.result.error.error;
        }
    }

    async onSaveSettings(controlPanelData: ControlPanelData) {
        this.isRunning = true;

        const save_request = {
            document_id: this.document_id,
            control_panel_data: controlPanelData,
        };

        const response = await this.httpPost<{
            web: {result: string; error?: string};
        }>("/chattim/settings_save", save_request);

        this.isRunning = false;
        if (response.ok) {
            const data = response.result;
            this.controlpanelError = data.web.error;
            this.controlpanelResponse = data.web.result;
        } else {
            this.controlpanelError = response.result.error.error;
        }
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
}

@NgModule({
    declarations: [ChatTIMComponent, ChatControlPanelComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        PurifyModule,
        DialogModule,
    ],
})
export class ChatTIMModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("chattim-runner", ChatTIMModule, ChatTIMComponent);
