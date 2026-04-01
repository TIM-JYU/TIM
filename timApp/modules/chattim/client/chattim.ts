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
import {DomSanitizer} from "@angular/platform-browser";
import {Users} from "tim/user/userService";
import type {CtrlPanelData} from "./controlpanel";
import {ChatControlPanelComponent} from "./controlpanel";
import {
    HttpClient,
    HttpClientModule,
    HttpDownloadProgressEvent,
    HttpEvent,
    HttpEventType,
} from "@angular/common/http";

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

export interface ChatEntry {
    user: string;
    agent: string;
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
    template: `
        <tim-dialog-frame class="chattim-dialog-frame" [size]="'md'">
            <ng-container body>
                <div class="scroll-box">
                    <div *ngFor="let entry of conversation">
                        <div class="chat-user">{{ entry.user }}</div>
                        <div class="chat-bot" [innerHTML]="entry.agent | purify"></div>
                    </div>
                </div>



                <div class="form-inline">
                    <label>{{inputStem}}
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
                        [selectedModel]="selectedModel"
                        [selectedMode]="selectedMode"
                        [maxTokens]="maxTokens"
                        [response]="controlpanelResponse"
                        [error]="controlpanelError">
                    </chattim-control-panel>
                </div>

                <tim-loading *ngIf="isRunning"></tim-loading>
                <div *ngIf="error" [innerHTML]="error | purify"></div>
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
    answer?: string;
    error?: string;
    isRunning: boolean = false;
    userInput = "";
    inputStem = "";
    document_id = -1;

    // TODO: fetch default values from server?
    selectedModel = "gpt-4.1-mini";
    selectedMode = "Summarizing";
    maxTokens = 1000;
    controlpanelError?: string;
    controlpanelResponse?: string;
    useStreaming: boolean = false;

    conversation: ChatEntry[] = [];

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer
    ) {
        super(el, http, domSanitizer);
    }

    ngAfterViewInit() {
        /* calling this.pluginMeta.getTaskIdUrl() too
         early crashes thus we call in ngAfterViewInit */
        this.initDocId();
    }

    async onEnter() {
        await this.sendUserInput();
    }

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
        this.userInput = "";
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

    canSendInput(): boolean {
        const trimmed: string = this.userInput.trim();
        return !this.isRunning && trimmed != "";
    }

    async doSendUserInput(): Promise<void> {
        this.isRunning = true;
        this.answer = undefined;

        const input: string = this.userInput;
        // TODO: ei tarvita user id?
        const user_id: number = Users.getCurrent().id;
        const document_id: number = this.document_id;
        const body: AskParams = {input, user_id, document_id};

        const entry: ChatEntry = {user: input, agent: ""};
        const len: number = this.conversation.push(entry);
        const index: number = len - 1;

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
    async askPost(body: any, entry_index: number): Promise<void> {
        const response = await this.httpPost<AskResponse>(
            this.route("ask"),
            body
        );

        if (response.ok) {
            const data = response.result;
            this.handleError(data.error, "server");
            this.answer = data.answer;
            this.conversation[entry_index].agent = this.answer ?? "";
            // TODO: For testing purposes
            if (data.usage != undefined) {
                this.conversation[entry_index].agent +=
                    "\nTokens used: " + data.usage ?? "";
            }
        } else {
            this.handleError(response.result.error.error, "http");
        }
    }

    async onSaveSettings(ctrlpanel_data: CtrlPanelData) {
        this.isRunning = true;

        // TODO: ei tarvita user id?
        const user_id: number = Users.getCurrent().id;

        const save_request = {
            user_id: user_id,
            document_id: this.document_id,
            cpanel_data: ctrlpanel_data,
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

        let entry: ChatEntry = this.conversation[entry_index];
        let buffer: string = "";
        let processedIdx: number = 0; // Index in the buffer

        // Function to handle the stream events
        const handleNextEvent = (event: HttpEvent<string>): void => {
            if (event.type != HttpEventType.DownloadProgress) return;
            const partial: string =
                (event as HttpDownloadProgressEvent).partialText ?? "";

            const chunk: string = partial.slice(buffer.length);
            buffer += chunk;

            // Drain the response
            while (true) {
                const remaining: string = buffer.slice(processedIdx);
                const idx: number = remaining.indexOf("\n");
                if (idx < 0) break;
                const nd_json: string = remaining.slice(0, idx);

                const res = this.tryParseAskResponse(nd_json);
                if (!res) break;
                entry.agent += res.answer ?? "";
                processedIdx += idx + 1;
                // TODO: For dev purposes. Handle tokens somehow else
                if (res.usage) {
                    entry.agent += "\nTokens used: " + res.usage;
                }
                this.handleError(res.error, "server");
            }
        };

        return new Promise((resolve, reject): void => {
            let sub = observable.subscribe({
                next: (event: HttpEvent<string>) => handleNextEvent(event),
                error: (err) => {
                    this.handleError(err, "stream");
                    sub.unsubscribe();
                    reject();
                },
                complete: () => {
                    console.log("Answer completed");
                    sub.unsubscribe();
                    resolve();
                },
            });
        });
    }

    /**
     * Try to parse a `AskResponse` from a string.
     * @param data The string to parse.
     * @returns AskResponse if valid or undefined.
     */
    tryParseAskResponse(data: string): AskResponse | undefined {
        const trimmed = data.trim();
        if (!trimmed) return undefined;
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
    handleError(err: any, scope?: string) {
        if (!err) return;
        this.error = err;
        if (scope) console.error(`error(${scope}):`, err);
        else console.error(err);
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
