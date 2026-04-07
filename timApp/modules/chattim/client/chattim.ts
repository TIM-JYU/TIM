/**
 * Defines the client-side implementation of an example plugin (a chattimndrome checker).
 */
import * as t from "io-ts";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
} from "tim/plugin/attributes";
import type {
    AfterViewInit,
    ApplicationRef,
    DoBootstrap,
    OnInit,
} from "@angular/core";
import {
    Component,
    NgModule,
    ElementRef,
    ViewEncapsulation,
} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {PurifyModule} from "tim/util/purify.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {DomSanitizer} from "@angular/platform-browser";
import {Users} from "tim/user/userService";
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

export interface ChatEntry {
    user: string;
    agent: string;
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
                            <div class="chat-user" >{{ entry.user }}</div>
                            <div class="chat-bot"  [innerHTML]="entry.agent | purify"></div>
                        </div>  
                    </div>
                

                
                    <div class="form-inline">
                        <label>{{inputstem}}
                            <input type="text"
                                   class="form-control"
                                   [(ngModel)]="userinput"
                                   (keyup.enter)="onEnter()"    
                            >
                        </label>
                        <button class="timButton"
                                *ngIf="buttonText()"
                                [disabled]="isRunning || !userinput"
                                (click)="sendUserInput()"
                                [innerHTML]="buttonText() | purify">
                        </button>
                            <chattim-control-panel
                    [(selectedModel)]="selectedModel"
                    [(temperature)]="temperature"
                    [(maxTokens)]="maxTokens">
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
    isRunning = false;
    userinput = "";
    inputstem = "";
    document_id = -1;

    selectedModel = "gpt-4o";
    temperature = 0.7;
    maxTokens = 1000;

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

    onEnter() {
        this.sendUserInput();
    }

    buttonText() {
        return super.buttonText() ?? "Send";
    }

    getDefaultMarkup() {
        return {};
    }

    async sendUserInput() {
        if (!this.userinput?.trim() || this.isRunning) {
            return;
        }
        await this.doSendUserInput();
        this.userinput = "";
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

    async doSendUserInput() {
        this.isRunning = true;
        this.answer = undefined;

        const input: string = this.userinput;
        const user_id: string = String(Users.getCurrent().id);
        const document_id: number = this.document_id;

        const response = await this.httpPost<{
            web: {result: string; error?: string};
        }>("/chattim/ask", {
            input,
            user_id,
            document_id,
        });

        this.isRunning = false;
        if (response.ok) {
            const data = response.result;
            this.error = data.web.error;
            this.answer = data.web.result;
            this.conversation.push({
                user: this.userinput,
                agent: this.answer,
            });
        } else {
            this.error = response.result.error.error;
        }
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
