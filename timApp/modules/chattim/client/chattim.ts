/**
 * Defines the client-side implementation of an example plugin (a chattimndrome checker).
 */
import * as t from "io-ts";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
} from "tim/plugin/attributes";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {PurifyModule} from "tim/util/purify.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";

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

// Huom: <tim-dialog-frame ei sisällä markupError attribuuttia
// joten täytyy joka tehdä oma versio tai muuten markupErroria ei nähdä
@Component({
    selector: "chattim-runner",
    template: `
        <tim-dialog-frame>
            <tim-plugin-header *ngIf="header" header>
                <span [innerHTML]="header | purify"></span>
            </tim-plugin-header>

            <ng-container body>
                <pre *ngIf="answer" [innerHTML]="answer | purify"></pre>
                <div class="form-inline">
                    <label>{{inputstem}}
                        <input type="text"
                               class="form-control"
                               [(ngModel)]="userinput"
                               >
                    </label>
                </div>
            <div class="button">
                <button class="timButton"
                        *ngIf="buttonText()"
                        [disabled]="isRunning || !userinput"
                        (click)="sendUserInput()"
                        [innerHTML]="buttonText() | purify">
                </button>
            </div>
                <tim-loading *ngIf="isRunning"></tim-loading>
                <div *ngIf="error" [innerHTML]="error | purify"></div>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["./chattim.scss"],
})
export class ChatTIMComponent extends AngularPluginBase<
    t.TypeOf<typeof PluginMarkupFields>,
    t.TypeOf<typeof PluginFields>,
    typeof PluginFields
> {
    answer?: string;
    error?: string;
    isRunning = false;
    userinput = "";
    inputstem = "Chat";

    buttonText() {
        return super.buttonText() ?? "Send";
    }

    getDefaultMarkup() {
        return {};
    }

    sendUserInput() {
        this.answer = "hei";
        // ei nyt vielä lähetetä mitään ennen kuin serveripuoli ok
        // this.doSendUserInput();
    }

    getAttributeType() {
        return PluginFields;
    }

    async doSendUserInput() {
        this.isRunning = true;
        this.answer = undefined;
        const params = {
            input: {
                userinput: this.userinput,
            },
        };

        const r = await this.postAnswer<{
            web: {result: string; error?: string};
        }>(params);

        this.isRunning = false;
        if (r.ok) {
            const data = r.result;
            this.error = data.web.error;
            this.answer = data.web.result;
        } else {
            this.error = r.result.error.error;
        }
    }
}

@NgModule({
    declarations: [ChatTIMComponent],
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
