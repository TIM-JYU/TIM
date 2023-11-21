/**
 * Defines the client-side implementation of a drag plugin.
 */
import * as t from "io-ts";
import {polyfill} from "mobile-drag-drop";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import type {DndDropEvent, EffectAllowed} from "ngx-drag-drop";
import {DndModule} from "ngx-drag-drop";
import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {
    Component,
    NgModule,
    ChangeDetectorRef,
    ElementRef,
} from "@angular/core";
import {scrollBehaviourDragImageTranslateOverride} from "mobile-drag-drop/scroll-behaviour";
import {SessionVerify} from "tim/util/session-verify.interceptor";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import type {ITimComponent} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {parseStyles} from "tim/plugin/util";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import type {DocIdDotName} from "tim/plugin/taskid";
import {defaultErrorMessage, isIOS} from "tim/util/utils";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {shuffle} from "tim/plugin/util";
import {DomSanitizer} from "@angular/platform-browser";

const DragMarkup = t.intersection([
    t.partial({
        inputstem: t.string,
        words: t.array(t.string),
        copy: t.keyof({target: null, source: null}),
        type: t.string,
        max: t.refinement(t.number, (x) => x > 0, "a positive number value"),
        trash: t.boolean,
        savebutton: t.boolean,
        shuffle: t.boolean,
        followid: t.string,
        autoSave: withDefault(t.boolean, false),
        ignorestyles: t.boolean,
        clearstyles: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        // All withDefaults should come here; NOT in t.partial.
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const DragAll = t.intersection([
    t.partial({}),
    t.type({
        info: Info,
        markup: DragMarkup,
        preview: t.boolean,
        state: nullable(
            t.intersection([
                t.type({c: nullable(t.array(t.string))}),
                t.partial({
                    styles: nullable(t.record(t.string, t.string)),
                }),
            ])
        ),
    }),
]);

interface WordObject {
    id: number;
    word: string;
    type?: string;
    effectAllowed: EffectAllowed;
    taskId?: DocIdDotName;
}

@Component({
    selector: `drag-runner`,
    template: `
        <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
        <div class="form-inline">
            <div class="draggingarea">
                <ul *ngIf="trash"
                    class="dropword"
                    [dndDropzone]
                    [dndDraggable]="[]"
                    dndEffectAllowed="all">
                    <li>TRASHCAN</li>
                </ul>
                <ul *ngIf="!trash"
                    class="dropword"
                    [dndDropzone]="[type]"
                    [dndHorizontal]="true"
                    [dndDraggable]="wordObjs"
                    [dndDisableIf]="wordObjs.length >= max"
                    [dndEffectAllowed]="effectAllowed"
                    (dndDrop)="handleDrop($event)"
                    [ngStyle]="styles"
                    [tooltip]="error"
                    [isOpen]="error !== undefined"
                    [ngClass]="{alertFrame: saveFailed}"
                >
                    <li class="dndPlaceholder" dndPlaceholderRef></li>
                    <li *ngFor="let item of wordObjs; let i = index"
                        [innerHTML]="item.word | purify"
                        class="dragword"
                        [dndDraggable]="item"
                        [dndType]="item.type"
                        (dndMoved)="handleMove(i)"
                        (dndCanceled)="(copy !== 'target') || wordObjs.splice(i, 1)"
                        [dndEffectAllowed]="item.effectAllowed"
                        [ngStyle]="styles"
                    ></li>
                </ul>
                <button class="timButton"
                        *ngIf="!saveButton && saveFailed"
                        (click)="save()">
                    {{buttonText()}}
                </button>
            </div>
            <button class="timButton"
                    *ngIf="saveButton"
                    (click)="save()">
                {{buttonText()}}
            </button>
        </div>
        <div *ngIf="error && saveButton" class="error" style="font-size: 12px" [innerHtml]="error | purify"></div>
        <div *ngIf="footer" class="plgfooter" [textContent]="footer"></div>
    `,
    styleUrls: ["./drag.scss"],
})
export class DragComponent
    extends AngularPluginBase<
        t.TypeOf<typeof DragMarkup>,
        t.TypeOf<typeof DragAll>,
        typeof DragAll
    >
    implements OnInit, ITimComponent
{
    error?: string;
    trash?: boolean;
    saveButton?: boolean;
    wordObjs: WordObject[] = [];
    type: string = " ";
    max!: number;
    copy?: string;
    effectAllowed: EffectAllowed = "copyMove";
    private forceSave = false;
    private shuffle?: boolean;

    styles: Record<string, string> = {};
    saveFailed = false;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        private cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer);
    }

    getAttributeType(): typeof DragAll {
        return DragAll;
    }

    getDefaultMarkup(): Partial<t.TypeOf<typeof DragMarkup>> {
        return {};
    }

    buttonText() {
        return super.buttonText() ?? $localize`Save`;
    }

    ngOnInit() {
        super.ngOnInit();
        this.max = this.markup.max ?? 1e100;
        this.copy = this.markup.copy ?? "";
        this.type = this.markup.type ?? " ";
        this.trash = this.markup.trash ?? false;
        this.shuffle = this.markup.shuffle ?? false;
        this.saveButton = this.markup.savebutton ?? false;
        const words = this.attrsall.state?.c ?? this.markup.words ?? [];
        this.createWordobjs(this.shuffle ? shuffle(words) : words);

        polyfill({
            // Use this to make use of the scroll behaviour.
            dragImageTranslateOverride:
                scrollBehaviourDragImageTranslateOverride,
            forceApply: isIOS(),
        });

        window.addEventListener(
            "touchmove",
            () => {
                // This is a fix for ios problems arising in safari 10+.
                // This is also used for detecting touchscreen to change css layout.
                this.element.addClass("touchdrag");
            },
            {passive: false}
        );

        if (!this.attrsall.preview) {
            this.vctrl?.addTimComponent(this);
        }
        if (this.attrsall.state?.styles && !this.markup.ignorestyles) {
            this.styles = parseStyles(this.attrsall.state.styles);
        }
    }

    ngOnDestroy() {
        if (!this.attrsall.preview) {
            this.vctrl?.removeTimComponent(this);
        }
    }

    /**
     * Creates word-object-models that have attributes essential to the functionality of drag and drop actions.
     * In addition, initializes program logic.
     * @param words Array of strings to be transformed into draggable models.
     */
    createWordobjs(words: string[]) {
        if (!words) {
            return;
        }
        this.wordObjs = [];
        this.effectAllowed = "move";
        if (this.copy == "source" || this.copy == "target") {
            this.effectAllowed = "copy";
        }
        const taskId = this.pluginMeta.getTaskId()?.docTask();
        this.wordObjs = words.map((x, i) => ({
            effectAllowed: this.effectAllowed,
            id: i,
            type: this.type,
            word: x,
            taskId: taskId,
        }));
        if (this.copy == "source") {
            this.max = this.wordObjs.length;
        }
    }

    /**
     * Returns content separated by a comma or undefined if no content is contained.
     * @returns {string} Returns content as a string separated by comma.
     */
    getContent() {
        const cont = this.getContentArray().join(",");
        if (cont.length < 1) {
            return undefined;
        }
        return cont;
    }

    /**
     * Returns contained words as a string array.
     * @returns {string} Plugin content in a string array.
     */
    getContentArray(): string[] {
        const words: string[] = [];
        if (this.wordObjs) {
            this.wordObjs.map((el) => {
                words.push(el.word);
            });
        }
        return words;
    }

    /**
     * Force the plugin to save its information.
     *
     * @param force Whether to force a save.
     */
    setForceAnswerSave(force: boolean) {
        this.forceSave = force;
    }

    /**
     * Sets words in plugin.
     * @param words The words to be set as draggable words in plugin.
     */
    setPluginWords(words: string[]) {
        if (this.shuffle) {
            this.createWordobjs(shuffle(words));
        } else {
            this.createWordobjs(words);
        }
    }

    resetField(): undefined {
        this.setPluginWords(this.markup.words ?? []);
        this.error = undefined;
        return undefined;
    }

    handleMove(index: number) {
        this.wordObjs.splice(index, 1);
        if (this.markup.autoSave && !this.trash) {
            void this.save();
        }
    }

    handleDrop(event: DndDropEvent) {
        const taskId = this.pluginMeta.getTaskId()?.docTask();
        const word = event.data as WordObject;
        this.wordObjs.splice(event.index ?? 0, 0, word);
        if (
            this.markup.autoSave &&
            !this.trash &&
            (!word.taskId || word.taskId != taskId)
        ) {
            void this.save();
        }
        word.taskId = taskId;
        // NOTE: It seems that on mobile the list is updated at a different time from desktop
        //  To normalize the behavior, we force a change detection here.
        this.cdr.detectChanges();
    }

    async save() {
        return await this.doSave();
    }

    async doSave(nosave: boolean = false) {
        this.error = undefined;
        const params = {
            input: {
                nosave: nosave,
                words: this.getContentArray(),
            },
            options: {
                forceSave: this.forceSave,
            },
        };

        const r = await this.postAnswer<{
            web: {result: string; error?: string};
        }>(params);

        if (r.ok) {
            const data = r.result;
            this.saveFailed = false;
            this.error = data.web.error;
            if (this.markup.clearstyles) {
                this.styles = {};
            }
        } else {
            this.error =
                r.result.error.error ??
                this.attrsall.markup.connectionErrorMessage ??
                defaultErrorMessage;
            this.saveFailed = true;
        }
        return {saved: r.ok, message: this.error};
    }

    isUnSaved() {
        return false; // TODO
    }
}

@NgModule({
    providers: [SessionVerify],
    declarations: [DragComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        DndModule,
        TimUtilityModule,
        PurifyModule,
        FormsModule,
        TooltipModule.forRoot(),
    ],
})
export class DragModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

registerPlugin("drag-runner", DragModule, DragComponent);
