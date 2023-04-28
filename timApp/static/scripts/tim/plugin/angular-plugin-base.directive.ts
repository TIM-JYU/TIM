import type {AfterContentInit, OnInit} from "@angular/core";
import {Directive, ElementRef, Input} from "@angular/core";
import type {
    IGenericPluginMarkup,
    IGenericPluginTopLevelFields,
} from "tim/plugin/attributes";
import type {Type} from "io-ts";
import {HttpClient, HttpHeaders} from "@angular/common/http";
import type {AngularError, Failure} from "tim/util/utils";
import {toPromise} from "tim/util/utils";
import type {PluginMarkupErrors} from "tim/plugin/util";
import {getDefaults, PluginBaseCommon, PluginMeta} from "tim/plugin/util";
import {DomSanitizer} from "@angular/platform-browser";
import type {JsonValue} from "tim/util/jsonvalue";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {
    handleAnswerResponse,
    prepareAnswerRequest,
} from "tim/document/interceptor";
import type {IAnswerSaveEvent} from "tim/answer/answer-browser.component";
import {isLeft} from "fp-ts/Either";
import {getErrors} from "tim/plugin/errors";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import type {ChangeType, ViewCtrl} from "tim/document/viewctrl";

/**
 * Plugin with initialization data passed from the server via JSON.
 */
export interface PluginJson {
    /**
     * The plugin's initialization data as JSON.
     */
    json: string;
}

/**
 * Base class for all Angular plugin components.
 */
@Directive()
export abstract class AngularPluginBase<
        MarkupType extends IGenericPluginMarkup,
        A extends IGenericPluginTopLevelFields<MarkupType>,
        T extends Type<A, unknown>
    >
    extends PluginBaseCommon
    implements AfterContentInit, OnInit, PluginJson
{
    attrsall: Readonly<A>;
    @Input() readonly json!: string;
    @Input() readonly plugintype?: string;
    @Input() readonly taskid?: string;
    protected vctrl!: ViewCtrl;
    protected requiresTaskId = true;

    markupError?: PluginMarkupErrors;
    protected pluginMeta: PluginMeta;

    buttonText() {
        return this.markup.button ?? this.markup.buttonText;
    }

    get undoButton() {
        return this.markup.undo?.button;
    }

    get undoTitle() {
        return this.markup.undo?.title;
    }

    get undoConfirmationTitle() {
        return this.markup.undo?.confirmationTitle;
    }

    get undoConfirmation() {
        return this.markup.undo?.confirmation;
    }

    get allowUnsavedLeave() {
        return this.markup.allowUnsavedLeave ?? false;
    }

    get disableUnchanged() {
        return this.markup.disableUnchanged;
    }

    get markup(): Readonly<MarkupType> {
        return this.attrsall.markup;
    }

    get footer() {
        return this.markup.footer;
    }

    get header() {
        return this.markup.header;
    }

    get stem() {
        return this.markup.stem;
    }

    get readonly(): boolean {
        if (this.markup.readonly !== undefined) {
            return this.markup.readonly;
        }
        return this.attrsall.access === "readonly";
    }

    constructor(
        private el: ElementRef<HTMLElement>,
        protected http: HttpClient,
        protected domSanitizer: DomSanitizer
    ) {
        super();
        this.attrsall = getDefaults(
            this.getAttributeType(),
            this.getDefaultMarkup()
        );
        this.pluginMeta = new PluginMeta(
            $(el.nativeElement),
            this.attrsall.preview
        );
    }

    get element() {
        return $(this.el.nativeElement);
    }

    ngOnInit() {
        const parsed = JSON.parse(atob(this.json)) as unknown;
        const validated = this.getAttributeType().decode(parsed);

        if (isLeft(validated)) {
            this.markupError = getErrors(validated);
        } else {
            this.attrsall = validated.right;
        }

        if (this.attrsall) {
            this.pluginMeta = new PluginMeta(
                this.element,
                this.attrsall.preview,
                this.plugintype,
                this.taskid
            );
        }
        this.vctrl = vctrlInstance!;
    }

    ngAfterContentInit() {
        if (this.requiresTaskId && !this.taskid) {
            // TODO: generic error element in all plugin templates
            const parId = this.getRootElement()
                .closest(".par")
                ?.getAttribute("id");
            if (parId) {
                const ldr = this.vctrl.getPluginLoaderWithoutTaskid(parId);
                if (ldr) {
                    ldr.warnAboutMissingTaskId();
                }
            }
        }
    }

    async tryResetChanges(e?: Event) {
        if (e) {
            e.preventDefault();
        }
        if (this.undoConfirmation) {
            if (
                !(await showConfirm(
                    this.undoConfirmationTitle ?? this.undoConfirmation,
                    this.undoConfirmation
                ))
            ) {
                return;
            }
        }
        this.resetChanges();
    }

    updateListeners(state: ChangeType) {
        if (!this.vctrl) {
            return;
        }
        const taskId = this.pluginMeta.getTaskId();
        if (!taskId) {
            return;
        }
        this.vctrl.informChangeListeners(
            taskId,
            state,
            this.attrsall.markup.tag ? this.attrsall.markup.tag : undefined
        );
    }

    protected httpGet<U>(
        url: string,
        params?: Record<string, string | string[]>
    ) {
        return toPromise(this.http.get<U>(url, {params}));
    }

    protected httpGetText(
        url: string,
        params?: Record<string, string | string[]>
    ) {
        return toPromise(
            this.http.get(url, {params: params, responseType: "text"})
        );
    }

    protected httpPost<U>(url: string, body: JsonValue) {
        return toPromise(this.http.post<U>(url, body));
    }

    protected httpPut<U>(
        url: string,
        body: JsonValue,
        headers: HttpHeaders = new HttpHeaders()
    ) {
        return toPromise(this.http.put<U>(url, body, {headers: headers}));
    }

    protected async postAnswer<U>(
        answerdata: {input: JsonValue; autosave?: boolean},
        headers: HttpHeaders = new HttpHeaders()
    ) {
        const tid = this.pluginMeta.getTaskId();
        if (!tid) {
            return {
                ok: false,
                result: {
                    error: {
                        error: $localize`Task id missing and required to answer this task.`,
                    },
                },
            } as Failure<AngularError>;
        }
        const dt = tid.docTaskIdFull();
        const {url, data} = prepareAnswerRequest(
            dt,
            this.pluginMeta.getAnswerUrl()
        );
        const finaldata = {
            ...answerdata,
            ...data,
        };
        const result = await this.httpPut<U & IAnswerSaveEvent>(
            url,
            finaldata,
            headers
        );
        if (result.ok) {
            handleAnswerResponse(dt, {
                savedNew: result.result.savedNew,
                errors: result.result.errors,
                feedback: result.result.feedback,
                topfeedback: result.result.topfeedback,
                valid: result.result.valid,
                refresh: result.result.refresh,
                refreshPoints: result.result.refreshPoints,
            });
        } else {
            handleAnswerResponse(dt, {
                savedNew: false,
                valid: false,
                errors: [result.result.error.error],
            });
        }
        return result;
    }

    abstract getAttributeType(): T;

    abstract getDefaultMarkup(): Partial<MarkupType>;
}
