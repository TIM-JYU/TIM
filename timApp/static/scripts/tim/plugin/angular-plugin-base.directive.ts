import {Directive, ElementRef, Input, OnInit} from "@angular/core";
import {
    IGenericPluginMarkup,
    IGenericPluginTopLevelFields,
} from "tim/plugin/attributes";
import {Type} from "io-ts";
import {HttpClient, HttpHeaders} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
import {
    baseOnInit,
    getDefaults,
    PluginBaseCommon,
    PluginMarkupErrors,
    PluginMeta,
} from "tim/plugin/util";
import {DomSanitizer} from "@angular/platform-browser";
import {JsonValue} from "tim/util/jsonvalue";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {
    handleAnswerResponse,
    prepareAnswerRequest,
} from "../document/interceptor";
import {IAnswerSaveEvent} from "../answer/answerbrowser3";

export interface PluginJson {
    json: string;
}

@Directive()
export abstract class AngularPluginBase<
        MarkupType extends IGenericPluginMarkup,
        A extends IGenericPluginTopLevelFields<MarkupType>,
        T extends Type<A, unknown>
    >
    extends PluginBaseCommon
    implements OnInit, PluginJson
{
    attrsall: Readonly<A>;
    @Input() readonly json!: string;
    @Input() readonly plugintype?: string;
    @Input() readonly taskid?: string;

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
        const result = baseOnInit(this);
        if (result) {
            this.pluginMeta = new PluginMeta(
                this.element,
                result.preview,
                this.plugintype,
                this.taskid
            );
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
        answerdata: {input: JsonValue},
        headers: HttpHeaders = new HttpHeaders()
    ) {
        const tid = this.pluginMeta.getTaskId();
        if (!tid) {
            throw Error("Task id missing.");
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
                error: result.result.error,
                feedback: result.result.feedback,
                topfeedback: result.result.topfeedback,
                valid: result.result.valid,
            });
        } else {
            handleAnswerResponse(dt, {
                savedNew: false,
                valid: false,
                error: result.result.error.error,
            });
        }
        return result;
    }

    abstract getAttributeType(): T;

    abstract getDefaultMarkup(): Partial<MarkupType>;
}
