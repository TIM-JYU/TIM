import {Directive, ElementRef, Input, OnInit} from "@angular/core";
import {IGenericPluginMarkup, IGenericPluginTopLevelFields} from "tim/plugin/attributes";
import {Type} from "io-ts";
import {HttpClient} from "@angular/common/http";
import {to2} from "tim/util/utils";
import {baseOnInit, getDefaults, PluginBaseCommon, PluginMarkupErrors, PluginMeta} from "tim/plugin/util";
import {DomSanitizer} from "@angular/platform-browser";
import {handleAnswerResponse, prepareAnswerRequest} from "../document/interceptor";
import {IAnswerSaveEvent} from "../answer/answerbrowser3";

@Directive()
export abstract class AngularPluginBase<MarkupType extends IGenericPluginMarkup,
    A extends IGenericPluginTopLevelFields<MarkupType>,
    T extends Type<A>> extends PluginBaseCommon implements OnInit {
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

    get undoConfirmation() {
        return this.markup.undo?.confirmation;
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
        return this.attrsall.access === "readonly";
    }

    constructor(private el: ElementRef<HTMLElement>, private http: HttpClient, protected domSanitizer: DomSanitizer) {
        super();
        this.attrsall = getDefaults(this.getAttributeType(), this.getDefaultMarkup());
        this.pluginMeta = new PluginMeta($(el.nativeElement), this.attrsall.preview);
    }

    get element() {
        return $(this.el.nativeElement);
    }

    ngOnInit() {
        const result = baseOnInit.call(this);
        if (result) {
            this.pluginMeta = new PluginMeta(
                this.element,
                result.preview,
                this.plugintype,
                this.taskid,
            );
        }
    }

    protected httpGet<T>(url: string) {
        return to2(this.http.get<T>(url).toPromise());
    }

    protected httpPost<T>(url: string, body: unknown) {
        return to2(this.http.post<T>(url, body).toPromise());
    }

    protected httpPut<T>(url: string, body: unknown) {
        return to2(this.http.put<T>(url, body).toPromise());
    }

    protected async postAnswer<T>(answerdata: { input: unknown }) {
        const tid = this.pluginMeta.getTaskId();
        if (!tid) {
            throw Error("Task id missing.");
        }
        const dt = tid.docTask();
        const {url, data} = prepareAnswerRequest(dt, this.pluginMeta.getAnswerUrl());
        const finaldata = {
            ...answerdata,
            ...data,
        };
        const result = await this.httpPut<T & IAnswerSaveEvent>(url, finaldata);
        if (result.ok) {
            handleAnswerResponse(dt, {
                savedNew: result.result.savedNew,
            });
        } else {
            handleAnswerResponse(dt, {
                savedNew: false,
                error: result.result.error.error,
            });
        }
        return result;
    }

    abstract getAttributeType(): T;

    abstract getDefaultMarkup(): Partial<MarkupType>;
}
