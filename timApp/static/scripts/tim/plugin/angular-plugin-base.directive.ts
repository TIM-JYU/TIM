import {Directive, ElementRef, Input, OnInit} from "@angular/core";
import {IGenericPluginMarkup, IGenericPluginTopLevelFields} from "tim/plugin/attributes";
import {Type} from "io-ts";
import {HttpClient} from "@angular/common/http";
import {to2} from "tim/util/utils";
import {baseOnInit, getDefaults, PluginMarkupErrors, PluginMeta} from "tim/plugin/util";
import {handleAnswerResponse, prepareAnswerRequest} from "../document/interceptor";
import {IAnswerSaveEvent} from "../answer/answerbrowser3";

@Directive()
export abstract class AngularPluginBase<MarkupType extends IGenericPluginMarkup,
    A extends IGenericPluginTopLevelFields<MarkupType>,
    T extends Type<A>> implements OnInit {
    attrsall: Readonly<A>;
    @Input() readonly json!: string;
    @Input() readonly plugintype?: string;
    @Input() readonly taskid?: string;

    markupError?: PluginMarkupErrors;
    protected pluginMeta: PluginMeta;

    buttonText() {
        return this.markup.button || this.markup.buttonText;
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

    constructor(private element: ElementRef, private http: HttpClient) {
        this.attrsall = getDefaults(this.getAttributeType(), this.getDefaultMarkup());
        this.pluginMeta = new PluginMeta($(element.nativeElement), this.attrsall.preview);
    }

    ngOnInit() {
        const result = baseOnInit.call(this);
        if (result) {
            this.pluginMeta = new PluginMeta(
                $(this.element.nativeElement),
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
                error: result.result.data.error,
            });
        }
        return result;
    }

    abstract getAttributeType(): T;

    abstract getDefaultMarkup(): Partial<MarkupType>;
}
