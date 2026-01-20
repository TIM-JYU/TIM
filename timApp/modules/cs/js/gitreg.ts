/* eslint no-underscore-dangle: ["error", { "allow": ["hide_"] }] */
import {ChangeDetectorRef, Component, ElementRef} from "@angular/core";
import {HttpClient, HttpHeaders} from "@angular/common/http";
import {DomSanitizer} from "@angular/platform-browser";
import {defaultTimeout, toPromise} from "tim/util/utils";
import type {IRunRequest, IRunResponse} from "./csPlugin";
import {CsController} from "./csPlugin";

@Component({
    selector: "cs-git-reg-runner",
    template: `
        <div [ngClass]="{'csRunDiv': markup.borders}" [class.cs-has-header]="header" class="type-{{rtype}}">
            <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <h4 *ngIf="header" [innerHTML]="header"></h4>
            <p *ngIf="stem" class="stem" [innerHTML]="stem"></p>
            <form *ngIf="!isRegistered" (ngSubmit)="submit($event)">
                <table>
                    <tr *ngFor="let field of askFields">
                        <td>{{field.charAt(0).toUpperCase() + field.slice(1)}}:</td>
                        <td><input name="{{field}}" type="{{field == 'password' ? 'password' : 'text'}}"></td>
                    </tr>
                    <tr><td>
                        <button *ngIf="!preventRun"
                                [disabled]="isRunning"
                                class="timButton btn-sm"
                                title="(Ctrl-S)"
                                i18n>Submit</button>
                    </td></tr>
                </table>
            </form>
            <div class="csRunErrorClass" *ngIf="error">
                <p class="pull-right">
                    <tim-close-button (click)="closeError()"></tim-close-button>
                </p>
                <pre class="csRunError" >{{error}}</pre>
                <p class="pull-right" style="margin-top: -1em">
                    <tim-close-button (click)="closeError()"></tim-close-button>
                </p>
            </div>
            <div class="csRunErrorClass" *ngIf="isRegistered && error">
                Errors occurred. Try reloading the page. If this message persists, contact the course personnel.
            </div>
            <pre *ngIf="isRegistered && !error">You are registered</pre>
            <p *ngIf="markup.footer" class="footer" [innerHTML]="markup.footer"></p>
        </div>`,
})
export class GitRegComponent extends CsController {
    preventRun: boolean = false;
    askFields: string[] = [];
    isRegistered: boolean = false;

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer, cdr);
    }

    ngOnInit() {
        super.ngOnInit();

        this.isRegistered = !!this.attrsall.gitRegistered;
        if (this.markup.git?.askFields) {
            this.askFields = this.markup.git.askFields;
        }
    }

    getContent(): string {
        return this.isRegistered
            ? "You are registered"
            : "Register to git here";
    }

    async submit(event: Event) {
        if (this.isRunning || !event.target) {
            return;
        }
        this.isRunning = true;

        const data: Record<string, string | undefined> = {};

        for (const field of this.askFields) {
            data[field] = (
                (event.target as HTMLFormElement)[field] as
                    | HTMLInputElement
                    | undefined
            )?.value;
        }

        const params: IRunRequest & {
            input: {gitRegFields: Record<string, string | undefined>};
        } = {
            input: {
                gitRegFields: data,
                submittedFiles: [],
                usercode: "",
                userinput: "",
                isInput: false,
                userargs: "",
                uploadedFiles: [],
                nosave: true,
                type: "gitreg",
            },
        };

        const url = this.pluginMeta.getAnswerUrl();
        const r = await toPromise(
            this.http.put<IRunResponse>(url, params, {
                headers: new HttpHeaders({
                    timeout: `${this.timeout + defaultTimeout}`,
                }),
            })
        );
        if (r.ok) {
            this.error = r.result.web.error;
            this.isRegistered = !!r.result.web.language;
        } else {
            this.error = r.result.error.error;
        }
        this.isRunning = false;
    }
}
