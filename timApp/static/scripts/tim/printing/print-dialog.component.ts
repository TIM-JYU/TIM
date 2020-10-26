import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import * as t from "io-ts";
import {FormsModule} from "@angular/forms";
import {TimStorage, to2} from "../util/utils";
import {IItem} from "../item/IItem";

export interface ITemplate extends IItem {}

export interface ITemplateParams {
    templates: ITemplate[];
    doctemplate: string;
}

export interface IPrintParams {
    document: IItem;
    params: ITemplateParams;
}

@Component({
    selector: "tim-print-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <div *ngIf="doctemplate===''">
                    <p *ngIf="templates.length">The following LaTeX templates were found that you can use.</p>
                    <h5>Please choose a template:</h5>
                </div>

                <form>
                    <div class="form-group input-group-sm">
                        <div *ngFor="let item of templates">
                            <div class="radio" *ngIf="showTemplate(item)">
                                <label>
                                    <input type="radio" [(ngModel)]="selectedTemplate" [value]="item"
                                           name="template-selection">
                                    <span>{{ item.title }} <span *ngIf="showPaths">({{ item.path }})</span></span>
                                    <a *ngIf="showPaths" href="/view/{{ item.path }}" target="_blank"
                                       class="force-hyphenate-link">
                                        <span class="glyphicon glyphicon-pencil"></span>
                                    </a>
                                </label>
                            </div>
                        </div>
                    </div>
                </form>

                <p *ngIf="templates.length === 0 && doctemplate === ''" class="text-danger">
                    No printing templates were found.
                </p>

                <tim-alert *ngIf="notificationmsg" severity="warning">
                    {{ notificationmsg }}
                </tim-alert>
                <tim-alert *ngIf="errormsg" severity="danger">
                    <strong>Error!</strong>
                    <p [innerHtml]="errormsg"></p>
                    <ul>
                        <li *ngIf="latexline">
                            <a [href]="latexline" target="_blank">View erroneous LaTeX line</a>
                        </li>
                        <li *ngIf="docUrl">
                            <a [href]="docUrl" target="_blank">Try to view PDF</a>
                        </li>
                        <li *ngIf="latex">
                            <a [href]="latex" target="_blank">View LaTeX</a>
                        </li>
                        <li *ngIf="docUrl">
                            <a [href]="docUrl+'&showerror=true'" target="_blank">Recreation link</a>
                        </li>
                    </ul>
                </tim-alert>
                <div>
                    <label class="footerLabel">
                        <input type="checkbox" [(ngModel)]="pluginsUserCode">
                        Show user answers in plugins
                    </label>
                </div>
                <div>
                    <label title="More output formats and other options">
                        <input type="checkbox" [(ngModel)]="showPaths">
                        Show advanced options
                    </label>
                </div>
                <div>
                    Output format: &nbsp;
                    <label class="footerLabel">
                        <input type="radio" [(ngModel)]="selected.name" value="PDF">
                        PDF
                    </label>&nbsp;
                    <label class="footerLabel">
                        <input type="radio" [(ngModel)]="selected.name" value="LaTeX">
                        LaTeX
                    </label>&nbsp;
                    <label class="footerLabel" *ngIf="showPaths">
                        <input type="radio" [(ngModel)]="selected.name" value="MD">
                        MD
                    </label>&nbsp;
                    <label class="footerLabel" *ngIf="showPaths">
                        <input type="radio" [(ngModel)]="selected.name" value="JSON">
                        JSON
                    </label>&nbsp;
                    <label class="footerLabel" *ngIf="showPaths">
                        <input type="radio" [(ngModel)]="selected.name" value="HTML">
                        HTML
                    </label>
                    <label class="footerLabel" *ngIf="showPaths">
                        <input type="radio" [(ngModel)]="selected.name" value="PLAIN">
                        Plain
                    </label>
                    <label class="footerLabel" *ngIf="showPaths">
                        <input type="radio" [(ngModel)]="selected.name" value="RST">
                        RST
                    </label>
                </div>
                <div *ngIf="showPaths">
                    <label class="footerLabel" title="If you think images have changed after last print">
                        <input type="checkbox" [(ngModel)]="removeOldImages">
                        Reload external images
                    </label>
                </div>
                <div *ngIf="showPaths">
                    <label class="footerLabel" title="Use this in case referenced docs have changed">
                        <input type="checkbox" [(ngModel)]="forceRefresh">
                        Force reprint (skip cache)
                    </label>
                </div>
                <button [disabled]="loading" class="timButton" (click)="create()">
                    Create
                </button>
                <tim-loading *ngIf="loading"></tim-loading>
                <tim-alert *ngIf="docUrl && !errormsg" severity="success">
                    Creation succeeded!
                    <a [href]="docUrl"
                       target="_blank">View the document.</a>
                </tim-alert>
            </ng-container>
            <ng-container footer></ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["./print-dialog.component.scss"],
})
export class PrintDialogComponent extends AngularDialogComponent<
    IPrintParams,
    void
> {
    protected dialogName = "Print";
    private storage = new TimStorage("PrintingTemplateId", t.number);
    errormsg?: string;
    notificationmsg?: string;
    docUrl?: string;
    latex?: string;
    latexline?: string;
    loading: boolean;
    showPaths: boolean;
    pluginsUserCode: boolean;
    selectedTemplate?: ITemplate;
    doctemplate?: string;
    createdUrl?: string;
    selected: {name: string};
    forceRefresh: boolean = false;
    removeOldImages: boolean = false;

    constructor(private http: HttpClient) {
        super();
        this.loading = false;
        this.showPaths = false;
        this.pluginsUserCode = false;
        this.selected = {
            name: "PDF",
        };
    }

    getTitle() {
        return "Printing document";
    }

    get templates() {
        return this.data.params.templates;
    }

    ngOnInit() {
        this.doctemplate = this.data.params.doctemplate;
        this.selectedTemplate = this.initTemplate();
    }

    private initTemplate() {
        let tpl;

        const prev = this.storage.get();
        if (prev && this.templates) {
            this.templates.forEach((template) => {
                if (template.id === prev) {
                    tpl = template;
                }
            });
        } else if (this.templates) {
            tpl = this.templates.find((tmpl) => tmpl.name !== "empty");
        }

        return tpl;
    }

    showTemplate(tpl: ITemplate) {
        if (this.showPaths) {
            return true;
        }
        if (tpl.name !== "empty") {
            return true;
        }
        if (this.selectedTemplate && this.selectedTemplate.id === tpl.id) {
            return true;
        }
        return false;
    }

    private async getPrintedDocument(fileType: string) {
        this.errormsg = undefined;
        this.docUrl = undefined;

        let chosenTemplateId = 0;
        if (!this.selectedTemplate) {
            if (this.doctemplate === "") {
                this.notificationmsg = "You need to choose a template first!";
                return;
            }
        } else {
            chosenTemplateId = this.selectedTemplate.id;
            this.storage.set(chosenTemplateId);
        }

        const pluginsUserCode = this.pluginsUserCode;
        const removeOldImages = this.removeOldImages;
        const force = this.forceRefresh;

        if (chosenTemplateId || this.doctemplate) {
            this.loading = true;
            this.notificationmsg = undefined;

            const r = await to2(
                this.http
                    .post<{
                        url: string;
                        errormsg?: string;
                        latex?: string;
                        latexline?: string;
                    }>("/print/" + this.data.document.path, {
                        fileType,
                        templateDocId: chosenTemplateId,
                        printPluginsUserCode: pluginsUserCode,
                        removeOldImages,
                        force,
                    })
                    .toPromise()
            );
            if (r.ok) {
                const response = r.result;
                this.docUrl = response.url;
                this.errormsg = response.errormsg;
                this.latex = response.latex;
                this.latexline = response.latexline;
                this.loading = false;
            } else {
                this.errormsg = r.result.error.error.split("\\n").join("<br/>");
                this.loading = false;
            }
        }
    }

    create() {
        this.createdUrl = undefined;
        this.getPrintedDocument(this.selected.name.toLowerCase());
    }

    private cancel() {
        this.dismiss();
    }
}

@NgModule({
    declarations: [PrintDialogComponent],
    imports: [
        BrowserModule,
        DialogModule,
        TimUtilityModule,
        HttpClientModule,
        FormsModule,
    ],
})
export class PrintDialogModule {}
