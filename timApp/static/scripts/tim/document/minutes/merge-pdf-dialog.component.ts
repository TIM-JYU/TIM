/**
 * Dialog for checking PDF attachment validity and merging them.
 */
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {to2} from "tim/util/utils";
import {IItem} from "tim/item/IItem";
import {showMessageDialog} from "tim/ui/showMessageDialog";

/**
 * @author Matti Leinonen
 * @author Ronja Lindholm
 * @author Visa Naukkarinen
 * @author Rami Pasanen
 * @author Enni Stylman
 * @licence MIT
 * @copyright 2018 Titus project authors
 */

export interface IMergeParams {
    document: IItem;
}

export interface IAttachment {
    url: string;
    macro: string;
    error: string;
    selected: boolean;
}

@Component({
    selector: "tim-merge-pdf-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{getTitle()}}
            </ng-container>
            <ng-container body>
                <div class="form-horizontal">
                    <p *ngIf="attachmentList.length > 0">
                        The following attachments were found in the current document:
                    </p>
                    <div class="form-group">
                        <div class="col-sm-12">
                            <ul class="list-unstyled">
                                <li *ngFor="let x of attachmentList">
                                    <label>
                                        <input type="checkbox"
                                               [(ngModel)]="x.selected"
                                               [disabled]="x.error">&ngsp;
                                        <a [href]="x.url" target="_blank">{{getFileName(x.url)}}</a>
                                    </label>
                                    &ngsp;
                                    <span [style.color]="macroColor(x.macro)">{{x.macro}}</span>
                                    &ngsp;
                                    <span *ngIf="x.error"
                                          style="color:red;"
                                          class="glyphicon glyphicon-warning-sign"
                                          [tooltip]="x.error"></span>
                                </li>
                            </ul>
                            <p *ngIf="!errorMessage && attachmentList.length == 0 && !checking">
                                No attachments found
                            </p>
                            <tim-alert *ngIf="errorMessage && !checking" severity="warning">
                                {{errorMessage}}
                            </tim-alert>
                        </div>
                    </div>
                    <tim-alert *ngIf="checking" severity="info">
                        <tim-loading></tim-loading>
                        Checking validity of the attachments, please wait...
                    </tim-alert>
                </div>
            </ng-container>
            <ng-container footer>
                <div style="float: left;">
                    <button class="timButton"
                            (click)="mergeClicked()"
                            [disabled]="attachmentList.length == 0"
                            title="Merge selected files">
                        <span *ngIf="loading"><tim-loading></tim-loading>
                        Merging</span>
                        <span *ngIf="!loading">Merge selected</span>
                    </button>
                    <button class="timButton" (click)="dismiss()"><span>Cancel</span></button>
                </div>
                <tim-alert style="float: right;" *ngIf="mergedUrl" severity="success">
                    Merging succeeded!
                    <a href="{{mergedUrl}}" target="_blank">View the document.</a>
                </tim-alert>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class MergePdfDialogComponent extends AngularDialogComponent<
    IMergeParams,
    void
> {
    protected dialogName = "MergePdf";
    attachmentList: IAttachment[] = [];
    loading: boolean = false; // Whether the merging process is underway.
    checking: boolean = true; // Whether the attachment check is underway.
    errorMessage?: string;
    mergedUrl?: string; // Link to file opening route with merge data as params.

    constructor(private http: HttpClient) {
        super();
    }

    getTitle() {
        return "Merge attachments";
    }

    /**
     * Gets a list of attachments in the documents and notes whether invalid files were found.
     */
    async listAttachments() {
        this.checking = true;
        const url = `/minutes/checkAttachments/${this.data.document.path}`;
        const r = await to2(this.http.get<IAttachment[]>(url, {}).toPromise());
        if (!r.ok) {
            this.errorMessage = r.result.error.error;
            this.checking = false;
            return;
        }
        this.attachmentList = r.result;
        this.checking = false;
    }

    async mergeClicked() {
        this.loading = true;
        const url = `/minutes/mergeAttachments`;
        const data = this.getSelectedAttachmentData();
        const r1 = await to2(
            this.http.post<{url: string}>(url, data).toPromise()
        );
        this.loading = false;
        if (!r1.ok) {
            await showMessageDialog(r1.result.error.error);
            return;
        }
        this.mergedUrl = r1.result.url;
    }

    ngOnInit() {
        (async () => {
            await this.listAttachments();
        })();
    }

    macroColor(macro: string) {
        if (macro == "liite") {
            return "green";
        }
        if (macro == "perusliite") {
            return "#999900";
        }
        // Error case.
        return "red";
    }

    /**
     * Format the attachment list as route data.
     */
    private getSelectedAttachmentData() {
        const urls: string[] = [];
        for (const attachment of this.attachmentList) {
            if (attachment.selected) {
                urls.push(attachment.url);
            }
        }
        return {doc_id: this.data.document.id, urls: urls};
    }

    /**
     * Show only the last part of a file URL or path (the file name).
     * @param url URL or path to shorten with either \ or /, doesn't need to be complete.
     */
    getFileName(url: string) {
        return url.replace(/^.*[\\\/]/, "");
    }
}

@NgModule({
    declarations: [MergePdfDialogComponent],
    imports: [
        BrowserModule,
        DialogModule,
        TimUtilityModule,
        FormsModule,
        HttpClientModule,
        TooltipModule.forRoot(),
    ],
})
export class MergePdfDialogModule {}
