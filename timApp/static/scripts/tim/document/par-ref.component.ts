import {Component, Input} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {IDocument} from "../item/IItem";
import {getGroupDesc, toPromise} from "../util/utils";

interface IParInfo {
    item: IDocument;
    par_name: string | null;
}

/**
 * A reference popup window component that is used in the document view.
 */
@Component({
    selector: "tim-par-ref",
    template: `
        <ng-template #tooltipHtml>
            <div style="text-align: left; white-space: nowrap;">
                This paragraph references another document.
                <ul class="list-unstyled">
                    <li><b>Title:</b> {{title}}</li>
                    <li><b>Authors:</b> {{owners}}</li>
                    <li><b>Paragraph:</b> {{par}}</li>
                </ul>
            </div>
        </ng-template>
        <a href="{{url}}">
            <i class="glyphicon glyphicon-share-alt"
               (mouseover)="loadData()"
               [tooltip]="tooltipHtml"
               [tooltipEnable]="data !== undefined"
               [isOpen]="isOpen"
            ></i>
        </a>
        <tim-loading style="position: absolute" *ngIf="loading"></tim-loading>
    `,
})
export class ParRefComponent {
    loading = false;
    @Input() docid!: string;
    @Input() parid!: string;
    private error?: string;
    data?: IParInfo;
    private loaded = false;
    url!: string;
    isOpen = false;

    constructor(private http: HttpClient) {}

    ngOnInit() {
        this.url = `/view/${this.docid}#${this.parid}`;
    }

    async loadData() {
        if (this.loaded) {
            return;
        }
        this.loaded = true;
        this.loading = true;
        const r = await toPromise(
            this.http.get<IParInfo>(`/par_info/${this.docid}/${this.parid}`)
        );
        this.loading = false;
        this.isOpen = true;
        if (r.ok) {
            this.data = r.result;
            this.url = `/view/${this.data.item.path}#${this.parid}`;
        } else {
            this.error = r.result.error.error;
        }
    }

    get title() {
        return this.data?.item.title ?? "";
    }

    get par() {
        return this.data?.par_name ?? this.parid;
    }

    get owners() {
        return (
            this.data?.item.owners.map((o) => getGroupDesc(o)).join(", ") ?? ""
        );
    }
}
