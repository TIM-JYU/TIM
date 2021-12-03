import {toPromise} from "tim/util/utils";
import {IItem} from "tim/item/IItem";
import {Component, Input, OnInit} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {showMessageDialog} from "tim/ui/showMessageDialog";

@Component({
    selector: "tim-template-list",
    template: `
        <bootstrap-panel title="Choose a template" [showClose]="true" [ngSwitch]="templateList.length">
            <span *ngSwitchCase="0">No templates found.</span>
            <ul *ngSwitchDefault>
                <li *ngFor="let template of templateList">
                    {{template.title}}
                    <button class="timButton btn-xs" (click)="loadTemplate(template)">Load</button>
                </li>
            </ul>
        </bootstrap-panel>
    `,
})
export class TemplateListComponent implements OnInit {
    @Input() private doc!: IItem;
    templateList: IItem[] = [];

    constructor(private http: HttpClient) {}

    async loadTemplate(t: IItem) {
        const r = await toPromise(
            this.http.post("/update/" + this.doc.id, {
                template_name: t.path,
            })
        );
        if (!r.ok) {
            await showMessageDialog(r.result.error.error);
        } else {
            window.location.reload();
        }
    }

    ngOnInit() {
        void this.getTemplates();
    }

    async getTemplates() {
        const response = await toPromise(
            this.http.get<IItem[]>("/getTemplates", {
                params: {
                    item_path: this.doc.path,
                },
            })
        );
        if (!response.ok) {
            return;
        }
        this.templateList = response.result;
    }
}
