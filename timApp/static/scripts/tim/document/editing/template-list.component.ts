import {toPromise} from "tim/util/utils";
import {IItem} from "tim/item/IItem";
import {Component, Input, OnInit} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {showMessageDialog} from "tim/ui/showMessageDialog";

@Component({
    selector: "tim-template-list",
    template: `
        <bootstrap-panel i18n-title title="Choose a template" [showClose]="true" *ngIf="templateList.length > 0">

            <!--
            Templates' panel will always be shown even without any content.
            By removing an empty templates list placeholder message
            “No templates found” was to ensure that users would not
            misinterpret the placeholder text as an error message.
            -->

            <ul>
                <li *ngFor="let template of templateList">
                    {{template.title}}
                    <button i18n class="timButton btn-xs" (click)="loadTemplate(template)">Load</button>
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

        if (r.ok) {
            window.location.reload();
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    ngOnInit() {
        void this.getTemplates();
    }

    async getTemplates() {
        const request = `/getTemplates/${this.doc.path}`;
        const response = await toPromise(this.http.get<IItem[]>(request));

        if (response.ok) {
            this.templateList = response.result;
        }
    }
}
