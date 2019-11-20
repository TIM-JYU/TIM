import {IController} from "angular";
import {timApp} from "tim/app";
import {IDocument} from "../item/IItem";
import {$http} from "../util/ngimport";
import {Binding, getGroupDesc, to} from "../util/utils";

interface IParInfo {
    item: IDocument;
    par_name: string | null;
}

export class ParRefController implements IController {
    private loading = false;
    private docid!: Binding<string, "@">;
    private parid!: Binding<string, "@">;
    private error?: string;
    private data?: IParInfo;
    private loaded = false;
    private url!: string;
    private isOpen = false;

    $onInit() {
        this.url = `/view/${this.docid}#${this.parid}`;
    }

    async loadData() {
        if (this.loaded) {
            return;
        }
        this.loaded = true;
        this.loading = true;
        const r = await to($http.get<IParInfo>(`/par_info/${this.docid}/${this.parid}`));
        this.loading = false;
        this.isOpen = true;
        if (r.ok) {
            this.data = r.result.data;
            this.url = `/view/${this.data.item.path}#${this.parid}`;
        } else {
            this.error = r.result.data.error;
        }
    }

    getTooltipHtml() {
        if (!this.data) {
            return this.error;
        }
        return `
<div style="text-align: left; white-space: nowrap;">
This paragraph references another document.
<ul class="list-unstyled">
    <li><b>Title:</b> ${this.data.item.title}</li>
    <li><b>Authors:</b> ${this.data.item.owners.map((o) => getGroupDesc(o)).join(", ")}</li>
    <li><b>Paragraph:</b> ${this.data.par_name || this.parid}</li>
</ul>
</div>
        `;
    }
}

/**
 * A reference popup window component that is used in the document view.
 */
timApp.component("timParRef", {
    bindings: {
        docid: "@",
        parid: "@",
    },
    controller: ParRefController,
    template: `
<a href="{{$ctrl.url}}">
    <i class="glyphicon glyphicon-share-alt"
       ng-mouseover="$ctrl.loadData()"
       uib-tooltip-html="$ctrl.getTooltipHtml()"
       tooltip-enable="$ctrl.data"
       tooltip-is-open="$ctrl.isOpen"
       ></i>
</a>
<tim-loading style="position: absolute" ng-if="$ctrl.loading"></tim-loading>
    `,
});
