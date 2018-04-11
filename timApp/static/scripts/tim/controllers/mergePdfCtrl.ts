import {ngStorage} from "ngstorage";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {IItem} from "../IItem";
import {$http} from "../ngimport";

export interface IMergeParams {
    document: IItem;
}

export class ShowMergePdfController extends DialogController<{ params: IMergeParams }, {}, "timMergePdf"> {
    private storage: ngStorage.StorageService & { timPrintingTemplateId: null | number };
    private docUrl?: string;
    private loading: boolean;
    private document: IItem;

    constructor() {
        super();
    }

    public getTitle() {
        return "Merging attachments";
    }

    async mergeClicked() {

        //this.document.path = `/mergeAttachments/${this.resolve.params.document.location}/${this.resolve.params.document.name}`;
        this.loading = true;
        //const postURL = "/print" + this.document.path;
        const response = await $http.get<{ url: string }>(`/mergeAttachments/${this.resolve.params.document.path}`);

        console.log(this.resolve.params.document.path);

        if(response) {
            this.loading = false;
        }

        var elem1 = document.createElement('label');
        var elem2 = document.createElement('a');
        var link = document.getElementById("link");

        this.docUrl = response.data.url;

        console.log(response.data.url);

        /*elem1.textContent = "Creations blob! "
        elem2.setAttribute("href", "response");
        elem2.textContent = " View the attachments.";

        link.appendChild(elem1);
        link.appendChild(elem2); */
        }

    $onInit() {
        super.$onInit();
        this.loading = false;
    }
}

registerDialogComponent("timMergePdf",
    ShowMergePdfController,
    {
        template:
            `
            <tim-dialog>
            <dialog-header ng-bind-html="$ctrl.getTitle()">
            </dialog-header>
            <dialog-body>
                <p id="link">
                </p>
                <button class="btn timButton" ng-click ="$ctrl.mergeClicked()">
                    <span ng-show="$ctrl.loading"><i class="glyphicon glyphicon-refresh glyphicon-refresh-animate"></i>
                    Merging</span>
                    <span ng-hide="$ctrl.loading">Merge</span>
                </button>
                <div id="merging-success" ng-show="$ctrl.docUrl" class="alert alert-success">
                    <p><span class="glyphicon-ok"></span></p>Merging succeeded! <a ng-href=""{{$ctrl.docUrl}}
                    "target"_blank">View the document.</a></div>
            </dialog-body>
            <dialog-footer></dialog-footer>
        </tim-dialog>
`,
    });

export async function showMergePdfDialog(p: IMergeParams) {
    return await showDialog<ShowMergePdfController>("timMergePdf", {params: () => p}).result;
}
