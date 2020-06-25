import {IScope} from "angular";
import {ngStorage} from "ngstorage";
import {DialogController} from "tim/ui/dialogController";
import {IItem} from "../item/IItem";
import {registerDialogComponent, showDialog} from "../ui/dialog";
import {$http, $localStorage} from "../util/ngimport";
import {to} from "../util/utils";

export interface ITemplate extends IItem {

}

export interface ITemplateParams {
    templates: ITemplate[];
    doctemplate: string;
}

export interface IPrintParams {
    document: IItem;
    params: ITemplateParams;
}

export class PrintCtrl extends DialogController<{params: IPrintParams}, void> {
    static component = "timPrint";
    static $inject = ["$element", "$scope"] as const;
    private storage: ngStorage.StorageService & {timPrintingTemplateId: null | number};
    private errormsg?: string;
    private notificationmsg?: string;
    private docUrl?: string;
    private latex?: string;
    private latexline?: string;
    private loading: boolean;
    private showPaths: boolean;
    private pluginsUserCode: boolean;
    private selectedTemplate?: ITemplate;
    private templates?: ITemplate[];
    private doctemplate?: string;
    private createdUrl?: string;
    private document!: IItem; // $onInit
    private selected: {name: string};
    private forceRefresh: boolean = false;
    private removeOldImages: boolean = false;

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);
        this.storage = $localStorage.$default({
            timPrintingTemplateId: null,
        });
        this.loading = false;
        this.showPaths = false;
        this.pluginsUserCode = false;
        this.selected = {
            name: "PDF",
        };
    }

    public getTitle() {
        return "Printing document";
    }

    $onInit() {
        super.$onInit();
        this.document = this.resolve.params.document;
        this.templates = this.resolve.params.params.templates;
        this.doctemplate = this.resolve.params.params.doctemplate;
        this.selectedTemplate = this.initTemplate();
    }

    private initTemplate() {
        let t;

        if (this.storage.timPrintingTemplateId && this.templates) {

            this.templates.forEach((template) => {
                if (template.id === this.storage.timPrintingTemplateId) {
                    t = template;
                }
            });

        } else if (this.templates) {
            t = this.templates.find((tmpl) => tmpl.name !== "empty");
        }

        return t;
    }

    private showTemplate(t: ITemplate) {
        if (this.showPaths) {
            return true;
        }
        if (t.name !== "empty") {
            return true;
        }
        if (this.selectedTemplate && this.selectedTemplate.id === t.id) {
            return true;
        }
        return false;
    }

    private async getPrintedDocument(fileType: string) {
        this.errormsg = undefined;
        this.docUrl = undefined;

        /*
        if (fileType !== 'latex' && fileType !== 'pdf') {
            console.log("The filetype '" + fileType + "' is not valid");
            return; //TODO: the error should do something visible
            // TODO: also kind of pointless as the filetype comes from the predefined functions
        }
        */

        let chosenTemplateId = 0;
        if (!this.selectedTemplate) {
            if (this.doctemplate === "") {
                this.notificationmsg = "You need to choose a template first!";
                return;
            }
        } else {
            chosenTemplateId = this.selectedTemplate.id;
            this.storage.timPrintingTemplateId = chosenTemplateId;
        }

        const pluginsUserCode = this.pluginsUserCode;
        const removeOldImages = this.removeOldImages;
        const force = this.forceRefresh;

        if (chosenTemplateId || this.doctemplate) {
            this.loading = true;
            this.notificationmsg = undefined;

            const postURL = "/print/" + this.document.path;
            const r = await to($http.post<{url: string, errormsg?: string, latex?: string, latexline?: string}>(postURL, {
                fileType,
                templateDocId: chosenTemplateId,
                printPluginsUserCode: pluginsUserCode,
                removeOldImages,
                force,
            }));
            if (r.ok) {
                const response = r.result;
                // console.log(response);

                // Uncomment this line to automatically open the created doc in a popup tab.
                // this.openURLinNewTab(requestURL);

                // this.docUrl = '/print/' + this.document.path + '?file_type=' + fileType
                //    + '&template_doc_id=' + chosenTemplateId + '&plugins_user_code=' + pluginsUserCode;
                this.docUrl = response.data.url;
                this.errormsg = response.data.errormsg;
                this.latex = response.data.latex;
                this.latexline = response.data.latexline;
                // console.log(this.docUrl);

                this.loading = false;

            } else {
                const reformatted = r.result.data.error.split("\\n").join("<br/>");
                this.errormsg = reformatted;
                this.loading = false;
            }
        }
    }

    private openURLinNewTab(url: string) {
        window.open(url, "_blank");
    }

    private create() {
        this.createdUrl = undefined;
        this.getPrintedDocument(this.selected.name.toLowerCase());
    }

    private cancel() {
        this.dismiss();
    }

    private formatPath(path: string) {
        return path.replace("templates/printing", "../..");
    }
}

registerDialogComponent(PrintCtrl,
    {templateUrl: "/static/templates/printDialog.html"});

export async function showPrintDialog(p: IPrintParams) {
    return await showDialog(PrintCtrl, {params: () => p}).result;
}
