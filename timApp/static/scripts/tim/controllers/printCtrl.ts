import angular from "angular";
import {ngStorage} from "ngstorage";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {IItem} from "../IItem";
import {$http, $localStorage, $window} from "../ngimport";

export interface ITemplate extends IItem {

}

export interface IPrintParams {
    document: IItem;
    templates: ITemplate[];
}

export class PrintCtrl extends DialogController<{params: IPrintParams}, {}, "timPrint"> {
    private storage: ngStorage.StorageService & {timPrintingTemplateId: null | number};
    private errormsg?: string;
    private notificationmsg?: string;
    private docUrl?: string;
    private loading: boolean;
    private showPaths: boolean;
    private pluginsUserCode: boolean;
    private selectedTemplate?: ITemplate;
    private templates: ITemplate[];
    private createdUrl?: string;
    private document: IItem;
    private selected: {name: string};
    private forceRefresh: boolean;
    private removeOldImages: boolean;

    public getTitle() {
        return "Printing document";
    }

    private $onInit() {
        this.storage = $localStorage.$default({
            timPrintingTemplateId: null,
        });

        this.document = this.resolve.params.document;
        this.templates = this.resolve.params.templates;
        this.loading = false;
        this.showPaths = false;
        this.pluginsUserCode = false;

        this.selectedTemplate = this.initTemplate();

        this.selected = {
            name: "PDF",
        };
    }

    private initTemplate() {
        let t;

        if (this.storage.timPrintingTemplateId && this.templates) {

            angular.forEach(this.templates, (template, key) => {
                if (template.id === this.storage.timPrintingTemplateId) {
                    t = template;
                }
            });

        } else if (this.templates) {
            if (this.templates.length > 0) {
                t = this.templates[0];
            }
        }

        return t;
    }

    private getPrintedDocument(fileType: string) {
        this.errormsg = undefined;
        this.docUrl = undefined;

        /*
        if (fileType !== 'latex' && fileType !== 'pdf') {
            console.log("The filetype '" + fileType + "' is not valid");
            return; //TODO: the error should do something visible
            // TODO: also kind of pointless as the filetype comes from the predefined functions
        }
        */

        if (!this.selectedTemplate) {
            this.notificationmsg = "You need to choose a template first!";
            return;
        }
        const chosenTemplateId = this.selectedTemplate.id;
        this.storage.timPrintingTemplateId = chosenTemplateId;

        const pluginsUserCode = this.pluginsUserCode;
        const removeOldImages = this.removeOldImages;
        const force = this.forceRefresh;

        if (chosenTemplateId) {
            this.loading = true;
            this.notificationmsg = undefined;

            const postURL = "/print/" + this.document.path;
            const data = JSON.stringify({
                fileType,
                templateDocId: chosenTemplateId,
                printPluginsUserCode: pluginsUserCode,
                removeOldImages,
                force,
            });
            $http.post<{url: string}>(postURL, data)
                .then((response) => {
                    // console.log(response);

                    // Uncomment this line to automatically open the created doc in a popup tab.
                    // this.openURLinNewTab(requestURL);

                    // this.docUrl = '/print/' + this.document.path + '?file_type=' + fileType
                    //    + '&template_doc_id=' + chosenTemplateId + '&plugins_user_code=' + pluginsUserCode;
                    this.docUrl = response.data.url;
                    // console.log(this.docUrl);

                    this.loading = false;

                }, (response) => {
                    const reformatted = response.data.error.split("\\n").join("<br/>");
                    this.errormsg = reformatted;
                    this.loading = false;
                })
            ;
        }
    }

    private openURLinNewTab(url: string) {
        $window.open(url, "_blank");
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

registerDialogComponent("timPrint",
    PrintCtrl,
    {templateUrl: "/static/templates/printDialog.html"});

export async function showPrintDialog(p: IPrintParams) {
    return await showDialog<PrintCtrl>("timPrint", {params: () => p}).result;
}
