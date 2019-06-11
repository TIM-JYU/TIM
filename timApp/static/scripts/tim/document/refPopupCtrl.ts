import {IController, IRootElementService} from "angular";
import {timApp} from "tim/app";
import {$http} from "../util/ngimport";
import {Binding, to} from "../util/utils";

class RefPopupController implements IController {
    static $inject = ["$element"];
    private loaded: boolean;
    private docid!: Binding<string, "@">;
    private parid!: Binding<string, "@">;
    private error?: string;
    private docName?: string;
    private docAuthor?: string;
    private parName?: string;
    private element: IRootElementService;

    constructor(element: IRootElementService) {
        this.loaded = false;
        this.element = element;
    }

    async $onInit() {
        const r = await to($http.get<{doc_name: string, doc_author: string, par_name: string}>(`/par_info/${this.docid}/${this.parid}`));
        if (r.ok) {
            this.docName = r.result.data.doc_name;
            this.docAuthor = r.result.data.doc_author;
            this.parName = r.result.data.par_name;
            this.loaded = true;
        } else {
            this.error = r.result.data.error;
        }
    }

    $postLink() {
        this.element.css("position", "absolute"); // IE needs this
    }
}

/**
 * A reference popup window directive that is used in the document view.
 */
timApp.component("refPopup", {
    bindings: {
        docid: "@",
        parid: "@",
    },
    controller: RefPopupController,
    templateUrl: "/static/templates/refPopup.html",
});
