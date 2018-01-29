import {IController, IRootElementService} from "angular";
import {timApp} from "tim/app";
import {$http} from "../ngimport";

class RefPopupController implements IController {
    private loaded: boolean;
    private ref_loaded: boolean;
    private docid: number;
    private parid: string;
    private error: string;
    private doc_name: string;
    private doc_author: string;
    private par_name: string;
    private element: IRootElementService;

    constructor(element: IRootElementService) {
        this.loaded = false;
        this.element = element;
    }

    async $onInit() {
        try {
            const response = await $http.get<{doc_name: string, doc_author: string, par_name: string}>("/par_info/" + this.docid + "/" + this.parid);
            this.doc_name = response.data.doc_name;
            this.doc_author = response.data.doc_author;
            this.par_name = response.data.par_name;
            this.loaded = true;
            this.ref_loaded = true;
        } catch (e) {
            this.error = e.data.error;
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
        docid: "<",
        parid: "<",
    },
    controller: RefPopupController,
    templateUrl: "/static/templates/refPopup.html",
});
