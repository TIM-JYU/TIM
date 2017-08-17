import {IRootElementService, IController} from "angular";
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

    constructor(element: IRootElementService) {
        this.loaded = false;
        $http.get("/par_info/" + this.docid + "/" + this.parid).then(this.loadSuccess, this.loadFail);
        this.ref_loaded = true;
        this.loaded = true;
        element.css("position", "absolute"); // IE needs this
    }

    $onInit() {

    }

    loadSuccess(response) {
        this.doc_name = response.data.doc_name;
        this.doc_author = response.data.doc_author;
        this.par_name = response.data.par_name;
        this.loaded = true;
    }

    loadFail(response) {
        this.error = response.data.error;
        this.loaded = true;
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
