import {IRootElementService, IScope} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {watchEditMode} from "tim/editmode";
import {$http, $window} from "../ngimport";

class PopupMenuController {
    private model: {editState: any};
    private element: IRootElementService;
    private contenturl: string;
    private srcid: string;
    private $pars: JQuery;
    private editContext: string | null;
    private editButton: boolean;
    private areaEditButton: boolean;
    private onClose: (pars: JQuery) => void;
    private colClass: string;
    private halfColClass: string;
    private storageAttribute: false; // TODO

    constructor(scope: IScope, element: IRootElementService) {
        this.$pars = $(this.srcid);
        this.editButton = false;
        this.areaEditButton = false;
        this.editContext = null;

        this.getContent(this.contenturl);
        this.colClass = this.storageAttribute ? "col-xs-10" : "col-xs-12";
        this.halfColClass = this.storageAttribute ? "col-xs-5" : "col-xs-6";

        this.model = {editState: $window.editMode};
        this.element = element;
        scope.$watch("model.editState", watchEditMode);
        scope.$watch("model.editState", this.watchEditMode);

        element.css("position", "absolute"); // IE needs this

    }

    closePopup() {
        this.element.remove();

        if (this.onClose) {
            this.onClose(this.$pars);
        }
    }

    /**
     * Angular expressions can't reference DOM elements, so we use a "proxy" function.
     * @param e Event object
     * @param f The function to call
     */
    callFunc(e, f) {
        f.func(e, this.$pars);
        this.closePopup();
    }

    getChecked(fDesc) {
        return ""; // TODO
    }

    clicked(fDesc) {
        // TODO
    }

    getContent(contentUrl) {
        if (!contentUrl) {
            $("#content").remove();
            return;
        }

        $http.get<{texts}>(contentUrl, {},
        ).then((response) => {
            //this.content = data.texts;
            $("#content").append(response.data.texts);
        }, () => {
            $window.alert("Error occurred when getting contents.");
        });
    }

    watchEditMode(newEditMode, oldEditMode) {
        //$log.info("Edit context set from " + oldEditMode + " to " + newEditMode);
        if (this.editContext && newEditMode && newEditMode != this.editContext) {
            // We don't want to destroy our scope before returning from this function
            $window.setTimeout(this.closePopup, 0.1);
        }
    }
}

/**
 * A popup menu directive that is used in the document view.
 * Requires a paragraph (element with class "par") or
 * an area (element with a class "area") as its ancestor.
 */
timApp.component("popupMenu", {
    bindings: {
        save: "@",
        onClose: "&",
        editcontext: "@",
        editbutton: "@",
        areaEditButton: "<",
        actions: "=",
        srcid: "@",
    },
    controller: PopupMenuController,
    templateUrl: "/static/templates/popupMenu.html",
});
