import {IController, IRootElementService, IScope} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {watchEditMode} from "tim/editmode";
import {ViewCtrl} from "../controllers/view/viewctrl";
import {MenuFunctionCollection, MenuFunctionEntry} from "../controllers/view/viewutils";
import {$http, $window} from "../ngimport";
import {DraggableController} from "./draggable";

export type EditMode = "par" | "area";

export class PopupMenuController implements IController {
    private static $inject = ["$scope", "$element"];
    private model: {editState: EditMode | null};
    public element: IRootElementService;
    private contenturl: string;
    private srcid: string;
    private $pars: JQuery;
    private editcontext: string | null;
    private editbutton: boolean;
    private areaEditButton: boolean;
    private onClose: (pars: JQuery) => void;
    private colClass: string;
    private halfColClass: string;
    private vctrl: ViewCtrl;
    private save: boolean;
    private scope: IScope;
    private actions: MenuFunctionCollection;
    private draggable: DraggableController | undefined;

    constructor(scope: IScope, element: IRootElementService) {
        this.element = element;
        this.scope = scope;
    }

    $onInit() {
        this.vctrl.registerPopupMenu(this);
        this.$pars = $(this.srcid);
        this.editbutton = false;
        this.areaEditButton = false;

        this.getContent(this.contenturl);
        this.colClass = this.save ? "col-xs-10" : "col-xs-12";
        this.halfColClass = this.save ? "col-xs-5" : "col-xs-6";

        this.model = {editState: $window.editMode};

        this.scope.$watch(() => this.model.editState, watchEditMode);
        this.scope.$watch(() => this.model.editState, (newEditMode, oldEditMode) => this.watchEditMode(newEditMode, oldEditMode));
    }

    closePopup() {
        if (this.draggable) {
            this.draggable.$destroy();
        }
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
    callFunc(e: Event, f: MenuFunctionEntry) {
        f.func(e, this.$pars);
        this.closePopup();
    }

    getChecked(fDesc: string) {
        if (fDesc == null || this.vctrl.$storage.defaultAction == null) {
            return "";
        }

        return fDesc === this.vctrl.$storage.defaultAction ? "checked" : "";
    }

    clicked(f: MenuFunctionEntry) {
        if (!this.save) {
            return;
        }
        if (this.vctrl.defaultAction && this.vctrl.defaultAction.desc === f.desc) {
            this.vctrl.defaultAction = null;
            this.vctrl.$storage.defaultAction = null;
        } else {
            this.vctrl.defaultAction = f;
            this.vctrl.$storage.defaultAction = f.desc;
        }
    }

    getContent(contentUrl: string) {
        if (!contentUrl) {
            $("#content").remove();
            return;
        }

        $http.get<{texts: string}>(contentUrl, {params: {doc_id: this.vctrl.docId}},
        ).then((response) => {
            $("#content").append(response.data.texts);
        }, () => {
            $window.alert("Error occurred when getting contents.");
        });
    }

    watchEditMode(newEditMode: EditMode | null, oldEditMode: EditMode | null) {
        if (this.editcontext && newEditMode && newEditMode !== this.editcontext) {
            // We don't want to destroy our scope before returning from this function
            $window.setTimeout(() => this.closePopup(), 0.1);
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
        actions: "=",
        areaEditButton: "<",
        contenturl: "@",
        editbutton: "@",
        editcontext: "@",
        onClose: "&",
        save: "<?",
        srcid: "@",
    },
    controller: PopupMenuController,
    require: {
        vctrl: "^timView",
        draggable: "?^timDraggableFixed",
    },
    templateUrl: "/static/templates/popupMenu.html",
});
