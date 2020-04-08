import {IScope} from "angular";
import $ from "jquery";
import {watchEditMode} from "tim/document/editing/editmode";
import {DialogController} from "tim/ui/dialogController";
import {registerDialogComponent, showDialog} from "../ui/dialog";
import {Pos} from "../ui/draggable";
import {documentglobals} from "../util/globals";
import {$http, $timeout} from "../util/ngimport";
import {to} from "../util/utils";
import {ViewCtrl} from "./viewctrl";
import {IMenuFunctionEntry, MenuFunctionList} from "./viewutils";

export type EditMode = "par" | "area";

export interface IPopupParams {
    actions: MenuFunctionList;
    areaEditButton: boolean;
    contenturl?: string;
    editbutton: boolean;
    editcontext?: EditMode;
    save: boolean;
    srcid: string;
    vctrl: ViewCtrl;
    pos?: Pos;
}

/**
 * A popup menu component that is used in the document view.
 */
export class PopupMenuController extends DialogController<{params: IPopupParams}, {}> {
    static component = "popupMenu";
    static $inject = ["$element", "$scope"] as const;
    public editState: EditMode | null;
    private content?: string;
    private olds: Partial<IPopupParams> & {editState?: EditMode | null} = {
        contenturl: undefined,
        editState: undefined,
    };
    private p!: IPopupParams;

    constructor(element: JQLite, scope: IScope) {
        super(element, scope);
        this.editState = documentglobals().editMode;
    }

    updateAttrs(p: Partial<IPopupParams>) {
        this.p = {...this.p, ...p};
    }

    get vctrl() {
        return this.p.vctrl;
    }

    get areaEditButton() {
        return this.p.areaEditButton;
    }

    get editbutton() {
        return this.p.editbutton;
    }

    get actions() {
        return this.p.actions;
    }

    get save() {
        return this.p.save;
    }

    public close() {
        super.close({});
    }

    $onInit() {
        super.$onInit();
        this.p = this.resolve.params;
        (async () => {
            await this.draggable.makeHeightAutomatic();
            const p = this.p.pos;
            if (p) {
                await this.moveTo(p);
            }
        })();
    }

    async $doCheck() {
        if (this.p.contenturl != this.olds.contenturl && this.p.contenturl) {
            this.olds.contenturl = this.p.contenturl;
            this.getContent(this.p.contenturl);
        }
        if (this.editState != this.olds.editState) {
            this.olds.editState = this.editState;
            watchEditMode(this.editState, this.olds.editState, this.scope);
            this.watchEditMode(this.editState, this.olds.editState);

            // When the number of visible buttons increases, the dialog may go off screen.
            await $timeout();
            this.draggable.ensureFullyInViewport();
        }
    }

    /**
     * Angular expressions can't reference DOM elements, so we use a "proxy" function.
     * @param e Event object
     * @param f The function to call
     */
    callFunc(e: JQuery.MouseEventBase, f: IMenuFunctionEntry) {
        f.func(e, $(this.p.srcid));
        if (f.closeAfter || f.closeAfter == null) {
            this.close();
        }
    }

    getChecked(fDesc: string) {
        if (fDesc == null || this.vctrl.$storage.defaultAction == null) {
            return "";
        }

        return fDesc === this.vctrl.$storage.defaultAction ? "checked" : "";
    }

    getInputTitle(f: IMenuFunctionEntry) {
        return `Set "${f.desc}" as the default action for double-click`;
    }

    clicked(f: IMenuFunctionEntry) {
        if (!this.p.save) {
            return;
        }
        if (this.vctrl.defaultAction && this.vctrl.defaultAction.desc === f.desc) {
            this.vctrl.defaultAction = undefined;
            this.vctrl.$storage.defaultAction = null;
        } else {
            this.vctrl.defaultAction = f;
            this.vctrl.$storage.defaultAction = f.desc;
        }
    }

    async getContent(contentUrl: string) {
        if (!contentUrl) {
            this.content = undefined;
            return;
        }

        const r = await to($http.get<{texts: string}>(contentUrl, {params: {doc_id: this.vctrl.item.id}}));
        if (r.ok) {
            this.content = r.result.data.texts;
            this.draggable.ensureFullyInViewport();
        }
    }

    watchEditMode(newEditMode: EditMode | null, oldEditMode: EditMode | null) {
        if (this.p.editcontext && newEditMode && newEditMode !== this.p.editcontext) {
            // We don't want to destroy our scope before returning from this function
            window.setTimeout(() => this.close(), 0.1);
        }
    }

    protected getTitle(): string {
        return " ";
    }
}

registerDialogComponent(PopupMenuController,
    {
        template: `
<tim-dialog>
    <dialog-body>
        <div class="flex cl">
            <div class="error" ng-show="$ctrl.vctrl.notification" ng-bind="$ctrl.vctrl.notification"></div>
            <div class="pastePreview" ng-if="$ctrl.content" ng-bind-html="$ctrl.content"></div>

            <div class="flex rw align-center" ng-repeat="f in $ctrl.actions | filter:{show: true}">
                <button class="timButton btn-sm flex-grow-5" ng-bind="f.desc" ng-click="$ctrl.callFunc($event, f)">
                </button>

                <input ng-if="$ctrl.save"
                       ng-checked="$ctrl.getChecked(f.desc)"
                       ng-click="$ctrl.clicked(f)"
                       style="margin: 5px 0 5px 15px;"
                       title="{{ $ctrl.getInputTitle(f) }}"
                       type="radio">
            </div>

            <div class="flex rw" style="padding-top: 6px; margin-right: 28px"
                 ng-if="$ctrl.editbutton && $ctrl.vctrl.item.rights.editable">
                <button class="timButton parEditButton flex-grow-5" ng-model="$ctrl.editState" uib-btn-radio="'par'"
                        uncheckable="true" title="Toggle paragraph edit mode">
                    <i class="glyphicon glyphicon-minus"></i>
                    <i class="glyphicon glyphicon-pencil"></i>
                </button>
                <button class="timButton areaEditButton flex-grow-5" ng-model="$ctrl.editState"
                        ng-disabled="true" uib-btn-radio="'area'"
                        uncheckable="true"
                        title="Toggle area edit mode">
                    <i class="glyphicon glyphicon-align-justify"></i>
                    <i class="glyphicon glyphicon-pencil"></i>
                </button>
            </div>
        </div>
    </dialog-body>
</tim-dialog>
    `,
    },
);

export function showPopupMenu(p: IPopupParams) {
    // debugTextToHeader("showPopupMenu" + JSON.stringify(p.pos));
    return showDialog(PopupMenuController, {params: () => p},
    // return showDialog<PopupMenuController>(null, {params: () => p},
        {
            absolute: true,
            showMinimizeButton: false,
            size: "xs",
        });
}
