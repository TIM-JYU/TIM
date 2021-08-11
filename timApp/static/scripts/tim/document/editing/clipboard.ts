import {IScope} from "angular";
import {MenuFunctionList} from "tim/document/viewutils";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {
    getMinimalUnbrokenSelection,
    UnbrokenSelection,
} from "tim/document/editing/unbrokenSelection";
import {ParContext} from "tim/document/structure/parContext";
import {createParContext} from "tim/document/structure/create";
import {
    getContextualAreaInfo,
    ParAreaInclusionKind,
} from "tim/document/structure/areaContext";
import {Users} from "../../user/userService";
import {$http} from "../../util/ngimport";
import {empty, to} from "../../util/utils";
import {getElementByParId} from "../parhelpers";
import {ViewCtrl} from "../viewctrl";
import {EditType, IParResponse} from "./edittypes";

export type ClipboardMetaResponse =
    | {
          area_name?: string;
          disable_ref: boolean;
          disable_content: boolean;
          last_action?: "copy" | "cut" | "paste";
          empty: false;
      }
    | {empty: true};

export interface IClipboardMeta {
    area_name?: string;
    allowPasteRef: boolean;
    allowPasteContent: boolean;
    last_action?: "copy" | "cut" | "paste";
    empty: boolean;
}

interface IClipBoardResponse {
    doc_ver: [number, number];
    pars: Array<{id: string}>;
}

export class ClipboardHandler {
    public sc: IScope;
    public viewctrl: ViewCtrl;

    constructor(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
    }

    async showPasteMenu(e: MouseEvent, parOrArea: ParContext) {
        this.viewctrl.parmenuHandler.showPopupMenu(e, parOrArea, {
            actions: await this.getPasteFunctions(parOrArea),
            contenturl: "/clipboard",
            editbutton: false,
            save: false,
        });
    }

    showMoveMenu(e: MouseEvent, parOrArea: ParContext) {
        this.viewctrl.parmenuHandler.showPopupMenu(e, parOrArea, {
            actions: this.getMoveFunctions(parOrArea),
            contenturl: "/clipboard",
            editbutton: false,
            save: false,
        });
    }

    pasteContentAbove(e: MouseEvent, par: ParContext) {
        this.pasteAbove(e, par, false);
    }

    pasteRefAbove(e: MouseEvent, par: ParContext) {
        this.pasteAbove(e, par, true);
    }

    pasteContentBelow(e: MouseEvent, par: ParContext) {
        this.pasteBelow(e, par, false);
    }

    pasteRefBelow(e: MouseEvent, par: ParContext) {
        this.pasteBelow(e, par, true);
    }

    async deleteFromSource(s: ParContext) {
        const r = await to(
            $http.post<IClipBoardResponse>(
                "/clipboard/deletesrc/" + this.viewctrl.docId,
                {}
            )
        );
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }
        const pars = r.result.data.pars;
        if (pars.length > 0) {
            const firstPar = pars[0].id;
            const lastPar = pars[pars.length - 1].id;
            this.viewctrl.editingHandler.handleDelete({
                pars: getMinimalUnbrokenSelection(
                    createParContext(getElementByParId(firstPar)[0]),
                    createParContext(getElementByParId(lastPar)[0])
                ),
                type: EditType.Edit,
            });
        }
    }

    async moveAbove(e: MouseEvent, parOrArea: ParContext) {
        const r = await to(
            $http.post<IParResponse>(
                "/clipboard/paste/" + this.viewctrl.docId,
                {
                    par_before: parOrArea.par.id,
                }
            )
        );
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(r.result.data, {
            type: EditType.AddAbove,
            par: parOrArea,
        });
        this.deleteFromSource(parOrArea);
    }

    async moveBelow(e: MouseEvent, parOrArea: ParContext) {
        const r = await to(
            $http.post<IParResponse>(
                "/clipboard/paste/" + this.viewctrl.docId,
                {
                    par_after: parOrArea.par.id,
                }
            )
        );
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(r.result.data, {
            type: EditType.AddBelow,
            par: parOrArea,
        });
        this.deleteFromSource(parOrArea);
    }

    async pasteAbove(
        e: MouseEvent,
        parOrArea: ParContext | undefined,
        asRef: boolean
    ) {
        const r = await to(
            $http.post<IParResponse>(
                "/clipboard/paste/" + this.viewctrl.docId,
                {
                    par_before: parOrArea ? parOrArea.originalPar.id : null,
                    as_ref: asRef,
                }
            )
        );
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(
            r.result.data,
            parOrArea
                ? {
                      type: EditType.AddAbove,
                      par: parOrArea,
                  }
                : {type: EditType.AddBottom}
        );
    }

    async pasteBelow(e: MouseEvent, parOrArea: ParContext, asRef: boolean) {
        const r = await to(
            $http.post<IParResponse>(
                "/clipboard/paste/" + this.viewctrl.docId,
                {
                    par_after: parOrArea.originalPar.id,
                    as_ref: asRef,
                }
            )
        );
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(r.result.data, {
            type: EditType.AddBelow,
            par: parOrArea,
        });
    }

    async updateClipboardStatus() {
        if (!Users.isLoggedIn()) {
            this.viewctrl.clipMeta.allowPasteContent = false;
            this.viewctrl.clipMeta.allowPasteRef = false;
            return;
        }

        const r = await to(
            $http.get<ClipboardMetaResponse>("/clipboardstatus", {})
        );
        if (!r.ok) {
            this.viewctrl.clipMeta.allowPasteContent = false;
            this.viewctrl.clipMeta.allowPasteRef = false;
            return;
        }
        if (r.result.data.empty) {
            this.viewctrl.clipMeta = {
                allowPasteContent: false,
                allowPasteRef: false,
                empty: true,
            };
        } else {
            this.viewctrl.clipMeta = {
                allowPasteContent: !r.result.data.disable_content,
                allowPasteRef: !r.result.data.disable_ref,
                area_name: r.result.data.area_name,
                empty: false,
                last_action: r.result.data.last_action,
            };
        }
    }

    async getPasteFunctions(par: ParContext): Promise<MenuFunctionList> {
        await this.updateClipboardStatus();
        const {refAreaContainment} = getContextualAreaInfo(par);
        const isOutside = refAreaContainment === ParAreaInclusionKind.Outside;
        const isStart = refAreaContainment === ParAreaInclusionKind.AtStart;
        const isEnd = refAreaContainment === ParAreaInclusionKind.AtEnd;
        return [
            {
                func: (e) => this.pasteRefAbove(e, par),
                desc: "Above, as a reference",
                show:
                    this.viewctrl.clipMeta.allowPasteRef &&
                    (isStart || isOutside),
            },
            {
                func: (e) => this.pasteContentAbove(e, par),
                desc: "Above, as content",
                show:
                    this.viewctrl.clipMeta.allowPasteContent &&
                    (isStart || isOutside),
            },
            {
                func: (e) => this.pasteRefBelow(e, par),
                desc: "Below, as a reference",
                show:
                    this.viewctrl.clipMeta.allowPasteRef &&
                    (isEnd || isOutside),
            },
            {
                func: (e) => this.pasteContentBelow(e, par),
                desc: "Below, as content",
                show:
                    this.viewctrl.clipMeta.allowPasteContent &&
                    (isEnd || isOutside),
            },
            {
                func: (e) => {},
                desc: "Cancel",
                show: true,
            },
        ];
    }

    getMoveFunctions(par: ParContext): MenuFunctionList {
        return [
            {
                func: (e) => this.moveAbove(e, par),
                desc: "Above",
                show: this.viewctrl.clipMeta.allowPasteContent,
            },
            {
                func: (e) => this.moveBelow(e, par),
                desc: "Below",
                show: this.viewctrl.clipMeta.allowPasteContent,
            },
            {func: empty, desc: "Cancel", show: true},
        ];
    }

    async cutPar(e: MouseEvent, par: ParContext) {
        if (!par.isDeletableOnItsOwn()) {
            showMessageDialog(
                "Can't delete this paragraph on its own. " +
                    "It's either the beginning or end of an area, " +
                    "or it's inside an area reference."
            );
            return;
        }
        const sel = getMinimalUnbrokenSelection(par, par);
        if (sel.hasMultiple()) {
            showMessageDialog(
                "Error cutting par: Unbroken selection shouldn't have had multiple pars."
            );
            return;
        }
        const docParId = [this.viewctrl.docId, par.originalPar.id];

        const r = await to(
            $http.post<IClipBoardResponse>(
                "/clipboard/cut/" +
                    docParId[0] +
                    "/" +
                    docParId[1] +
                    "/" +
                    docParId[1],
                {}
            )
        );
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }
        const pars = r.result.data.pars;
        if (pars.length > 0) {
            this.viewctrl.editingHandler.handleDelete({
                pars: sel,
                type: EditType.Edit,
            });
        }
    }

    async copyPar(e: MouseEvent, par: ParContext) {
        const docParId = [par.par.docId, par.par.id];
        const r = await to(
            $http.post(
                "/clipboard/copy/" +
                    docParId[0] +
                    "/" +
                    docParId[1] +
                    "/" +
                    docParId[1],
                {}
            )
        );
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }
    }

    async copyOrCutArea(e: MouseEvent, unb: UnbrokenSelection, cut: boolean) {
        const areaStart = unb.start.originalPar.id;
        const areaEnd = unb.end.originalPar.id;
        const docId = this.viewctrl.docId;

        if (cut) {
            const r = await to(
                $http.post<IClipBoardResponse>(
                    `/clipboard/cut/${docId}/${areaStart}/${areaEnd}`,
                    {}
                )
            );
            if (!r.ok) {
                await showMessageDialog(r.result.data.error);
                return;
            }

            this.viewctrl.editingHandler.handleDelete({
                pars: unb,
                type: EditType.Edit,
            });
            this.viewctrl.editingHandler.setSelection(undefined);
        } else {
            const r = await to(
                $http.post(
                    `/clipboard/copy/${docId}/${areaStart}/${areaEnd}`,
                    {}
                )
            );
            if (!r.ok) {
                await showMessageDialog(r.result.data.error);
                return;
            }
            this.viewctrl.editingHandler.setSelection(undefined);
        }
    }

    cutSelection(e: MouseEvent, parOrArea: UnbrokenSelection) {
        this.copyOrCutArea(e, parOrArea, true);
    }

    copySelection(e: MouseEvent, parOrArea: UnbrokenSelection) {
        this.copyOrCutArea(e, parOrArea, false);
    }
}
