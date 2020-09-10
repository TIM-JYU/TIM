import {IScope} from "angular";
import {showMessageDialog} from "../../ui/dialog";
import {Users} from "../../user/userService";
import {documentglobals} from "../../util/globals";
import {$http} from "../../util/ngimport";
import {empty, to} from "../../util/utils";
import {
    dereferencePar,
    getAreaDocId,
    getAreaId,
    getElementByParId,
    getFirstParId,
    getLastParId,
    getParId,
    getPars,
    Paragraph,
    ParOrArea,
} from "../parhelpers";
import {ViewCtrl} from "../viewctrl";
import {EditType, IParResponse} from "./edittypes";

export type ClipboardMetaResponse = {
    area_name?: string;
    disable_ref: boolean;
    disable_content: boolean;
    last_action?: "copy" | "cut" | "paste";
    empty: false;
} | {empty: true};

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

    async showPasteMenu(e: JQuery.MouseEventBase, parOrArea: ParOrArea) {
        this.viewctrl.parmenuHandler.showPopupMenu(e, parOrArea, {
            actions: await this.getPasteFunctions(),
            contenturl: "/clipboard",
            editbutton: false,
            save: false,
        });
    }

    showMoveMenu(e: JQuery.MouseEventBase, parOrArea: ParOrArea) {
        this.viewctrl.parmenuHandler.showPopupMenu(e, parOrArea, {
            actions: this.getMoveFunctions(),
            contenturl: "/clipboard",
            editbutton: false,
            save: false,
        });
    }

    pasteContentAbove(e: JQuery.Event, par: Paragraph) {
        this.pasteAbove(e, par, false);
    }

    pasteRefAbove(e: JQuery.Event, par: Paragraph) {
        this.pasteAbove(e, par, true);
    }

    pasteContentBelow(e: JQuery.Event, par: Paragraph) {
        this.pasteBelow(e, par, false);
    }

    pasteRefBelow(e: JQuery.Event, par: Paragraph) {
        this.pasteBelow(e, par, true);
    }

    async deleteFromSource() {
        const r = await to($http.post<IClipBoardResponse>("/clipboard/deletesrc/" + this.viewctrl.docId, {}));
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }
        const pars = r.result.data.pars;
        if (pars.length > 0) {
            const firstPar = pars[0].id;
            const lastPar = pars[pars.length - 1].id;
            this.viewctrl.editingHandler.handleDelete({
                pars: getPars(getElementByParId(firstPar), getElementByParId(lastPar)),
                type: EditType.Edit,
            });
        }
    }

    async moveAbove(e: JQuery.Event, parOrArea: ParOrArea) {

        const r = await to($http.post<IParResponse>("/clipboard/paste/" + this.viewctrl.docId, {
            par_before: getFirstParId(parOrArea),
        }));
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(r.result.data, {type: EditType.AddAbove, par: parOrArea});
        this.deleteFromSource();
    }

    async moveBelow(e: JQuery.Event, parOrArea: ParOrArea) {

        const r = await to($http.post<IParResponse>("/clipboard/paste/" + this.viewctrl.docId, {
            par_after: getLastParId(parOrArea),
        }));
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(r.result.data, {type: EditType.AddBelow, par: parOrArea});
        this.deleteFromSource();
    }

    async pasteAbove(e: JQuery.Event, parOrArea: ParOrArea | undefined, asRef: boolean) {

        const r = await to($http.post<IParResponse>("/clipboard/paste/" + this.viewctrl.docId, {
            par_before: parOrArea ? getFirstParId(parOrArea) : null,
            as_ref: asRef,
        }));
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(r.result.data, parOrArea ? {
            type: EditType.AddAbove,
            par: parOrArea,
        } : {type: EditType.AddBottom});
    }

    async pasteBelow(e: JQuery.Event, parOrArea: ParOrArea, asRef: boolean) {
        const r = await to($http.post<IParResponse>("/clipboard/paste/" + this.viewctrl.docId, {
            par_after: getLastParId(parOrArea),
            as_ref: asRef,
        }));
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(r.result.data, {type: EditType.AddBelow, par: parOrArea});
    }

    async updateClipboardStatus() {
        if (!Users.isLoggedIn()) {
            this.viewctrl.clipMeta.allowPasteContent = false;
            this.viewctrl.clipMeta.allowPasteRef = false;
            return;
        }

        const r = await to($http.get<ClipboardMetaResponse>("/clipboardstatus", {}));
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

    async getPasteFunctions() {
        await this.updateClipboardStatus();
        return [
            {
                func: (e: JQuery.Event, p: Paragraph) => this.pasteRefAbove(e, p),
                desc: "Above, as a reference",
                show: this.viewctrl.clipMeta.allowPasteRef,
            },
            {
                func: (e: JQuery.Event, p: Paragraph) => this.pasteContentAbove(e, p),
                desc: "Above, as content",
                show: this.viewctrl.clipMeta.allowPasteContent,
            },
            {
                func: (e: JQuery.Event, p: Paragraph) => this.pasteRefBelow(e, p),
                desc: "Below, as a reference",
                show: this.viewctrl.clipMeta.allowPasteRef,
            },
            {
                func: (e: JQuery.Event, p: Paragraph) => this.pasteContentBelow(e, p),
                desc: "Below, as content",
                show: this.viewctrl.clipMeta.allowPasteContent,
            },
            {
                func: (e: JQuery.Event, p: Paragraph) => {
                }, desc: "Cancel", show: true,
            },
        ];
    }

    getMoveFunctions() {
        return [
            {
                func: (e: JQuery.Event, p: Paragraph) => this.moveAbove(e, p),
                desc: "Above",
                show: this.viewctrl.clipMeta.allowPasteContent,
            },
            {
                func: (e: JQuery.Event, p: Paragraph) => this.moveBelow(e, p),
                desc: "Below",
                show: this.viewctrl.clipMeta.allowPasteContent,
            },
            {func: empty, desc: "Cancel", show: true},
        ];
    }

    async cutPar(e: JQuery.Event, par: Paragraph) {
        const docParId = [this.viewctrl.docId, par.attr("id")];

        const r = await to($http.post<IClipBoardResponse>("/clipboard/cut/" + docParId[0] + "/" + docParId[1] + "/" + docParId[1], {}));
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }
        const pars = r.result.data.pars;
        if (pars.length > 0) {
            const firstPar = pars[0].id;
            const lastPar = pars[pars.length - 1].id;
            this.viewctrl.editingHandler.handleDelete({
                pars: getPars(getElementByParId(firstPar), getElementByParId(lastPar)),
                type: EditType.Edit,
            });
        }
    }

    async copyPar(e: JQuery.Event, par: Paragraph) {
        const docParId = dereferencePar(par);
        if (!docParId) {
            return;
        }

        const r = await to($http.post("/clipboard/copy/" + docParId[0] + "/" + docParId[1] + "/" + docParId[1], {}));
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }
    }

    async copyOrCutArea(e: JQuery.Event, parOrArea: ParOrArea, overrideDocId: number, cut: boolean) {
        let refDocId;
        let areaName;
        let areaStart;
        let areaEnd;

        if (documentglobals().editMode === "area") {
            refDocId = getAreaDocId(parOrArea);
            areaName = getAreaId(parOrArea);
            areaStart = getFirstParId(parOrArea);
            areaEnd = getLastParId(parOrArea);
        } else {
            refDocId = null;
            areaName = null;
            areaStart = getParId(this.viewctrl.selection.start);
            areaEnd = getParId(this.viewctrl.selection.end);
        }

        const docId = overrideDocId ? overrideDocId : this.viewctrl.docId;

        if (cut) {

            const r = await to($http.post<IClipBoardResponse>("/clipboard/cut/" + docId + "/" + areaStart + "/" + areaEnd, {
                area_name: areaName,
            }));
            if (!r.ok) {
                await showMessageDialog(r.result.data.error);
                return;
            }
            this.viewctrl.selection.start = undefined;
            this.viewctrl.selection.end = undefined;

            if (docId === this.viewctrl.docId) {
                const pars = r.result.data.pars;
                if (pars.length > 0) {
                    const firstPar = pars[0].id;
                    const lastPar = pars[pars.length - 1].id;
                    this.viewctrl.editingHandler.handleDelete({
                        pars: getPars(getElementByParId(firstPar), getElementByParId(lastPar)),
                        type: EditType.Edit,
                    });
                }
            }

        } else {
            const r = await to($http.post("/clipboard/copy/" + docId + "/" + areaStart + "/" + areaEnd, {
                refDocId,
                areaName,
            }));
            if (!r.ok) {
                await showMessageDialog(r.result.data.error);
                return;
            }
            this.viewctrl.selection.start = undefined;
            this.viewctrl.selection.end = undefined;
        }
    }

    cutArea(e: JQuery.Event, parOrArea: ParOrArea) {
        this.copyOrCutArea(e, parOrArea, this.viewctrl.docId, true);
    }

    copyArea(e: JQuery.Event, parOrArea: ParOrArea) {
        this.copyOrCutArea(e, parOrArea, this.viewctrl.docId, false);
    }
}
