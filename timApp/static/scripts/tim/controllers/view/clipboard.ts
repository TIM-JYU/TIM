import {IScope} from "angular";
import {IParResponse} from "../../edittypes";
import {$http, $window} from "../../ngimport";
import {Users} from "../../services/userService";
import {Coords, empty} from "../../utils";
import {
    createNewPar,
    dereferencePar,
    getAreaDocId,
    getAreaId, getElementByParId,
    getFirstParId,
    getLastParId,
    getParId, getPars,
    Paragraph,
    ParOrArea,
} from "./parhelpers";
import {ViewCtrl} from "./viewctrl";
import {getEmptyCoords, viewCtrlDot} from "./viewutils";
import {EditType} from './editing';

export class ClipboardHandler {
    public sc: IScope;
    public viewctrl: ViewCtrl;

    constructor(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        view.allowPasteContent = false;
        view.allowPasteRef = false;

        view.pasteFunctions = this.getPasteFunctions();
    }

    showPasteMenu(e: Event, $parOrArea: ParOrArea, coords?: Coords) {
        this.viewctrl.pasteFunctions = this.getPasteFunctions();
        this.viewctrl.parmenuHandler.showPopupMenu(e, $parOrArea, coords || getEmptyCoords(), {
            actions: viewCtrlDot("pasteFunctions"),
            contenturl: "/clipboard",
            save: false
        });
    }

    showMoveMenu(e: Event, $parOrArea: ParOrArea, coords?: Coords) {
        this.viewctrl.pasteFunctions = this.getMoveFunctions();
        this.viewctrl.parmenuHandler.showPopupMenu(e, $parOrArea, coords || getEmptyCoords(), {
            actions: viewCtrlDot("pasteFunctions"),
            contenturl: "/clipboard",
            save: false
        });
    }

    pasteContentAbove(e: Event, $par: Paragraph) {
        this.pasteAbove(e, $par, false);
    }

    pasteRefAbove(e: Event, $par: Paragraph) {
        this.pasteAbove(e, $par, true);
    }

    pasteContentBelow(e: Event, $par: Paragraph) {
        this.pasteBelow(e, $par, false);
    }

    pasteRefBelow(e: Event, $par: Paragraph) {
        this.pasteBelow(e, $par, true);
    }

    async deleteFromSource() {
        try {
            var response = await $http.post<{doc_ver: any, pars: any[]}>("/clipboard/deletesrc/" + this.viewctrl.docId, {});
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        const doc_ver = response.data.doc_ver;
        const pars = response.data.pars;
        if (pars.length > 0) {
            const firstPar = pars[0].id;
            const lastPar = pars[pars.length - 1].id;
            this.viewctrl.editingHandler.handleDelete({
                pars: getPars(getElementByParId(firstPar), getElementByParId(lastPar)),
                type: EditType.Edit,
            });
        }

        this.viewctrl.allowPasteContent = false;
        this.viewctrl.allowPasteRef = false;
    }

    async moveAbove(e: Event, $parOrArea: ParOrArea) {
        try {
            var response = await $http.post<IParResponse>("/clipboard/paste/" + this.viewctrl.docId, {
                par_before: getFirstParId($parOrArea),
            });
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        if (response.data == null) {
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(response.data, {type: EditType.AddAbove, par: $parOrArea});
        this.deleteFromSource();
    }

    async moveBelow(e: Event, $parOrArea: ParOrArea) {
        try {
            var response = await $http.post<IParResponse>("/clipboard/paste/" + this.viewctrl.docId, {
                par_after: getLastParId($parOrArea),
            });
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        if (response.data == null) {
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(response.data, {type: EditType.AddBelow, par: $parOrArea});
        this.deleteFromSource();
    }

    async pasteAbove(e: Event, $parOrArea: ParOrArea | undefined, asRef: boolean) {
        try {
            var response = await $http.post<IParResponse>("/clipboard/paste/" + this.viewctrl.docId, {
                par_before: $parOrArea ? getFirstParId($parOrArea) : null,
                as_ref: asRef,
            });
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        if (response.data == null) {
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(response.data, $parOrArea ? {type: EditType.AddAbove, par: $parOrArea} : {type: EditType.AddBottom});
    }

    async pasteBelow(e: Event, $parOrArea: ParOrArea, asRef: boolean) {
        try {
            var response = await $http.post<IParResponse>("/clipboard/paste/" + this.viewctrl.docId, {
                par_after: getLastParId($parOrArea),
                as_ref: asRef,
            });
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        if (response.data == null) {
            return;
        }

        this.viewctrl.editingHandler.addSavedParToDom(response.data, {type: EditType.AddBelow, par: $parOrArea});
    }

    async updateClipboardStatus() {
        if (!Users.isLoggedIn()) {
            this.viewctrl.allowPasteContent = false;
            this.viewctrl.allowPasteRef = false;
            return;
        }
        try {
            var response = await $http.get<{empty: any, disable_ref: any}>("/clipboardstatus", {});
        } catch (e) {
            this.viewctrl.allowPasteContent = false;
            this.viewctrl.allowPasteRef = false;
            return;
        }
        if (!("empty" in response.data) || response.data.empty) {
            this.viewctrl.allowPasteContent = false;
            this.viewctrl.allowPasteRef = false;
        } else {
            this.viewctrl.allowPasteContent = true;
            this.viewctrl.allowPasteRef = !("disable_ref" in response.data && response.data.disable_ref);
        }
    }

    getPasteFunctions() {
        this.updateClipboardStatus();
        return [
            {
                func: (e: Event, $par: Paragraph) => this.pasteRefAbove(e, $par),
                desc: "Above, as a reference",
                show: this.viewctrl.allowPasteRef
            },
            {
                func: (e: Event, $par: Paragraph) => this.pasteContentAbove(e, $par),
                desc: "Above, as content",
                show: this.viewctrl.allowPasteContent
            },
            {
                func: (e: Event, $par: Paragraph) => this.pasteRefBelow(e, $par),
                desc: "Below, as a reference",
                show: this.viewctrl.allowPasteRef
            },
            {
                func: (e: Event, $par: Paragraph) => this.pasteContentBelow(e, $par),
                desc: "Below, as content",
                show: this.viewctrl.allowPasteContent
            },
            {func: (e: Event, $par: Paragraph) => {}, desc: "Cancel", show: true},
        ];
    }

    getMoveFunctions() {
        return [
            {
                func: (e: Event, $par: Paragraph) => this.moveAbove(e, $par),
                desc: "Above",
                show: this.viewctrl.allowPasteContent
            },
            {
                func: (e: Event, $par: Paragraph) => this.moveBelow(e, $par),
                desc: "Below",
                show: this.viewctrl.allowPasteContent
            },
            {func: empty, desc: "Cancel", show: true},
        ];
    }

    async cutPar(e: Event, $par: Paragraph) {
        const docParId = [this.viewctrl.docId, $par.attr("id")];

        try {
            var response = await $http.post<{doc_ver: any, pars: any[]}>("/clipboard/cut/" + docParId[0] + "/" + docParId[1] + "/" + docParId[1], {});
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        const doc_ver = response.data.doc_ver;
        const pars = response.data.pars;
        if (pars.length > 0) {
            const firstPar = pars[0].id;
            const lastPar = pars[pars.length - 1].id;
            this.viewctrl.editingHandler.handleDelete({
                pars: getPars(getElementByParId(firstPar), getElementByParId(lastPar)),
                type: EditType.Edit,
            });
        }

        this.viewctrl.allowPasteContent = true;
        this.viewctrl.allowPasteRef = false;
    }

    async copyPar(e: Event, $par: Paragraph) {
        const docParId = dereferencePar($par);
        if (!docParId) {
            return;
        }

        try {
            await $http.post("/clipboard/copy/" + docParId[0] + "/" + docParId[1] + "/" + docParId[1], {});
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        this.viewctrl.allowPasteContent = true;
        this.viewctrl.allowPasteRef = true;
    }

    async copyOrCutArea(e: Event, $parOrArea: ParOrArea, overrideDocId: number, cut: boolean) {
        let refDocId;
        let areaName;
        let areaStart;
        let areaEnd;

        if ($window.editMode === "area") {
            refDocId = getAreaDocId($parOrArea);
            areaName = getAreaId($parOrArea);
            areaStart = getFirstParId($parOrArea);
            areaEnd = getLastParId($parOrArea);
        } else {
            refDocId = null;
            areaName = null;
            areaStart = getParId(this.viewctrl.selection.start);
            areaEnd = getParId(this.viewctrl.selection.end);
        }

        const docId = overrideDocId ? overrideDocId : this.viewctrl.docId;

        if (cut) {
            try {
                var response = await $http.post<{doc_ver: any, pars: any[]}>("/clipboard/cut/" + docId + "/" + areaStart + "/" + areaEnd, {
                    area_name: areaName,
                });
            } catch (e) {
                $window.alert(e.data.error);
                return;
            }
            this.viewctrl.selection.start = null;
            this.viewctrl.selection.end = null;

            if (docId === this.viewctrl.docId) {
                const doc_ver = response.data.doc_ver;
                const pars = response.data.pars;
                if (pars.length > 0) {
                    const firstPar = pars[0].id;
                    const lastPar = pars[pars.length - 1].id;
                    this.viewctrl.editingHandler.handleDelete({
                        pars: getPars(getElementByParId(firstPar), getElementByParId(lastPar)),
                        type: EditType.Edit,
                    });

                    this.viewctrl.allowPasteContent = true;
                    this.viewctrl.allowPasteRef = false;
                }
            }

        } else {
            try {
                await $http.post("/clipboard/copy/" + docId + "/" + areaStart + "/" + areaEnd, {
                    refDocId,
                    areaName,
                });
            } catch (e) {
                $window.alert(e.data.error);
                return;
            }
            this.viewctrl.selection.start = null;
            this.viewctrl.selection.end = null;
            this.viewctrl.allowPasteContent = true;
            this.viewctrl.allowPasteRef = true;
        }
    }

    cutArea(e: Event, $parOrArea: ParOrArea) {
        this.copyOrCutArea(e, $parOrArea, this.viewctrl.docId, true);
    }

    copyArea(e: Event, $parOrArea: ParOrArea) {
        this.copyOrCutArea(e, $parOrArea, this.viewctrl.docId, false);
    }
}
