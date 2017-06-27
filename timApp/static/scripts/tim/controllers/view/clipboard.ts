import {$http, $window} from "../../ngimport";
import {
    createNewPar, dereferencePar, getAreaDocId, getAreaId, getFirstParId, getLastParId,
    getParId,
} from "./parhelpers";
import {Users} from "../../services/userService";
import {IScope} from "angular";
import {ViewCtrl} from "./ViewCtrl";

export class ClipboardHandler {
    public sc: IScope;
    public viewctrl: ViewCtrl;

    initClipboard(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        view.allowPasteContent = false;
        view.allowPasteRef = false;

        view.pasteFunctions = this.getPasteFunctions();
    }

    showPasteMenu(e, $parOrArea, coords) {
        this.viewctrl.pasteFunctions = this.getPasteFunctions();
        this.viewctrl.showPopupMenu(e, $parOrArea, coords, {actions: "pasteFunctions", contenturl: "/clipboard"});
    }

    showMoveMenu(e, $parOrArea, coords) {
        this.viewctrl.pasteFunctions = this.getMoveFunctions();
        this.viewctrl.showPopupMenu(e, $parOrArea, coords, {actions: "pasteFunctions", contenturl: "/clipboard"});
    }

    pasteContentAbove(e, $par) {
        this.pasteAbove(e, $par, false);
    }

    pasteRefAbove(e, $par) {
        this.pasteAbove(e, $par, true);
    }

    pasteContentBelow(e, $par) {
        this.pasteBelow(e, $par, false);
    }

    pasteRefBelow(e, $par) {
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
            this.viewctrl.handleDelete({version: doc_ver}, {par: firstPar, area_start: firstPar, area_end: lastPar});
        }

        this.viewctrl.allowPasteContent = false;
        this.viewctrl.allowPasteRef = false;
    }

    async moveAbove(e, $parOrArea) {
        try {
            var response = await $http.post("/clipboard/paste/" + this.viewctrl.docId, {
                par_before: getFirstParId($parOrArea),
            });
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        if (response.data === null) {
            return;
        }

        const $newpar = createNewPar();
        $parOrArea.before($newpar);

        const extraData = {
            docId: this.viewctrl.docId, // current document id
            par: getFirstParId($newpar), // the id of paragraph on which the editor was opened
            par_next: $parOrArea.id, // the id of the paragraph that follows par
        };

        this.viewctrl.addSavedParToDom(response.data, extraData);
        this.deleteFromSource();
    }

    async moveBelow(e, $parOrArea) {
        try {
            var response = await $http.post("/clipboard/paste/" + this.viewctrl.docId, {
                par_after: getLastParId($parOrArea),
            });
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        if (response.data === null) {
            return;
        }

        const $newpar = createNewPar();
        $parOrArea.after($newpar);

        const extraData = {
            docId: this.viewctrl.docId, // current document id
            par: getFirstParId($newpar), // the id of paragraph on which the editor was opened
            par_next: $parOrArea.id, // the id of the paragraph that follows par
        };

        this.viewctrl.addSavedParToDom(response.data, extraData);
        this.deleteFromSource();
    }

    async pasteAbove(e, $parOrArea, asRef) {
        try {
            var response = await $http.post("/clipboard/paste/" + this.viewctrl.docId, {
                par_before: getFirstParId($parOrArea),
                asRef,
            });
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        if (response.data === null) {
            return;
        }

        const $newpar = createNewPar();
        $parOrArea.before($newpar);

        const extraData = {
            docId: this.viewctrl.docId, // current document id
            par: getFirstParId($newpar), // the id of paragraph on which the editor was opened
            par_next: $parOrArea.id, // the id of the paragraph that follows par
        };

        this.viewctrl.addSavedParToDom(response.data, extraData);
    }

    async pasteBelow(e, $parOrArea, asRef) {
        try {
            var response = await $http.post("/clipboard/paste/" + this.viewctrl.docId, {
                par_after: getLastParId($parOrArea),
                asRef,
            });
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        if (response.data === null) {
            return;
        }

        const $newpar = createNewPar();
        $parOrArea.after($newpar);

        const extraData = {
            docId: this.viewctrl.docId, // current document id
            par: getFirstParId($newpar), // the id of paragraph on which the editor was opened
            par_next: $parOrArea.id, // the id of the paragraph that follows par
        };

        this.viewctrl.addSavedParToDom(response.data, extraData);
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
            {func: this.pasteRefAbove, desc: "Above, as a reference", show: this.viewctrl.allowPasteRef},
            {func: this.pasteContentAbove, desc: "Above, as content", show: this.viewctrl.allowPasteContent},
            {func: this.pasteRefBelow, desc: "Below, as a reference", show: this.viewctrl.allowPasteRef},
            {func: this.pasteContentBelow, desc: "Below, as content", show: this.viewctrl.allowPasteContent},
            {func: this.viewctrl.nothing, desc: "Cancel", show: true},
        ];
    }

    getMoveFunctions() {
        return [
            {func: this.moveAbove, desc: "Above", show: this.viewctrl.allowPasteContent},
            {func: this.moveBelow, desc: "Below", show: this.viewctrl.allowPasteContent},
            {func: this.viewctrl.nothing, desc: "Cancel", show: true},
        ];
    }

    async cutPar(e, $par) {
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
            this.viewctrl.handleDelete({version: doc_ver}, {par: firstPar, area_start: firstPar, area_end: lastPar});
        }

        this.viewctrl.allowPasteContent = true;
        this.viewctrl.allowPasteRef = false;
    }

    async copyPar(e, $par) {
        const docParId = dereferencePar($par);

        try {
            await $http.post("/clipboard/copy/" + docParId[0] + "/" + docParId[1] + "/" + docParId[1], {});
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        this.viewctrl.allowPasteContent = true;
        this.viewctrl.allowPasteRef = true;
    }

    async copyArea(e, $parOrArea, overrideDocId, cut) {
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
                    this.viewctrl.handleDelete({version: doc_ver}, {
                        par: firstPar,
                        area_start: firstPar,
                        area_end: lastPar,
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

    cutArea(e, $parOrArea, cut) {
        this.copyArea(e, $parOrArea, this.viewctrl.docId, true);
    }
}
