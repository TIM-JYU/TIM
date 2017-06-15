import {$http, $window} from "../../ngimport";
import {dereferencePar, getAreaDocId, getFirstParId, getLastParId, getParId} from "./parhelpers";

export function defineClipboard(sc) {
    sc.showPasteMenu = (e, $parOrArea, coords) => {
        sc.pasteFunctions = sc.getPasteFunctions();
        sc.showPopupMenu(e, $parOrArea, coords, {actions: "pasteFunctions", contenturl: "/clipboard"});
    };

    sc.showMoveMenu = (e, $parOrArea, coords) => {
        sc.pasteFunctions = sc.getMoveFunctions();
        sc.showPopupMenu(e, $parOrArea, coords, {actions: "pasteFunctions", contenturl: "/clipboard"});
    };

    sc.pasteContentAbove = (e, $par) => {
        sc.pasteAbove(e, $par, false);
    };

    sc.pasteRefAbove = (e, $par) => {
        sc.pasteAbove(e, $par, true);
    };

    sc.pasteContentBelow = (e, $par) => {
        sc.pasteBelow(e, $par, false);
    };

    sc.pasteRefBelow = (e, $par) => {
        sc.pasteBelow(e, $par, true);
    };

    sc.deleteFromSource = async () => {
        try {
            var response = await $http.post<{ doc_ver: any, pars: any[] }>("/clipboard/deletesrc/" + sc.docId, {});
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        const doc_ver = response.data.doc_ver;
        const pars = response.data.pars;
        if (pars.length > 0) {
            const firstPar = pars[0].id;
            const lastPar = pars[pars.length - 1].id;
            sc.handleDelete({version: doc_ver}, {par: firstPar, area_start: firstPar, area_end: lastPar});
        }

        sc.allowPasteContent = false;
        sc.allowPasteRef = false;
    };

    sc.moveAbove = async (e, $parOrArea) => {
        try {
            var response = await $http.post("/clipboard/paste/" + sc.docId, {
                par_before: getFirstParId($parOrArea),
            });
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        if (response.data === null) {
            return;
        }

        const $newpar = sc.createNewPar();
        $parOrArea.before($newpar);

        const extraData = {
            docId: sc.docId, // current document id
            par: getFirstParId($newpar), // the id of paragraph on which the editor was opened
            par_next: $parOrArea.id, // the id of the paragraph that follows par
        };

        sc.addSavedParToDom(response.data, extraData);
        sc.deleteFromSource();
    };

    sc.moveBelow = async (e, $parOrArea) => {
        try {
            var response = await $http.post("/clipboard/paste/" + sc.docId, {
                par_after: getLastParId($parOrArea),
            });
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        if (response.data === null) {
            return;
        }

        const $newpar = sc.createNewPar();
        $parOrArea.after($newpar);

        const extraData = {
            docId: sc.docId, // current document id
            par: getFirstParId($newpar), // the id of paragraph on which the editor was opened
            par_next: $parOrArea.id, // the id of the paragraph that follows par
        };

        sc.addSavedParToDom(response.data, extraData);
        sc.deleteFromSource();
    };

    sc.pasteAbove = async (e, $parOrArea, asRef) => {
        try {
            var response = await $http.post("/clipboard/paste/" + sc.docId, {
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

        const $newpar = sc.createNewPar();
        $parOrArea.before($newpar);

        const extraData = {
            docId: sc.docId, // current document id
            par: getFirstParId($newpar), // the id of paragraph on which the editor was opened
            par_next: $parOrArea.id, // the id of the paragraph that follows par
        };

        sc.addSavedParToDom(response.data, extraData);
    };

    sc.pasteBelow = async (e, $parOrArea, asRef) => {
        try {
            var response = await $http.post("/clipboard/paste/" + sc.docId, {
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

        const $newpar = sc.createNewPar();
        $parOrArea.after($newpar);

        const extraData = {
            docId: sc.docId, // current document id
            par: getFirstParId($newpar), // the id of paragraph on which the editor was opened
            par_next: $parOrArea.id, // the id of the paragraph that follows par
        };

        sc.addSavedParToDom(response.data, extraData);
    };

    sc.updateClipboardStatus = async () => {
        try {
            var response = await $http.get<{ empty: any, disable_ref: any }>("/clipboardstatus", {});
        } catch (e) {
            sc.allowPasteContent = false;
            sc.allowPasteRef = false;
            return;
        }
        if (!("empty" in response.data) || response.data.empty) {
            sc.allowPasteContent = false;
            sc.allowPasteRef = false;
        } else {
            sc.allowPasteContent = true;
            sc.allowPasteRef = !("disable_ref" in response.data && response.data.disable_ref);
        }
    };

    sc.getPasteFunctions = () => {
        sc.updateClipboardStatus();
        return [
            {func: sc.pasteRefAbove, desc: "Above, as a reference", show: sc.allowPasteRef},
            {func: sc.pasteContentAbove, desc: "Above, as content", show: sc.allowPasteContent},
            {func: sc.pasteRefBelow, desc: "Below, as a reference", show: sc.allowPasteRef},
            {func: sc.pasteContentBelow, desc: "Below, as content", show: sc.allowPasteContent},
            {func: sc.nothing, desc: "Cancel", show: true},
        ];
    };

    sc.getMoveFunctions = () => [
        {func: sc.moveAbove, desc: "Above", show: sc.allowPasteContent},
        {func: sc.moveBelow, desc: "Below", show: sc.allowPasteContent},
        {func: sc.nothing, desc: "Cancel", show: true},
    ];

    sc.cutPar = async (e, $par) => {
        const docParId = [sc.docId, $par.attr("id")];

        try {
            var response = await $http.post<{ doc_ver: any, pars: any[] }>("/clipboard/cut/" + docParId[0] + "/" + docParId[1] + "/" + docParId[1], {});
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        const doc_ver = response.data.doc_ver;
        const pars = response.data.pars;
        if (pars.length > 0) {
            const firstPar = pars[0].id;
            const lastPar = pars[pars.length - 1].id;
            sc.handleDelete({version: doc_ver}, {par: firstPar, area_start: firstPar, area_end: lastPar});
        }

        sc.allowPasteContent = true;
        sc.allowPasteRef = false;
    };

    sc.copyPar = async (e, $par) => {
        const docParId = dereferencePar($par);

        try {
            await $http.post("/clipboard/copy/" + docParId[0] + "/" + docParId[1] + "/" + docParId[1], {});
        } catch (e) {
            $window.alert(e.data.error);
            return;
        }
        sc.allowPasteContent = true;
        sc.allowPasteRef = true;
    };

    sc.copyArea = async (e, $parOrArea, overrideDocId, cut) => {
        let refDocId;
        let areaName;
        let areaStart;
        let areaEnd;

        if ($window.editMode === "area") {
            refDocId = getAreaDocId($parOrArea);
            areaName = sc.getAreaId($parOrArea);
            areaStart = getFirstParId($parOrArea);
            areaEnd = getLastParId($parOrArea);
        } else {
            refDocId = null;
            areaName = null;
            areaStart = getParId(sc.selection.start);
            areaEnd = getParId(sc.selection.end);
        }

        const docId = overrideDocId ? overrideDocId : sc.docId;

        if (cut) {
            try {
                var response = await $http.post<{ doc_ver: any, pars: any[] }>("/clipboard/cut/" + docId + "/" + areaStart + "/" + areaEnd, {
                    areaName,
                });
            } catch (e) {
                $window.alert(e.data.error);
                return;
            }
            sc.selection.start = null;
            sc.selection.end = null;

            if (docId === sc.docId) {
                const doc_ver = response.data.doc_ver;
                const pars = response.data.pars;
                if (pars.length > 0) {
                    const firstPar = pars[0].id;
                    const lastPar = pars[pars.length - 1].id;
                    sc.handleDelete({version: doc_ver}, {
                        par: firstPar,
                        area_start: firstPar,
                        area_end: lastPar,
                    });

                    sc.allowPasteContent = true;
                    sc.allowPasteRef = false;
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
            sc.selection.start = null;
            sc.selection.end = null;
            sc.allowPasteContent = true;
            sc.allowPasteRef = true;
        }
    };

    sc.cutArea = (e, $parOrArea, cut) => {
        sc.copyArea(e, $parOrArea, sc.docId, true);
    };

    sc.allowPasteContent = true;
    sc.allowPasteRef = true;

    sc.pasteFunctions = sc.getPasteFunctions();
}
