
export function defineClipboard(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {

    sc.showPasteMenu = function(e, $par_or_area, coords) {
        sc.pasteFunctions = sc.getPasteFunctions();
        sc.showPopupMenu(e, $par_or_area, coords, {actions: "pasteFunctions", contenturl: "/clipboard"});
    };

    sc.showMoveMenu = function(e, $par_or_area, coords) {
        sc.pasteFunctions = sc.getMoveFunctions();
        sc.showPopupMenu(e, $par_or_area, coords, {actions: "pasteFunctions", contenturl: "/clipboard"});
    };

    sc.pasteContentAbove = function(e, $par) {
        sc.pasteAbove(e, $par, false);
    };

    sc.pasteRefAbove = function(e, $par) {
        sc.pasteAbove(e, $par, true);
    };

    sc.pasteContentBelow = function(e, $par) {
        sc.pasteBelow(e, $par, false);
    };

    sc.pasteRefBelow = function(e, $par) {
        sc.pasteBelow(e, $par, true);
    };

    sc.deleteFromSource = function() {
        http.post("/clipboard/deletesrc/" + sc.docId, {}).success(function(data, status, headers, config) {
            const doc_ver = data.doc_ver;
            const pars = data.pars;
            if (pars.length > 0) {
                const first_par = pars[0].id;
                const last_par = pars[pars.length - 1].id;
                sc.handleDelete({version: doc_ver}, {par: first_par, area_start: first_par, area_end: last_par});
            }

            sc.allowPasteContent = false;
            sc.allowPasteRef = false;
        }).error(function(data, status, headers, config) {
            $window.alert(data.error);
        });
    };

    sc.moveAbove = function(e, $par_or_area) {
        http.post("/clipboard/paste/" + sc.docId, {
            par_before: sc.getFirstParId($par_or_area),
        }).success(function(data, status, headers, config) {
            if (data === null)
                return;

            const $newpar = sc.createNewPar();
            $par_or_area.before($newpar);

            const extra_data = {
                docId: sc.docId, // current document id
                par: sc.getFirstParId($newpar), // the id of paragraph on which the editor was opened
                par_next: $par_or_area.id, // the id of the paragraph that follows par
            };

            sc.addSavedParToDom(data, extra_data);
            sc.deleteFromSource();

        }).error(function(data, status, headers, config) {
            $window.alert(data.error);
        });
    };

    sc.moveBelow = function(e, $par_or_area) {
        http.post("/clipboard/paste/" + sc.docId, {
            par_after: sc.getLastParId($par_or_area),
        }).success(function(data, status, headers, config) {
            if (data === null)
                return;

            const $newpar = sc.createNewPar();
            $par_or_area.after($newpar);

            const extra_data = {
                docId: sc.docId, // current document id
                par: sc.getFirstParId($newpar), // the id of paragraph on which the editor was opened
                par_next: $par_or_area.id, // the id of the paragraph that follows par
            };

            sc.addSavedParToDom(data, extra_data);
            sc.deleteFromSource();

        }).error(function(data, status, headers, config) {
            $window.alert(data.error);
        });
    };

    sc.pasteAbove = function(e, $par_or_area, as_ref) {
        http.post("/clipboard/paste/" + sc.docId, {
            par_before: sc.getFirstParId($par_or_area),
            as_ref,
        }).success(function(data, status, headers, config) {
            if (data === null)
                return;

            const $newpar = sc.createNewPar();
            $par_or_area.before($newpar);

            const extra_data = {
                docId: sc.docId, // current document id
                par: sc.getFirstParId($newpar), // the id of paragraph on which the editor was opened
                par_next: $par_or_area.id, // the id of the paragraph that follows par
            };

            sc.addSavedParToDom(data, extra_data);

        }).error(function(data, status, headers, config) {
            $window.alert(data.error);
        });
    };

    sc.pasteBelow = function(e, $par_or_area, as_ref) {
        http.post("/clipboard/paste/" + sc.docId, {
            par_after: sc.getLastParId($par_or_area),
            as_ref,
        }).success(function(data, status, headers, config) {
            if (data === null)
                return;

            const $newpar = sc.createNewPar();
            $par_or_area.after($newpar);

            const extra_data = {
                docId: sc.docId, // current document id
                par: sc.getFirstParId($newpar), // the id of paragraph on which the editor was opened
                par_next: $par_or_area.id, // the id of the paragraph that follows par
            };

            sc.addSavedParToDom(data, extra_data);

        }).error(function(data, status, headers, config) {
            $window.alert(data.error);
        });
    };

    sc.updateClipboardStatus = function() {
        http.get("/clipboardstatus", {}).success(function(data, status, headers, config) {
            if (!("empty" in data) || data.empty) {
                sc.allowPasteContent = false;
                sc.allowPasteRef = false;
            } else {
                sc.allowPasteContent = true;
                sc.allowPasteRef = !("disable_ref" in data && data.disable_ref);
            }
        }).error(function(data, status, headers, config) {
            sc.allowPasteContent = false;
            sc.allowPasteRef = false;
        });
    };

    sc.getPasteFunctions = function() {
        sc.updateClipboardStatus();
        return [
            {func: sc.pasteRefAbove, desc: "Above, as a reference", show: sc.allowPasteRef},
            {func: sc.pasteContentAbove, desc: "Above, as content", show: sc.allowPasteContent},
            {func: sc.pasteRefBelow, desc: "Below, as a reference", show: sc.allowPasteRef},
            {func: sc.pasteContentBelow, desc: "Below, as content", show: sc.allowPasteContent},
            {func: sc.nothing, desc: "Cancel", show: true},
        ];
    };

    sc.getMoveFunctions = function() {
        return [
            {func: sc.moveAbove, desc: "Above", show: sc.allowPasteContent},
            {func: sc.moveBelow, desc: "Below", show: sc.allowPasteContent},
            {func: sc.nothing, desc: "Cancel", show: true},
        ];
    };

    sc.cutPar = function(e, $par) {
        const doc_par_id = [sc.docId, $par.attr("id")];

        http.post("/clipboard/cut/" + doc_par_id[0] + "/" + doc_par_id[1] + "/" + doc_par_id[1], {}).success(function(data, status, headers, config) {
            const doc_ver = data.doc_ver;
            const pars = data.pars;
            if (pars.length > 0) {
                const first_par = pars[0].id;
                const last_par = pars[pars.length - 1].id;
                sc.handleDelete({version: doc_ver}, {par: first_par, area_start: first_par, area_end: last_par});
            }

            sc.allowPasteContent = true;
            sc.allowPasteRef = false;
        }).error(function(data, status, headers, config) {
            $window.alert(data.error);
        });
    };

    sc.copyPar = function(e, $par) {
        const doc_par_id = sc.dereferencePar($par);

        http.post("/clipboard/copy/" + doc_par_id[0] + "/" + doc_par_id[1] + "/" + doc_par_id[1], {}).success(function(data, status, headers, config) {
            sc.allowPasteContent = true;
            sc.allowPasteRef = true;
        }).error(function(data, status, headers, config) {
            $window.alert(data.error);
        });
    };

    sc.copyArea = function(e, $par_or_area, override_doc_id, cut) {
        let ref_doc_id, area_name, area_start, area_end;

        if ($window.editMode === "area") {
            ref_doc_id = sc.getAreaDocId($par_or_area);
            area_name = sc.getAreaId($par_or_area);
            area_start = sc.getFirstParId($par_or_area);
            area_end = sc.getLastParId($par_or_area);
        } else {
            ref_doc_id = null;
            area_name = null;
            area_start = sc.getParId(sc.selection.start);
            area_end = sc.getParId(sc.selection.end);
        }

        const doc_id = override_doc_id ? override_doc_id : sc.docId;

        if (cut) {
            http.post("/clipboard/cut/" + doc_id + "/" + area_start + "/" + area_end, {
                area_name,
            }).success(function(data, status, headers, config) {
                sc.selection.start = null;
                sc.selection.end = null;

                if (doc_id === sc.docId) {
                    const doc_ver = data.doc_ver;
                    const pars = data.pars;
                    if (pars.length > 0) {
                        const first_par = pars[0].id;
                        const last_par = pars[pars.length - 1].id;
                        sc.handleDelete({version: doc_ver}, {
                            par: first_par,
                            area_start: first_par,
                            area_end: last_par,
                        });

                        sc.allowPasteContent = true;
                        sc.allowPasteRef = false;
                    }
                }
            }).error(function(data, status, headers, config) {
                $window.alert(data.error);
            });

        } else {
            http.post("/clipboard/copy/" + doc_id + "/" + area_start + "/" + area_end, {
                ref_doc_id,
                area_name,
            }).success(function(data, status, headers, config) {
                sc.selection.start = null;
                sc.selection.end = null;
                sc.allowPasteContent = true;
                sc.allowPasteRef = true;
            }).error(function(data, status, headers, config) {
                $window.alert(data.error);
            });
        }
    };

    sc.cutArea = function(e, $par_or_area, cut) {
        sc.copyArea(e, $par_or_area, sc.docId, true);
    };

    sc.allowPasteContent = true;
    sc.allowPasteRef = true;

    sc.pasteFunctions = sc.getPasteFunctions();
}
