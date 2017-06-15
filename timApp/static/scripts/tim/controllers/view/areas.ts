import $ from "jquery";
import * as nameArea from "tim/directives/nameArea";
import {markAsUsed} from "tim/utils";
import {$compile, $http, $timeout, $window} from "../../ngimport";
import {getArea, getFirstParId, getLastParId} from "./parhelpers";
import {onClick, onMouseOverOut} from "./eventhandlers";
import {closeOptionsWindow} from "./parmenu";

markAsUsed(nameArea);

function selectArea(areaName, className, selected) {
    const $selection = $(".area.area_" + areaName).children(className);
    if (selected) {
        $selection.addClass("manualhover");
    } else {
        $selection.removeClass("manualhover");
    }
}

export function defineAreas(sc) {
    onMouseOverOut(".areaeditline1", ($this, e, select) => {
        const areaName = $this.attr("data-area");
        selectArea(areaName, ".areaeditline1", select);
    });

    onMouseOverOut(".areaeditline2", ($this, e, select) => {
        const areaName = $this.attr("data-area");
        selectArea(areaName, ".areaeditline2", select);
    });

    onMouseOverOut(".areaeditline3", ($this, e, select) => {
        const areaName = $this.attr("data-area");
        selectArea(areaName, ".areaeditline3", select);
    });

    onClick(".areaeditline1", ($this, e) => sc.onAreaEditClicked($this, e, ".areaeditline1"));

    onClick(".areaeditline2", ($this, e) => sc.onAreaEditClicked($this, e, ".areaeditline2"));

    onClick(".areaeditline3", ($this, e) => sc.onAreaEditClicked($this, e, ".areaeditline3"));

    sc.onAreaEditClicked = ($this, e, className) => {
        closeOptionsWindow();
        const areaName = $this.attr("data-area");
        const $pars = getArea(areaName).find(".par");
        const $areaPart = $this.parent().filter(".area");
        const coords = {left: e.pageX - $areaPart.offset().left, top: e.pageY - $areaPart.offset().top};

        sc.selectedAreaName = areaName;
        $(".area.area_" + areaName).children(className).addClass("menuopen");

        // We need the timeout so we don't trigger the ng-clicks on the buttons
        $timeout(() => {
            sc.showAreaOptionsWindow(e, $areaPart, $pars, coords);
        }, 80);
        return false;
    };

    onClick(".areaexpand, .areacollapse", ($this, e) => {
        if ($(e.target).hasClass("areareadline") ||
            $(e.target).hasClass("readline") ||
            $(e.target).hasClass("editline")) {
            return;
        }
        let expanding = true;
        let newClass = "areacollapse";
        if ($this.hasClass("areacollapse")) {
            expanding = false;
            newClass = "areaexpand";
        }
        $this.removeClass("areaexpand areacollapse");
        const areaName = $this.attr("data-area");
        const toggle = $this.children(".areatoggle");
        toggle.attr("class", "");
        if (expanding) {
            getArea(areaName).removeClass("collapsed");
            $(".areawidget_" + areaName).removeClass("collapsed");
            toggle.attr("class", "areatoggle glyphicon glyphicon-minus");
        } else {
            getArea(areaName).addClass("collapsed");
            $(".areawidget_" + areaName).addClass("collapsed");
            toggle.attr("class", "areatoggle glyphicon glyphicon-plus");
        }

        $this.addClass(newClass);
    });

    sc.showAreaOptionsWindow = (e, $area, $pars, coords) => {
        sc.updateClipboardStatus();
        sc.showPopupMenu(e, $pars, coords, sc.popupMenuAttrs, $area, "area");
    };

    sc.startArea = (e, $par) => {
        sc.extendSelection($par);
    };

    sc.nameArea = (e, $pars) => {
        let $newArea = $('<div class="area" id="newarea" />');
        $newArea.attr("data-doc-id", sc.docId);
        sc.selection.pars.wrapAll($newArea);

        $newArea = $("#newarea");
        const $popup = $("<name-area>");
        $popup.attr("tim-draggable-fixed", "");
        $popup.attr("onok", "nameAreaOk");
        $popup.attr("oncancel", "nameAreaCancel");
        $newArea.prepend($popup);

        $compile($popup[0])(sc);
    };

    sc.nameAreaOk = async ($area, areaName, options) => {
        $area.attr("data-name", areaName);

        try {
            await $http.post("/name_area/" + sc.docId + "/" + areaName, {
                area_start: getFirstParId($area.first()),
                area_end: getLastParId($area.last()),
                options,
            });
            //$area.children().wrapAll('<div class="areaContent">');
            //$area.append('<div class="areaeditline1">');
            //if (options.collapsible)
            sc.reload();
        } catch (e) {
            $window.alert(e.data.error);
            sc.nameAreaCancel($area);
        }
    };

    sc.nameAreaCancel = ($area) => {
        $area.children().unwrap();
    };

    sc.cancelArea = (e, $par) => {
        sc.selection.start = null;
        sc.selection.end = null;
    };

    sc.removeAreaMarking = async (e, $pars) => {
        const areaName = sc.selectedAreaName;
        if (!areaName) {
            $window.alert("Could not get area name");
        }

        try {
            await $http.post("/unwrap_area/" + sc.docId + "/" + areaName, {});
            sc.reload();
        } catch (e) {
            $window.alert(e.data.error);
        }
    };
}
