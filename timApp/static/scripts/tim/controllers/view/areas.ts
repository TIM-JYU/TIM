import {IScope} from "angular";
import $ from "jquery";
import * as nameArea from "tim/directives/nameArea";
import {Coords, markAsUsed} from "tim/utils";
import {$compile, $http, $timeout, $window} from "../../ngimport";
import {onClick, onMouseOverOut} from "./eventhandlers";
import {Area, getArea, getFirstParId, getLastParId, Paragraph, Paragraphs} from "./parhelpers";
import {closeOptionsWindow} from "./parmenu";
import {ViewCtrl} from "./ViewCtrl";

markAsUsed(nameArea);

export interface INameAreaOptions {
    collapsible: boolean;
    timed: boolean;
    starttime: string;
    endtime: string;
    alttext: string;
    collapse: boolean;
    hlevel: number;
    title: string;
}

function selectArea(areaName: string, className: string, selected: boolean) {
    const $selection = $(".area.area_" + areaName).children(className);
    if (selected) {
        $selection.addClass("manualhover");
    } else {
        $selection.removeClass("manualhover");
    }
}

export class AreaHandler {
    public selectedAreaName: string;
    public sc: IScope;
    public viewctrl: ViewCtrl;

    initAreas(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;

        onMouseOverOut(".areaeditline1", ($this, e, select) => {
            const areaName = $this.attr("data-area");
            if (!areaName) {
                return;
            }
            selectArea(areaName, ".areaeditline1", select);
        });

        onMouseOverOut(".areaeditline2", ($this, e, select) => {
            const areaName = $this.attr("data-area");
            if (!areaName) {
                return;
            }
            selectArea(areaName, ".areaeditline2", select);
        });

        onMouseOverOut(".areaeditline3", ($this, e, select) => {
            const areaName = $this.attr("data-area");
            if (!areaName) {
                return;
            }
            selectArea(areaName, ".areaeditline3", select);
        });

        onClick(".areaeditline1", ($this, e) => this.onAreaEditClicked($this, e, ".areaeditline1"));

        onClick(".areaeditline2", ($this, e) => this.onAreaEditClicked($this, e, ".areaeditline2"));

        onClick(".areaeditline3", ($this, e) => this.onAreaEditClicked($this, e, ".areaeditline3"));

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
            if (!areaName) {
                return;
            }
            const toggle = $this.children(".areatoggle");
            toggle.attr("class", "");
            const area = getArea(areaName);
            if (expanding) {
                area.removeClass("collapsed");
                $(".areawidget_" + areaName).removeClass("collapsed");
                toggle.attr("class", "areatoggle glyphicon glyphicon-minus");
            } else {
                area.addClass("collapsed");
                $(".areawidget_" + areaName).addClass("collapsed");
                toggle.attr("class", "areatoggle glyphicon glyphicon-plus");
            }

            $this.addClass(newClass);
        });
    }

    onAreaEditClicked($this: JQuery, e: JQueryEventObject, className: string) {
        closeOptionsWindow();
        const areaName = $this.attr("data-area");
        if (!areaName) {
            return;
        }
        const $pars = getArea(areaName).find(".par");
        const $areaPart = $this.parent().filter(".area");
        const offset = $areaPart.offset() || {left: 0, top: 0};
        const coords = {left: e.pageX - offset.left, top: e.pageY - offset.top};

        this.selectedAreaName = areaName;
        $(".area.area_" + areaName).children(className).addClass("menuopen");

        // We need the timeout so we don't trigger the ng-clicks on the buttons
        $timeout(() => {
            this.showAreaOptionsWindow(e, $areaPart, $pars, coords);
        }, 80);
    }

    showAreaOptionsWindow(e: Event, $area: Area, $pars: Paragraphs, coords: Coords) {
        this.viewctrl.updateClipboardStatus();
        this.viewctrl.showPopupMenu(e, $pars, coords, this.viewctrl.popupMenuAttrs, $area, "area");
    }

    startArea(e: Event, $par: Paragraph) {
        this.viewctrl.extendSelection($par);
    }

    nameArea(e: Event, $pars: Paragraphs) {
        let $newArea = $('<div class="area" id="newarea" />');
        $newArea.attr("data-doc-id", this.viewctrl.docId);
        if (!this.viewctrl.selection.pars) {
            return;
        }
        this.viewctrl.selection.pars.wrapAll($newArea);

        $newArea = $("#newarea");
        const $popup = $("<name-area>");
        $popup.attr("tim-draggable-fixed", "");
        $popup.attr("onok", "vctrl.nameAreaOk");
        $popup.attr("oncancel", "vctrl.nameAreaCancel");
        $newArea.prepend($popup);

        $compile($popup[0])(this.sc);
    }

    async nameAreaOk($area: Area, areaName: string, options: INameAreaOptions) {
        $area.attr("data-name", areaName);

        try {
            await $http.post("/name_area/" + this.viewctrl.docId + "/" + areaName, {
                area_start: getFirstParId($area.first()),
                area_end: getLastParId($area.last()),
                options,
            });
            //$area.children().wrapAll('<div class="areaContent">');
            //$area.append('<div class="areaeditline1">');
            //if (options.collapsible)
            this.viewctrl.reload();
        } catch (e) {
            $window.alert(e.data.error);
            this.nameAreaCancel($area);
        }
    }

    nameAreaCancel($area: Area) {
        $area.children().unwrap();
    }

    cancelArea(e: Event, $par: Paragraph) {
        this.viewctrl.selection.start = null;
        this.viewctrl.selection.end = null;
    }

    async removeAreaMarking(e: Event, $pars: Paragraph) {
        const areaName = this.selectedAreaName;
        if (!areaName) {
            $window.alert("Could not get area name");
        }

        try {
            await
                $http.post("/unwrap_area/" + this.viewctrl.docId + "/" + areaName, {});
            this.viewctrl.reload();
        } catch (e) {
            $window.alert(e.data.error);
        }
    }
}
