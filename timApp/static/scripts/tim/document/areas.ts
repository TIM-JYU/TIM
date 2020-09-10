import {IScope} from "angular";
import $ from "jquery";
import * as nameArea from "tim/document/editing/nameArea";
import {markAsUsed, to} from "tim/util/utils";
import {showMessageDialog} from "../ui/dialog";
import {$http, $timeout} from "../util/ngimport";
import {INameAreaOptions, showNameAreaDialog} from "./editing/nameArea";
import {onClick, onMouseOverOut} from "./eventhandlers";
import {
    Area,
    getArea,
    getFirstParId,
    getLastParId,
    Paragraph,
    Paragraphs,
} from "./parhelpers";
import {ViewCtrl} from "./viewctrl";

markAsUsed(nameArea);

function selectArea(areaName: string, className: string, selected: boolean) {
    const selection = $(".area.area_" + areaName).children(className);
    if (selected) {
        selection.addClass("manualhover");
    } else {
        selection.removeClass("manualhover");
    }
}

export class AreaHandler {
    public selectedAreaName: string | undefined;
    public sc: IScope;
    public viewctrl: ViewCtrl;

    constructor(sc: IScope, view: ViewCtrl) {
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

        onClick(".areaeditline1", ($this, e) =>
            this.onAreaEditClicked($this, e, ".areaeditline1")
        );

        onClick(".areaeditline2", ($this, e) =>
            this.onAreaEditClicked($this, e, ".areaeditline2")
        );

        onClick(".areaeditline3", ($this, e) =>
            this.onAreaEditClicked($this, e, ".areaeditline3")
        );

        onClick(".areaexpand, .areacollapse", ($this, e) => {
            if (
                $(e.target).hasClass("areareadline") ||
                $(e.target).hasClass("readline") ||
                $(e.target).hasClass("editline")
            ) {
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

    onAreaEditClicked(
        $this: JQuery,
        e: JQuery.MouseEventBase,
        className: string
    ) {
        this.viewctrl.closePopupIfOpen();
        const areaName = $this.attr("data-area");
        if (!areaName) {
            return;
        }
        const pars = getArea(areaName).find(".par");
        const areaPart = $this.parent().filter(".area");

        this.selectedAreaName = areaName;
        $(".area.area_" + areaName)
            .children(className)
            .addClass("menuopen");

        // We need the timeout so we don't trigger the ng-clicks on the buttons
        $timeout(() => {
            this.showAreaOptionsWindow(e, areaPart, pars);
        }, 80);
    }

    showAreaOptionsWindow(
        e: JQuery.MouseEventBase,
        $area: Area,
        $pars: Paragraphs
    ) {
        this.viewctrl.parmenuHandler.showPopupMenu(
            e,
            $pars,
            this.viewctrl.parmenuHandler.getPopupAttrs(),
            "area"
        );
    }

    startArea(e: JQuery.Event, par: Paragraph) {
        this.viewctrl.editingHandler.extendSelection(par);
    }

    async nameArea(e: JQuery.Event, $pars: Paragraphs) {
        if (!this.viewctrl.selection.pars) {
            return;
        }
        const result = await showNameAreaDialog();
        await this.nameAreaOk(
            this.viewctrl.selection.pars,
            result.areaName,
            result.options
        );
    }

    async nameAreaOk($area: Area, areaName: string, options: INameAreaOptions) {
        const r = await to(
            $http.post("/name_area/" + this.viewctrl.docId + "/" + areaName, {
                area_start: getFirstParId($area.first()),
                area_end: getLastParId($area.last()),
                options,
            })
        );
        if (r.ok) {
            this.viewctrl.reload();
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    cancelArea() {
        this.viewctrl.selection.start = undefined;
        this.viewctrl.selection.end = undefined;
    }

    async removeAreaMarking(e: JQuery.Event, $pars: Paragraph) {
        const areaName = this.selectedAreaName;
        if (!areaName) {
            await showMessageDialog("Could not get area name");
        }

        const r = await to(
            $http.post(
                "/unwrap_area/" + this.viewctrl.docId + "/" + areaName,
                {}
            )
        );
        if (r.ok) {
            this.viewctrl.reload();
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }
}
