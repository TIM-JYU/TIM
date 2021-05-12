import {AreaStartPar} from "tim/document/structure/area";

/**
 * Controls for a collapsible {@link Area}.
 */
export class CollapseControls {
    private readonly toggleElem: Element;

    constructor(public titlepar: AreaStartPar) {
        const toggle = titlepar.par.htmlElement.querySelector(".areatoggle");
        if (!toggle) {
            throw Error("area toggle not found");
        }
        this.toggleElem = toggle;
    }

    toggle() {
        const elem = this.titlepar.par.htmlElement;
        let newClass = "areacollapse";
        const toggle = this.toggleElem;
        const area = this.titlepar.par.htmlElement.nextElementSibling!;
        if (elem.classList.contains("areacollapse")) {
            newClass = "areaexpand";
            area.classList.add("collapsed");
            toggle.className = "areatoggle glyphicon glyphicon-plus";
        } else {
            area.classList.remove("collapsed");
            toggle.className = "areatoggle glyphicon glyphicon-minus";
        }
        elem.classList.remove("areaexpand", "areacollapse");
        elem.classList.add(newClass);
    }

    isCollapsed() {
        return this.toggleElem.classList.contains("glyphicon-plus");
    }
}
