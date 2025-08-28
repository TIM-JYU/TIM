import type {AreaStartPar} from "tim/document/structure/area";
import {getCollapseControlForElement} from "tim/document/areas";

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
        this.setCollapseState(!this.isCollapsed());
    }

    setCollapseState(collapsed: boolean) {
        const elem = this.titlepar.par.htmlElement;
        let newClass = "areacollapse";
        const toggle = this.toggleElem;
        const area = this.titlepar.par.htmlElement.nextElementSibling!;
        if (!collapsed) {
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

    toggleGroup() {
        const toggleGroup = this.titlepar.par.htmlElement.dataset.toggleGroup;
        if (!toggleGroup) {
            this.toggle();
            return;
        }

        const toggleGroupCollection =
            this.titlepar.par.htmlElement.dataset.toggleGroupCollection;

        const areaTogglesInGroup = document.querySelectorAll(
            `.paragraphs div[data-toggle-group="${toggleGroup}"] .areatoggle`
        );
        const areaCollapseControls: [string | undefined, CollapseControls][] =
            [];
        for (const at of areaTogglesInGroup) {
            if (!at.parentElement) {
                continue;
            }
            const areaCollapse = getCollapseControlForElement(at.parentElement);
            if (!areaCollapse) {
                continue;
            }
            areaCollapseControls.push([
                at.parentElement.dataset.toggleGroupCollection,
                areaCollapse,
            ]);
        }

        const newState = !this.isCollapsed();
        for (const [acToggleGroupCollection, ac] of areaCollapseControls) {
            if (toggleGroupCollection) {
                if (acToggleGroupCollection == toggleGroupCollection) {
                    ac.setCollapseState(newState);
                } else {
                    ac.setCollapseState(false);
                }
            } else {
                ac.setCollapseState(newState);
            }
        }
    }

    isCollapsed() {
        const elem = this.titlepar.par.htmlElement;
        return elem.classList.contains("areacollapse");
    }
}
