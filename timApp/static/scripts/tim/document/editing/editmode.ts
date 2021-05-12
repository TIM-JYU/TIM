import $ from "jquery";
import {documentglobals} from "tim/util/globals";
import {getParContainerElem} from "tim/document/structure/parsing";
import {EditMode} from "../popup-menu-dialog.component";

export function watchEditMode(newVal: EditMode | null) {
    documentglobals().editMode = newVal;
    showHidden(newVal);
}

function showHidden(showParam: EditMode | null) {
    const displayValue = showParam == null ? "none" : "initial";
    const displayValueBlock = showParam == null ? "" : "block";

    const cont = getParContainerElem();
    if (cont) {
        if (showParam === null) {
            cont.classList.remove("editmode");
        } else {
            cont.classList.add("editmode");
        }
    }

    // css hide/show page break for edit mode
    $("#CSSpagebreak > p").css("display", displayValue);
    $(".printpagebreak").css("display", displayValueBlock);
}
