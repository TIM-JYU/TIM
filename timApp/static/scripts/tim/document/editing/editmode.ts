import $ from "jquery";
import {documentglobals} from "../../util/globals";
import {EditMode} from "../popup-menu-dialog.component";

export function watchEditMode(
    newVal: EditMode | null,
    oldVal: string | null | undefined
) {
    documentglobals().editMode = newVal;
    $(".editmode").removeClass("editmode");

    if (newVal == null) {
        $(".parEditButton").removeClass("active");
        $(".areaEditButton").removeClass("active");
        enableParEdit();
        enableAreaEditPassive();
    } else if (newVal === "par") {
        $(".parEditButton").addClass("active");
        $(".areaEditButton").removeClass("active");
        enableParEdit();
        disableAreaEdit();
        // $('.par').addClass('editmode');
    } else if (newVal === "area") {
        $(".parEditButton").removeClass("active");
        $(".areaEditButton").addClass("active");
        disableParEdit();
        enableAreaEditActive();
        // $('.area').addClass('editmode');
    }

    showHidden(newVal);
}

function enableParEdit() {
    $(".editline-disabled")
        .removeClass("editline-disabled")
        .addClass("editline");
}

function disableParEdit() {
    $(".editline").removeClass("editline").addClass("editline-disabled");
}

function enableAreaEditActive() {}

function enableAreaEditPassive() {}

function disableAreaEdit() {}

function showHidden(showParam: string | null) {
    const displayValue = showParam == null ? "none" : "initial";
    const displayValueBlock = showParam == null ? "" : "block";
    $(".mdcontent").css("display", displayValue);

    // css hide/show page break for edit mode
    $("#CSSpagebreak > p").css("display", displayValue);
    $(".printpagebreak").css("display", displayValueBlock);
}
