import $ from "jquery";
export function watchEditMode(newVal, oldVal, $scope) {
    const w: any = window;
    w.editMode = newVal;
    $(".editmode").removeClass("editmode");

    if (newVal === null) {
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
    $(".editline-disabled").removeClass("editline-disabled").addClass("editline");
}

function disableParEdit() {
    $(".editline").removeClass("editline").addClass("editline-disabled");
}

function enableAreaEditActive() {
}

function enableAreaEditPassive() {
}

function disableAreaEdit() {
}

function showHidden(showParam) {
    const displayValue = showParam == null ? "none" : "initial";
    $(".mdcontent").css("display", displayValue);
}
