function watchEditMode(newVal, oldVal, $scope) {
    window.editMode = newVal;
    $('.editmode').removeClass('editmode');

    if (newVal === null) {
        $('.parEditButton').removeClass('active');
        $('.areaEditButton').removeClass('active');
        enable_par_edit();
        enable_area_edit_passive();
    } else if (newVal === "par") {
        $('.parEditButton').addClass('active');
        $('.areaEditButton').removeClass('active');
        enable_par_edit();
        disable_area_edit();
        //$('.par').addClass('editmode');
    } else if (newVal === "area") {
        $('.parEditButton').removeClass('active');
        $('.areaEditButton').addClass('active');
        disable_par_edit();
        enable_area_edit_active();
        //$('.area').addClass('editmode');
    }

    show_hidden(newVal);
}

function enable_par_edit() {
    $('.editline-disabled').removeClass('editline-disabled').addClass('editline');
}

function disable_par_edit () {
    $('.editline').removeClass('editline').addClass('editline-disabled');
}

function enable_area_edit_active() {
}

function enable_area_edit_passive() {
}

function disable_area_edit() {
}

function show_hidden(show_param) {
    var display_value = show_param == null ? 'none' : 'initial';
    $('.mdcontent').css('display', display_value)
}
