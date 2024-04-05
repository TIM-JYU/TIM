export function staticDynamicImport(s: string) {
    switch (s) {
        case "angular-ui-grid":
            return import("angular-ui-grid");
        case "/jsrunner/javascripts/build/jsrunner.js":
            return import(
                "modules/jsrunner/client/javascripts/jsrunner-plugin.component"
            );
        case "/field/js/build/numericfield.js":
            return import("modules/fields/js/numericfield-plugin.component");
        case "/field/js/build/textfield.js":
            return import("modules/fields/js/textfield-plugin.component");
        case "/pali/js/build/pali.js":
            return import("modules/pali/client/pali");
        case "reviewcanvas":
            return import("tim/plugin/reviewcanvas/review-canvas.component");
        case "tableForm":
            return import("tim/plugin/tableForm/table-form.component");
        case "tim/plugin/imagex":
            return import("tim/plugin/imagex/imagex.component");
        case "tim/document/slide":
            return import("tim/document/slide");
        case "qst":
            return import("tim/plugin/qst/qst.component");
        case "tape":
            return import("tim/plugin/tape/tape-plugin.component");
        case "importData":
            return import("tim/plugin/import-data/import-data.component");
        case "timTable":
            return import("tim/plugin/timTable/tim-table.component");
        case "cbcountfield":
            return import("modules/fields/js/cbcountfield-plugin.component");
        case "timMenu":
            return import("tim/plugin/tim-menu/tim-menu-plugin.component");
        case "/cs/js/build/csModule.js":
            return import("modules/cs/js/csModule");
        case "/cs/js/build/stack.js":
            return import("modules/cs/js/stack");
        case "/cs/js/build/geogebra.js":
            return import("modules/cs/js/geogebra");
        case "/cs/js/build/jsav.js":
            return import("modules/cs/js/jsav");
        case "/cs/js/build/jsframe.js":
            return import("modules/cs/js/jsframe");
        case "/field/js/build/multisave.js":
            return import("modules/fields/js/multisave");
        case "/svn/js/video.js":
            return import("modules/svn/js/video.component");
        case "/svn/js/images.js":
            return import("modules/svn/js/images.component");
        case "/field/js/build/rbfield.js":
            return import("modules/fields/js/rbfield-plugin.component");
        case "/field/js/build/cbfield.js":
            return import("modules/fields/js/cbfield-plugin.component");
        case "/field/js/build/dropdown.js":
            return import("modules/fields/js/dropdown-plugin.component");
        case "/drag/js/build/drag.js":
            return import("modules/drag/client/drag");
        case "/feedback/js/build/feedback.js":
            return import("modules/feedback/js/feedback");
        case "/field/js/build/goaltable.js":
            return import("modules/fields/js/goaltable-plugin.component");
        // Redirect mmcq scripts.
        case "/mmcq/script2.js":
        case "/mmcq/SimpleDirective.js":
        case "/mcq/script2.js":
        case "/mcq/SimpleDirective.js":
            return import("tim/plugin/mmcq/mmcq");
        case "lectureMenu":
            return import("tim/lecture/lecture-menu.component");
        case "lectureInfo":
            return import("tim/lecture/lecture-info.component");
        case "userSelect":
            return import("tim/plugin/userselect/user-select.component");
        case "timMessageListManagement":
            return import("tim/messaging/manage/message-list-manage.module");
        case "timExamGroupManager":
            return import(
                "tim/plugin/examGroupManager/exam-group-manager.component"
            );
        case "oauthAuthorize":
            return import("tim/user/oauth-authorize.component");
        case "timArchive":
            return import("tim/messaging/archive/tim-archive.module");
        case "slideView":
            return import("tim/item/slide-view.component");
        case "userActionVerify":
            return import("tim/user/user-action-verify.component");
        case "userSettings":
            return import("tim/user/settings.component");
        case "stylePreview":
            return import("tim/ui/style-preview.component");
        case "messageSend":
            return import("tim/messaging/tim-message-send.component");
        case "calendar":
            return import("tim/plugin/calendar/calendar.component");
        case "groupJoin":
            return import("tim/plugin/groupJoin/group-join.component");
        case "quantumCircuit":
            return import(
                "tim/plugin/quantumcircuit/quantum-circuit.component"
            );
        case "symbolbutton":
            return import(
                "tim/plugin/symbolbutton/symbol-button-plugin.component"
            );
    }
    throw Error(`Module was not statically known: ${s}`);
}
