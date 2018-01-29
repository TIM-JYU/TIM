import $ from "jquery";
import {addEvents} from "tim/marktree";
import {$timeout} from "../../ngimport";

export function initIndex() {
    // call marktree.js initialization function so that TOC clicking works
    addEvents();
    $timeout(() => {
        const indexHeadings = $("#menuTabs").find(".subexp .exp");
        const subHeadings = indexHeadings.find("ul.sub li.basic");
        if (indexHeadings.length === 1 || indexHeadings.length + subHeadings.length < 40) {
            indexHeadings.attr("class", "col");
            indexHeadings.children(".sub").attr("class", "subexp");
        }
    });
}
