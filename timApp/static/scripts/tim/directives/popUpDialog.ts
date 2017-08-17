import {IRootElementService, IController} from "angular";
import {timApp} from "tim/app";

/**
 * FILL WITH SUITABLE TEXT
 * @module popUpDialog
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

function toDragOrNot() {
    const width = (window.innerWidth > 0) ? window.innerWidth : screen.width;
    if (width > 500) {
        return 'tim-draggable-fixed caption="{{caption}}"';
    }
}

class PopupDialogController implements IController {
    private static $inject = ["$element"];
    private element: IRootElementService;
    private mouseDownX: number;
    private mouseDownY: number;

    constructor(element: IRootElementService) {
        this.element = element;
    }

    $onInit() {

    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:popUpDialog
     * @param e
     */
    checkDown(e: MouseEvent) {
        this.mouseDownX = e.clientX;
        this.mouseDownY = e.clientY;
        const window = this.element.find("popUpBack");
        const ctx = window.context as HTMLElement;
        ctx.style.position = "absolute";
        ctx.style.bottom = "auto";

    }

    /**
     * FILL WITH SUITABLE TEXT
     * @memberof module:popUpDialog
     */
    checkUp() {
        const window = this.element.find("popUpBack");
        const ctx = window.context as HTMLElement;
        ctx.style.position = "fixed";
    }
}

timApp.component("popUpDialog", {
    bindings: {
        caption: "=",
        elemId: "=",
        show: "=",
    },
    controller: PopupDialogController,
    template: `<div class='pop-up' ng-show='show' id='popUpBack'>
    <div class='pop-up-overlay'></div>
    <div id='{{elemId}}'
         class='pop-up-dialog'
         ${toDragOrNot()}
         ng-mousedown='$ctrl.checkDown($event)'
         ng-mouseup='$ctrl.checkUp($event)'
         style='top:0; left: 0'>
        <div class='pop-up-dialog-content' ng-transclude>

        </div>
    </div>
</div>`,
    transclude: true,
});
