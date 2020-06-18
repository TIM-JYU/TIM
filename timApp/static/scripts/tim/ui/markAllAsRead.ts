import {IController} from "angular";
import {timApp} from "../app";
import {to} from "../util/utils";
import {getActiveDocument} from "../document/activedocument";
import {$http} from "../util/ngimport";
import {showMessageDialog} from "./dialog";

export class MarkAllAsReadCtrl implements IController {
    private buttonText?: string;
    private itemId!: number;

    $onInit() {
        if (!this.buttonText) {
            this.buttonText = "Mark all as read";
        }
    }

    async clicked() {
        if (this.itemId) {
            const r = await to($http.put("/read/" + this.itemId, {}));
            if (!r.ok) {
                await showMessageDialog("Could not mark the document as read.");
                return;
            }
            $(".readline").attr("class", "readline read");
            getActiveDocument().refreshSectionReadMarks();
        }
    }
}

timApp.component("timMarkAllAsRead", {
    bindings: {
        buttonText: "<?",
        itemId: "<",
    },
    controller: MarkAllAsReadCtrl,
    template: `
<button class="timButton" style="font-size: x-small;margin-right: 0px;display: block;margin-left: auto;" ng-click="$ctrl.clicked()">{{ ::$ctrl.buttonText }}</button>
    `,
});