/*
 * A simple label for displaying tags.
 */

import {IController} from "angular";
import {IItem} from "../IItem";
import {to} from "tim/utils";
import {timApp} from "../app";
import {showMessageDialog} from "../dialog";
import {$http, $window} from "../ngimport";

class TagLabelCtrl implements IController {
    public tagText: string;
    private doc: IItem;

    private async loadTemplate(t: IItem) {
        const [err] = await to($http.post("/update/" + this.doc.id, {
            template_name: t.path,
        }));
        if (err) {
            await showMessageDialog(err.data.error);
        } else {
            $window.location.reload();
        }
    }

   async $onInit() {
    }

}

timApp.component("tagLabel", {
    bindings: {
        tagText: "<",
    },
    controller: TagLabelCtrl,
    template: `
        <span class="btn-primary btn-xs">{{$ctrl.tagText}}</span>
    `,
});
