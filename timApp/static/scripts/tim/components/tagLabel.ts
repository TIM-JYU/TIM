/*
 * A simple label for displaying tags.
 */

import {IController} from "angular";
import {timApp} from "../app";
import {Binding} from "../utils";

class TagLabelCtrl implements IController {
    public tagText!: Binding<string, "<">;
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
