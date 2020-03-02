// Tässä toteutetaan virheen merkkaava ja korjausehdotukset näyttävä elementti

import {IController} from "angular";
import {timApp} from "../../app";
import {PareditorController} from "../../editor/pareditor";
import {copyToClipboard, Require} from "../../util/utils";
import {ViewCtrl} from "../viewctrl";
import {showSpellErrorDialog} from "./spellErrorDialog";

class SpellErrorController implements IController {
        static $inject = ["$element", "$scope"];
        private vctrl!: Require<ViewCtrl>;
        private wordID: number = -1;
        private word: string = "";
        private suggestions: string[] = [];
        private position: number = 0;
        private length: number = 0;
        private pare: PareditorController;

        async $onInit() {
            this.pare = this.vctrl.editingHandler.getParEditor() as PareditorController;
            this.wordID = this.wordid;
            this.getSuggestions();
        }

        async selectSuggestion(s: string) {
            copyToClipboard(s);
            // this.selectWord();
        }

        async replaceWord(s: string) {
            this.closeDropdown();
            this.pare.getEditor()!.focus();
            this.pare.getEditor()!.setPosition([this.position, this.position + this.length]);
            this.pare.getEditor().editor.replaceSelectedText(s);
            this.pare.editorChanged();
        }

        async closeDropdown() {
            angular.element(document).find(".dropdown-content").hide();
        }

        async selectWord(e) {
            const sep = {suggestions: this.suggestions, vc: this.vctrl, position: this.position, length: this.length, dialogX: e.clientX, dialogY: e.clientY};
            await showSpellErrorDialog(sep);
        }

        async ignoreWord() {
            this.pare.getEditor()!.surroundClicked("[", "]{.nospell}");
        }

        async getSuggestions() {
            const scdata = this.pare.spellcheckData;
            if (typeof scdata !== "undefined" && typeof scdata.words !== "undefined") {
                for (let i = 0; i < scdata.words.length; i++) {
                    if (scdata.words[i].id == Number(this.wordID)) {
                        this.word = scdata.words[i].word;
                        this.position = scdata.words[i].position;
                        this.length = scdata.words[i].length;
                        this.suggestions = scdata.words[i].suggestions;
                    }
                }
            }
        }
    }

    timApp.component("spellError", {
    controller: SpellErrorController,
    require: {
        vctrl: "^timView",
    },
    bindings: { text: "@", wordid: "="},
    template: `
<style>
.dropdown-error {
  position: relative;
  display: inline-block;
  border-bottom-style: solid;
  border-bottom-color: red;
}
</style>

<span class="dropdown-error" ng-click="$ctrl.selectWord($event)">{{$ctrl.text}}</span>`,
});
