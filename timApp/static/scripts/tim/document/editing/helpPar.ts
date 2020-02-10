import {IController} from "angular";
import {timApp} from "../../app";
import {$http} from "../../util/ngimport";
import {Require, to} from "../../util/utils";
import {getParId, isActionablePar} from "../parhelpers";
import {ViewCtrl} from "../viewctrl";

timApp.component("timHelpParContent", {
    bindings: {},
    controller: class implements IController {
        private vctrl!: Require<ViewCtrl>;
        private showSettingsYaml = false;
        private showHelp = false;
        private hasText = false;

        $onInit() {
            const eh = this.vctrl.editingHandler;
            const spars = eh.findSettingsPars();
            this.hasText = eh.hasNonSettingsPars();
            this.showHelp = !this.hasText && this.canEdit();
            this.showSettingsYaml = spars.length > 0 && this.showHelp;
        }

        canEdit() {
            return this.vctrl.item.rights.editable;
        }

        editSettings() {
            this.vctrl.editingHandler.editSettingsPars();
        }
    },
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="text-center text-smaller">
    <div ng-if="!$ctrl.hasText && !$ctrl.canEdit()">
        This document is currently empty.
    </div>
    <div ng-if="::$ctrl.showSettingsYaml">
        <p>Since this document has no text content, the settings paragraphs are visible above.</p>
    </div>
    <div ng-if="$ctrl.showHelp">
        <p>Click left side to edit. You can get help with editing from editor's Help tab.</p>
        <p>This is an automatically added help paragraph. It will disappear when you add content.</p>
    </div>
</div>
    `,
});

// TODO This component is unused for now.
timApp.component("timSettingsPar", {
    bindings: {},
    controller: class implements IController {
        static $inject = ["$element"];
        private vctrl!: Require<ViewCtrl>;
        private showSettingsYaml = false;
        private yaml?: string;

        constructor(private element: JQLite) {

        }

        async $onInit() {
            const p = this.element.parents(".par");
            this.showSettingsYaml = !this.vctrl.editingHandler.hasNonSettingsPars()
                && this.vctrl.item.rights.editable
                && isActionablePar(p);
            if (!this.showSettingsYaml) {
                return;
            }

            const parId = getParId($(p));
            const r = await to($http.get<{text: string}>("/getBlock", {
                params: {
                    doc_id: this.vctrl.item.id,
                    par_id: parId,
                    use_exported: false,
                },
            }));
            if (r.ok) {
                this.yaml = r.result.data.text;
            }
        }
    },
    require: {
        vctrl: "^timView",
    },
    template: `
<pre ng-if="$ctrl.showSettingsYaml" ng-bind="::$ctrl.yaml"></pre>
    `,
});
