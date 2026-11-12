import {Component} from "@angular/core";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import type {ViewCtrl} from "tim/document/viewctrl";

@Component({
    selector: "tim-help-par-content",
    template: `
        <div class="text-center text-smaller">
          @if (!hasText && !canEdit()) {
            <div>
              This document is currently empty.
            </div>
          }
          @if (showSettingsYaml) {
            <div>
              <p>Since this document has no text content, the settings paragraphs are visible above.</p>
            </div>
          }
          @if (showHelp) {
            <div>
              <p>You can get help with editing from editor's Help tab.</p>
              <p>This is an automatically added help paragraph. It will disappear when you add content.</p>
            </div>
          }
        </div>
        `,
    standalone: false,
})
export class HelpParContent {
    private vctrl!: ViewCtrl;
    showSettingsYaml = false;
    showHelp = false;
    hasText = false;

    ngOnInit() {
        this.vctrl = vctrlInstance!;
        const eh = this.vctrl.editingHandler;
        const spars = eh.findSettingsPars();
        this.hasText = eh.hasNonSettingsPars();
        this.showHelp = !this.hasText && this.canEdit();
        this.showSettingsYaml = spars !== undefined && this.showHelp;
    }

    canEdit() {
        return this.vctrl.item.rights.editable;
    }

    editSettings() {
        this.vctrl.editingHandler.editSettingsPars();
    }
}
