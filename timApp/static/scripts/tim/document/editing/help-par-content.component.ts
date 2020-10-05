import {Component} from "@angular/core";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {ViewCtrl} from "../viewctrl";

@Component({
    selector: "tim-help-par-content",
    template: `
        <div class="text-center text-smaller">
            <div *ngIf="!hasText && !canEdit()">
                This document is currently empty.
            </div>
            <div *ngIf="showSettingsYaml">
                <p>Since this document has no text content, the settings paragraphs are visible above.</p>
            </div>
            <div *ngIf="showHelp">
                <p>Click left side to edit. You can get help with editing from editor's Help tab.</p>
                <p>This is an automatically added help paragraph. It will disappear when you add content.</p>
            </div>
        </div>
    `,
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
        this.showSettingsYaml = spars.length > 0 && this.showHelp;
    }

    canEdit() {
        return this.vctrl.item.rights.editable;
    }

    editSettings() {
        this.vctrl.editingHandler.editSettingsPars();
    }
}
