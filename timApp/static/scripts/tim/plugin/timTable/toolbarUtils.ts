import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {
    IToolbarTemplate,
    TimTableComponent,
} from "tim/plugin/timTable/tim-table.component";
import type {
    ITimTableEditorToolbarParams,
    TimTableEditorToolbarDialogComponent as TimTableEditorToolbarDialogComponentType,
} from "tim/plugin/timTable/tim-table-editor-toolbar-dialog.component";

export let instance: TimTableEditorToolbarDialogComponentType | undefined;

export function setToolbarInstance(
    t: TimTableEditorToolbarDialogComponentType | undefined
) {
    instance = t;
}

export function isToolbarEnabled() {
    return true;
}

export function handleToolbarKey(
    ev: KeyboardEvent,
    toolBarTemplates: IToolbarTemplate[] | undefined
) {
    if (!instance || !toolBarTemplates) {
        return false;
    }
    for (const templ of toolBarTemplates) {
        if (!templ || !templ.shortcut) {
            continue;
        }
        const k = templ.shortcut.split("+");
        let key = k[0];
        let ekey = ev.key;
        if (ekey.charCodeAt(0) === 8211) {
            ekey = "-";
        } // Mac Slash
        let mod = "";
        if (k.length >= 2) {
            mod = k[0];
            key = k[1];
        }
        if (mod === "" && key.length === 1 && instance.callbacks.isEdit()) {
            continue; // ei käytetä yhden näppäimen shortcutteja jos ollaan editissä
        }
        if (templ.chars) {
            // regexp for keys to use
            if (ekey.match("^" + templ.chars + "$")) {
                templ.cell = ekey;
                instance.applyTemplate(templ);
                return true;
            }
            continue;
        }
        const evmod =
            (ev.altKey ? "a" : "") +
            (ev.ctrlKey ? "c" : "") +
            (ev.shiftKey ? "s" : "");
        if (mod != evmod) {
            continue;
        }
        let code = ev.code;
        if (code.startsWith("Key")) {
            code = code.substr(3).toLowerCase();
        }
        if (ekey === key || code === key) {
            instance.applyTemplate(templ);
            return true;
        }
    }
    return false;
}

export function isToolbarOpen() {
    return !!instance;
}

export async function showTableEditorToolbar(p: ITimTableEditorToolbarParams) {
    if (instance && !instance.isClosed()) {
        if (instance.activeTable == p.activeTable) {
            instance.show(p.callbacks, p.activeTable);
            return;
        }
        instance.hideThis(false);
    }
    const {TimTableEditorToolbarDialogComponent} = await import(
        "./tim-table-editor-toolbar-dialog.component"
    );
    await angularDialog.open(TimTableEditorToolbarDialogComponent, p);
}

export function hideToolbar(closingTable: TimTableComponent) {
    if (instance) {
        instance.hideThis(instance.activeTable != closingTable);
    }
}
