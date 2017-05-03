import Editor = AceAjax.Editor;
import VirtualRenderer = AceAjax.VirtualRenderer;

// Ace editor typings are slightly incomplete, so we extend them here.
interface IAceVirtualRenderer extends VirtualRenderer {
    setScrollMargin(top: number, bottom: number, left: number, right: number): void;
    setVScrollBarAlwaysVisible(visible: boolean): void;
}

export interface IAceEditor extends Editor {
    renderer: IAceVirtualRenderer;
    setFontSize(size: number | string);
}

export interface IAce extends AceAjax.Ace {
    UndoManager: { new(): AceAjax.UndoManager };
}
