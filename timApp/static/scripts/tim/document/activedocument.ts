import type {TimDocument} from "tim/document/timDocument";

let activeDocument: TimDocument | null = null;

export function setActiveDocument(d: TimDocument) {
    activeDocument = d;
}

/**
 * Returns whether the document structure has been built, i.e. whether getActiveDocument can be called.
 */
export function hasActiveDocument() {
    return activeDocument != null;
}

export function getActiveDocument(): TimDocument {
    if (activeDocument == null) {
        throw new Error(
            "Active document was null; getActiveDocument was probably called before setActiveDocument was called"
        );
    }
    return activeDocument;
}
